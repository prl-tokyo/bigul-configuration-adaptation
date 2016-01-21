{-# LANGUAGE KindSignatures, GADTs, Rank2Types, ImpredicativeTypes, FlexibleContexts #-}
module BiFlux.DTD.Subtyping where

import BiFlux.DTD.Type as BType
import Control.Monad.Except
import Data.List
import Text.PrettyPrint
import Control.Arrow ((&&&), (***))

-- Define the bidirectional conversion between Type a and Type b.
type Conversion a b = (a -> b, b -> Maybe a)

data Response :: * -> * -> * where
  Fail        :: Response a b
  Success     :: Conversion a b -> Response a b
  OnePremise  :: (Eq c, Eq d) => Type c -> Type d -> (Conversion c d -> Conversion a b) -> Response a b
  TwoPremises :: (Eq c0, Eq c1, Eq d0, Eq d1) =>
                   Type c0 -> Type d0 -> Type c1 -> Type d1 ->
                   (Conversion c0 d0 -> Conversion c1 d1 -> Conversion a b) -> Response a b

type Rule = (Eq a, Eq b) => Type a -> Type b -> Response a b

rules :: [Rule]
rules = [either_left, either_right, either_elim, pair_cong, list, singleton_list, empty_string]
  where
    reflexivity :: Rule
    --reflexivity ta tb = if ta == tb then Success (id, Just) else Fail
    reflexivity ta tb = case teq ta tb of
                             Just Eq -> Success (id, Just)
                             Nothing       -> Fail

    either_left :: Rule
    either_left ta (Either tb0 tb1) = OnePremise ta tb0 (\(u, d) -> (Left . u, either d (const Nothing)))

    either_right :: Rule
    either_right ta (Either tb0 tb1) = OnePremise ta tb1 (\(u, d) -> (Right . u, either (const Nothing) d))

    either_elim :: Rule
    either_elim (Either ta0 ta1) tb =
      TwoPremises ta0 tb ta1 tb (\(u0, d0) (u1, d1) -> (either u0 u1, uncurry mplus . ((liftM Left . d0) &&& (liftM Right . d1))))
    elither_elim _               _ = Fail

    pair_cong :: Rule
    pair_cong (Prod ta0 tb0) (Prod ta1 tb1) =
      TwoPremises ta0 ta1 tb0 tb1 (\(ua, da) (ub, db) -> (ua *** ub, uncurry (liftM2 (,)) . (da *** db)))
    pair_cong _              _              = Fail

    list :: Rule
    list (List ta) (List tb) = OnePremise ta tb (\(u, d) -> (map u, mapM d))
    list _         _         = Fail

    singleton_list :: Rule
    singleton_list ta (List tb) = OnePremise ta tb (\(u, d) -> ((:[]) . u, (\xs -> case xs of [x] -> d x; _ -> Nothing))) -- b must be a singleton.
    singleton_lst  _  _         = Fail

    empty_string :: Rule
    empty_string One String = Success (const (BType.Str ""), (\(BType.Str s) -> if null s then Just () else Nothing ))
    empty_string _   _      = Fail



backchain :: (Eq a, Eq b) => Type a -> Type b -> [Conversion a b]
backchain ta tb = do
  --rule <- rules -- !comment: why
  (rule :: Rule) <- rules
  case rule ta tb of
    Fail  -> []
    Success c -> return c
    OnePremise ta' tb' f -> liftM f (backchain ta' tb')
    TwoPremises ta0 tb0 ta1 tb1 f -> liftM2 f (backchain ta0 tb0) (backchain ta1 tb1)

isSubtypeOf :: (Eq a, Eq b) => Type a -> Type b -> Bool
isSubtypeOf ta tb = not(null (backchain ta tb))


ucast :: (MonadError Doc m, Eq a, Eq b) => Type a -> Type b -> a -> m b
ucast ta tb x =
  case backchain ta tb of
       [] -> throwError $ text (show ta) <+> text "cannot be dedeuced to a subtype of" <+> text (show tb)
       ((u, _) :_) -> return (u x)

dcast :: (MonadError Doc m, Eq a, Eq b) => Type a -> Type b -> b -> m a
dcast ta tb y =
  case backchain ta tb of
       [] -> throwError $ text (show ta) <+> text "cannot be dedeuced to a subtype of" <+> text (show tb)
       ((_, d): _) ->
                case d y of
                     Just x -> return x
                     Nothing -> throwError $ text "downcasting from" $+$ nest 2 (
                       text "value: " $+$ nest  2 (text (gshow tb y)) $+$ text "with type: " $+$ nest 2 (text (show tb)) $+$
                         text "to type: " $+$ nest 2 (text (show ta)) $+$ text "fails"
                       )


