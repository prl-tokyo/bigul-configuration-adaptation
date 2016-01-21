{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, FlexibleContexts, RankNTypes #-}
module BiFlux.DTD.Type where

import Text.XML.HaXml.DtdToHaskell.TypeDef hiding (List, Maybe, List1, Any, String, mkAtt)
import Text.XML.HaXml.XmlContent (XmlContent)
import GHC.Generics
import GHC.InOut
import Control.Monad
import BiFlux.DTD.TypeDef hiding (mkAtt)
import Data.List (intersperse, isPrefixOf)
import Text.PrettyPrint as PP (Doc, parens, brackets, comma, colon, text, punctuate, empty, (<>), (<+>), ($+$))
import Unsafe.Coerce
import qualified Data.Map as Map
import Lang.AST (Var, Var(..))
-- A wrap string type.
data Str = Str { unStr :: String }

instance Generic Str where
  type Rep Str = K1 R Str
  from x = K1 x
  to (K1 x) = x

instance Show Str where
  show (Str x) = show x

instance Eq Str where
  (Str x) == (Str y) = x == y

instance Ord Str where
  compare (Str x) (Str y) = compare x y

data DynType where
    DynT :: Eq a => Type a -> DynType

instance Show DynType where
  show (DynT t) = "DynT " ++ show t

instance Ord DynType where
  a <= b = show a <= show b

instance Eq DynType where
  (DynT a) == (DynT b) = case teq a b of { Just Eq -> True; otherwise -> False }


applyDynT :: (forall a.  Eq a => Type a -> b) -> DynType -> b
applyDynT f (DynT t) = f t

type TypeEnv = Map.Map String DynType

data Type a where
  Int :: Type Int
  Float :: Type Float
  String :: Type Str
  Bool :: Type Bool
  One :: Type ()
  Either :: (Eq a, Eq b) => Type a -> Type b -> Type (Either a b)
  Prod :: (Eq a, Eq b) => Type a -> Type b -> Type (a, b)
  List :: Eq a => Type a -> Type [a]
  Data :: (Eq a, Eq (F a), InOut a) => Name -> Type (F a) -> Type a
  List1 :: Eq a => Type a -> Type (a, [a])
  Tag :: (Eq a) => String -> Type a -> Type a
  Doc :: (Eq a) => Type a -> Type a
  VVar :: (Eq a) => Type a -> Type (Var a)
  -- DTD --> Type translated DTD's List1 to this List1

instance Show (Type a) where
  show Int          = "Int"
  show Float        = "Float"
  show String       = "String"
  show Bool         = "Bool"
  show One          = "One"
  show (Either a b) = "(Either " ++ show a ++ " " ++ show b ++ ")"
  show (Prod   a b) = "(Prod "   ++ show a ++ " " ++ show b ++ ")"
  show (List   a  ) = "(List "    ++ show a ++ ")"
  --TODO: Data Name is ax XML attribute
  show (Data (Name x n) t) = "((Data (Name " ++ show x ++ " " ++ show n ++ ") " ++ "typeof) :: Type " ++ n ++ ")"
  show (List1  a  ) = "(List1 " ++ show a ++ ")"
  show (Tag    x t) = "(Tag " ++ show x ++ " " ++ show t ++ ")"
  show (Doc    a  ) = "(Doc " ++ show a ++ ")"
  show (VVar    a  ) = "(Var " ++ show a  ++ ")"

instance (Eq a) =>  Eq (Var a) where
  (Var a) == (Var b) = a == b

instance Eq (Type a) where
  x == y =
    case teq x y of
         Just Eq -> True
         Nothing -> False

data Equal a b where
  Eq :: Equal a a

teq :: MonadPlus m => Type a -> Type b -> m (Equal a b)
teq Int              Int                = return Eq
teq Float            Float              = return Eq
teq String           String             = return Eq
teq Bool             Bool               = return Eq
teq One              One                = return Eq
teq (Either a b)     (Either c d)       = do
  Eq <- teq a c
  Eq <- teq b d
  return Eq
teq (Prod a b)       (Prod c d)         = do
  Eq <- teq a c
  Eq <- teq b d
  return Eq
teq (List a)        (List b)            = do
  Eq <- teq a b
  return Eq
teq (Data n a)      (Data m b)          = do
  guard (hName n == hName m)
  --teq a b
  --return Eq
  return (unsafeCoerce Eq)
teq (List1 a)       (List1 b)           = do
  Eq <- teq a b
  return Eq
teq (Tag m a)       (Tag n b)           = do
  guard (m == n)
  teq a b
teq (Doc a)         (Doc b)             = do
  Eq <- teq a b
  return Eq
teq (VVar a)        (VVar b)            = do
  Eq <- teq a b
  return Eq


-- | Explicit Haskell Type Representations
class Eq a => Typeable a where
  typeof :: Type a

instance Typeable Int where
  typeof = Int
instance Typeable Float where
  typeof = Float
instance Typeable Str where
  typeof = String
instance Typeable Bool where
  typeof = Bool
instance Typeable () where
  typeof = One
instance (Typeable a, Typeable b) => Typeable (Either a b) where
  typeof = Either typeof typeof
instance (Typeable a, Typeable b) => Typeable (a, b) where
  typeof = Prod typeof typeof
instance (Typeable a) => Typeable [a] where
  typeof = List typeof

gshow :: Type a -> a -> String
gshow Int                   int        = show int
gshow Float                 float      = show float
gshow String                string     = show string
gshow Bool                  bool       = show bool
gshow One                   one        = show "()"
gshow (Either a b)          (Left l)   = "(Left " ++ gshow a l ++ ")"
gshow (Either a b)          (Right r)  = "(Right " ++ gshow b r ++ ")"
gshow (Prod   a b)          (l, r)     = "(" ++ gshow a l ++ ", " ++ gshow b r ++ ")"
gshow (List   a  )          xs         = "[" ++ concat (intersperse (", ") (map (gshow a) xs)) ++ "]"
gshow (Data   (Name _ n) a) x          = n ++ "[" ++ gshow a (out x) ++ "]"
gshow (List1  a  )          (x, xs)    = "(" ++ gshow a x ++ ", " ++ gshow (List a) xs ++ ")"
gshow (Tag    n a)          x          = gshow a x
gshow (Doc    a  )          x          = gshow a x
--gshow (VVar    a  )          x          = gshow a x
--gshow a                     _          = error $ "gshow not defined for " ++ show a

--gpPrint :: Type a -> a -> Doc
--gpPrint = undefined
-- A wrap string type.





isVar n = isPrefixOf "$" n
mkVar n = 'n' : n
isAtt n = isPrefixOf "@" n
mkAtt n = '@' : n

varName :: String -> String
varName ('$': n) = n
varName s        = error $ show s ++ " not a variable"

attName :: String -> String
attName ('@': n) = n
attName s        = error $ show s ++ " not an attribute"

isVarTag :: Type a -> Maybe (String, Type a)
isVarTag (Tag n v) = if isVar n then Just (n, v) else Nothing
isVarTag _         = Nothing

getLiteral :: Type a -> Maybe (Type a)
getLiteral a = if isLiteral a then Just a else Nothing

isLiteral Int = True
isLiteral Float = True
isLiteral Bool = True
isLiteral String = True
isLiteral a = False
