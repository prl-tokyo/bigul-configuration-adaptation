{-# LANGUAGE ViewPatterns, GADTs, TypeFamilies #-}
module BiFlux.Trans.Patterns where

import BiFlux.Lang.AST
import BiFlux.DTD.Type as Type
import Control.Monad
import Data.Map (Map(..))
import qualified Data.Map as Map

inferAstType :: MonadPlus m => TypeEnv -> AstType -> m DynType
inferAstType tenv EmptyT = return $ DynT One
inferAstType tenv (NameT n) = case Map.lookup n tenv of
  Just t -> return t
  Nothing -> fail $ "type declaration " ++ show n ++ " not defined"
inferAstType tenv (ElementT n t) =
  case Map.lookup n tenv of
    Just dt@(DynT (Data _ childrenT)) -> do
      (DynT childrenT') <- inferAstType tenv t
      case teq childrenT childrenT' of
        Just Eq -> return dt
        _ -> fail $ "incorrect children type for element " ++ n
    Nothing -> fail $ "unknown element " ++ n
inferAstType tenv (AttributeT n t) = liftM (applyDynT toAttribute) $ inferAstType tenv t
    where toAttribute :: Eq a => Type a -> DynType
          toAttribute a@(getLiteral -> Just _) = DynT (Tag (mkAtt n) a)
          toAttribute a = error $ "non-primitive attribute content " ++ show a
inferAstType tenv (SequenceT t1 t2) = do
  DynT a <- inferAstType tenv t1
  DynT b <- inferAstType tenv t2
  return $ DynT $ Prod a b
inferAstType tenv (StarT t) = liftM (applyDynT (DynT . List)) $ inferAstType tenv t
inferAstType tenv (ChoiceT t1 t2) = do
  DynT a <- inferAstType tenv t1
  DynT b <- inferAstType tenv t2
  return $ DynT $ Either a b
inferAstType tenv StringT = return $ DynT String

