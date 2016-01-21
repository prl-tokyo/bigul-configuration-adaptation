{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, DeriveGeneric, TupleSections #-}
module Generics.BiGUL.MonadBiGULError where
import Control.Monad.Except
import Text.PrettyPrint

class MonadError e m => MonadError' e m where
  catchBind :: m a -> (a -> m b) -> (e -> m b) -> m b

instance MonadError' e (Either e) where
  -- catchBind :: Either e a -> (a -> Either e b) -> (e -> Either e b) -> Either e b
  catchBind ma f g = either g f ma


class PrettyPrintable s where
  pPrint :: s -> Doc

--instance PrettyPrintable s where


-- break point (expression), source, view, inner error info.
data ErrorInfo = ErrorInfo String
  deriving (Show)

-- | Storing error information
-- simplified error information
-- break point (expression)
-- current source
-- current view
-- inner structured error informations
--data ErrorInfo where
--  ErrorInfo :: (PrettyPrintable s, PrettyPrintable v) => Doc -> Doc -> s -> v -> [ErrorInfo] -> ErrorInfo

