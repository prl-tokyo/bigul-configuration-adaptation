{-# LANGUAGE DeriveGeneric #-}
module MonitorCommon where

import GHC.Generics
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B

-- Environment Infomation
data Environment = Environment { requestsLastMinute :: Int, 
                                 avgResponseTimeLastMinute :: Int -- in micro-seconds
                               }
                                
  deriving (Show, Eq)
