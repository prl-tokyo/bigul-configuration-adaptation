{-# LANGUAGE DeriveGeneric #-}
module ApacheMonitor where

import GHC.Generics
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import MonitorCommon

-- Apache Log Type
-- A JSON format is assumed:
--     LogFormat "{\"responseTime\" : %D, \"stamp\" : %{sec}t}" monitorJSON 
data ApacheLogReq = ApacheLogReq{ responseTime :: Int, stamp :: Int } deriving (Show, Generic)

instance ToJSON ApacheLogReq where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ApacheLogReq

apacheMonitor :: IO Environment
apacheMonitor = do
  log <- readFile "/var/log/apache2/monitor.log"
  let requests = catMaybes $ map (decode . B.pack) (lines log) :: [ApacheLogReq]
  currentTime <- fmap floor getPOSIXTime
  let reqLastMinute = filter (\req ->  (currentTime - stamp req) <= 60) requests
  return $ Environment{ 
                        requestsLastMinute = length reqLastMinute
                      , avgResponseTimeLastMinute = (sum . map responseTime $ reqLastMinute) 
                                                        `quot` (length reqLastMinute + 1)
                      }
