{-# LANGUAGE DeriveGeneric #-}
module NginxMonitor where

import GHC.Generics
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import MonitorCommon

-- Nginix Log Type
-- A JSON format is assumed:
--     LogFormat "{\"responseTime\" : $request_time, \"stamp\" : $msec}" monitorJSON 
data NginxLogReq = ApacheLogReq{ responseTime :: Float, stamp :: Float } deriving (Show, Generic)

instance ToJSON NginxLogReq where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON NginxLogReq

nginxMonitor :: IO Environment
nginxMonitor = do
  log <- readFile "/var/log/nginx/monitor.log"
  let requests = catMaybes $ map (decode . B.pack) (lines log) :: [NginxLogReq]
  -- F**k Data.Time.Clock
  currentTime <- (fmap floor getPOSIXTime :: IO Int)
  let reqLastMinute = filter (\req ->  (currentTime - round (stamp req)) <= 60) requests
  return $ Environment{ 
                        requestsLastMinute = length reqLastMinute
                      , avgResponseTimeLastMinute = round (1000 * 1000 * (sum . map responseTime $ reqLastMinute) /  fromIntegral (length reqLastMinute + 1))
                      }
