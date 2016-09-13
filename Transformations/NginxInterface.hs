{-# LANGUAGE DeriveGeneric #-}
module NginxInterface where

import GHC.Generics
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import MonitorCommon

--parser-printer imports
import NginxPrettyPrinter
import NginxSourceCreator
import TreeConfigNginxFiller

----server imports
import TypeFiles.NginxTypes
import NginxDefaultValues

import TypeFiles.Common

----io
import Control.Concurrent 
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Format
import System.Process

----misc
import Data.Either
import Debug.Trace

import TransfoNginx

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

nginxReader :: String -> IO NginxWebserver
nginxReader filename = do
  res <- parseTreeNginx filename
  if isLeft res 
    then fail "Parser failed"
    else do
      let (Right tree) = res
      return (createSourceNginx tree)

nginxWriterReal :: String -> NginxWebserver -> IO ()
nginxWriterReal filename src = do
  let text = printNginx src
  writeFile filename (show text)
  system "sudo nginx -s reload"
  return ()

nginxWriter :: String -> NginxWebserver -> IO ()
nginxWriter filename src = do
  let text = printNginx src
  putStrLn "write configuration:"
  print text
  writeFile filename (show text)

