{-# LANGUAGE DeriveGeneric #-}
module ApacheInterface where

import GHC.Generics
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import MonitorCommon

--parser-printer imports
import ApachePrettyPrinter
import ApacheSourceCreator
import TreeConfigApacheFiller

----server imports
import TypeFiles.ApacheTypes
import ApacheDefaultValues

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

import TransfoApache

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

apacheReader :: String -> IO ApacheWebserver
apacheReader filename = do
  res <- parseTreeApache filename
  if isLeft res 
    then fail "Parser failed"
    else do
      let (Right tree) = res
      return (createSourceApache tree)


apacheWriterReal :: String -> ApacheWebserver -> IO ()
apacheWriterReal filename src = do
  let text = printApache src
  writeFile filename (show text)
  system "sudo apache2ctl restart"
  return ()

apacheWriter :: String -> ApacheWebserver -> IO ()
apacheWriter filename src = do
  let text = printApache src
  -- putStrLn "write configuration:"
  -- print text
  writeFile filename (show text)

