{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric  #-}

---------
--IMPORTS
---------
----BiGUL imports
import Generics.BiGUL
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Control.Monad
import GHC.Generics

--parser-printer imports
import ApachePrettyPrinter
import ApacheSourceCreator
import TreeConfigApacheFiller
import NginxPrettyPrinter
import NginxSourceCreator
import TreeConfigNginxFiller

----server imports
import TypeFiles.ApacheTypes
import ApacheDefaultValues
import ApacheInterface

import TypeFiles.NginxTypes
import NginxDefaultValues
import NginxInterface

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

import MonitorCommon
import TransfoApache
import TransfoNginx

adapter :: IO a -> (a -> IO ()) -> IO Environment -> BiGUL (Either ErrorInfo) a CommonWebserver -> IO ()

adapter reader writer monitor bx = do
  putStrLn "Reading configuration file."
  concrete <- reader

  let (Right abstract) = get bx concrete

  putStrLn "Monitoring environment"
  env <- monitor
  print env

  --------- Apply strategies ----------

  let new_abstract = adaptContentType env abstract

  -------------------------------------

  let (Right new_concrete) = put bx concrete new_abstract

  if new_abstract == abstract
    then putStrLn "No changes in configuration"
    else do
      putStrLn "Writing to configuration file."
      writer new_concrete

  threadDelay 1000000
  putStrLn ""
  adapter reader writer monitor bx


adaptContentType :: Environment -> CommonWebserver -> CommonWebserver
adaptContentType env abstract = 
  if avgResponseTimeLastMinute env >= 1000000
    then trace "serve lower fidelity" $ lowerFidelity abstract
    else 
      if avgResponseTimeLastMinute env < 100000
        then trace "serve higher fidelity" $ higherFidelity abstract
        else abstract

lowerFidelity :: CommonWebserver -> CommonWebserver
lowerFidelity cmm = cmm{vServers = lower (vServers cmm)}
  where lower [] = []
        lower (x:xs) 
          | vServRoot x == "/var/www/blog" = x{vServRoot = "/var/www/blog-low"} : xs
          | vServRoot x == "/var/www/blog/" = x{vServRoot = "/var/www/blog-low/"} : xs
          | True = x : lower xs

higherFidelity :: CommonWebserver -> CommonWebserver
higherFidelity cmm = cmm{vServers = higher (vServers cmm)}
  where higher [] = []
        higher (x:xs) 
          | vServRoot x == "/var/www/blog-low" = x{vServRoot = "/var/www/blog"} : xs
          | vServRoot x == "/var/www/blog-low/" = x{vServRoot = "/var/www/blog/"} : xs
          | True = x : higher xs


main = do
  putStrLn "Start adapter..."
  adapter (nginxReader "/etc/nginx/nginx.conf")
          (nginxWriterReal "/etc/nginx/nginx.conf")
          nginxMonitor
          (transNginx NginxDefaultValues.defaults)

-- main = do
--   putStrLn "Start adapter..."
--   adapter (apacheReader "/etc/apache2/apache2.conf")
--           (apacheWriterReal "/etc/apache2/apache2.conf")
--           apacheMonitor
--           (transApache ApacheDefaultValues.defaults)
