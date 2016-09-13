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

----misc
import Data.Either
import Data.Maybe
import Debug.Trace

import MonitorCommon
import TransfoApache
import TransfoNginx
import TransfoNginxNewToOld

putS :: (Show abs) => BiGUL (Either ErrorInfo) c1 abs -> BiGUL (Either ErrorInfo) c2 abs -> c1 -> c2 -> Either ErrorInfo c1
putS b1 b2 s1 s2 = do
  abs <- get b2 s2
  put b1 s1 abs

migrateA2N :: String -> String -> String -> IO ()
migrateA2N apacheFilename nginxFilename nginxOutput = do
  apacheConcrete <- apacheReader apacheFilename
  nginxConcrete <- nginxReader nginxFilename
  let newConcrete' = putS (transNginx NginxDefaultValues.defaults) (transApache ApacheDefaultValues.defaults) nginxConcrete apacheConcrete
  if isLeft newConcrete'
     then putStrLn "failed to put back to nginx"
     else nginxWriter nginxOutput ((\(Right x) -> x) newConcrete')

main = migrateA2N "apache.conf" "nginx.conf" "nginx.conf.out"
