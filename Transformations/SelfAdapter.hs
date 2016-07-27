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

----apache imports
import TypeFiles.ApacheTypes
import ApacheDefaultValues
import TypeFiles.Common

----io
import Control.Concurrent 
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Format
import System.Process

----parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Text
import Text.Parsec.Combinator
import Text.Parsec.Error

----misc
import Data.Either
import Debug.Trace

import ApacheMonitor
import TransfoApache

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
  if avgResponseTimeLastMinute env >= 1000
    then trace "serve lower fidelity" $ lowerFidelity abstract
    else 
      if avgResponseTimeLastMinute env < 500
        then trace "serve higher fidelity" $ higherFidelity abstract
        else abstract

lowerFidelity :: CommonWebserver -> CommonWebserver
lowerFidelity cmm = cmm{vServers = lower (vServers cmm)}
  where lower [] = []
        lower (x:xs) 
          | vServRoot x == "/var/www/blog" = x{vServRoot = "/var/www/blog-low"} : xs
          | True = x : lower xs

higherFidelity :: CommonWebserver -> CommonWebserver
higherFidelity cmm = cmm{vServers = higher (vServers cmm)}
  where higher [] = []
        higher (x:xs) 
          | vServRoot x == "/var/www/blog-low" = x{vServRoot = "/var/www/blog"} : xs
          | True = x : higher xs


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

main = do
  putStrLn "Start adapter..."
  adapter (apacheReader "/etc/apache2/apache2.conf")
          (apacheWriterReal "/etc/apache2/apache2.conf")
          apacheMonitor
          (transApache defaults)
