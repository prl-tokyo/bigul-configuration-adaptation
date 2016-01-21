module BiFlux.DTD.GenHaskellDTD where

import Text.XML.HaXml.DtdToHaskell.TypeDef hiding (ppTypeDef, mangle)
import Text.XML.HaXml.DtdToHaskell.Convert
import Text.XML.HaXml.Types hiding (Name)
import Text.XML.HaXml.Parse
import BiFlux.DTD.TypeDef
import Data.List
import Text.PrettyPrint
import System.IO
import Text.XML.HaXml.XmlContent
import BiFlux.DTD.Type


run :: String -> Maybe a -> IO a
run err = maybe (error err) return

--loadXml :: (XmlContent a, Show a) => FilePath -> Type a -> IO a
--loadXml fpath t = fReadXml fpath
loadXml :: (XmlContent a) => FilePath -> IO a
loadXml fpath = fReadXml fpath

writeXml :: XmlContent a => FilePath -> a -> IO ()
writeXml fpath dat = fWriteXml fpath dat

genHaskellDTD :: FilePath -> FilePath -> IO ()
genHaskellDTD dtdFileName hsFileName = do
  content <- readFile dtdFileName
  DTD name _ markup <- run "No DTD file" (dtdParse dtdFileName content)
  out <- openFile hsFileName WriteMode
  let realName = mangle (trim hsFileName)
  hPutStrLn out (
    "{-# LANGUAGE TypeOperators, TypeFamilies #-}\n" ++
    "module " ++  realName ++ " where\n\n" ++
    "import Data.Map (Map(..))\n"++
    "import qualified Data.Map as Map\n"++
    "import Text.XML.HaXml.XmlContent hiding (List1)\n"++
    "import Text.XML.HaXml.Types\n"++
    "import BiFlux.DTD.TypeDef\n"++
    "import BiFlux.DTD.Type\n" ++
    "import Text.XML.HaXml.DtdToHaskell.TypeDef (Name(..))\n"++
    "import Control.Monad\n"++
    "import GHC.Generics \n\n"++
    "type EMPTY = ()\n\n") -- declares the special DTD empty type
  let dtdDecls = dtd2TypeDef markup
  hPutStrLn out "\n"
  -- Write datatype definitions
  let hsDefs = map (ppTypeDef realName) dtdDecls
  mapM (hPutStrLn out . render) hsDefs
  hPutStrLn out "\n"
  mapM (hPutStrLn out . render . mkInstance realName) dtdDecls
  hPutStrLn out "\n"
  mapM (hPutStrLn out . render . mkTypeable realName) dtdDecls
  hPutStrLn out "\n"
  mapM (hPutStrLn out . render . ppDataTypeGeneric realName) dtdDecls
  hClose out

