{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module TreeConfigApacheFiller where

--{{{ Module imports
import TypesAndFunctions
---}}}

--{{{ Haskell imports
import Text.Peggy
import Data.Either
import Data.Maybe
--}}}

--{{{ Definition of the parser using Peggy
[peggy|
configFileApache :: TreeFile
    = directive* {let ds = catMaybes $1 in TreeFile "root" (lefts ds) (rights ds)}
directive :: Maybe (Either Instruction TreeFile)
    = instruction { Just (Left $1) }
    / block { Just (Right $1) }
    / comment { Nothing }
instruction ::: Instruction
    = "#ID" [a-z0-9]+ { ("ID",$1) }
    / [A-Za-z]+ [ ] [^\n]+ { ($1,$3) }
block :: TreeFile
    = "<Directory" [^<>\n]+ ">" directive* "</Directory>"
	{ let ds = catMaybes $2 in TreeFile "Directory"
	([("Match","False")]++[("Path",$1)]++lefts ds) (rights ds) }
    / "<DirectoryMatch" [^<>\n]+ ">" directive* "</DirectoryMatch>"
	{ let ds = catMaybes $2 in TreeFile "Directory"
	([("Match","True")]++[("Path",$1)]++lefts ds) (rights ds) }
    / "<Files" [^<>\n]+ ">" directive* "</Files>"
	{ let ds = catMaybes $2 in TreeFile "Files"
	([("Match","False")]++[("Path",$1)]++lefts ds) (rights ds) }
    / "<FilesMatch" [^<>\n]+ ">" directive* "</FilesMatch>"
	{ let ds = catMaybes $2 in TreeFile "Files"
	([("Match","True")]++[("Path",$1)]++lefts ds) (rights ds) }
    / "<Location" [^<>\n]+ ">" directive* "</Location>"
	{ let ds = catMaybes $2 in TreeFile "Location"
	([("Match","False")]++[("Path",$1)]++lefts ds) (rights ds) }
    / "<LocatioMatch" [^<>\n]+ ">" directive* "</LocationMatch>"
	{ let ds = catMaybes $2 in TreeFile "Location"
	([("Match","True")]++[("Path",$1)]++lefts ds) (rights ds) }
    / "<RequireNone>" directive* "</RequireNone>"
	{ let ds = catMaybes $1 in TreeFile "Require"
	([("Path","None")]++lefts ds) (rights ds) }
    / "<RequireAny>" directive* "</RequireAny>"
	{ let ds = catMaybes $1 in TreeFile "Require"
	([("Path","Any")]++lefts ds) (rights ds) }
    / "<RequireAll>" directive* "</RequireAll>"
	{ let ds = catMaybes $1 in TreeFile "Require"
	([("Path","All")]++lefts ds) (rights ds) }
    / "<VirtualHost" [^<>\n]+ ">" directive* "</" [a-zA-Z]+ ">"
	{ let ds = catMaybes $2 in TreeFile $3
	([("Path",$1)]++lefts ds) (rights ds) }
comment ::: ()
    = "#" [^\n]* { () }
|]
--}}}

--{{{ Function that parse the configuration file into a TreeFile or an error
parseTreeApache :: FilePath -> IO (Either ParseError TreeFile)
parseTreeApache file =
	return . parseString configFileApache "<stdin>" =<< readFile file
--}}}

--{{{ Function that print the resulting tree
printTreeApache :: IO ()
printTreeApache = do
	tree <- parseTreeApache "apache.conf"
	case tree of
		Left err   -> putStrLn ("Parse error: " ++ show err)
		Right tree -> print tree
--}}}
