{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module TreeConfigNginxFiller where

--{{{ Module imports
import TypesAndFunctions
---}}}

--{{{ Haskell imports
import Text.Peggy
import Data.Either
import Data.Maybe
---}}}

--{{{ Definition of the parser using Peggy
[peggy|
configFileNginx :: TreeFile
    = directive* 
	{let ds = catMaybes $1 in TreeFile "root" (lefts ds) (rights ds)}
directive :: Maybe (Either Instruction TreeFile)
    = instruction { Just (Left $1) }
    / block { Just (Right $1) }
    / comment { Nothing }
instruction ::: Instruction
    = "charset_map" [a-z A-Z 0-9 \- _ { ; \n \t]+ "}" 
	{ ("charset_map",putBrace $1) }
    / "map" [a-z A-Z 0-9 \- _ { ; \n \t]+ "}" { ("map",putBrace $1) }
    / "types" [a-z A-Z 0-9 \- _ { ; \n \t]+ "}" { ("type",putBrace $1) }
    / "internal;" { ("internal","on") }
    / "#ID" [a-z 0-9]+ { ("ID",$1) }
    / [0-9 a-z A-Z _ / \ ' : = ! ~ ^ , { } \[ \] \- \. \* $ "]+ ';' 
	{ removeFirstSpace (break (==' ') $1) }
block ::: TreeFile 
    = "location" [a-z A-Z 0-9 / \- \_]* "{" directive* "}" 
	{ let ds = catMaybes $2 in TreeFile "location" ([("location_path",fst 
	(break (==' ') $1))]++lefts ds) (rights ds) }
    / [a-z /]+ "{" directive* "}" 
	{ let ds = catMaybes $2 in 
	TreeFile (fst (break (==' ') $1)) (lefts ds) (rights ds) }
comment ::: ()
    = "#" [^\n]* { () }
|]
--}}}

--{{{ Function that parse the configuration file into a TreeFile or an error
parseTreeNginx :: FilePath -> IO (Either ParseError TreeFile)
parseTreeNginx file = 
	return . parseString configFileNginx "<stdin>" =<< readFile file
---}}}

--{{{ Function that print the resulting tree
printTreeNginx :: IO ()
printTreeNginx = do
	tree <- parseTreeNginx "nginx.conf"
	case tree of
		Left err   -> putStrLn ("Parse error: " ++ show err)
		Right tree -> print tree
---}}}
