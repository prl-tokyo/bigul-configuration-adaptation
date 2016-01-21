module TypesAndFunctions where

--{{{ Haskell imports
import Text.PrettyPrint
--}}}

-- Types that will be used to parse the configuration file to the source
type Instruction = 
	(String, String)

data TreeFile = 
	TreeFile String [Instruction] [TreeFile]
	deriving (Show)

--{{{ Function that will return the value of the given instruction 
--on the current node
getValue :: TreeFile -> String -> Maybe String
getValue (TreeFile name instructionList treeFileList) instructionName = 
	lookup instructionName instructionList
--}}}

--{{{ Function that will return a list of value for the same instruction
getValueList :: TreeFile -> String -> Maybe [String]
getValueList (TreeFile name instructionList treeFileList) instructionName = 
	if (elem instructionName (map fst instructionList)) then 
	Just (map snd (filter ((==instructionName) . fst) instructionList)) else
	Nothing
--}}}

--{{{ Function that returns a TreeFile according to a Path beginning from a
--node of a TreeFile
giveTree :: [TreeFile] -> [String] -> [TreeFile]
giveTree [] [y] = []
giveTree ((TreeFile name instructionList treeList):xs) [y] = if (name==y) then 
	(TreeFile name instructionList treeList) : (giveTree xs [y]) else
	giveTree xs [y]
giveTree ((TreeFile name _ treeList):xs) (y:ys) = if (name==y) then
	giveTree treeList ys else
	giveTree xs (y:ys)
giveTree _ _ = []
--}}}

--{{{ Functions that returhs a Bool if a node is in a list of TreeFile
nodeInList :: [TreeFile] -> String -> Bool
nodeInList [] _ = False
nodeInList ((TreeFile name instructionList treeList):xs) givenName = 
	if (name == givenName) then True
	else nodeInList xs givenName
--}}}

--{{{ Function that returns a Boolean to know if a node is at least one time
--in a tree or not
isInChildren :: TreeFile -> String -> Bool
isInChildren (TreeFile name instructionList treeList) givenName = 
	nodeInList treeList givenName
--}}}

--{{{ Function that removes the firt space of the second argument of a pair of
--string
removeFirstSpace :: (String,String) -> (String,String)
removeFirstSpace (instruction,value) = (instruction,snd(splitAt 1 value))
--}}}

--{{{ Function that sends True if the Either type is a Right (needed for unit
--testing the parser)
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
--}}}

--{{{ Function that put a brace after a String (needed for the charset
--instruction)
putBrace :: String -> String
putBrace x = x++"}"
--}}}

--{{{ Function that prints a maybe type: it prints "" if Nothing found and call 
--an other printing function if a Just value is found
printMaybe :: (a -> Doc) -> Maybe a -> Doc
printMaybe printfunction value = maybe empty printfunction value
--}}}

--{{{ Funtion that just prints the value
printValueInstruction :: String -> Doc
printValueInstruction value = text value
--}}}

--{{{ Function that get rid of the Maybe monad
noMaybe :: Maybe a -> a
noMaybe (Just a) = a
--}}}
