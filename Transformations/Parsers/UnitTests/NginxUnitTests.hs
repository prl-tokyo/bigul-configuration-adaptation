--Module import
--import SourceCreator
import TreeConfigFiller
import TreeTypes

-- Haskell imports
import Test.HUnit
import Text.Peggy

-- Tests of the TreeFile type and its functions
treeFileFunctions1 = TestCase (assertEqual "getValue in TreeFile - value found" (Just "18") (getValue (TreeFile "tree1" [("toto","18"),("jesus","5did")] []) "toto"))

treeFileFunctions2 = TestCase (assertEqual "getValue in TreeFile - empty" Nothing (getValue (TreeFile "tree1" [] []) "toto"))

treeFileFunctions3 = TestCase (assertEqual "getValue in TreeFile - 2 times value" (Just "18") (getValue (TreeFile "tree1" [("toto","18"),("jesus","5did"),("toto","54")] []) "toto"))

treeFileFunctions4 = TestCase (assertEqual "getValue in TreeFile - not good value" Nothing (getValue (TreeFile "tree1" [("toto","18"),("jesus","5did")] []) "notToto"))

treeFileFunctionsTests = TestList [TestLabel "treeFileFunctions1" treeFileFunctions1, TestLabel "treeFileFunctions2" treeFileFunctions2, TestLabel "treeFileFunctions3" treeFileFunctions3, TestLabel "treeFileFunctions4" treeFileFunctions4]

--Tests of the configuration file parser and its functions
parserTest1 = TestCase (parseTree "../Nginx_Parser/nginx.conf" >>= assertBool "parseTree - parsing with right file" . isRight)

parserTest2 = TestCase (parseTree "../Nginx_Parser/nginx.conf" >>= \ (Right tree) -> assertEqual "parseTree - random value in current node taken after parsing" (Just "www-data") (getValue tree "user"))

parserTests = TestList [TestLabel "parserTest1" parserTest1, TestLabel "parserTest2" parserTest2]
