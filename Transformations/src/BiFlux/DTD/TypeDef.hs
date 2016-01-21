{-# LANGUAGE TypeOperators, TypeFamilies #-}
module BiFlux.DTD.TypeDef where

import Text.XML.HaXml.DtdToHaskell.TypeDef (TypeDef (..),Name(..),Constructors(..),AttrFields(..),StructType(..))
import Data.Char (isLower, isUpper, toLower, toUpper, isDigit)
import Data.List (intersperse)
import Text.PrettyPrint
import Text.XML.HaXml.TypeMapping (HTypeable(..))
import qualified Text.XML.HaXml.TypeMapping as TypeMapping
import Text.XML.HaXml.XmlContent (XmlContent(..),Attribute(..))
import qualified Text.XML.HaXml.XmlContent as XmlContent
import Text.ParserCombinators.Poly hiding (empty)
import GHC.Generics
import Data.List
import qualified Data.Map as Map
import Data.Map (Map(..))

type List1 a = (a,[a])
type Mb a = Either () a


mb2maybe :: Mb a -> Maybe a
mb2maybe (Right a) = Just a
mb2maybe (Left ()) = Nothing
maybe2mb :: Maybe a -> Mb a
maybe2mb (Just a) = Right a
maybe2mb Nothing = Left ()

maybeMb :: b -> (a -> b) -> Mb a -> b
maybeMb b f = either (const b) f

optionalMb :: Alternative f => f a -> f (Mb a)
optionalMb = liftA maybe2mb . optional

possibleMbA :: (String->Attribute->Maybe a) -> String -> [Attribute] -> Mb a
possibleMbA from at = maybe2mb . XmlContent.possibleA from at

mbToAttr :: (String->a->Maybe Attribute) -> String -> Mb a -> Maybe Attribute
mbToAttr to n = XmlContent.maybeToAttr to n . mb2maybe


mkTypeable :: String -> TypeDef -> Doc
mkTypeable modname (DataDef _ n [] []) = text "instance" <+> text "Typeable" <+> ppHName modname n <+> text "where" $$
        nest 2 (text "typeof" <+> text "=" <+> text "Data" <+> text (showName n) <+> text "One")
mkTypeable modname (DataDef _ n atts []) = mkTypeableAtts True n atts modname
mkTypeable modname (DataDef _ n atts _) = mkTypeable' modname n $$ mkTypeableAtts False n atts modname
mkTypeable modname (EnumDef n _) = mkTypeable' modname n
mkTypeable' modname n = text "instance" <+> text "Typeable" <+> ppHName modname n <+> text "where" $$
        nest 2 (text "typeof" <+> text "=" <+> text "Data" <+> text (showName n) <+> text "typeof")

mkTypeableAtts :: Bool -> Name -> AttrFields -> String -> Doc
mkTypeableAtts s n [] modname = empty
mkTypeableAtts s n atts modname = text "instance" <+> text "Typeable" <+> ppName n <+> text "where" $$
        nest 2 (text "typeof" <+> text "=" <+> text "Data" <+> text (cName n) <+> mkAtts atts)
    where ppName = if s then ppHName modname else ppAName modname
          cName = if s then showName else showAttName

-- use right-biased strategy.
mkAtts :: AttrFields -> Doc
mkAtts [a] = mkAtt a
mkAtts (a: as) = text "Prod" <+> mkAtts [a] <+> text "(" <+> mkAtts as <+> text ")"

mkAtt :: (Name, StructType) -> Doc
mkAtt (n, Maybe s) = parens (text "Either One" <+> parens (text "Tag" <+> text (show ('@':xName n)) <+> text "typeof")) -- optional attributes
mkAtt (n, s      ) = parens (text "Tag" <+> text (show ('@': xName n)) <+> text "typeof")

showName (Name x h) = "(Text.XML.HaXml.DtdToHaskell.TypeDef.Name "++show x++" "++show h++")"
showAttName (Name x h) = "(Text.XML.HaXml.DtdToHaskell.TypeDef.Name "++show "@"++" "++show (h++"_Attrs")++")"

-- | Trim the Haskell File path
trim :: [Char] -> [Char]
trim name | '/' `elem` name  = (trim . tail . dropWhile (/='/')) name
          | '.' `elem` name  = takeWhile (/='.') name
          | otherwise        = name

-- | Convert an XML name to a Haskell conid.
mangle :: String -> String
mangle (n:ns)
    | isLower n   = notPrelude (toUpper n: map decolonify ns)
    | isDigit n   = 'I': n: map decolonify ns
    | otherwise   = notPrelude (n: map decolonify ns)

-- | Ensure a generated name does not conflict with a standard haskell one.
notPrelude :: String -> String
notPrelude "Bool"    = "ABool"
notPrelude "Bounded" = "ABounded"
notPrelude "Char"    = "AChar"
notPrelude "Double"  = "ADouble"
notPrelude "Either"  = "AEither"
notPrelude "Enum"    = "AEnum"
notPrelude "Eq"      = "AEq"
notPrelude "FilePath"= "AFilePath"
notPrelude "Float"   = "AFloat"
notPrelude "Floating"= "AFloating"
notPrelude "Fractional"= "AFractional"
notPrelude "Functor" = "AFunctor"
notPrelude "IO"      = "AIO"
notPrelude "IOError" = "AIOError"
notPrelude "Int"     = "AInt"
notPrelude "Integer" = "AInteger"
notPrelude "Integral"= "AIntegral"
notPrelude "List1"   = "AList1" -- part of HaXml
notPrelude "Maybe"   = "AMaybe"
notPrelude "Monad"   = "AMonad"
notPrelude "Num"     = "ANum"
notPrelude "Ord"     = "AOrd"
notPrelude "Ordering"= "AOrdering"
notPrelude "Rational"= "ARational"
notPrelude "Read"    = "ARead"
notPrelude "ReadS"   = "AReadS"
notPrelude "Real"    = "AReal"
notPrelude "RealFloat" = "ARealFloat"
notPrelude "RealFrac"= "ARealFrac"
notPrelude "Show"    = "AShow"
notPrelude "ShowS"   = "AShowS"
notPrelude "String"  = "Str"
notPrelude n         = n

-- | Convert colon to prime, hyphen to underscore.
decolonify :: Char -> Char
decolonify ':' = '\''   -- TODO: turn namespaces into qualified identifiers
decolonify '-' = '_'
decolonify '.' = '_'
decolonify  c  = c

---- Pretty-printing typedefs ---- takes also a module name to avoid Haskell name clashes (uses every variable qualified)
ppTypeDef :: String -> TypeDef -> Doc

--      no attrs, no constructors
ppTypeDef modname (DataDef _ n [] []) =
    let hn = ppBindHName n in
    text "data" <+> hn <+> text "=" <+> hn $$ nest 4 derives

--      no attrs, single constructor
ppTypeDef modname (DataDef _ n [] [c@(_,[_])]) =
    text "newtype" <+> ppBindHName n <+> text "=" <+> ppC modname c $$  nest 4 derives
-- newtype Title =Title Str

--      no attrs, multiple constructors.
ppTypeDef modname (DataDef _ n [] cs) =
    text "data" <+> ppBindHName n <+>
           ( text "=" <+> ppC modname (head cs) $$
             vcat (map (\c-> text "|" <+> ppC modname c) (tail cs)) $$
             derives )

--      nonzero attrs, no constructors
ppTypeDef modname (DataDef _ n fs []) =
    let nme = ppBindHName n in
    text "data" <+> nme <+> text "=" <+> nme $$
    nest 4 ( text "{" <+> ppF modname (head fs) $$
             vcat (map (\f-> text "," <+> ppF modname f) (tail fs)) $$
             text "}" <+> derives )

--      nonzero attrs, one or more constrs
ppTypeDef modname (DataDef _ n fs cs) =
    let attr = ppBindAName n in
    text "data" <+> ppBindHName n <+>
           ( text "=" <+> ppAC modname attr (head cs) $$
             vcat (map (\c-> text "|" <+> ppAC modname attr c) (tail cs)) $$
             derives )  $$
    text "data" <+> attr <+> text "=" <+> attr $$
    nest 4 ( text "{" <+> ppF modname (head fs) $$
             vcat (map (\f-> text "," <+> ppF modname f) (tail fs)) $$
             text "}" <+> derives )

--      enumerations (of attribute values)
ppTypeDef modname (EnumDef n es) =
    text "data" <+> ppBindHName n <+>
    ( text "=" <+>
      fsep (intersperse (text " | ") (map ppBindHName es))
    $$ derives )

-- | pretty-print structType, prefixed with the mode name
ppST :: String -> StructType -> Doc
ppST modname (Defaultable st _)  = parens (text "Defaultable" <+> ppST modname st)
ppST modname (Maybe st)          = parens (text "Mb" <+> ppST modname st)
ppST modname (List st)           = text "[" <> ppST modname st <> text "]"
ppST modname (List1 st)          = parens (text "List1" <+> ppST modname st)
ppST modname (Tuple sts)         = parens (commaList (map (ppST modname) sts))
ppST modname (OneOf [])          = text "()"
ppST modname (OneOf [st])        = ppST modname st
ppST modname (OneOf (st:sts))    = parens (text "Either" <+> ppST modname st <+> ppST modname (OneOf sts))
ppST modname StringMixed         = text "Str"
ppST modname String              = text "Str"
ppST modname Any                 = text "ANYContent"
ppST modname (Defined n)         = ppHName modname n

-- | pretty-print for each constructor and components
ppC :: String -> (Name, [StructType]) -> Doc
ppC modname (n, sts) = ppBindHName n <+> fsep (map (ppST modname) sts)

-- attribute (fieldname and type)
ppF :: String -> (Name, StructType) -> Doc
ppF modname (n, st) = ppBindHName n <+> text "::" <+> ppST modname st

-- constructor and components with initial attr-type
ppAC :: String -> Doc -> (Name, [StructType]) -> Doc
ppAC modname atype (n, sts) = ppBindHName n <+> fsep (atype: map (ppST modname) sts)

-- | Pretty print Haskell name. (non-qualified)
ppBindHName :: Name -> Doc
ppBindHName (Name _ s) = text s

-- | Pretty print Haskell name. (qualified)
ppHName :: String -> Name -> Doc
ppHName modname (Name _ s) = text modname <> text "." <> text s

-- | Pretty print XML name.
ppXName :: Name -> Doc
ppXName (Name s _) = text s
-- | Pretty print Haskell attributes name. (qualified)
ppAName :: String -> Name -> Doc
ppAName modname (Name _ s) = text modname <> text "." <> text s <> text "_Attrs"

-- | Pretty print Haskell attributes name. (non-qualified)
ppBindAName :: Name -> Doc
ppBindAName (Name _ s) = text s <> text "_Attrs"

ppBindANameVar :: Name -> Doc
ppBindANameVar (Name _ s) = text (manglef s) <> text "_Attrs"


appendAttrs :: Name -> Name
appendAttrs (Name xn hn) = Name (xn ++ "_Attrs") (hn ++ "_Attrs")

derives :: Doc
derives = text "deriving" <+> parens (commaList (map text ["Eq","Show"]))

commaList :: [Doc] -> Doc
commaList = hcat . intersperse comma

---- Some operations on Names ----

-- | Make a type name valid in both XML and Haskell.
name :: String -> Name
name n     = Name { xName = n
                  , hName = mangle n }

-- | Append an underscore to the Haskell version of the name.
name_ :: String -> Name
name_ n    = Name { xName = n
                  , hName = mangle n ++ "_" }

-- | Prefix an attribute enumeration type name with its containing element
--   name.
name_a :: String -> String -> Name
name_a e n = Name { xName = n
                  , hName = mangle e ++ "_" ++ map decolonify n }

-- | Prefix an attribute enumeration constructor with its element-tag name,
--   and its enumeration type name.
name_ac :: String -> String -> String -> Name
name_ac e t n = Name { xName = n
                     , hName = mangle e ++ "_" ++ map decolonify t
                                        ++ "_" ++ map decolonify n }

-- | Prefix a field name with its enclosing element name.
name_f :: String -> String -> Name
name_f e n = Name { xName = n
                  , hName = manglef e ++ mangle n }

-- | Convert an XML name to a Haskell varid.
manglef :: String -> String
manglef (n:ns)
    | isUpper n   = toLower n: map decolonify ns
    | isDigit n   = '_': n: map decolonify ns
    | otherwise   = n: map decolonify ns

instance (XmlContent a,XmlContent b) => XmlContent (Either a b) where
    parseContents =
        (XmlContent.choice Left $ XmlContent.choice Right
        $ fail "Either")
    toContents (Left x) = XmlContent.toContents x
    toContents (Right x) = XmlContent.toContents x

--instance (HTypeable a) => HTypeable (List1 a) where
--	toHType (Wrap1 x) = list1HType (TypeMapping.toHType x)
--	toHType (Cons1 x xs) = list1HType (TypeMapping.toHType x)
--list1HType hx = TypeMapping.Defined "List1" [hx] [TypeMapping.Constr "Wrap1" [hx] [hx], TypeMapping.Constr "Cons1" [hx] [hx,TypeMapping.List hx]]

--instance (XmlContent a) => XmlContent (List1 a) where
--	toContents (Wrap1 x) = XmlContent.toContents x
--	toContents (Cons1 x xs) = concatMap XmlContent.toContents (x:xs)
--	parseContents = many1 XmlContent.parseContents >>= return . toList1

instance XmlContent () where
    toContents () = []
    parseContents = return ()


-- * Cloned from Text.XML.HaXml.DtdToHaskellInstance

-- | Convert typedef to appropriate instance declaration, either @XmlContent@,
--   @XmlAttributes@, or @XmlAttrType@.
mkInstance :: String -> TypeDef -> Doc

-- no constructors - represents an element with empty content but attributes.
mkInstance modname (DataDef _ n fs []) =
    let (_, frattr, topat, toattr) = attrpats fs
        frretval = if null fs then ppHName modname n else frattr
        topatval = if null fs then ppHName modname n else topat
    in
    text "instance HTypeable" <+> ppHName modname n <+> text "where" $$
    nest 4 ( text "toHType x = Defined \"" <> ppXName n <> text "\" [] []" )
    $$
    text "instance XmlContent" <+> ppHName modname n <+> text "where" $$
    nest 4 (
             text "toContents" <+> topatval <+> text "=" $$
             nest 4 (text "[CElem (Elem (N \"" <> ppXName n <> text "\")"
                          <+> toattr <+> text "[]) ()]")
           $$
             text "parseContents = do" $$
             nest 4 (text "{ (Elem _ as []) <- element [\""
                             <> ppXName n <> text "\"]" $$
                     text "; return" <+> frretval $$
                     text "} `adjustErr` (\"in <" <> ppXName n
                                                  <> text ">, \"++)"
                    )
           )
    $$
    mkInstanceAttrs modname Same n fs

-- single constructor, "real" (non-auxiliary) type
mkInstance modname (DataDef False n fs [(n0,sts)]) =
    let vs = nameSupply sts
        (frpat, frattr, topat, toattr) = attrpats fs
    in
    text "instance HTypeable" <+> ppHName modname n <+> text "where" $$
    nest 4 ( text "toHType x = Defined \"" <> ppXName n <> text "\" [] []" )
    $$
    text "instance XmlContent" <+> ppHName modname n <+> text "where" $$
    nest 4 (
             text "toContents" <+> parens (mkCpat modname n0 topat vs) <+> text "=" $$
             nest 4 (text "[CElem (Elem (N \"" <> ppXName n <> text "\")"
                          <+> toattr <+> parens (mkToElem sts vs)
                          <> text ") ()]")
           $$
             text "parseContents = do" $$
             nest 4 (text "{ e@(Elem _"<+> frpat <+> text "_) <- element [\""
                             <> ppXName n <> text "\"]"
                     $$ text "; interior e $"
                           <+> (mkParseConstr modname frattr (n0,sts))
                     $$ text "} `adjustErr` (\"in <" <> ppXName n
                                                     <> text ">, \"++)")
           )
    $$
    mkInstanceAttrs modname Extended n fs

-- single constructor, auxiliary type (i.e. no corresponding element tag)
--   cannot be attributes here?
mkInstance modname (DataDef True n [] [(n0,sts)]) =
    let vs = nameSupply sts
    in
    text "instance HTypeable" <+> ppHName modname n <+> text "where" $$
    nest 4 ( text "toHType x = Defined \"" <> ppXName n <> text "\" [] []" )
    $$
    text "instance XmlContent" <+> ppHName modname n <+> text "where" $$
    nest 4 ( text "toContents" <+> parens (mkCpat modname n0 empty vs)
                               <+> text "="
                               $$  nest 4 (parens (mkToElem sts vs))
           $$
             text "parseContents =" <+> mkParseConstr modname empty (n0,sts)
           )

-- multiple constructors (real)
mkInstance modname (DataDef False n fs cs) =
    let _ = nameSupply cs
        (frpat, frattr, topat, toattr) = attrpats fs
        _ = if null fs then False else True
    in
    text "instance HTypeable" <+> ppHName modname n <+> text "where" $$
    nest 4 ( text "toHType x = Defined \"" <> ppXName n <> text "\" [] []" )
    $$
    text "instance XmlContent" <+> ppHName modname n <+> text "where" $$
    nest 4 ( vcat (map (mkToMult modname n topat toattr) cs)
           $$ text "parseContents = do "
           $$ nest 4 (text "{ e@(Elem _"<+> frpat <+> text "_) <- element [\""
                                                  <> ppXName n <> text "\"]"
                     $$ text "; interior e $ oneOf"
                     $$ nest 4 ( text "[" <+> mkParseConstr modname frattr (head cs)
                               $$ vcat (map (\c-> text "," <+> mkParseConstr modname frattr c)
                                            (tail cs))
                               $$ text "] `adjustErr` (\"in <" <> ppXName n
                                                             <> text ">, \"++)"
                               )
                     $$ text "}"
                     )
           )
    $$
    mkInstanceAttrs modname Extended n fs

-- multiple constructors (auxiliary)
mkInstance modname (DataDef True n fs cs) =
    let _ = nameSupply cs
        (_, frattr, _, _) = attrpats fs
        mixattrs = if null fs then False else True
    in
    text "instance HTypeable" <+> ppHName modname n <+> text "where" $$
    nest 4 ( text "toHType x = Defined \"" <> ppXName n <> text "\" [] []" )
    $$
    text "instance XmlContent" <+> ppHName modname n <+> text "where" $$
    nest 4 ( vcat (map (mkToAux modname mixattrs) cs)
           $$ text "parseContents = oneOf"
           $$ nest 4 ( text "[" <+> mkParseConstr modname frattr (head cs)
                     $$ vcat (map (\c-> text "," <+> mkParseConstr modname frattr c)
                                  (tail cs))
                     $$ text "] `adjustErr` (\"in <" <> ppXName n
                                                     <> text ">, \"++)"
                     )
           )
    $$
    mkInstanceAttrs modname Extended n fs

-- enumeration of attribute values
mkInstance modname (EnumDef n es) =
    text "instance XmlAttrType" <+> ppHName modname n <+> text "where" $$
    nest 4 ( text "fromAttrToTyp n (N n',v)" $$
             nest 4 (text "| n==n'     = translate (attr2str v)" $$
                     text "| otherwise = Nothing") $$
             nest 2 (text "where" <+> mkTranslate modname es)
           $$
             vcat (map (mkToAttr modname) es)
           )


data SameName = Same | Extended

mkInstanceAttrs        :: String -> SameName -> Name -> AttrFields -> Doc
mkInstanceAttrs modname _ _ [] = empty
mkInstanceAttrs modname s n fs =
    let ppName = case s of { Same-> ppHName modname;  Extended-> ppAName modname; }
    in
    text "instance XmlAttributes" <+> ppName n <+> text "where" $$
    nest 4 ( text "fromAttrs as =" $$
             nest 4 ( ppName n $$
                      nest 2 (vcat ((text "{" <+> mkFrFld modname n (head fs)):
                                     map (\x -> comma <+> mkFrFld modname n x) (tail fs)) $$
                              text "}"))
           $$
             text "toAttrs v = catMaybes " $$
             nest 4 (vcat ((text "[" <+> mkToFld modname (head fs)):
                           map (\x-> comma <+> mkToFld modname x) (tail fs)) $$
                     text "]")
           )


--                  respectively (frpat,frattr,topat,toattr)
attrpats :: AttrFields -> (Doc,Doc,Doc,Doc)
attrpats fs =
  if null fs then (text "[]", empty, empty, text "[]")
  else (text "as", parens (text "fromAttrs as"), text "as", parens (text "toAttrs as"))

mkParseConstr :: String -> Doc -> (Name, [StructType]) -> Doc
mkParseConstr modname frattr (c,sts) =
        fsep (text "return" <+> parens (ppHName modname c <+> frattr)
             : map mkParseContents sts)

mkParseContents :: StructType -> Doc
mkParseContents st =
  let ap = text "`apply`" in
          case st of
            (Maybe String)    -> ap <+> text "optionalMb text"
            (Maybe _)         -> ap <+> text "optionalMb parseContents"
            (List String)     -> ap <+> text "many text"
            (List _)          -> ap <+> text "many parseContents"
            (List1 _)         -> ap <+> text "parseContents"
            (Tuple _)         -> ap <+> text "parseContents"
            (OneOf _)         -> ap <+> text "parseContents"
            (StringMixed)     -> ap <+> text "text"
            (String)          -> ap <+> text "(liftM Str text `onFail` return (Str \"\"))"
            (Any)             -> ap <+> text "parseContents"
            (Defined _)       -> ap <+> text "parseContents"
            (Defaultable _ _) -> ap <+> text "nyi_fromElem_Defaultable"

--
mkToElem :: [StructType] -> [Doc] -> Doc
mkToElem []  [] = text "[]"
mkToElem sts vs =
    fsep (intersperse (text "++") (zipWith toElem sts vs))
  where
    toElem st v =
      case st of
        (Maybe String)    -> text "maybeMb [] (toText . unStr)" <+> v
        (Maybe _)         -> text "maybeMb [] toContents" <+> v
        (List String)     -> text "concatMap (toText . unStr)" <+> v
        (List _)          -> text "concatMap toContents" <+> v
        (List1 _)         -> text "toContents" <+> v
        (Tuple _)         -> text "toContents" <+> v
        (OneOf _)         -> text "toContents" <+> v
        (StringMixed)     -> text "(toText . unStr)" <+> v
        (String)          -> text "(toText . unStr)" <+> v
        (Any)             -> text "toContents" <+> v
        (Defined _)       -> text "toContents" <+> v
        (Defaultable _ _) -> text "nyi_toElem_Defaultable" <+> v

-- mkRpat :: [Doc] -> Doc
-- mkRpat [v] = v
-- mkRpat vs  = (parens . hcat . intersperse comma) vs

mkCpat :: String -> Name -> Doc -> [Doc] -> Doc
mkCpat modname n i vs = ppHName modname n <+> i <+> fsep vs

nameSupply :: [b] -> [Doc]
nameSupply  ss = take (length ss) (map char ['a'..'z']
                                  ++ map text [ a:n:[] | n <- ['0'..'9']
                                                       , a <- ['a'..'z'] ])
-- nameSupply2 ss = take (length ss) [ text ('c':v:[]) | v <- ['a'..]]

mkTranslate :: String -> [Name] -> Doc
mkTranslate modname es =
    vcat (map trans es) $$
    text "translate _ = Nothing"
  where
    trans n = text "translate \"" <> ppXName n <> text "\" =" <+>
              text "Just" <+> ppHName modname n

mkToAttr :: String -> Name -> Doc
mkToAttr modname n = text "toAttrFrTyp n" <+> ppHName modname n <+> text "=" <+>
             text "Just (N n, str2attr" <+> doubleQuotes (ppXName n) <> text ")"

mkFrFld :: String -> Name -> (Name,StructType) -> Doc
mkFrFld modname tag (n,st) =
    ppHName modname n <+> text "=" <+>
    ( case st of
        (Defaultable String s) -> text "defaultA (\\a b -> fmap Str $ fromAttrToStr a b)" <+>
                                                 doubleQuotes (text s)
        (Defaultable _ s)      -> text "defaultA fromAttrToTyp" <+> text s
        (Maybe String)         -> text "possibleMbA (\\a b -> fmap Str $ fromAttrToStr a b)"
        (Maybe _)              -> text "possibleMbA fromAttrToTyp"
        String                 -> text "definiteA (\\a b -> fmap Str $ fromAttrToStr a b)" <+>
                                                 doubleQuotes (ppXName tag)
        _                      -> text "definiteA fromAttrToTyp" <+>
                                                 doubleQuotes (ppXName tag)
    ) <+> doubleQuotes (ppXName n) <+> text "as"

mkToFld :: String -> (Name,StructType) -> Doc
mkToFld modname (n,st) =
    ( case st of
        (Defaultable String _) -> text "defaultToAttr (\\s x -> toAttrFrStr s (unStr x))"
        (Defaultable _ _)      -> text "defaultToAttr toAttrFrTyp"
        (Maybe String)         -> text "mbToAttr (\\s x -> toAttrFrStr s (unStr x))"
        (Maybe _)              -> text "mbToAttr toAttrFrTyp"
        String                 -> text "(\\s x -> toAttrFrStr s (unStr x))"
        _                      -> text "toAttrFrTyp"
    ) <+> doubleQuotes (ppXName n) <+> parens (ppHName modname n <+> text "v")

mkToAux :: String -> Bool -> (Name,[StructType]) -> Doc
mkToAux modname mixattrs (n,sts) =
    let vs = nameSupply sts
        attrs = if mixattrs then text "as" else empty
    in
    text "toContents" <+> parens (mkCpat modname n attrs vs) <+> text "=" <+>
    mkToElem sts vs

mkToMult :: String -> Name -> Doc -> Doc -> (Name,[StructType]) -> Doc
mkToMult modname tag attrpat attrexp (n,sts) =
    let vs = nameSupply sts
    in
    text "toContents" <+> parens (mkCpat modname n attrpat vs) <+> text "="
    $$ nest 4 (text "[CElem (Elem (N \"" <> ppXName tag <> text "\")"<+> attrexp
              <+> parens (mkToElem sts vs) <+> text ") ()]")


--------------------------------------------------------------------------
-- | pretty-print Generic representation of Haskell DTDs.

pK1REmpty = text "K1" <+> text "R" <+> text "()"
pK1VEmpty = text "K1" <+> text "()"

ppDataTypeGeneric :: String -> TypeDef -> Doc

-- no attrs, no constructors
ppDataTypeGeneric modname (DataDef _ n [] []) =
  let hn = ppHName modname n
   in pFirstLine hn $+$
     nest 2 (
        pRepBefore hn <+> pK1REmpty <+> text ":*:" <+> pK1REmpty $+$
        pFromBefore hn <+> pK1VEmpty <+> text ":*:" <+> pK1VEmpty $+$
        pToBefore (parens (pK1VEmpty <+> text ":*:" <+> pK1VEmpty)) <+> hn
       )

-- no attrs, single constructor
ppDataTypeGeneric modname (DataDef _ n [] [(cn, [cstruct])]) =
  let hn = ppHName modname n
      hcn = ppHName modname cn
      vcs = ppStructVar cstruct
   in pFirstLine hn $+$
     nest 2 (
       pRepBefore hn <+> pK1REmpty <+> text ":*:" <+> ppRepStructGeneric modname cstruct $+$
       pFromBefore (parens (hcn <+> vcs)) <+> pK1VEmpty <+> text ":*:" <+> pK1 <+> vcs $+$
       pToBefore (parens (pK1VEmpty <+> text ":*:" <+> pK1 <+> vcs)) <+> hcn <+> vcs
       )

-- no attrs, single constructor
ppDataTypeGeneric modname (DataDef _ n [] [(cn, cstructs)]) =
  let hn = ppHName modname n
      hcn = ppHName modname cn
      (vcs, _) = ppStructVarProd cstructs Map.empty
      (gvcs, _) = ppGenericStructVarProd cstructs Map.empty
   in pFirstLine hn $+$
     nest 2 (
       pRepBefore hn <+> pK1REmpty <+> text ":*:" <+> ppRepStructGenericProd modname cstructs $+$
       pFromBefore (parens (hcn <+> vcs)) <+> pK1VEmpty <+> text ":*:" <+> gvcs $+$
       pToBefore (parens (pK1VEmpty <+> text ":*:" <+> parens gvcs)) <+> hcn <+> vcs
       )
-- no attrs, multiple constructors
ppDataTypeGeneric modname (DataDef _ n [] cs) =
  let hn = ppHName modname n
   in pFirstLine hn $+$
     nest 2 (
       pRepBefore hn <+> pK1REmpty <+> text ":*:" <+> parens (ppRepConstructorList  modname cs) $+$
       -- create a list of from. comment: construct from && to at the same time.
       pFromToConstructors modname cs
       )

-- nonzero attrs, no constructors
ppDataTypeGeneric modname (DataDef _ n fs []) =
  let hn = ppHName modname n
      genericVarAttrList = ppGenericVarAttrList modname fs
      varAttrList = ppVarAttrList modname fs
   in pFirstLine hn $+$
     nest 2 (
       pRepBefore hn <+> parens (ppRepAttrList  modname fs) <+> text ":*:" <+> pK1REmpty $+$
       pFromBefore (parens (hn <+> varAttrList)) <+> parens genericVarAttrList <+> text ":*:" <+> pK1VEmpty $+$
       pToBefore (parens (parens genericVarAttrList <+> text ":*:" <+> pK1VEmpty)) <+> hn <+> varAttrList
       )

-- nonzero attrs, one or more constrs
ppDataTypeGeneric modname (DataDef _ n fs cs) =
  let attrDataTypeName = ppBindAName n
      attrVarName      = ppBindANameVar n
   in ppAttrElemGenericInstance modname n attrDataTypeName attrVarName cs -- $+$
      --ppDataTypeGeneric modname (DataDef False (appendAttrs n) fs [])


-- enumerations (of attribute values)
ppDataTypeGeneric modname (EnumDef n es) =
 let hn = ppHName modname n
  in pFirstLine hn $+$
     nest 2 (
       pRepBefore hn <+> pEnumGeneric es $+$
       pFromToEnum modname es
       )

pFirstLine  hn = text "instance" <+> text "Generic" <+> hn  <+> text "where"
pRepBefore hn = text "type" <+> text "Rep" <+> hn <+> text "="
pU1 = text "U1"
pK1 = text "K1"
pFromBefore hn = text "from" <+> hn <+> text "="
pToBefore hn = text "to" <+> hn <+> text "="
pFrom = text "from"
pTo = text "to"
pEq = text "="
pK1R = text "K1" <+> text "R"

ppRepConstructorList :: String -> Constructors -> Doc
ppRepConstructorList modname [(cn, structs)] = ppRepStructGenericProd modname structs
ppRepConstructorList modname (x:xs) = (ppRepConstructorList modname [x]) <+> text ":+:" <+> (ppRepConstructorList modname xs)



ppRepStructGenericProd :: String -> [StructType] -> Doc
ppRepStructGenericProd modname [x]    = ppRepStructGeneric modname x
ppRepStructGenericProd modname (x:xs) = (ppRepStructGeneric modname x) <+> text ":*:" <+> (ppRepStructGenericProd modname xs)

ppRepStructGeneric :: String -> StructType -> Doc
ppRepStructGeneric modname (Defaultable st _)     = ppRepStructGeneric modname st
ppRepStructGeneric modname (Maybe st)             = pK1R <+> parens (text "Mb" <+> ppST modname st)
ppRepStructGeneric modname lst@(List st)          = pK1R <+> ppST modname lst
ppRepStructGeneric modname lst1@(List1 st)        = pK1R <+> parens (text "List1" <+> ppST modname st)
ppRepStructGeneric modname tp@(Tuple _)           = undefined
ppRepStructGeneric modname oneOf@(OneOf [])       = pK1R <+> text "()"
ppRepStructGeneric modname oneOf@(OneOf [st])     = pK1R <+> ppST modname st
ppRepStructGeneric modname oneOf@(OneOf (st:sts)) = pK1R <+> parens (ppST modname st <+> ppRepStructGeneric modname (OneOf sts))
ppRepStructGeneric modname StringMixed            = pK1R <+> text "Str"
ppRepStructGeneric modname String                 = pK1R <+> text "Str"
ppRepStructGeneric modname Any                    = undefined
ppRepStructGeneric modname (Defined n)            = pK1R <+> ppHName modname n -- TODO: not sure


type VarEnv = Map String Int

-- for create a Map for duplication checking.
structTypeToString :: StructType -> String
structTypeToString (Defaultable st _)        = structTypeToString st
structTypeToString  (Maybe st)               = "maybe" ++ structTypeToString st
structTypeToString (List st)                 = structTypeToString st ++ "List"
structTypeToString (List1 st)                = structTypeToString st ++ "List1"
structTypeToString (OneOf [])                = "OneOf"
structTypeToString (OneOf [st])              = structTypeToString st
structTypeToString (OneOf (st:sts))          = "OneOfListEither"
structTypeToString StringMixed               = "String"
structTypeToString String                    = "String"
structTypeToString Any                       = "anyString"
structTypeToString (Defined (Name _ hname))  = hname

ppStructVarProd :: [StructType] -> VarEnv -> (Doc, VarEnv)
ppStructVarProd  [x]    varEnv = let str = structTypeToString x
                                     re  = ppStructVar x
                                  in case Map.lookup str varEnv of
                                          Just n  -> ( re <> text (show (n+1)), Map.update (\n -> Just (n+1)) str varEnv)
                                          Nothing -> (re, Map.insert  str 0 Map.empty )
ppStructVarProd  (x:xs) varEnv = let (ppx, varEnv') = ppStructVarProd [x] varEnv
                                     (ppxs, varEnv'') = ppStructVarProd xs varEnv'
                                  in (ppx <+> ppxs, varEnv'')

-- Generate variables for generic from and to function
ppStructVar :: StructType -> Doc
ppStructVar (Defaultable st _)        = ppStructVar st
ppStructVar (Maybe st)                = text "maybeVar" --todo
ppStructVar (List st)                 = ppStructVar st <> text "lst"
ppStructVar (List1 st)                = ppStructVar st <> text "lst1"
ppStructVar (Tuple _)                 = text "tupleVar" --TODO
ppStructVar (OneOf [])                = text "()" --todo
ppStructVar (OneOf [st])              = ppStructVar st
ppStructVar (OneOf (st:sts))          = text "eitherVar"
ppStructVar StringMixed               = text "str"
ppStructVar String                    = text "str"
ppStructVar Any                       = text "anyVar"
ppStructVar (Defined n)               = ppXName n




ppGenericStructVarProd :: [StructType] -> VarEnv -> (Doc, VarEnv)
ppGenericStructVarProd  [x]    varEnv = let str = structTypeToString x
                                            re  = ppGenericStructVar x
                                         in case Map.lookup str varEnv of
                                                 Just n  -> ( re <> text (show (n+1)), Map.update (\n -> Just (n+1)) str varEnv)
                                                 Nothing -> (re, Map.insert  str 0 Map.empty )
ppGenericStructVarProd  (x:xs) varEnv = let (ppx, varEnv') = ppGenericStructVarProd [x] varEnv
                                            (ppxs, varEnv'') = ppGenericStructVarProd xs varEnv'
                                         in (ppx <+> text ":*:" <+> ppxs, varEnv'')

ppGenericStructVar :: StructType -> Doc
ppGenericStructVar (Defaultable st _)        = ppGenericStructVar st
ppGenericStructVar (Maybe st)                = pK1 <+> text "maybeVar" --todo
ppGenericStructVar (List st)                 = pK1 <+> ppStructVar st <> text "lst"
ppGenericStructVar (List1 st)                = pK1 <+> ppStructVar st <> text "lst1"
ppGenericStructVar (Tuple _)                 = pK1 <+> text "tupleVar"
ppGenericStructVar (OneOf [])                = pK1 <+> text "()"
ppGenericStructVar (OneOf [st])              = ppGenericStructVar st
ppGenericStructVar (OneOf (st:sts))          = pK1 <+> text "eitherVar"
ppGenericStructVar StringMixed               = pK1 <+> text "str"
ppGenericStructVar String                    = pK1 <+> text "str"
ppGenericStructVar Any                       = pK1 <+> text "anyVar"
ppGenericStructVar (Defined n)               = pK1 <+> ppXName n



pFromToConstructors :: String -> Constructors -> Doc
pFromToConstructors modname constructors = let (froms, tos) = pFromToConstructorsHelp constructors modname [] (length constructors == 1) True
                                            in foldl ($+$) empty (froms ++ tos)

-- the third argument remeber the current depth, and the path: R1 R1 ...
pFromToConstructorsHelp :: Constructors -> String -> [String] -> Bool -> Bool -> ([Doc],[Doc])
pFromToConstructorsHelp [(cn, structs)] modname path bool firstRound =
    let hcn = ppHName modname cn
        (vcs, _) = ppStructVarProd structs Map.empty
        (gvcs, _) = ppGenericStructVarProd structs Map.empty
        surfaceDoc = parens (hcn <+> vcs)
        genericDocR = pPathGenericR path gvcs
        genericDocL = pPathGenericL path gvcs
     in if bool
        then if firstRound
             then ([pFromBefore surfaceDoc <+> pK1VEmpty <+> text ":*:" <+> parens genericDocR],
                   [pToBefore (parens (pK1VEmpty <+> text ":*:" <+> (parens genericDocR))) <+> surfaceDoc])
             else ([pFromBefore surfaceDoc <+> pK1VEmpty <+> text ":*:" <+> parens genericDocL],
                   [pToBefore (parens (pK1VEmpty <+> text ":*:" <+> (parens genericDocL))) <+> surfaceDoc])
        else if null path
             then ([pFromBefore surfaceDoc <+> pK1VEmpty <+> text ":*:" <+> parens (text "L1" <+> (parens gvcs))],
                   [pToBefore (parens (pK1VEmpty <+> text ":*:" <+>  parens ((text "L1") <+> (parens gvcs)))) <+> surfaceDoc])
             else ([pFromBefore surfaceDoc <+> pK1VEmpty <+> text ":*:" <+> parens genericDocR],
                   [pToBefore (parens (pK1VEmpty <+> text ":*:" <+> (parens genericDocR))) <+> surfaceDoc])
pFromToConstructorsHelp (firstCon: restCons) modname path bool firstRound =
  let (firstFDoc, firstToDoc) = pFromToConstructorsHelp [firstCon] modname path True False
      (restFDoc, restToDoc) = pFromToConstructorsHelp restCons modname (path ++ ["R1"]) False False
   in (firstFDoc ++ restFDoc, firstToDoc ++ restToDoc)

pPathGenericR :: [String] -> Doc -> Doc
pPathGenericR [] gvcs = gvcs
pPathGenericR (x:xs) gvcs = (text x) <+> parens (pPathGenericR xs gvcs)
pPathGenericL :: [String] -> Doc -> Doc
pPathGenericL [] gvcs = text "L1" <+> parens gvcs
pPathGenericL (x:xs) gvcs = (text x) <+> parens (pPathGenericL xs gvcs)


ppRepAttrList :: String -> AttrFields -> Doc
ppRepAttrList modname [(fn, struct)] = ppRepStructGeneric modname struct
ppRepAttrList modname (x : xs)       = ppRepAttrList modname [x] <+> text ":*:" <+> ppRepAttrList modname xs



ppGenericVarAttrList :: String -> AttrFields -> Doc
ppGenericVarAttrList modname [(fn, struct)] = pK1 <+> ppXName fn
ppGenericVarAttrList modname (x:xs)         = ppGenericVarAttrList modname [x] <+> text ":*:" <+> ppGenericVarAttrList modname xs

ppVarAttrList :: String -> AttrFields -> Doc
ppVarAttrList modname [(fn, struct)] = ppXName fn
ppVarAttrList modname (x:xs)         = ppVarAttrList modname [x] <+> ppVarAttrList modname xs


ppAttrElemGenericInstance :: String -> Name -> Doc -> Doc -> Constructors -> Doc
ppAttrElemGenericInstance modname n attrDataTypeName attrVarName cs =
  let hn = ppHName modname n
   in pFirstLine hn $+$
     nest 2 (
       pRepBefore hn <+> pK1R <+> attrDataTypeName <+> text ":*:" <+> parens (ppRepConstructorList modname cs) $+$
       pFromToConstructorsWithAttr modname attrVarName cs
       )
pFromToConstructorsWithAttr :: String -> Doc -> Constructors -> Doc
pFromToConstructorsWithAttr modname attrVarName cs =
  let (froms, tos) = pFromToConstructorsWithAttrHelp cs modname attrVarName [] (length cs == 1) True
   in foldl ($+$) empty (froms ++ tos)

pFromToConstructorsWithAttrHelp :: Constructors -> String -> Doc -> [String] -> Bool -> Bool -> ([Doc], [Doc])
pFromToConstructorsWithAttrHelp [(cn, structs)] modname attrVarName path bool firstRound =
    let hcn = ppHName modname cn
        (vcs, _) = ppStructVarProd structs Map.empty
        (gvcs, _) = ppGenericStructVarProd structs Map.empty
        surfaceDoc = parens (hcn <+> attrVarName <+> vcs)
        genericDocR = pPathGenericR path gvcs
        genericDocL = pPathGenericL path gvcs
     in if bool
        then if firstRound
             then ([pFromBefore surfaceDoc <+> pK1 <+> attrVarName <+> text ":*:" <+> parens genericDocR],
                   [pToBefore (parens (pK1 <+> attrVarName  <+> text ":*:" <+> (parens genericDocR))) <+> surfaceDoc])
             else ([pFromBefore surfaceDoc <+> pK1 <+> attrVarName <+> text ":*:" <+> parens genericDocL],
                   [pToBefore (parens (pK1 <+> attrVarName  <+> text ":*:" <+> (parens genericDocL))) <+> surfaceDoc])
        else if null path
             then ([pFromBefore surfaceDoc <+> pK1 <+> attrVarName <+> text ":*:" <+> parens (text "L1" <+> (parens gvcs))],
                   [pToBefore (parens (pK1 <+> attrVarName <+> text ":*:" <+>  parens ((text "L1") <+> (parens gvcs)))) <+> surfaceDoc])
             else ([pFromBefore surfaceDoc <+> pK1 <+> attrVarName  <+> text ":*:" <+> parens genericDocR],
                   [pToBefore (parens (pK1 <+> attrVarName  <+> text ":*:" <+> (parens genericDocR))) <+> surfaceDoc])
pFromToConstructorsWithAttrHelp (firstCon: restCons) modname attrVarName path book firstRound =
  let (firstFDoc, firstToDoc) = pFromToConstructorsWithAttrHelp [firstCon] modname attrVarName path True False
      (restFDoc, restToDoc) = pFromToConstructorsWithAttrHelp restCons modname attrVarName (path ++ ["R1"]) False False
   in (firstFDoc ++ restFDoc, firstToDoc ++ restToDoc)



pEnumGeneric :: [Name] -> Doc
pEnumGeneric es = hsep (intersperse (text ":+:") (map (\_ -> text "K1 R ()") es))



pFromToEnum :: String -> [Name] -> Doc
pFromToEnum modname es = let (froms, tos) = pFromToEnumHelp modname es [] (length es == 1) True
                  in foldl ($+$) empty (froms ++ tos)

pFromToEnumHelp :: String -> [Name] -> [String] -> Bool -> Bool -> ([Doc], [Doc])
pFromToEnumHelp modname [n] path bool firstRound =
  let hn = ppHName modname n
      atomicPat = text "K1 ()"
      genericPat = pPathGenericPat path atomicPat
      genericPatL = pPathGenericPatL path atomicPat
   in if bool
      then if firstRound
           then ([pFromBefore hn <+> genericPat],
                 [pToBefore (parens genericPat) <+> hn])
           else ([pFromBefore hn <+> genericPatL],
                 [pToBefore (parens genericPatL) <+> hn])
      else if null path
           then ([pFromBefore hn <+> text "L1" <+> parens genericPat],
                 [pToBefore (parens (text "L1" <+> parens genericPat) <+> hn)])
           else ([pFromBefore hn <+> genericPat],
                 [pToBefore (parens genericPat)  <+> hn])
pFromToEnumHelp modname (firstN: restN) path bool firstRound =
  let (firstFDoc, firstToDoc) = pFromToEnumHelp modname [firstN] path True False
      (restFDoc, restToDoc) = pFromToEnumHelp modname restN (path ++ ["R1"]) False False
   in (firstFDoc ++ restFDoc, firstToDoc ++ restToDoc)

pPathGenericPat :: [String] -> Doc -> Doc
pPathGenericPat []      atomicPat = atomicPat
pPathGenericPat (x: xs) atomicPat = (text x) <+> parens (pPathGenericPat xs atomicPat)

pPathGenericPatL :: [String] -> Doc -> Doc
pPathGenericPatL []      atomicPat = text "L1" <+> parens atomicPat
pPathGenericPatL (x: xs) atomicPat = (text x) <+> parens (pPathGenericPat xs atomicPat)
