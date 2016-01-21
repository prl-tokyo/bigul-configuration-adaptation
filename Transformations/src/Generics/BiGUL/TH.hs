{-# LANGUAGE TemplateHaskell, TupleSections, DeriveDataTypeable #-}
module Generics.BiGUL.TH( branch, normal',adaptive',adaptive,normal,rearr,update,rearrAndUpdate,deriveBiGULGeneric) where
import Data.Data
import Data.Maybe
import Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Quote
import Generics.BiGUL
import Generics.BiGUL.AST
import Control.Monad


data ConTag = L | R
    deriving (Show, Data, Typeable)


data PatTag = PTag | UTag | RTag | ETag

instance Show PatTag where
   show PTag = "P"
   show UTag = "U"
   show RTag = "R"
   show ETag = "E"

contag :: a -> a -> ConTag -> a
contag a1 _  L = a1
contag _  a2 R = a2


class ConTagSeq a where
  toConTags :: a -> Name -> [ConTag]


type TypeConstructor = String
type ValueConstructor = String
type ErrorMessage = String


lookupName :: (String -> Q (Maybe Name)) -> ErrorMessage -> String -> Q Name
lookupName f errMsg name = f name >>= maybe (fail errMsg) return

-- ["Generic", "K1", "U1", ":+:", ":*:", "Rep"]
lookupNames :: [TypeConstructor] -> [ValueConstructor] -> ErrorMessage -> Q ([Name], [Name])
lookupNames typeCList valueCList errMsg = liftM2 (,) (mapM (lookupName lookupTypeName  errMsg) typeCList)
                                                     (mapM (lookupName lookupValueName errMsg) valueCList)

-- Find the Type Dec by Name
-- Construct an InstanceDec.
deriveBiGULGeneric :: Name -> Q [InstanceDec]
deriveBiGULGeneric name = do
  (name, typeVars, constructors) <-
    do
      info <- reify name
      case info of
        (TyConI (DataD [] name typeVars constructors _)) -> return (name, typeVars, constructors)
        _            -> fail ( "cannot find " ++ nameBase name ++ ", or not a (supported) datatype.")
  ([nGeneric, nRep, nK1, nR, nU1, nSum, nProd, nV1, nS1, nSelector, nDataType], [vFrom, vTo, vK1, vL1, vR1, vU1, vProd, vSelName, vDataTypeName, vModuleName, vM1]) <-
    lookupNames [ "GHC.Generics." ++ s | s <- ["Generic", "Rep", "K1", "R", "U1", ":+:", ":*:", "V1", "S1", "Selector", "Datatype"] ]
                [ "GHC.Generics." ++ s | s <- ["from", "to", "K1", "L1", "R1", "U1", ":*:", "selName", "datatypeName", "moduleName", "M1"] ]
                "cannot find type/value constructors from GHC.Generics."
  env <- consToEnv constructors
  let selectorsNameList =  generateSelectorNames constructors
  let selectorDataDMaybeList = generateSelectorDataD selectorsNameList
  let selectorDataTypeMaybeList = map (generateSelectorDataType nDataType vDataTypeName vModuleName (maybe "" id (nameModule name))) selectorsNameList
  let selectorNameAndConList = zip selectorsNameList constructors
  let selectorInstanceDecList = map (generateSelectorInstanceDec nSelector vSelName) selectorNameAndConList
  let fromClauses = map (constructFuncFromClause (vK1, vU1, vL1, vR1, vProd, vM1)) env
  let toClauses   = map (constructFuncToClause (vK1, vU1, vL1, vR1, vProd, vM1)) env
  --conTagsClause <- toconTagsClause env
  return $ listMaybe2Just selectorDataDMaybeList ++
           listMaybe2Just (concat selectorDataTypeMaybeList) ++ 
           listMaybe2Just (concat selectorInstanceDecList) ++ 
           [InstanceD []
                     (AppT (ConT nGeneric) (generateTypeVarsType name typeVars))
                     [TySynInstD nRep
                                 (TySynEqn
                                    [generateTypeVarsType name typeVars]
                                    (constructorsToSum (nSum, nV1) (map (constructorToProduct (nK1, nR, nU1, nProd, nS1)) selectorNameAndConList))),
                      FunD vFrom fromClauses,
                      FunD vTo toClauses ]
            ] 

listMaybe2Just :: [Maybe a] -> [a]
listMaybe2Just xs = foldr (\a b -> case a of {Just v -> v:b; Nothing -> b}) [] xs


toconTagsClause :: [(Bool, Name, [ConTag], [Name])] -> Q Clause
toconTagsClause env = do
  (_, [vEq, vError]) <- lookupNames [] ["==", "error"] "cannot find functions for eq or error."
  conTagsVarName <- newName "name"
  expEnv <- mapM (\(b, n, conTags, _) -> liftM2 (,) (dataToExpQ (const Nothing) n) (dataToExpQ (const Nothing) conTags)) env
  let conTagsClauseBody = (foldr (\(nExp, lrsExp) e -> CondE ((VarE vEq `AppE` nExp) `AppE` VarE conTagsVarName) lrsExp e)
                (VarE vError `AppE` LitE (StringL "cannot find name."))
                expEnv)
  return $ Clause [WildP, VarP conTagsVarName] (NormalB conTagsClauseBody) []



constructorsToSum :: (Name, Name) -> [Type] -> Type
constructorsToSum (sum, v1) []  = ConT v1 -- empty
constructorsToSum (sum, v1) tps = foldr1 (\t1 t2 -> (ConT sum `AppT` t1) `AppT` t2) tps


constructorToProduct :: (Name, Name, Name, Name, Name) -> ([Maybe Name], Con) -> Type
constructorToProduct (k1, r, u1, prod, s1) (_,     NormalC _ [] ) = ConT u1
constructorToProduct (k1, r, u1, prod, s1) (_,     NormalC _ sts) = foldr1 (\t1 t2 -> (ConT prod `AppT` t1 ) `AppT` t2) $ map (AppT (ConT k1 `AppT` ConT r) . snd) sts
constructorToProduct (k1, r, u1, prod, s1) (names, RecC    _ sts) = foldr1 (\t1 t2 -> (ConT prod `AppT` t1 ) `AppT` t2) $ map (\(Just n, st) -> AppT (ConT s1 `AppT` ConT n) ((ConT k1 `AppT` ConT r) `AppT` third st)) (zip names sts)
constructorToProduct _ _ = error "not supported Con"

third :: (a, b, c) -> c
third  (_, _, z) = z

-- Bool indicates: if Normal then False else RecC True
constructorToPatAndBody :: Con -> Q (Bool, Name, [Name])
constructorToPatAndBody (NormalC name sts) = liftM (False, name,) $ replicateM (length sts) (newName "var")
constructorToPatAndBody (RecC    name sts) = liftM (True, name,) $ replicateM (length sts) (newName "var")
constructorToPatAndBody _ = fail "not supported Cons"


zipWithLRs :: [(Bool, Name, [Name])] ->  [(Bool, Name, [ConTag], [Name])]
zipWithLRs nns = zipWith (\(b, n, ns) lrs -> (b, n, lrs, ns)) nns (constructLRs (length nns))

consToEnv :: [Con] -> Q [(Bool, Name, [ConTag], [Name])]
consToEnv cons = liftM zipWithLRs $ mapM constructorToPatAndBody cons

constructFuncFromClause :: (Name, Name, Name, Name, Name, Name) -> (Bool, Name, [ConTag], [Name]) -> Clause
constructFuncFromClause (vK1, vU1, vL1, vR1, vProd, vM1) (b, n, lrs, names) =  Clause [ConP n (map VarP names)] (NormalB (wrapLRs lrs (deriveGeneric names))) []
  where
    wrapLRs :: [ConTag] -> Exp -> Exp
    wrapLRs lrs exp = foldr (\lr e -> ConE (contag vL1 vR1 lr) `AppE` e) exp lrs

    deriveGeneric :: [Name] -> Exp
    deriveGeneric []    = ConE vU1
    deriveGeneric names = foldr1 (\e1 e2 -> (ConE vProd `AppE` e1) `AppE` e2) $ map (\name -> if b then ConE vM1 `AppE` (ConE vK1 `AppE` VarE name) else ConE vK1 `AppE` VarE name) names


constructFuncToClause :: (Name, Name, Name, Name, Name, Name) -> (Bool, Name, [ConTag], [Name])  -> Clause
constructFuncToClause (vK1, vU1, vL1, vR1, vProd, vM1) (b, n, lrs, names)  = Clause [wrapLRs lrs (deriveGeneric names)] (NormalB (foldl (\e1 name -> e1 `AppE` (VarE name)) (ConE n) names) ) []
  where
    wrapLRs :: [ConTag] -> TH.Pat -> TH.Pat
    wrapLRs lrs pat = foldr (\lr p -> ConP (contag vL1 vR1 lr) [p]) pat lrs

    deriveGeneric :: [Name] -> TH.Pat
    deriveGeneric []    = ConP vU1 []
    deriveGeneric names = foldr1 (\p1 p2 -> ConP vProd [p1, p2]) $ map (\name -> if b then (ConP vM1 ((:[]) (ConP vK1 ((:[]) (VarP name))))) else (ConP vK1 ((:[]) (VarP name)))) names

-- construct selector names from constructors
generateSelectorNames :: [Con] -> [[Maybe Name]]
generateSelectorNames = map (\con -> 
  case con of { 
      RecC _ sts -> map (\(n, _, _) -> Just (mkName ( "Selector_" ++ nameBase n))) sts;
      _          -> []
    })

generateSelectorDataD :: [[Maybe Name]] -> [Maybe Dec]
generateSelectorDataD names = map (\name -> case name of {Just n -> Just $ DataD [] n [] [] []; Nothing -> Nothing}) (concat names)

-- Selector DataType Generation
generateSelectorDataType :: Name -> Name -> Name -> String -> [Maybe Name] -> [Maybe Dec]
generateSelectorDataType nDataType vDataTypeName vModuleName moduleName = map (generateSelectorDataType' nDataType vDataTypeName vModuleName moduleName)

generateSelectorDataType' :: Name -> Name -> Name -> String -> Maybe Name -> Maybe Dec
generateSelectorDataType' nDataType vDataTypeName vModuleName moduleName (Just selectorName) =
  Just $ InstanceD []
    (AppT (ConT nDataType) (ConT selectorName))
    [FunD vDataTypeName ([Clause [WildP] (NormalB (LitE (StringL (show selectorName)))) []]),
     FunD vModuleName   ([Clause [WildP] (NormalB (LitE (StringL moduleName))) []]) 
    ]
generateSelectorDataType' nDataType vDataTypeName vModuleName moduleName _ = Nothing

-- Selector Instance Declaration generation
generateSelectorInstanceDec :: Name -> Name -> ([Maybe Name], Con) -> [Maybe Dec]
generateSelectorInstanceDec nSelector vSelName ([]   , _  ) = []
generateSelectorInstanceDec nSelector vSelName (names, (RecC _ sts)) = map (generateSelectorInstanceDec' nSelector vSelName) (zip names sts)

generateSelectorInstanceDec' :: Name -> Name -> (Maybe Name, THS.VarStrictType) -> Maybe Dec
generateSelectorInstanceDec' nSelector vSelName (Just selectorName, (name, _, _)) = 
  Just $ InstanceD []
            (AppT (ConT nSelector) (ConT selectorName))
            [FunD vSelName ([Clause [WildP] (NormalB (LitE (StringL (nameBase name)))) []])]
generateSelectorInstanceDec' _         _         _                          = Nothing


-- generate type representation of polymorhpic type
-- e.g. VBook a b is represented as: AppT (ConT name) (ConT name_a `AppT` ConT name_b)
generateTypeVarsType :: Name -> [TyVarBndr] -> Type
generateTypeVarsType n []    = ConT n -- not polymorphic case.
generateTypeVarsType n tvars = foldl (\a b -> AppT a b) (ConT n) $ map (\tvar -> 
   case tvar of 
    { PlainTV  name      -> VarT name; 
      KindedTV name kind -> VarT name-- error "kind type variables are not supported yet."
    }) tvars 


constructLRs :: Int -> [[ConTag]]
constructLRs 0 = []
constructLRs 1 = [[]]
constructLRs n = [L] : map (R:) (constructLRs (n-1))

lookupLRs :: Name -> Q [ConTag]
lookupLRs conName = do
  info <- reify conName
  datatypeName <-
    case info of
      DataConI _ _ n _ -> return n
      _ -> fail $ nameBase conName ++ " is not a data constructor"
  TyConI (DataD _ _ _ cons _) <- reify datatypeName
  return $ constructLRs (length cons) !!
             fromJust (List.findIndex (== conName) (map (\con -> case con of { NormalC n _ -> n; RecC n _ -> n}) cons))

lookupRecordLength :: Name -> Q Int
lookupRecordLength conName = do
  info <- reify conName
  datatypeName <-
    case info of
      DataConI _ _ n _ -> return n
      _ -> fail $ nameBase conName ++ " is not a data constructor"
  TyConI (DataD _ _ _ cons _) <- reify datatypeName
  return $ (\(RecC _ fs) -> length fs) (fromJust (List.find (\(RecC n _) -> n == conName) cons))

lookupRecordField :: Name -> Name -> Q Int
lookupRecordField conName fieldName = do
  info <- reify conName
  datatypeName <-
    case info of
      DataConI _ _ n _ -> return n
      _ -> fail $ nameBase conName ++ " is not a data constructor"
  TyConI (DataD _ _ _ cons _) <- reify datatypeName
  case (List.findIndex (\(n,_,_) -> n == fieldName) ((\(RecC _ fs) -> fs) $ fromJust (List.find (\(RecC n _) -> n == conName) cons))) of
       Just res -> return res
       Nothing -> fail $ nameBase fieldName ++ " is not a field in " ++ nameBase conName


mkConstrutorFromLRs :: [ConTag] -> PatTag -> Q (Exp -> Exp)
mkConstrutorFromLRs lrs patTag = do (_, [gin, gleft, gright]) <- lookupNames [] [ "Generics.BiGUL.AST." ++ show patTag ++ s | s <- ["In", "Left", "Right"] ] "cannot find data constructors *what* from Generic.BiGUL.AST"
                                    return $ foldl (.) (AppE (ConE gin)) (map (AppE . ConE . contag gleft gright) lrs)


astNameSpace :: String
astNameSpace = "Generics.BiGUL.AST."

mkPat :: TH.Pat -> PatTag -> Q TH.Exp

mkPat (LitP c) patTag  = do
  (_, [gconst]) <- lookupNames [] [astNameSpace ++ show patTag ++ "Const"] (notFoundMsg $ show patTag ++ "Const")
  return $ ConE gconst `AppE` LitE c


-- user defined datatypes && unit pattern
mkPat (ConP name ps) patTag = do
  ConP name' [] <- [p| () |]
  if name == name' && ps == []
  then do
       unitt         <- [| () |]
       (_, [gconst]) <- lookupNames [] [astNameSpace ++ show patTag ++ s | s <- ["Const"]] (notFoundMsg $ show patTag ++ "Const")
       return $ ConE gconst `AppE` unitt
  else do
       lrs <- lookupLRs name
       conInEither <- mkConstrutorFromLRs lrs patTag
       pes         <- case ps of
                       [] -> [p| () |] >>= flip mkPat patTag
                       _  -> mkPat (TupP ps)  patTag
       return $ conInEither pes



mkPat (RecP name ps) patTag = do
  len <- lookupRecordLength name
  indexs <- mapM (\(n,_) -> lookupRecordField name n) ps
  let nps = map snd ps
  mkPat (ConP name (helper 0 len (zip indexs nps) [])) patTag
  where findInPair [] i = WildP
        findInPair ((j,p):xs) i | i == j = p
                                | otherwise = findInPair xs i
        helper i n pairs acc  | i == n = acc
                              | otherwise = helper (i+1) n pairs (acc++[findInPair pairs i])


mkPat (ListP []) patTag = [p| [] |] >>= flip mkPat patTag

mkPat (ListP (p:xs)) patTag = do
  hexp <- mkPat p patTag
  rexp <- mkPat (ListP xs) patTag
  (_, [gin,gright,gprod]) <- lookupNames [] [astNameSpace ++ show patTag ++ s | s <- ["In","Right","Prod"]] (notFoundMsg $ (concatWith " ". map (withPatTag patTag)) ["In","Right","Prod"])
  return $ ConE gin `AppE` (ConE gright `AppE` (ConE gprod `AppE` hexp `AppE` rexp))

mkPat (InfixP pl name pr) patTag = do
  ConE name' <- [| (:) |]
  if name == name'
  then do lpat <- mkPat pl patTag
          rpat <- mkPat pr patTag
          (_, [gin,gright,gprod]) <- lookupNames [] [astNameSpace ++ show patTag ++ s | s <- ["In","Right","Prod"]] (notFoundMsg $ (concatWith " ". map (withPatTag patTag)) ["In","Right","Prod"])
          return $ ConE gin `AppE` (ConE gright `AppE` (ConE gprod `AppE` lpat `AppE` rpat))
  else fail $ "constructors mismatch: " ++ nameBase name ++ " and " ++ nameBase name'


mkPat (TupP [p]) patTag = mkPat p patTag
mkPat (TupP (p:ps)) patTag = do
  lexp <- mkPat p patTag
  rexp <- mkPat (TupP ps) patTag
  (_, [gprod]) <- lookupNames [] [astNameSpace ++ show patTag ++ s | s <- ["Prod"]] (notFoundMsg "Prod")
  return ((ConE gprod `AppE` lexp) `AppE` rexp)


mkPat (WildP) PTag = do
  (_, [pvar])       <- lookupNames [] [astNameSpace ++ "PVar"]  (notFoundMsg "PVar")
  return $ ConE pvar
mkPat (WildP) UTag = do
  (_, [uvar, skip]) <- lookupNames [] [astNameSpace ++ s | s <- ["UVar", "Skip"]] (notFoundMsg "UVar, Skip")
  return $ ConE uvar `AppE` ConE skip
mkPat (WildP) RTag = fail $ "Wildcard(_) connot be used in lambda pattern expression."


mkPat (VarP name) PTag =  fail $ "Please do not use variables here, use wildcard(_) instead."
mkPat (VarP name) UTag =  do
  (_, [uvar])       <- lookupNames [] [astNameSpace ++ "UVar"] (notFoundMsg "UVar")
  return $ ConE uvar `AppE` VarE name
mkPat (VarP name) RTag =  do
  (_, [rvar])       <- lookupNames [] [astNameSpace ++ "RVar"] (notFoundMsg "RVar")
  return $ ConE rvar

mkPat _ patTag = fail $ "Pattern not handled yet."









-- rearrange all (VarE name) with env, generalized version
rearrangeExp :: Exp -> Map String Exp -> Q Exp
rearrangeExp (VarE name) env  =
  case Map.lookup (nameBase name) env of
    Just val -> return val
    Nothing  -> fail $ "cannot find name " ++ nameBase name ++ " in env."
rearrangeExp (AppE e1 e2) env = liftM2 AppE (rearrangeExp e1 env) (rearrangeExp e2 env)
rearrangeExp (ConE name) env  = return $ ConE name
rearrangeExp (LitE c)    env  = return $ LitE c
rearrangeExp _           env  = fail $ "Invalid representation of bigul program in TemplateHaskell ast"









mkEnvForRearr :: TH.Pat -> Q (Map String Exp)
mkEnvForRearr (LitP c) = return Map.empty

-- empty list is ok , mkEnvForRearr return Q Map.empty for it
mkEnvForRearr (ConP name ps) = mkEnvForRearr (TupP ps)

mkEnvForRearr (RecP name ps) = do
  len <- lookupRecordLength name
  indexs <- mapM (\(n,_) -> lookupRecordField name n) ps
  let nps = map snd ps
  mkEnvForRearr (ConP name (helper 0 len (zip indexs nps) []))
  where findInPair [] i = WildP
        findInPair ((j,p):xs) i | i == j = p
                                | otherwise = findInPair xs i
        helper i n pairs acc  | i == n = acc
                              | otherwise = helper (i+1) n pairs (acc++[findInPair pairs i])

mkEnvForRearr (ListP []) = return Map.empty
mkEnvForRearr (ListP (pl:pr))     = do
  (_, [dleft,dright]) <- lookupNames [] [ astNameSpace ++ s | s <- ["DLeft", "DRight"] ] (notFoundMsg "DLeft, DRight")
  lenv <- mkEnvForRearr pl
  renv <- mkEnvForRearr (ListP pr)
  return $ Map.map (ConE dleft `AppE`) lenv `Map.union`
          Map.map (ConE dright `AppE`) renv

mkEnvForRearr (InfixP pl name pr) = do
  (_, [dleft,dright]) <- lookupNames [] [ astNameSpace ++ s | s <- ["DLeft", "DRight"] ] (notFoundMsg "DLeft, DRight")
  lenv <- mkEnvForRearr pl
  renv <- mkEnvForRearr pr
  return $ Map.map (ConE dleft `AppE`) lenv `Map.union`
          Map.map (ConE dright `AppE`) renv

mkEnvForRearr (TupP ps) = do
  (_, [dleft,dright]) <- lookupNames [] [ astNameSpace ++ s | s <- ["DLeft", "DRight"] ] (notFoundMsg "DLeft, DRight")
  subenvs             <- mapM mkEnvForRearr ps
  let envs            =  zipWith (Map.map . foldr (.) id . map (AppE . ConE . contag dleft dright))
                                 (constructLRs (length ps)) subenvs
  return $ Map.unions envs

mkEnvForRearr WildP = return Map.empty

mkEnvForRearr (VarP name) = do
  (_, [dvar]) <- lookupNames [] [ astNameSpace ++ s | s <- ["DVar"] ] (notFoundMsg "DVar")
  return $ Map.singleton (nameBase name) (ConE dvar)

mkEnvForRearr  _    =  fail $ "Pattern not handled yet."





splitDataAndCon:: TH.Exp -> Q (TH.Exp -> TH.Exp ,[TH.Exp])

splitDataAndCon (AppE (ConE name) e2) = do
  lrs <- lookupLRs name
  con <- mkConstrutorFromLRs lrs ETag
  d   <- mkBodyExpForRearr e2
  return (con,[d])

splitDataAndCon (AppE e1 e2) = do
  (c, ds) <- splitDataAndCon e1
  d        <- mkBodyExpForRearr e2
  return (c,ds++[d])

splitDataAndCon _            =  fail $ "Invalid data constructor in lambda body expression"



mkBodyExpForRearr :: TH.Exp -> Q TH.Exp

mkBodyExpForRearr (LitE c) = do
  (_, [econst]) <- lookupNames [] [astNameSpace ++ "EConst"] (notFoundMsg "EConst")
  return $ ConE econst `AppE` (LitE c)

mkBodyExpForRearr (VarE name) =  return $ VarE name

mkBodyExpForRearr (AppE e1 e2) = do
  (_, [eprod]) <- lookupNames [] [astNameSpace ++ "EProd"] (notFoundMsg "EProd")
  (con, ds)   <- splitDataAndCon (AppE e1 e2)
  return $ con (foldr1 (\d1 d2 -> ConE eprod `AppE` d1 `AppE` d2) ds)


mkBodyExpForRearr (ConE name) =  do
  (ConE name') <- [| () |]
  (_, [econst]) <- lookupNames [] [astNameSpace ++ s | s <- ["EConst"] ] (notFoundMsg "EConst")
  if name == name'
  then return $ ConE econst `AppE` (ConE name)
  else mkBodyExpForRearr (AppE (ConE name) (ConE name'))

mkBodyExpForRearr (RecConE name es) = do
  (ConE name') <- [| () |]
  (_, [econst,eprod]) <- lookupNames [] [astNameSpace ++ s | s <- ["EConst","EProd"]] (notFoundMsg "EConst and EProd")
  len <- lookupRecordLength name
  indexs <- mapM (\(n,_) -> lookupRecordField name n) es
  let nes =  map snd es
  mkBodyExpForRearr (foldl (\acc e -> acc `AppE` e) (ConE name) (helper 0 len (zip indexs nes) [] (ConE name')))
  where findInPair [] i  unit = unit
        findInPair ((j,p):xs) i unit | i == j = p
                                     | otherwise = findInPair xs i unit
        helper i n pairs acc unit | i == n = acc
                                  | otherwise = helper (i+1) n pairs (acc ++[(findInPair pairs i unit)]) unit

-- restrict infix op to : for now
mkBodyExpForRearr (InfixE (Just e1) (ConE name) (Just e2)) = do
  (ConE name') <- [| (:) |]
  if name == name'
  then do le <- mkBodyExpForRearr e1
          re <- mkBodyExpForRearr e2
          (_, [ein,eright,eprod]) <- lookupNames [] [astNameSpace ++ s | s <- ["EIn","ERight","EProd"]] (notFoundMsg "EIn, ERight, EProd")
          return $ ConE ein `AppE` (ConE eright `AppE` (ConE eprod `AppE` le `AppE` re))
  else fail $ "only (:) infix operator is allowed in lambda body expression"

mkBodyExpForRearr (ListE [])  = do
  unitt                   <- [| () |]
  (_, [ein,eleft,econst]) <- lookupNames [] [astNameSpace ++ s | s <- ["EIn","ELeft","EConst"]] (notFoundMsg "EIn, ELeft, EConst")
  return $ ConE ein `AppE` (ConE eleft `AppE` (ConE econst `AppE` unitt))
mkBodyExpForRearr (ListE (e:es)) = do
  hexp <- mkBodyExpForRearr e
  rexp <- mkBodyExpForRearr (ListE es)
  (_, [ein,eright,eprod]) <- lookupNames [] [astNameSpace ++ s | s <- ["EIn","ERight","EProd"]] (notFoundMsg "EIn, ERight, EProd")
  return $ ConE ein `AppE` (ConE eright `AppE` (ConE eprod `AppE` hexp `AppE` rexp))

mkBodyExpForRearr (TupE [e])    = mkBodyExpForRearr e
mkBodyExpForRearr (TupE (e:es)) = do
  lexp <- mkBodyExpForRearr e
  rexp <- mkBodyExpForRearr (TupE es)
  (_, [eprod]) <- lookupNames [] [astNameSpace ++ "EProd"] (notFoundMsg "EProd")
  return ((ConE eprod `AppE` lexp) `AppE` rexp)
mkBodyExpForRearr _           = fail $ "Invalid syntax in lambda body expression"






rearr' :: TH.Exp -> Q TH.Exp
rearr' (LamE [p] e) = do
  (_, [edir,rearrc]) <- lookupNames [] [astNameSpace ++ s | s <- ["EDir","Rearr"] ] (notFoundMsg "EDir, Rearr")
  pat <- mkPat p RTag
  exp <- mkBodyExpForRearr e
  env <- mkEnvForRearr p
  newexp <- rearrangeExp exp (Map.map (ConE edir `AppE`) env)
  return ((ConE rearrc `AppE` pat) `AppE` newexp)

rearr :: Q TH.Exp -> Q TH.Exp
rearr = (rearr' =<<)



mkExpFromPat :: TH.Pat -> Q TH.Exp
mkExpFromPat (LitP c) = return (LitE c)
mkExpFromPat (ConP name ps) = do
  es <- mapM mkExpFromPat ps
  return $ foldl (\acc e -> (AppE acc e)) (ConE name) es
mkExpFromPat (RecP name ps) = do
  rs <- mapM mkExpFromPat (map snd ps)
  let es = zip (map fst ps) rs
  return (RecConE name es)
mkExpFromPat (ListP ps) = do
  es <- mapM mkExpFromPat ps
  return (ListE es)
mkExpFromPat (InfixP pl name pr) = do
  epl <- mkExpFromPat pl
  epr <- mkExpFromPat pr
  return (InfixE (Just epl) (ConE name) (Just epr))
mkExpFromPat (TupP ps) = do
  es <- mapM mkExpFromPat ps
  return (TupE es)
mkExpFromPat (VarP name) = return (VarE name)
mkExpFromPat WildP = [| () |]
mkExpFromPat _ = fail $ "pattern not handled in mkExpFromPat"

toProduct :: TH.Exp -> Q TH.Exp
toProduct (AppE e1 e2) = do
  (ConE unitn) <- [| () |]
  (_, [econst,ein,eleft,eright]) <- lookupNames [] [ astNameSpace ++ s | s <- ["EConst","EIn","ELeft", "ERight"] ] (notFoundMsg "EConst, EIn, ELeft, ERight")
  re2 <- toProduct e2
  re1 <- toProduct e1
  if e1 == (ConE eleft) || e1 == (ConE eright) || e1 == (ConE ein)
  then return re2
  else if e1 == (ConE econst)
       then return (AppE e1 (ConE unitn))
       else return (AppE re1 re2)


toProduct other = return other

rearrAndUpdate :: Q TH.Pat -> Q TH.Pat -> Q [TH.Dec] -> Q TH.Exp
rearrAndUpdate qrp qup qud = do
  (_, [edir,rearrc,upd]) <- lookupNames [] [astNameSpace ++ s | s <- ["EDir","Rearr","Update"] ] (notFoundMsg "EDir, Rearr, Update")
  rp <- qrp
  up <- qup
  ud <- qud
  rpat <- mkPat rp RTag
  bexp <- mkExpFromPat up
  rexp <- mkBodyExpForRearr bexp
  proexp <- toProduct rexp
  renv <- mkEnvForRearr rp
  newrexp <- rearrangeExp proexp (Map.map (ConE edir `AppE`) renv)
  upat <- mkPat up UTag
  uenv <- mkEnvForUpdate ud
  ubigul <- rearrangeExp (ConE upd `AppE` upat) uenv
  return $ ((ConE rearrc `AppE` rpat) `AppE` newrexp) `AppE` ubigul


mkEnvForUpdate :: [TH.Dec] -> Q (Map String TH.Exp)
mkEnvForUpdate []                                     = return Map.empty
mkEnvForUpdate ((ValD (VarP name) (NormalB e) _ ):ds) = do
  renv <- mkEnvForUpdate ds
  return $ Map.singleton (nameBase name) e `Map.union` renv
mkEnvForUpdate (_:ds) = fail $ "Invalid syntax in update bindings\n" ++
                               "Please use syntax like x1 = e1 x2 = e2... here"


update :: Q TH.Pat -> Q [TH.Dec] -> Q TH.Exp
update qp qds = do
  (_, [upd]) <- lookupNames [] [astNameSpace ++ "Update"] (notFoundMsg "Update")
  p   <- qp
  ds  <- qds
  pat <- mkPat p UTag
  env <- mkEnvForUpdate ds
  rearrangeExp (ConE upd `AppE` pat) env

--branch :: Q TH.Pat -> Q TH.Exp
--branch mp = do
--  p <- mp
--  pat <- mkPat p PTag
--  (_, [caseVBranch]) <- lookupNames [] [astNameSpace ++ "CaseVBranch"] (notFoundMsg "CaseVBranch")
--  return $ ConE caseVBranch `AppE` pat



patToFunc :: TH.Pat -> Q TH.Exp
patToFunc p =  do
  (_, [hreturn,htrue,hfalse]) <- lookupNames [] ["return","True","False"] (notFoundMsg "return,True,False")
  name                        <-  newName "s"
  case p of
    TH.WildP -> return $ LamE [VarP name] (AppE (VarE hreturn) (ConE htrue))
    _        -> return $ LamE [VarP name] (AppE (VarE hreturn) (CaseE (VarE name)
                        [Match p (NormalB (ConE htrue)) [], Match WildP (NormalB (ConE hfalse)) []]))


addNormal :: TH.Exp -> Q TH.Exp
addNormal exp = do
  (_, [bnormal]) <- lookupNames [] [astNameSpace ++ "Normal"] (notFoundMsg "Normal")
  return $ TupE [exp,ConE bnormal]

addAdaptive :: TH.Exp -> Q TH.Exp
addAdaptive exp = do
  (_, [badaptive]) <- lookupNames [] [astNameSpace ++ "Adaptive"] (notFoundMsg "Adaptive")
  return $ TupE [exp,ConE badaptive]

normal' :: Q TH.Exp -> Q TH.Exp
normal' me  = do
  (_, [hreturn,hcomposition]) <- lookupNames [] ["return","."] (notFoundMsg "return,composition")
  e <- me
  let a = addNormal $ InfixE (Just (VarE hreturn)) (VarE hcomposition) (Just e)
  [| \update -> fmap ($ update) $(a) |]
  --case e of
  --  (LamE pat exp) -> do
  --    exp' <- [| return $ $(return exp) |]
  --    let mexp = return (LamE pat exp')
  --    let a = [| ( $(mexp) , Normal) |]
  --    [| \update -> fmap ($ update) $(a) |]
  --  _  -> do
  --    let a = [| ( $([| return . $(me) |]), Normal) |]
  --    [| \update -> fmap ($ update) $(a) |]


adaptive' :: Q TH.Exp -> Q TH.Exp
adaptive' me = do
  (_, [hreturn,hcomposition]) <- lookupNames [] ["return","."] (notFoundMsg "return,composition")
  e <- me
  let a = addAdaptive $ InfixE (Just (VarE hreturn)) (VarE hcomposition) (Just e)
  [| \update -> fmap ($ update) $(a) |]

branch :: Q TH.Pat -> Q TH.Exp
branch mpat = do
  pat <- mpat
  checkVariables pat
  [| \bigul -> ($(patToFunc pat),bigul) |]

normal :: Q TH.Pat -> Q TH.Exp
normal mpat = do
  pat <- mpat
  checkVariables pat
  (_, [bnormal]) <- lookupNames [] [astNameSpace ++ "Normal"] (notFoundMsg "Normal")
  exp <- patToFunc pat
  a   <- addNormal exp
  [| \update -> fmap ($ update) $(return a)   |]


adaptive :: Q TH.Pat -> Q TH.Exp
adaptive mpat = do
  pat <- mpat
  checkVariables pat
  (_, [badaptive]) <- lookupNames [] [astNameSpace ++ "Adaptive"] (notFoundMsg "Adaptive")
  exp <- patToFunc pat
  a   <- addAdaptive exp
  [| \adapt -> fmap ($ adapt) $(return a) |]


--
notFoundMsg :: String -> String
notFoundMsg s = "cannot find data constructors " ++ s ++ " from Generic.BiGUL.AST"

withPatTag :: PatTag -> String -> String
withPatTag tag con = show tag ++ con

concatWith :: String -> [String] -> String
concatWith sep [] = ""
concatWith sep (x:xs) = x ++ sep ++ concatWith sep xs

checkVariables :: TH.Pat -> Q Bool
checkVariables pat = do
  k <- mkPat pat PTag
  return True



