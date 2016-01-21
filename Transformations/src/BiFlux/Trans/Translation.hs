{-# LANGUAGE FlexibleContexts, GADTs, TypeFamilies, ViewPatterns, RankNTypes #-}
module BiFlux.Trans.Translation where

import Lang.AST as BiGUL
import Lang.MonadBiGULError
import Control.Monad
import Control.Monad.Except
import GHC.InOut
import BiFlux.Lang.AST as BiFlux
import BiFlux.DTD.Type as BType
import Text.PrettyPrint
import qualified BiFlux.XPath.HXT.XPathDataTypes as XPath hiding (Name(..),Env(..))
import Text.XML.HaXml.DtdToHaskell.TypeDef hiding (List,Maybe,List1,Any,String,mkAtt)
import Text.XML.HXT.DOM.QualifiedName
import Data.Map as Map
import BiFlux.Trans.Patterns (inferAstType)


stmt2bigul :: (MonadError Doc m, MonadError' ErrorInfo m') => Stmt -> Type s -> Type v -> DynRPat -> DirectionEnv -> TypeEnv -> m (BiGUL m' s v)
---- example 1. update $book in $s/book by ... for view ... where $book/year > 2012
---- example 2. update $book in $s/book by ... for view p[$v1, $v2] in $v where  $v1 := $v2
---- In summarise, whereConds is either a conjunction of where conditons on source or view dependency bindings.
---- whereConds shall be passed into, and decide how to do the translation later.
stmt2bigul (StmtUpd upd whereConds) ts tv dynRPat dirEnv typeEnv = update2bigul upd whereConds ts tv dynRPat dirEnv typeEnv


update2bigul :: (MonadError Doc m, MonadError' ErrorInfo m') => Upd -> [WhereCond] -> Type s -> Type v -> DynRPat -> DirectionEnv -> TypeEnv -> m (BiGUL m' s v)

-- example 1. REPLACE $s/a WITH <b>{$v}</b>
--            SingleReplace Nothing (CPathSlash (CPathVar "$s") (CPathString "a")) (XQElem "b" (XQPath (CPathVar "$v")))
-- Version 0.1: ignore whereConds
-- Computation Steps:
--    Step 1. CPath to UPat
--    Step 2. (Maybe Pat) to UPat
--    Step 3. XQExpr -> Rearr
--    Step 4. Replace
-- Comment: Step 2 and Step 3 shall be combined together.
update2bigul (SingleReplace Nothing cpath xqexpr ) whereConds ts tv dynRPat dirEnv typeEnv = undefined
--   CUPat ts' fupat <- cpath2UPat ts tv typeEnv

--update2bigul (SingleReplace (Just pat) cpath xqexpr ) whereConds ts tv dynRPat dirEnv typeEnv = undefined


-- exist a s', and a UPat m' s' v, we could compute UPat m' s v.
-- not all s' is accetable.
data CUPat m m' s v where
  CUPat :: Type s' -> (forall v'. Eq v' => Type v' -> UPat m' s' v' -> Expr v v' -> m (UPatExprTuple m' s v)) -> CUPat m m' s v

data UPatExprTuple  m' s v where
  UPatExprTuple :: Eq v'' => Type v'' -> UPat m' s v'' -> Expr v v'' -> UPatExprTuple m' s v


cpath2UPat :: (MonadError Doc m, MonadError' ErrorInfo m') => CPath -> Type s -> Type v -> TypeEnv -> m (CUPat m m' s v)
cpath2UPat CPathSelf          ts                       tv typeEnv =
  return $ CUPat ts (\tv' upat' expr' -> return (UPatExprTuple tv' upat' expr'))
cpath2UPat CPathChild         (Data _ subts)           tv typeEnv =
  return $ CUPat subts (\tv' upat' expr' -> return (UPatExprTuple tv' (UOut upat') expr'))
cpath2UPat CPathAttribute     (Tag (isAtt -> True) subts) tv typeEnv =
  return $ CUPat subts (\tv' upat' expr' -> return (UPatExprTuple tv' upat' expr'))
cpath2UPat CPathDoS           _                        _  _ = throwError $ text "CPathDoS not support yet"
cpath2UPat (CPathNodeTest nt) ts                       tv typeEnv = cpathNodeTest2UPat nt ts tv
cpath2UPat p@(CPathSlash p1 p2) ts tv typeEnv =
  catchError
    (do
      CUPat ts1' bigf1 <- cpath2UPat p1 ts   tv typeEnv
      CUPat ts2' bigf2 <- cpath2UPat p2 ts1' tv typeEnv
      return $ CUPat ts2' (\tv' upat' expr' -> do
                          UPatExprTuple tv2' upat2' expr2' <- bigf2 tv' upat' expr'
                          bigf1 tv2' upat2' expr2'
        )
    )
    (\e -> throwError $ text "CPathSlash error on left hand" <+> text (show e))
cpath2UPat (CPathFilter expr) ts tv typeEnv = throwError $ text "CPathFilter not supported yet"
cpath2UPat (CPathVar    var ) ts tv typeEnv =
  case Map.lookup var typeEnv of
       Just (DynT ts') -> case teq ts ts' of
                               Just BType.Eq -> return $ CUPat ts (\tv' upat' expr' -> return(UPatExprTuple tv' upat' expr'))
                               Nothing -> throwError $ text "CPathVar type mismatch"
       Nothing -> throwError $ text "CPathVar cannot find var in typeEnv"
cpath2UPat (CPathString str ) ts tv typeEnv =
  case teq ts BType.String of
       Just BType.Eq -> return $ CUPat ts (\tv' upat' expr' -> return (UPatExprTuple tv' upat' expr'))
       Nothing -> throwError $ text "CPathString: source type shall be String."
cpath2UPat (CPathBool    b  ) ts tv typeEnv =
 case teq ts BType.Bool of
       Just BType.Eq -> return $ CUPat ts (\tv' upat' expr' -> return (UPatExprTuple tv' upat' expr'))
       Nothing -> throwError $ text "CPathBool: source type shall be Bool."
cpath2UPat (CPathSnapshot pat cpath) ts ts' bigul' = throwError $ text "CPathSnapshot not supported yet"
cpath2UPat (CPathFct    _  _) _  _   _  = throwError $ text "CPath function call unsupported"
cpath2UPat (CPathIndex  int ) ts ts' bigul' =
  case ts of
       BType.List  ta -> throwError $ text "CPathIndex: not supported yet" --TODO
       BType.List1 ta -> throwError $ text "CPathIndex: not supported yet" --TODO
       _              -> throwError $ text "CPathIndex: source shall be a list type"

cpathNodeTest2UPat :: (MonadError Doc m, MonadError' ErrorInfo m') => XPath.NodeTest -> Type s -> Type v -> m (CUPat m m' s v)
-- NameTest for Data Name is just a filter, nothing else.
-- Previously, I wrote UOut, not correct.
-- $s/a is CPathVar "$s"/ CPathChild / CPathNodeTest ...
-- In the CPathChild step, we will use UOut.
cpathNodeTest2UPat (XPath.NameTest qname) ts@(Data (Name dtdName _) subts) tv =
  if qualifiedName qname == dtdName
     then return $ CUPat ts (\tv' upat' expr' -> return (UPatExprTuple tv' (upat') expr'))
     else throwError $ text "NameTest: name not match"
cpathNodeTest2UPat ntqname@(XPath.NameTest qname) (Either ta tb) tv =
  catchError
    ( do
      CUPat ts' bigf <- cpathNodeTest2UPat ntqname ta tv
      return $ CUPat ts' (\tv' upat' expr' -> liftM (\(UPatExprTuple tv'' upat'' expr'') -> UPatExprTuple tv'' (ULeft upat'') expr'') (bigf tv' upat' expr'))
      )
    (\e -> do
      CUPat ts' bigf <- cpathNodeTest2UPat ntqname tb tv
      return $ CUPat ts' (\tv' upat' expr' -> liftM (\(UPatExprTuple tv'' upat'' expr'') -> UPatExprTuple tv'' (URight upat'') expr'') (bigf tv' upat' expr'))
      )
cpathNodeTest2UPat ntqname@(XPath.NameTest qname) (Prod ta tb) tv =
  catchError
    ( do
      CUPat ts' bigf <- cpathNodeTest2UPat ntqname ta tv
      return $ CUPat ts' (\tv' upat' expr' -> liftM (\(UPatExprTuple tv'' upat'' expr'') -> UPatExprTuple (Prod tv'' One) (UProd upat'' (UVar Skip)) (EProd expr'' (EConst ()))) (bigf tv' upat' expr'))
      )
    (\e -> do
      CUPat ts' bigf <- cpathNodeTest2UPat ntqname tb tv
      return $ CUPat ts' (\tv' upat' expr' -> liftM (\(UPatExprTuple tv'' upat'' expr'') -> UPatExprTuple (Prod One tv'') (UProd (UVar Skip) upat'') (EProd (EConst ()) expr'')) (bigf tv' upat' expr'))
      )

-- We do not support filter yet.
-- So only if the whole List matches, we put then all into source. Otherwise fail.
-- We also not support complex List type, for example:  (a|b)
cpathNodeTest2UPat ntqname@(XPath.NameTest qname) lta@(List ta@(Data (Name dtdName _) subts)) tv =
  if qualifiedName qname == dtdName
     then return $ CUPat lta (\tv' upat' expr' -> return (UPatExprTuple tv' upat' expr'))
     else throwError $ text "CPath NameTest name not match"

cpathNodeTest2UPat ntqname@(XPath.NameTest qname) _ _ = throwError $ text "NameTest type not match"
cpathNodeTest2UPat (XPath.PI str) _ _ = throwError $ text "PI not supported"
cpathNodeTest2UPat (XPath.TypeTest XPath.XPNode) ts tv =
  return $ CUPat ts (\tv' upat' expr' -> return (UPatExprTuple tv' upat' expr'))
cpathNodeTest2UPat (XPath.TypeTest XPath.XPCommentNode) _ _ = throwError $ text "XPCommentNode not suppoted"
cpathNodeTest2UPat (XPath.TypeTest XPath.XPPINode) _ _ = throwError $ text "XPINode not support"
cpathNodeTest2UPat (XPath.TypeTest XPath.XPTextNode) ts tv =
  return $ CUPat ts (\tv' upat' expr' -> do
                    case teq tv' String of
                         Just BType.Eq -> return (UPatExprTuple tv' upat' expr')
                         _             -> throwError $ text "view expression type shall be String"
                         )
cpathNodeTest2UPat (XPath.TypeTest XPath.XPString) ts tv =
  return $ CUPat ts (\tv' upat' expr' -> do
                    case teq tv' String of
                         Just BType.Eq -> return (UPatExprTuple tv' upat' expr')
                         _             -> throwError $ text "view expression type shall be String"
                         )

-- in the pattern pat, we need construct the following information for later usage.

data DynDirection where
  DynD :: (Eq t, Eq v) => Type v -> Type origin -> Type t -> Direction origin t -> RPat v origin con -> DynDirection

data DynExpr env where
  DynE :: (Eq v') => Type v' -> Expr env v' -> DynExpr env

type DirectionEnv = Map.Map String DynDirection

-- I don't know it is correct  or not !
data EBiGUL m m' s v where
  EBiGUL ::  Eq v' => Type v' -> (forall v'. BiGUL m' s v' -> m (BiGUL m' s v)) -> EBiGUL m m' s v

-- | Rearrange XQExpr to a proper type
-- How to construct RPat ?
-- suppose given from existing from Pattern procedure
-- Seems need another env to build the mapping between $v and Direction, so these two are constructed from Pattern.
-- xqexpr2BiGUL :: (MonadError Doc m, MonadError' ErrorInfo m') => XQExpr -> Type v -> RPat v env con -> DirectionEnv -> TypeEnv -> m (EBiGUL m m' s v)
-- xqexpr2BiGUL xqexpr tv rpat directionEnv typeEnv = do
--   DynE tv' expr <- xqexpr2expr xqexpr tv rpat directionEnv typeEnv
--   return $ EBiGUL tv' (\bigul' -> return (Rearr rpat expr bigul'))


xqexpr2expr :: (MonadError Doc m) => XQExpr -> Type v -> RPat v env con -> DirectionEnv -> TypeEnv -> m (DynExpr env)
xqexpr2expr XQEmpty                  tv rpat directionEnv typeEnv = return $ DynE One (EConst ())
xqexpr2expr (XQProd xqexprl xqexprr) tv rpat directionEnv typeEnv = do
  --TODO: split directionEnv according to xqexprl and xqexprr
  DynE vl' exprl <- xqexpr2expr xqexprl tv rpat directionEnv typeEnv
  DynE vr' exprr <- xqexpr2expr xqexprr tv rpat directionEnv typeEnv
  return $ DynE (Prod vl' vr') (EProd exprl exprr)
-- we need 1. type env to find type of n
--         2. DynE shall wrap the type v', which is used for comparison here.
xqexpr2expr (XQElem n xqexpr)       tv rpat directionEnv typeEnv = do
  DynE subv' expr <- xqexpr2expr xqexpr tv rpat directionEnv typeEnv
  --TODO: there is a problem, how to distinguish source and view type env ?
  --      2. how about list type ? if n points to a Data Name ...; while we need in fact maybe a list type
  case Map.lookup n typeEnv of
       Just (DynT tv) -> case tv of
                              Data (Name _ _) subtv -> case teq subv' subtv of
                                                            Just BType.Eq -> return $ DynE tv (EIn expr)
                                                            Nothing       -> throwError $ text "XQElem subelem type mismatch"
                              otherwise             -> throwError $ text "shall not happen"
       Nothing -> throwError $ text "cannot find n in typeEnv"
xqexpr2expr (XQAttr str xqexpr) tv rpat directionEnv typeEnv = do
  DynE subv' expr <- xqexpr2expr xqexpr tv rpat directionEnv typeEnv
  case Map.lookup str typeEnv of
       Just (DynT tv) -> case tv of
                              Tag _ subtv -> case teq subtv subv' of
                                                  Just BType.Eq -> return $ DynE tv expr
                                                  Nothing       -> throwError $ text "XQAttr type mismatch"
                              otherwise   -> throwError $ text "attribute type not match with Tag" --TODO: need check whether having Data case for attr.
       Nothing -> throwError $ text "cannot find attribute in typeEnv"
--TODO
xqexpr2expr (XQLet pat xqexpr1 xqexpr2)    tv rpat directionEnv typeEnv = throwError $ text "Haven't implemented yet"
--TODO
xqexpr2expr (XQIf xqexpr0 xqexpr1 xqexpr2) tv rpat directionEnv typeEnv = throwError $ text "Haven't implemented yet"
--TODO
xqexpr2expr (XQBinOp op xqexprl xqexprr)   tv rpat directionEnv typeEnv = throwError $ text "Haven't implemented yet"
-- TODO
xqexpr2expr (XQFor var xqexprl xqexprr) tv rpat directionEnv typeEnv = throwError $ text "Haven't implemented yet"
xqexpr2expr (XQPath cpath) tv rpat directionEnv typeEnv = cpath2expr cpath tv rpat directionEnv typeEnv


cpath2expr :: (MonadError Doc m) => CPath -> Type v -> RPat v env con -> DirectionEnv -> TypeEnv -> m (DynExpr env)
cpath2expr (CPathVar var) tv rpat directionEnv typeEnv =
  case Map.lookup var directionEnv of
       Just (DynD tv' tenv' tt d rpat') -> case req tv rpat tv' rpat' of
                                       Just REq -> return $ DynE tt (EDir d)
                                       otherwise -> throwError $ text "wrong direction"
       Nothing       -> throwError $ text "cannot find var in Direction Env"
cpath2expr _ _ _ _ _ = throwError $ text "other CPath in expr not supported yet"


-- | The following is for judging two RPat is the same.
data REqual a1 a2 b1 b2 c1 c2 where
  REq :: REqual a1 a1 b1 b1 c1 c1

req :: MonadPlus m => Type v -> RPat v env con -> Type v' -> RPat v' env' con' -> m (REqual v v' env env' con con')
req tv RVar tv' RVar  =
  case teq tv tv' of
       Just BType.Eq -> return REq
       otherwise -> mzero
req tv (RConst c1) tv' (RConst c2) =
  case teq tv tv' of
       Just BType.Eq -> if c1 == c2 then return REq else mzero
       otherwise -> mzero
req tv@(Prod tl tr) (RProd rpatl rpatr) tv'@(Prod tl' tr') (RProd rpatl' rpatr') =
  case teq tv tv' of
       Just BType.Eq -> do
         REq <- req tl rpatl tl' rpatl'
         REq <- req tr rpatr tr' rpatr'
         return REq
       otherwise -> mzero
req tv (RProd rpatl rpatr) tv' (RProd rpatl' rpatr') = mzero
req tv@(Either tl tr) (RLeft rpat) tv'@(Either tl' tr') (RLeft rpat') =
  case teq tv tv' of
       Just BType.Eq -> do
         REq <- req tl rpat tl' rpat'
         return REq
       otherwise -> mzero
req tv@(Either tl tr) (RRight rpat) tv'@(Either tl' tr') (RRight rpat') =
  case teq tv tv' of
       Just BType.Eq -> do
         REq <- req tr rpat tr' rpat'
         return REq
       otherwise -> mzero
req tv@(Data (Name dtdName _) subtv) (ROut rpat) tv'@(Data (Name dtdName' _) subtv') (ROut rpat') =
  case teq tv tv' of
       Just BType.Eq -> do
         REq <- req subtv rpat subtv' rpat'
         return REq
       otherwise -> mzero
--  RElem  :: RPat a b c -> RPat [a] b' c' -> RPat [a] (b, b') (c, c')
req tv@(List subtv) (RElem rpath rpatrest) tv'@(List subtv')  (RElem rpath' rpatrest') =
  case teq tv tv' of
       Just BType.Eq -> do
         REq <- req subtv rpath subtv' rpath'
         REq <- req tv rpatrest tv' rpatrest' -- TODO: I am not sure it will terminated !!!
         return REq
       otherwise -> mzero


-- Here wrap Type v is for the comparion for PElem
data DynPat where
  DynP :: (Eq v', Eq v) => Type v -> Type v' -> BiGUL.Pat v v' -> DynPat

-- | View Pattern handling
--   Translate into: Rearr rPat expr bigul'
--   giving a Pattern, I shall compute:
--   1. RPat
--   2. Direction for each view var in expr
--   And suppose you will give me bigul' :: m' s v'.
--   The type of v' is the same with rearranged expr type
-- In summarise, use Pat to construct a Pat and an env for Direction at the same time.
-- Finally, we could compute RPat from Pat, so we do not store RPat here.
viewPat2BigulPat :: (MonadError Doc m, Eq v) => BiFlux.Pat -> Type v -> TypeEnv -> m DynPat
viewPat2BigulPat (SequencePat []) tv typeEnv =
  case teq tv One of
       Just BType.Eq -> return $ DynP tv One (PConst ())
       Nothing       -> throwError $ text "view type must be ()"
viewPat2BigulPat (SequencePat (pat : pats)) (Prod th tt) typeEnv = do
  DynP th th' ph <- viewPat2BigulPat pat th typeEnv
  DynP tt tt' pt <- viewPat2BigulPat pat tt typeEnv
  return $ DynP (Prod th tt) (Prod th' tt') (PProd ph pt)
-- if view is a list, there is possibility it is empty and the pattern fails.
viewPat2BigulPat (SequencePat (pat : pats)) ltv@(List tv) typeEnv = do
  DynP tv th' ph <- viewPat2BigulPat pat tv typeEnv
  DynP ltv@(List tv') tt' pt <- viewPat2BigulPat (SequencePat pats) ltv typeEnv
  case teq tv tv' of
       Just BType.Eq ->  return $ DynP ltv (Prod th' tt') (PElem ph pt)
       otherwise     -> throwError $ text "list type shall match"
viewPat2BigulPat (SequencePat (pat : pats)) (List1 tv) typeEnv = do
  DynP tv th' ph <- viewPat2BigulPat pat tv typeEnv
  DynP ltv@(List tv') tt' pt <- viewPat2BigulPat (SequencePat pats) (List tv) typeEnv
  case teq tv tv' of
       Just BType.Eq ->  return $ DynP ltv (Prod th' tt') (PElem ph pt)
       otherwise     -> throwError $ text "list type shall match"
viewPat2BigulPat (SequencePat (pat : pats)) _  _ = throwError $ text "type mismatch"
viewPat2BigulPat (ElementPat name pats) tv@(Data (Name dtdName _) subtv) typeEnv =
  if name == dtdName
     then do
       DynP osubtv subtv' psub <- viewPat2BigulPat (SequencePat pats) subtv typeEnv
       case teq osubtv subtv of
            Just BType.Eq -> return $ DynP tv subtv' (POut psub)
            otherwise     -> throwError $ text "type mismatch"
     else ttext "Element pat name not match with type"
viewPat2BigulPat (ElementPat name pats) _ _ = ttext " type mismatch"
viewPat2BigulPat (AttributePat name varp) (Tag ('@':attName) tv) typeEnv =
  if name == attName
     then viewVarP2BigulPat varp tv typeEnv
     else ttext "attribute name mismatch"
viewPat2BigulPat (StringPat str) tv typeEnv =
  case teq tv String of
       Just BType.Eq -> return $ DynP tv One (PConst (BType.Str str))
       _             -> ttext "view type must be string"
viewPat2BigulPat (VarPat varp) tv typeEnv = viewVarP2BigulPat varp tv typeEnv


viewVarP2BigulPat :: (MonadError Doc m, Eq v) => BiFlux.VarP -> Type v -> TypeEnv -> m DynPat
viewVarP2BigulPat (VarV var) tv typeEnv = ttext "var without type not supported yet"
viewVarP2BigulPat (VarT var asttype) tv typeEnv =
  maybe (ttext "infer Ast type fails")
        (
          \(DynT tv') ->
            case teq tv tv' of --tv' `isSubtypeOf` tv TODO: in fact, need better reaarrange from v to v'
               Just BType.Eq  -> return $ DynP tv tv PVar
               otherwise ->  ttext "inferred ast type cannot match with exptected type"
          )
        (inferAstType typeEnv asttype)
viewVarP2BigulPat (VarTP asttype) tv typeEnv = ttext "on view pattern, not supported"


ttext str = throwError $ text str

data DynRPat where
  DynR :: (Eq v, Eq env) => Type v -> Type env -> RPat v env con -> DynRPat


-- Pat and RPat are different for (Data Name ..) type.
viewPat2DirectionEnv  :: (MonadError Doc m) => BiFlux.Pat -> DynRPat -> m DirectionEnv
viewPat2DirectionEnv (SequencePat []) _ = return Map.empty
viewPat2DirectionEnv (SequencePat (path: patt)) (DynR tv@(Prod tvh tvt) ev@(Prod evh evt) rpat@(RProd rpath rpatt)) = do
  deh <- viewPat2DirectionEnv path (DynR tvh evh rpath)
  det <- viewPat2DirectionEnv (SequencePat patt) (DynR tvt evt rpatt)
  return $ Map.union (wrapDLeft deh tv ev rpat)  (wrapDRight det tv ev rpat)
viewPat2DirectionEnv (SequencePat _) _                = ttext "pattern not match"
viewPat2DirectionEnv (ElementPat name pats)   dynRPat = viewPat2DirectionEnv (SequencePat pats) dynRPat --already handled from Pat to RPat
viewPat2DirectionEnv (AttributePat name varp) dynRPat = varPat2DirectionEnv varp dynRPat
viewPat2DirectionEnv (StringPat str)          _       = return Map.empty
viewPat2DirectionEnv (VarPat varp)            dynRPat = varPat2DirectionEnv varp dynRPat

-- how to wrap with MonadError ?
wrapDLeft :: DirectionEnv -> Type v -> Type env -> RPat v env con -> DirectionEnv
wrapDLeft denv tv tenv rpat = Map.mapWithKey (\_ dir -> uDLeft dir tv tenv rpat) denv
-- data DynDirection where
--   DynD :: (Eq t, Eq v) => Type v -> Type env -> Type t -> Direction env t -> RPat v env con -> DynDirection
-- | compute Direction Env from pat
uDLeft :: DynDirection -> Type v -> Type env -> RPat v env con -> DynDirection
uDLeft (DynD tv' tenv' t' d' rpat') tv@(Prod tvl tvr) tenv@(Prod tenvl tenvr) rpat =
  case teq tv' tvl of
       Just BType.Eq -> case teq tenv' tenvl of
                             Just BType.Eq -> DynD tv tenv t' (DLeft d') rpat
                             Nothing -> error "wrap direction DLeft fail"
       Nothing -> error "wrap direction DLeft fail"


wrapDRight :: DirectionEnv -> Type v -> Type env -> RPat v env con -> DirectionEnv
wrapDRight denv tv tenv rpat = Map.mapWithKey (\_ dir -> uDRight dir tv tenv rpat) denv
uDRight :: DynDirection -> Type v -> Type env -> RPat v env con -> DynDirection
uDRight (DynD tv' tenv' t' d' rpat') tv@(Prod tvl tvr) tenv@(Prod tenvl tenvr) rpat =
  case teq tv' tvr of
       Just BType.Eq -> case teq tenv' tenvr of
                             Just BType.Eq -> DynD tv tenv t' (DRight d') rpat
                             Nothing -> error "wrap direction DRight fail"
       Nothing -> error "wrap direction DRight fail"



varPat2DirectionEnv :: MonadError Doc m => BiFlux.VarP -> DynRPat -> m DirectionEnv
varPat2DirectionEnv (VarV var  ) _                           = ttext "var pattern without asttype not supported"
varPat2DirectionEnv (VarT var _) dynRPat@(DynR tv' ev' RVar) = return $ Map.singleton var (DynD tv' ev' tv' DVar RVar)
varPat2DirectionEnv (VarT var _) _                           = ttext "RPat shall be RVar"
varPat2DirectionEnv (VarTP _   ) _                           = return Map.empty


-- | Compute RPat v' env con from Pat v v'
-- Memo: this is different from directly computing RPat v env con from view v
pat2rpat :: (MonadError Doc m) => DynPat -> m DynRPat
pat2rpat (DynP tv tv' PVar) = return $ DynR tv' (BType.VVar tv') RVar -- TODO: is it correct ?
pat2rpat (DynP tv One (PConst c)) = return $ DynR One One (RConst ())
pat2rpat (DynP (Prod tvl tvr) (Prod tvl' tvr') (PProd patl patr))= do
  DynR tvl'' evl'' rpatl <- pat2rpat (DynP tvl tvl' patl)
  DynR tvr'' evr'' rpatr <- pat2rpat (DynP tvr tvr' patr)
  return $ DynR (Prod tvl'' tvr'') (Prod evl'' evr'') (RProd rpatl rpatr)
pat2rpat (DynP (Either tvl tvr) tvl' (PLeft patl)) = do
  DynR tvl'' evl'' rpatl <- pat2rpat (DynP tvl tvl' patl)
  return $ DynR tvl'' evl'' rpatl
pat2rpat (DynP (Either tvl tvr) tvr' (PRight patr)) = do
  DynR tvr'' evr'' rpatr <- pat2rpat (DynP tvr tvr' patr)
  return $ DynR tvr'' evr'' rpatr
pat2rpat (DynP (Data _ subtv) tv' (POut pat)) = do
  DynR tv'' ev'' pat' <- pat2rpat (DynP subtv tv' pat)
  case teq tv'' tv' of
       Just BType.Eq -> return $ DynR tv'' ev'' pat'
       _             -> ttext "type mismatch"
pat2rpat (DynP tv tv' (POut pat)) = ttext "POut shall be type of Data"
pat2rpat (DynP (List tv) (Prod tvh' tvt') (PElem path patt)) = do
  DynR tvh'' evh'' path' <- pat2rpat (DynP tv tvh' path)
  DynR tvt'' evt'' patt' <- pat2rpat (DynP (List tv) tvt' patt)
  return $ DynR (Prod tvh'' tvt'') (Prod evh'' evt'') (RProd path' patt')


-- | Source Pat handling
--
