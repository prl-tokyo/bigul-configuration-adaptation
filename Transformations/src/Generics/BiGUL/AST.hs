{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, DeriveGeneric, TupleSections #-}
module Generics.BiGUL.AST where

import Control.Monad.Except
import GHC.Generics
import GHC.InOut
import Text.PrettyPrint
import Generics.BiGUL.MonadBiGULError
import Data.List(intersperse)


data Pat :: * -> * -> *  where
  PVar   :: Pat a a
  PConst :: Eq a => a -> Pat a ()
  PProd  :: Pat a a' -> Pat b b' -> Pat (a, b) (a', b')
  PLeft  :: Pat a a' -> Pat (Either a b) a'
  PRight :: Pat b b'  -> Pat (Either a b) b'
  PIn    :: InOut a => Pat (F a) b -> Pat a b
  PElem  :: Pat a b -> Pat [a] b' -> Pat [a] (b, b')

deconstruct :: MonadError' ErrorInfo m => Pat a b -> a -> m b
deconstruct  PVar             x         = return x
deconstruct (PConst y)        x         = if x == y then return () else throwError $ ErrorInfo "unmatched constant pattern"
deconstruct (PProd lpat rpat) (x, y)    = liftM2 (,) (deconstruct lpat x) (deconstruct rpat y)
deconstruct (PLeft  pat)      (Left  x) = deconstruct pat x
deconstruct (PLeft  pat)      (Right y) = throwError $ ErrorInfo  "left pattern for right value"
deconstruct (PRight pat)      (Left  x) = throwError $ ErrorInfo "right pattern for left value"
deconstruct (PRight pat)      (Right y) = deconstruct pat y
deconstruct (PIn    pat)      x         = deconstruct pat (out x)
deconstruct (PElem hpat tpat) []        = throwError $ ErrorInfo "head-tail pattern for empty list"
deconstruct (PElem hpat tpat) (x : xs)  = liftM2 (,) (deconstruct hpat x) (deconstruct tpat xs)

construct :: Pat a b -> b -> a
construct  PVar             x      = x
construct (PConst y)        _      = y
construct (PProd lpat rpat) (x, y) = (construct lpat x, construct rpat y)
construct (PLeft  pat)      x      = Left  (construct pat x)
construct (PRight pat)      y      = Right (construct pat y)
construct (PIn    pat)      x      = inn (construct pat x)
construct (PElem hpat tpat) (x, y) = construct hpat x : construct tpat y



data UPat :: (* -> *) -> * -> * -> * where
  UVar   :: BiGUL m s v -> UPat m s v
  UConst :: (Eq s,Show s) => s -> UPat m s ()
  UProd  :: UPat m s v -> UPat m s' v' -> UPat m (s, s') (v, v')
  ULeft  :: UPat m s v -> UPat m (Either s s') v
  URight :: UPat m s' v -> UPat m (Either s s') v
  UIn    :: InOut s => UPat m (F s) v -> UPat m s v
  UElem  :: UPat m s v -> UPat m [s] v' -> UPat m [s] (v, v')

instance Show (UPat m s v) where
  show (UVar bigul)    = "(UVar " ++ show bigul ++ " )"
  show (UConst c)      = show c
  show (UProd up1 up2) = "(UProd " ++ show up1 ++ " " ++ show up2 ++ " )"
  show (ULeft up)      = "(ULeft " ++ show up ++ " )"
  show (URight up)     = "(URight " ++ show up ++ " )"
  show (UIn up)        = "(UIn " ++ show up ++ " )"
  show _               = "show error in UPat"

data CaseSBranch m s v = Normal (BiGUL m s v) | Adaptive (s -> v -> m s)

instance Show (CaseSBranch m s v) where
  show (Normal bigul) = "Normal " ++ show bigul
  show (Adaptive _) = "Adaptive <adaptive function>"

--data CaseVBranch m s v where
--  CaseVBranch :: Pat v v' -> BiGUL m s v' -> CaseVBranch m s v

data BiGUL :: (* -> *) -> * -> * -> * where
  Fail    :: BiGUL m s v
  Skip    :: BiGUL m s ()
  Replace :: BiGUL m s s
  Update  :: UPat m s v -> BiGUL m s v
  Rearr   :: (Eq v') => RPat v env con -> Expr env v' -> BiGUL m s v' -> BiGUL m s v
  Dep     :: (Eq v') => (v -> v') -> BiGUL m s v -> BiGUL m s (v, v')
  CaseS   :: [(s -> m Bool, CaseSBranch m s v)] -> BiGUL m s v
  CaseV   :: [(v -> m Bool, BiGUL m s v)] -> BiGUL m s v
  CaseSV  :: [(s -> v -> m Bool,BiGUL m s v)] -> BiGUL m s v
  Align   :: (s -> m Bool)
          -> (s -> v -> m Bool)
          -> BiGUL m s v
          -> (v -> m s)
          -> (s -> m (Maybe s))
          -> BiGUL m [s] [v]
  Emb     :: (s -> m v)
          -> (s -> v -> m s)
          -> BiGUL m s v
  Compose :: BiGUL m s u
          -> BiGUL m u v
          -> BiGUL m s v
  Seq     :: (Eq s0) => BiGUL m s0 v0
          -> BiGUL m s0 v1
          -> BiGUL m s0 (v0,v1)
  ConstV   :: BiGUL m s ()  -> v -> BiGUL m s v

constV :: v -> BiGUL m s v
constV = ConstV Skip

instance Show (BiGUL m s v) where
  show Fail = "Fail"
  show Skip = "Skip"
  show Replace = "Replace"
  show (Update up) = "(Update " ++ show up ++ " )"
  show (Rearr rp exp bigul) = "(Rearr " ++ show rp ++ "  " ++ show exp ++ "  " ++ show bigul ++ " )"
  show (Dep _ bigul) = "(Dep   <dependency function>  " ++ show bigul ++ " )"
  show (CaseS bs) = "(CaseS [" ++ unwords (intersperse "\n" (map (\(_,b) -> "(precidtion , " ++ show b ++ " )") bs)) ++ " ])"
  show (CaseV bs) = "(CaseV [" ++ unwords (intersperse "\n" (map (\(_,b) -> "(precidtion , " ++ show b ++ " )") bs)) ++ " ])"
  show (Align _ _ bigul _ _) = "(Align <source condition>\n       <match condition>\n       " ++ show bigul ++ "\n       <create function>\n       <conceal function>)"
  show _ = "Invalid BiGUL program in show"

newtype Var a = Var a

instance Show a => Show (Var a) where
  show (Var a) = "Var: " ++ show a

-- RPat (view type) (environment type) (container type)
data RPat :: * -> * -> * -> * where
  RVar   :: Eq a => RPat a (Var a) (Maybe a)
  RConst :: (Eq a,Show a) => a -> RPat a () ()
  RProd  :: RPat a a' a'' -> RPat b b' b'' -> RPat (a, b) (a', b') (a'', b'')
  RLeft  :: RPat a a' a'' -> RPat (Either a b) a' a''
  RRight :: RPat b b' b'' -> RPat (Either a b) b' b''
  RIn    :: InOut a => RPat (F a) b c -> RPat a b c
  RElem  :: RPat a b c -> RPat [a] b' c' -> RPat [a] (b, b') (c, c')


instance Show (RPat v e c) where
  show  RVar           = "RVar"
  show (RConst c)      = show c
  show (RProd rp1 rp2) = "(RProd " ++ show rp1 ++ " " ++ show rp2 ++ " )"
  show (RLeft rp)      = "(RLeft " ++ show rp ++ " )"
  show (RRight rp)     = "(RRight " ++ show rp ++ " )"
  show (RIn rp)        = "(RIn " ++ show rp ++ " )"
  show _               = "show error in RPat"

deconstructR :: MonadError' ErrorInfo m => RPat v env con -> v -> m env
deconstructR RVar                v          = return $ Var v
deconstructR (RConst c)          v          = if c == v then return () else throwError $ ErrorInfo "view must be a constant"
deconstructR (RProd rpatl rpatr) (vl, vr)   = liftM2 (,) (deconstructR rpatl vl) (deconstructR rpatr vr)
deconstructR (RLeft rpatl)       (Left vl)  = deconstructR rpatl vl
deconstructR (RLeft rpatl)       _          = throwError $ ErrorInfo "RLeft pattern error"
deconstructR (RRight rpatr)      (Right vr) = deconstructR rpatr vr
deconstructR (RRight rpatr)      _          = throwError $ ErrorInfo "RRight pattern error"
deconstructR (RIn rpat)          v          = deconstructR rpat (out v)
deconstructR (RElem rpath rpatt) []         = throwError $ ErrorInfo "view element cannot be empty"
deconstructR (RElem rpath rpatt) (v: vs)    = liftM2 (,) (deconstructR rpath v) (deconstructR rpatt vs)

constructR   :: MonadError' ErrorInfo m => RPat v env con -> con -> m v
constructR RVar                Nothing      = throwError $ ErrorInfo "RVar canot be empty"
constructR RVar                (Just v)     = return v
constructR (RConst c)          con          = return c
constructR (RProd rpatl rpatr) (conl, conr) = liftM2 (,) (constructR rpatl conl) (constructR rpatr conr)
constructR (RLeft rpat)        con          = liftM Left (constructR rpat con)
constructR (RRight rpat)       con          = liftM Right (constructR rpat con)
constructR (RIn rpat)          con          = liftM inn (constructR rpat con)
constructR (RElem rpath rpatt) (conh, cont) = liftM2 (:) (constructR rpath conh) (constructR rpatt cont)

emptyContainer :: RPat v env con -> con
emptyContainer RVar                           = Nothing
emptyContainer (RConst  c)                    = ()
emptyContainer (RProd rpatl rpatr)            = (emptyContainer rpatl, emptyContainer rpatr)
emptyContainer (RLeft pat        )            = emptyContainer pat
emptyContainer (RRight pat       )            = emptyContainer pat
emptyContainer (RIn  pat         )            = emptyContainer pat
emptyContainer (RElem rpath rpatt)            = (emptyContainer rpath, emptyContainer rpatt)


-- You need explicitly specify the type arguments at the type level when using the Direction type.
-- From type, you could know the type of the data you want.
-- !comment: DMaybe did not used.
data Direction :: * -> * -> * where
  DVar    :: Direction (Var a) a
  DLeft   :: Direction a t -> Direction (a, b) t
  DRight  :: Direction b t -> Direction (a, b) t

instance Show (Direction a t) where
  show  DVar = "DVar"
  show (DLeft dir)  = "(DLeft " ++ show dir ++ " )"
  show (DRight dir) = "(DRight " ++ show dir ++ " )"

retrieve :: (Eq t) => Direction a t -> a -> t
retrieve  DVar      (Var x) = x
retrieve (DLeft  p) (x, y)  = retrieve p x
retrieve (DRight p) (x, y)  = retrieve p y

data Expr :: * -> * -> * where
  EDir     :: (Eq a)              => Direction orig a -> Expr orig a
  EConst   :: (Eq a, Show a)      =>  a -> Expr orig a
  EIn      :: (InOut a, Eq (F a)) => Expr orig (F a) -> Expr orig a
  EProd    :: (Eq a, Eq b)        => Expr orig a -> Expr orig b -> Expr orig (a, b)
  ELeft    :: (Eq a, Eq b)        => Expr orig a -> Expr orig (Either a b)
  ERight   :: (Eq a, Eq b)        => Expr orig b -> Expr orig (Either a b)
  EElem    :: (Eq a)              => Expr orig a -> Expr orig [a] -> Expr orig [a]
  ECompare :: (Eq a)              => Expr orig a -> a -> Expr orig (Either () a)

instance Show (Expr orig a) where
  show (EDir dir)      = "(EDir " ++ show dir ++ " )"
  show (EConst c)      = "(EConst " ++ show c ++ " )"
  show (EProd e1 e2)   = "(EProd " ++ show e1 ++ " " ++ show e2 ++ " )"
  show (ELeft e)       = "(ELeft " ++ show e ++ " )"
  show (ERight e)      = "(ERight " ++ show e ++ " )"
  show (EIn e)         = "(EIn " ++ show e ++ " )"
  show _               = "show error in Expr"

eval :: (Eq v') => Expr env v' -> env -> v'
eval (EDir dir)          env = retrieve dir env
eval (EConst c)          env = c
eval (EIn expr)          env = inn (eval expr env)
eval (EProd exprl exprr) env = (eval exprl env, eval exprr env)
eval (ELeft expr       ) env = Left $ eval expr env
eval (ERight expr      ) env = Right $ eval expr env
eval (EElem exprh exprt) env = eval exprh env : eval exprt env
eval (ECompare expr v  ) env = let v' = eval expr env
                                in if v == v' then Left () else Right v'

-- The goal is to update the "Maybe" con to fill in proper values.
-- con follow the structure of RPat
-- we have updated value v', which follows the structure of Expr
uneval :: (MonadError' ErrorInfo m ) => RPat v env con -> Expr env v' -> v' -> con -> m con
uneval rpat (EDir dir)          v'          con = updateRPat rpat dir v' con
uneval rpat (EConst c)          v'          con = if c == v' then return con else throwError $ ErrorInfo "const not matched."
uneval rpat (EIn expr)          v'          con = uneval rpat expr (out v') con
uneval rpat (EProd exprl exprr) (vl', vr')  con = uneval rpat exprl vl' con >>= uneval rpat exprr vr'
uneval rpat (ELeft expr)        (Left vl')  con = uneval rpat expr vl' con
uneval rpat (ELeft expr)        _           con = throwError $ ErrorInfo "view shall be Either Left."
uneval rpat (ERight expr)       (Right vr') con = uneval rpat expr vr' con
uneval rpat (ERight expr)       _           con = throwError $ ErrorInfo "view shall be Either Right."
uneval rpat (EElem exprh exprt) []          con = throwError $ ErrorInfo "view list length is not correct."
uneval rpat (EElem exprh exprt) (vh' : vs') con = uneval rpat exprh vh' con >>= uneval rpat exprt vs'

updateRPat :: (MonadError' ErrorInfo m) => RPat v env con -> Direction env v' -> v' -> con -> m con
updateRPat RVar                DVar          v' (Just v'')   = if v' == v'' then return (Just v') else throwError $ ErrorInfo "multiple updating un equal"
updateRPat RVar                DVar          v' Nothing      = return $ Just v'
updateRPat (RConst c)          _             v' con          = return con
updateRPat (RProd rpatl rpatr) (DLeft dir)   v' (conl, conr) = liftM (, conr) (updateRPat rpatl dir v' conl)
updateRPat (RProd rpatl rpatr) (DRight dir)  v' (conl, conr) = liftM (conl ,) (updateRPat rpatr dir v' conr)
updateRPat (RLeft rpatl      ) dir           v' con          = updateRPat rpatl dir v' con
updateRPat (RRight rpatr     ) dir           v' con          = updateRPat rpatr dir v' con
updateRPat (RIn rpat         ) dir           v' con          = updateRPat rpat  dir v' con
updateRPat (RElem rpath rpatt) (DLeft dir)   v' (conl, conr) = liftM (, conr) (updateRPat rpath dir v' conl)
updateRPat (RElem rpath rpatt) (DRight dir)  v' (conl, conr) = liftM (conl ,) (updateRPat rpatt dir v' conr)

-- static check of the full embedding
checkFullEmbed :: MonadError' ErrorInfo m => BiGUL m s v -> m Bool
checkFullEmbed Fail = return True
checkFullEmbed Skip = return True
checkFullEmbed Replace = return True
checkFullEmbed (Update upat) = checkUPat upat
checkFullEmbed (Rearr rpat expr bigul) = checkRearr expr rpat >>= \b -> if b then checkFullEmbed bigul else return False
checkFullEmbed (Dep f bigul) = checkFullEmbed bigul
checkFullEmbed (CaseS sbranches) = liftM and $ mapM checkSBranch sbranches
checkFullEmbed (CaseV vbranches) = liftM and $ mapM checkVBranch vbranches
checkFullEmbed (Align filter matchCond bigul create seal) = checkFullEmbed bigul
checkFullEmbed (Emb g p) = return True
checkFullEmbed (Compose a b) = liftM2 (&&) (checkFullEmbed a) (checkFullEmbed b)

checkFullEmbed' :: MonadError' ErrorInfo m => BiGUL m s v -> m ()
checkFullEmbed' Fail = return ()
checkFullEmbed' Skip = return ()
checkFullEmbed' Replace = return ()
checkFullEmbed' (Update upat) = checkUPat' upat
checkFullEmbed' whole@(Rearr rpat expr bigul) =
  checkRearr expr rpat >>= \b -> if b then checkFullEmbed' bigul else throwError (ErrorInfo (show expr))
checkFullEmbed' (Dep f bigul) = checkFullEmbed' bigul
checkFullEmbed' (CaseS sbranches) = mapM_ checkSBranch' sbranches
checkFullEmbed' (CaseV vbranches) = mapM_ checkVBranch' vbranches
checkFullEmbed' (Align filter matchCond bigul create seal) = checkFullEmbed' bigul
checkFullEmbed' (Emb g p) = return ()
checkFullEmbed' (Compose a b) = (checkFullEmbed' a) >> (checkFullEmbed' b)

checkSBranch :: MonadError' ErrorInfo m => (s -> m Bool, CaseSBranch m s v)  -> m Bool
checkSBranch (cond, (Normal bigul)) = checkFullEmbed bigul
checkSBranch (cond, _)              = return True

checkSBranch' :: MonadError' ErrorInfo m => (s -> m Bool, CaseSBranch m s v)  -> m ()
checkSBranch' (cond, (Normal bigul)) = checkFullEmbed' bigul
checkSBranch' (cond, _)              = return ()

checkVBranch :: MonadError' ErrorInfo m => (v -> m Bool, BiGUL m s v)  ->  m Bool
checkVBranch (cond, bigul) = checkFullEmbed bigul

checkVBranch' :: MonadError' ErrorInfo m => (v -> m Bool, BiGUL m s v)  ->  m ()
checkVBranch' (cond, bigul) = checkFullEmbed' bigul

checkUPat :: MonadError' ErrorInfo m => UPat m s v -> m Bool
checkUPat (UVar bigul) = checkFullEmbed bigul
checkUPat (UConst c)   = return True
checkUPat (UProd upatl upatr) = checkUPat upatl >>= \b -> if b then checkUPat upatr else return False
checkUPat (ULeft upatl)       = checkUPat upatl
checkUPat (URight upatr)      = checkUPat upatr
checkUPat (UIn upat)          = checkUPat upat
checkUPat (UElem upath upatt) = checkUPat upath >>= \b -> if b then checkUPat upatt else return False

checkUPat' :: MonadError' ErrorInfo m => UPat m s v -> m ()
checkUPat' (UVar bigul) = checkFullEmbed' bigul
checkUPat' (UConst c)   = return ()
checkUPat' (UProd upatl upatr) = checkUPat' upatl >> checkUPat' upatr
checkUPat' (ULeft upatl)       = checkUPat' upatl
checkUPat' (URight upatr)      = checkUPat' upatr
checkUPat' (UIn upat)          = checkUPat' upat
checkUPat' (UElem upath upatt) = checkUPat' upath >> checkUPat' upatt

checkRearr :: MonadError' ErrorInfo m => Expr env v' -> RPat v env con -> m Bool
checkRearr expr rpat =  updateCon expr rpat (emptyContainer rpat) >>= checkCon rpat

checkCon :: MonadError' ErrorInfo m => RPat v env con -> con -> m Bool
checkCon RVar                 (Just _)     = return True
checkCon RVar                 Nothing      = return False
checkCon (RConst c)           _            = return True
checkCon (RProd rpatl rpatr)  (conl, conr) = checkCon rpatl conl >>= \b -> if b then checkCon rpatr conr else return b
checkCon (RLeft rpatl      )  con          = checkCon rpatl con
checkCon (RRight rpatr     )  con          = checkCon rpatr con
checkCon (RIn rpat         )  con          = checkCon rpat  con
checkCon (RElem rpath rpatt)  (conl, conr) = checkCon rpath conl >>= \b -> if b then checkCon rpatt conr else return b

updateCon :: MonadError' ErrorInfo m => Expr env v' -> RPat v env con -> con -> m con
updateCon (EDir dir) rpat con = updateDir rpat dir con
updateCon (EConst c) rpat con = return con
updateCon (EIn expr) rpat con = updateCon expr rpat con
updateCon (EProd exprl exprr) rpat con = updateCon exprl rpat con >>= updateCon exprr rpat
updateCon (ELeft expr)        rpat con = updateCon expr  rpat con
updateCon (ERight expr)       rpat con = updateCon expr  rpat con
updateCon (EElem exprh exprt) rpat con = updateCon exprh rpat con >>= updateCon exprt rpat

updateDir :: MonadError' ErrorInfo m => RPat v env con -> Direction env v' -> con -> m con
updateDir RVar                DVar         Nothing      = return $ Just undefined
updateDir RVar                DVar         (Just _)     = return $ Just undefined
updateDir (RConst c)          _            con          = return con
updateDir (RProd rpatl rpatr) (DLeft dir)  (conl, conr) = liftM (, conr) (updateDir rpatl dir conl)
updateDir (RProd rpatl rpatr) (DRight dir) (conl, conr) = liftM (conl, ) (updateDir rpatr dir conr)
updateDir (RLeft rpatl      ) dir          con          = updateDir rpatl dir con
updateDir (RRight rpatr     ) dir          con          = updateDir rpatr dir con
updateDir (RIn rpat         ) dir          con          = updateDir rpat  dir con
updateDir (RElem rpath rpatt) (DLeft dir)  (conl, conr) = liftM (, conr) (updateDir rpath dir conl)
updateDir (RElem rpath rpatt) (DRight dir) (conl, conr) = liftM (conl, ) (updateDir rpatt dir conr)

-- iteration over source list
iter :: (MonadError' ErrorInfo m, Eq v)  => BiGUL m s v -> BiGUL m [s] v
iter bigul = CaseS [
--  (return . null, Normal Fail),
  (return . (== 1) . length, Normal (Rearr RVar (EProd (EDir DVar) (EConst ())) (Update (UElem (UVar bigul) (UVar Skip))))),
  (return . not . null, Normal(Rearr RVar (EProd (EDir DVar) (EDir DVar)) (Update (UElem (UVar bigul) (UVar (iter bigul))))))
  ]

{-

data BiGUL = Fail
           | Skip
           | Replace -- id_lens: get is id, put ignore s
           | Rearr XQExpr BiGUL
           | Iter BiGUL -- map on list.
           | Align (s -> m Bool)
                   (s -> v -> m Bool)
                   BiGUL
                   (v -> m s)
                   (s -> m (Maybe s)) -- may have deletion on s.
           | CaseS [(s -> m Bool, Either (s -> m s) BiGUL)]
           | CaseV [(Pat, BiGUL)]
           | Update Pat
        deriving (Eq,Show)


data XQExpr = XQEmpty              -- ()
            | XQProd XQExpr XQExpr        -- e,e'
            | XQElem String XQExpr        -- n[e] when we construct an element we must put all attributes first and sorted
            | XQAttr String XQExpr        -- @n='e'
--            | XQString String          -- w
--            | XQVar XVar            -- x (any variable)
            | XQLet Pat XQExpr XQExpr      -- let x = e in e'
--            | XQBool Bool            -- true | false
            | XQIf XQExpr XQExpr XQExpr      -- if c then e else e'
            | XQBinOp XPath.Op XQExpr XQExpr  -- e ~~ e'
            | XQFor XVar XQExpr XQExpr      -- for x- \in e return e' (still for tree variables)
            | XQPath CPath            -- special case to consider core paths (since their implementation as core uXQ is very different)
  deriving (Eq,Show)

type XVar = String

-}
