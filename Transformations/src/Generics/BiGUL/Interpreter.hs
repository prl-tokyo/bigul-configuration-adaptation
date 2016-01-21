{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, DeriveGeneric, TupleSections #-}
module Generics.BiGUL.Interpreter (put, get) where

import Generics.BiGUL.AST
import Generics.BiGUL.MonadBiGULError
import Control.Monad
import Control.Monad.Except
import GHC.InOut
import Data.Maybe (catMaybes)
import Data.Foldable
import Control.Arrow

put :: MonadError' ErrorInfo m => BiGUL m s v -> s -> v -> m s
put Fail s v = throwError $ ErrorInfo "update fails"
put Skip s v = return s
put Replace s v = return v
put (Update upat) s v = putUPat upat s v
put (Rearr rpat expr bigul) s v = deconstructR rpat v >>= put bigul s . eval expr
put (Dep f bigul) s (v, v') = if f v == v' then put bigul s v else throwError $ ErrorInfo "view dependency not match"
put (CaseS branchList) s v = putCaseS branchList s v
put (CaseV branchList) s v = putCaseV branchList s v
put (CaseSV branchList) s v = putCaseSV branchList s v
put (Align sourceCond matchCond matchBigul create conceal) s v = putAlign sourceCond matchCond matchBigul create conceal s v
put (Emb g p) s v = p s v
put (Compose bigul1 bigul2) s v = do u <- get bigul1 s
                                     u2 <- put bigul2 u v
                                     put bigul1 s u2

putUPat :: MonadError' ErrorInfo m => UPat m s v -> s -> v -> m s
putUPat (UVar bigul) s v = put bigul s v
putUPat (UConst c  ) s v = if s == c then return c else throwError $ ErrorInfo "source is not a const: "
putUPat (UProd upatl upatr) (sl, sr) (vl, vr) = liftM2 (,) (putUPat upatl sl vl) (putUPat upatr sr vr)
putUPat (ULeft upat) (Left s) v = liftM Left (putUPat upat s v)
putUPat (ULeft _   ) _ _ = throwError $ ErrorInfo "Either Left not match"
putUPat (URight upat) (Right s) v = liftM Right (putUPat upat s v)
putUPat (URight _) _ _ = throwError $ ErrorInfo "Either Right not match"
putUPat (UIn upat) s v = liftM inn (putUPat upat (out s) v)
putUPat (UElem upath upatt) [] (v, vs)  = throwError $ ErrorInfo "UElem pat not match"
putUPat (UElem upath upatt) (s:xs) (v, vs) = liftM2 (:) (putUPat upath s v) (putUPat upatt xs vs)


putCaseS :: MonadError' ErrorInfo m => [(s -> m Bool, CaseSBranch m s v)] -> s -> v -> m s
putCaseS branches s v = putCaseSAccu branches s v branches False []


putCaseSAccu :: MonadError' ErrorInfo m => [(s -> m Bool, CaseSBranch m s v)] -> s -> v -> [(s -> m Bool, CaseSBranch m s v)] -> Bool -> [s -> m Bool] -> m s
putCaseSAccu [] s v _ _ _ = throwError $ ErrorInfo "No matching pattern for caseS"
putCaseSAccu branches@(x@(p, branch): xs) s v allBranches flag accuPs = p s >>=
  \b -> if b
    then case branch of
      Normal bigul -> put bigul s v >>= \s' -> p s' >>= \b' -> if b' then checkAccuBranches accuPs s' else throwError $ ErrorInfo "updated source does not satisfy the condition." 
      Adaptive f   -> if flag then throwError $ ErrorInfo "meet adaptive branch again"
                              else f s v >>= \s' -> putCaseSAccu allBranches s' v allBranches True []
    else putCaseSAccu xs s v allBranches flag (accuPs ++ [p])

-- AccuBranches only contains normal branches.
checkAccuBranches :: MonadError' ErrorInfo m => [s -> m Bool] -> s -> m s
checkAccuBranches [] s = return s
checkAccuBranches (p: xs) s = p s >>=
  \b -> if b
    then throwError $ ErrorInfo "Updated source matched with previous branch"
    else checkAccuBranches xs s

--putCaseSHelp :: MonadError' ErrorInfo m => [(s -> m Bool, CaseSBranch m s v)] -> s -> v -> [(s -> m Bool, CaseSBranch m s v)] -> Bool -> m s
--putCaseSHelp [] s v _ _ = throwError $ ErrorInfo "caseS empty with no matching pat"
--putCaseSHelp branches@(x@(p, branch): xs) s v backBranches flag = p s >>=
--  \b -> if b
--           then
--             case branch of
--                  Normal bigul -> put bigul s v >>= \s' -> p s' >>= \b' -> if b' then return s' else throwError $ ErrorInfo "update changes the branch."
--                  Adaptive f -> if flag
--                                   then throwError $ ErrorInfo "meet adaptive branch again"
--                                   else f s v >>= \s' -> putCaseSHelp backBranches s' v backBranches True
--           else putCaseSHelp xs s v  backBranches flag >>= \s' -> p s' >>= \b -> if b then throwError $ ErrorInfo "previous pat matches the updated source" else return s'


putCaseV :: MonadError' ErrorInfo m => [(v -> m Bool, BiGUL m s v)] -> s -> v -> m s
putCaseV [] s v = throwError $ ErrorInfo "caseV pattern is empty"
putCaseV (x@(p , bigul) :xs) s v = p v >>=
  \b -> if b
    then put bigul s v
    else putCaseV xs s v >>= \s' ->  catchBind (getSelected p bigul s') (\_ -> throwError $ ErrorInfo "get of previous caseV satisfied") (\_ -> return s')
--catchBind (deconstruct patv2v' v)
--          (put bigul s)
--          (\_ -> putCaseV xs s v >>= \s' -> catchBind (get bigul s') (\_ -> throwError $ ErrorInfo "get of previous caseV satisfied") (\_ -> return s'))
-- catchBind (get bigul s') (\v' -> if v == v' then return s' else throwError $ ErrorInfo ""  ) (\_ -> return s')

putCaseSV :: MonadError' ErrorInfo m => [(s -> v -> m Bool, BiGUL m s v)] -> s -> v -> m s
putCaseSV [] s v = throwError $ ErrorInfo "caseSV pattern is empty"
putCaseSV (x@(p , bigul) : xs) s v = p s v >>=
  \b -> if b
    then put bigul s v
    else putCaseSV xs s v >>= \s' -> catchBind (getSelected' p bigul s') (\_ -> throwError $ ErrorInfo "get of previous caseSV satisfied") (\_ -> return s')


putAlign :: MonadError' ErrorInfo m =>
             (s -> m Bool)
          -> (s -> v -> m Bool)
          -> BiGUL m s v
          -> (v -> m s)
          -> (s -> m (Maybe s))
          -> [s]
          -> [v]
          -> m [s]
putAlign sourceCond matchCond matchBigul create conceal ss vs =
  filterSourceList sourceCond ss >>= \(filtered, residual) ->
    align vs filtered sourceCond matchCond matchBigul create conceal >>= \(concealed, synced) ->  return (unfilterP (concealed ++ synced) residual)
-- put the unmatched source on the top, and this will not affect matched element for view.

-- Filter tries to remember the location.
filterSourceList :: MonadError' ErrorInfo m => (s -> m Bool) -> [s] -> m ([s], [Maybe s])
filterSourceList p [] = return $ ([], [])
filterSourceList p (s: ss) = p s >>= \b -> liftM (\(ls, rs) -> if b then (s:ls, (Nothing: rs)) else (ls, (Just s) :rs)) (filterSourceList p ss)


align :: MonadError' ErrorInfo m => [v] -> [s] ->
             (s -> m Bool)
          -> (s -> v -> m Bool)
          -> BiGUL m s v
          -> (v -> m s)
          -> (s -> m (Maybe s))
          -> m ([s], [s])
align []         ss         sourceCond matchCond matchBigul create conceal = liftM (, []) $ concealSourceList ss sourceCond conceal
align vss@(v:vs) []         sourceCond matchCond matchBigul create conceal = liftM ([] ,) $ createSourceList vss create sourceCond (put matchBigul) matchCond
align (v:vs)     sss@(s:ss) sourceCond matchCond matchBigul create conceal = firstMatch v sss matchCond >>=
            maybe (liftM2 (\s' -> id *** (s':)) (createAndCheck v create sourceCond (put matchBigul) matchCond) (align vs sss sourceCond matchCond matchBigul create conceal))
                  (\(matchedS, sRest) -> putAndCheck matchedS v sourceCond (put matchBigul) matchCond >>= \s' -> liftM (id *** (s':)) (align vs sRest sourceCond matchCond matchBigul create conceal))



-- for a list of unmatched source, make them disappear in the view.
-- Make this list disappear by unsatisfying the condition.
-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
-- foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
concealSourceList :: MonadError' ErrorInfo m => [s] -> (s -> m Bool) -> (s -> m (Maybe s)) -> m [s]
concealSourceList ss p conceal = foldrM (\s ls -> conceal s >>= \ms' -> case ms' of {Just s' -> p s' >>= \b -> if b then throwError $ ErrorInfo "shall not satisfy cond anymore" else return (s': ls) ; Nothing -> return ls}) [] ss

-- for a list of unmatched view, create a source list.
createSourceList :: MonadError' ErrorInfo m => [v] -> (v -> m s) -> (s -> m Bool) -> (s -> v -> m s) -> (s -> v -> m Bool) -> m [s]
createSourceList []      create p elemPut matchCond = return []
createSourceList (v: vs) create p elemPut matchCond = liftM2 (:) (createAndCheck v create p elemPut matchCond) (createSourceList vs create p elemPut matchCond)

createAndCheck :: MonadError' ErrorInfo m => v -> (v -> m s) -> (s -> m Bool) -> (s -> v -> m s) -> (s -> v -> m Bool)-> m s
createAndCheck v create p elemPut  matchCond = create v >>= \s -> putAndCheck s v p elemPut matchCond


putAndCheck :: MonadError' ErrorInfo m => s -> v -> (s -> m Bool) -> (s -> v -> m s) -> (s -> v -> m Bool)-> m s
putAndCheck s v p elemPut matchCond = elemPut s v
                                      >>= \s' -> matchCond s' v
                                      >>= \b -> if b
                                              then p s' >>= \b' ->
                                                  if b' then return s' else throwError $ ErrorInfo "created source not satisfy the source filter condition"
                                              else throwError $ ErrorInfo "created source not aligned with the matched view"



firstMatch :: MonadError' ErrorInfo m => v -> [s] -> (s -> v -> m Bool) -> m (Maybe (s, [s]))
firstMatch v []      matchCond = return Nothing
firstMatch v (s: ss) matchCond =
  matchCond s v >>= \b -> if b
                  then return (Just (s, ss))
                  else liftM (fmap (id *** (s:))) (firstMatch v ss matchCond)
                  --else liftM (fmap (\(s1, slast) -> (s1, s:slast))) (firstMatch v ss matchCond)
                  --else liftM (\mtuple -> case mtuple of {Just (s1, slast) -> Just (s1, s:slast); Nothing -> Nothing }) (firstMatch v ss matchCond)


unfilterP :: [s] -> [Maybe s] -> [s]
unfilterP xs [] = xs
unfilterP [] myss@(my: mys) = catMaybes myss
unfilterP (x: xs) (Nothing : mys) = x : unfilterP xs mys
unfilterP xss@(x: xs) (Just y  : mys) = y : unfilterP xss mys

get :: MonadError' ErrorInfo m => BiGUL m s v -> s -> m v
get Fail s = throwError $ ErrorInfo "get failed"
get Skip s = return $ ()
get Replace s = return s
get (Update upat) s = getUPat upat s
get (Rearr rpat expr bigul) s = get bigul s >>= \v' -> uneval rpat expr v' (emptyContainer rpat) >>= constructR rpat
get (Dep f bigul) s = get bigul s >>= \v -> return $ (v, f v)
get (CaseS sbranches) s = getCaseS sbranches s
get (CaseV vbranches) s = getCaseV vbranches s
get (Align sourceCond matchCond matchBigul create conceal) s = getAlign sourceCond matchCond matchBigul create conceal s
get (Emb g p) s = g s
get (Compose bigul1 bigul2) s = do u <- get bigul1 s
                                   get bigul2 u


getUPat :: MonadError' ErrorInfo m => UPat m s v -> s -> m v
getUPat (UVar bigul) s = get bigul s
getUPat (UConst c  ) s = if c== s then return () else throwError $ ErrorInfo "source is not a constant."
getUPat (UProd upatl upatr) (s, s') = liftM2 (,) (getUPat upatl s) (getUPat upatr s')
getUPat (ULeft  upat) (Left s)  = getUPat upat s
getUPat (ULeft  upat) _         = throwError $ ErrorInfo "ULeft pat not match"
getUPat (URight upat) (Right s) = getUPat upat s
getUPat (URight upat) _         = throwError $ ErrorInfo "URight pat not match"
getUPat (UIn    upat) s          = getUPat upat (out s)
getUPat (UElem upath upatt) []  = throwError $ ErrorInfo "UElem cannot accept empty source list"
getUPat (UElem upath upatt) (x: xs) = liftM2 (,) (getUPat upath x) (getUPat upatt xs)

getCaseS :: MonadError' ErrorInfo m => [(s -> m Bool, CaseSBranch m s v)] -> s -> m v
getCaseS []  s = throwError $ ErrorInfo "Get: caseS branch is empty"
getCaseS (branch@(p, caseSBranch) : restBranches) s =
  p s >>= \b ->
    if b
    then case caseSBranch of
              Normal bigul -> get bigul s
              Adaptive f   -> throwError $ ErrorInfo "Get: caseS shall not match adaptive branch"
    else getCaseS restBranches s


getSelected :: MonadError' ErrorInfo m => (v -> m Bool) -> BiGUL m s v -> s -> m v
getSelected p bigul s = get bigul s >>= \v -> p v >>= \b -> if b then return v else throwError $ ErrorInfo "fail to satisfy p."

getSelected' :: MonadError' ErrorInfo m => (s -> v -> m Bool) -> BiGUL m s v -> s -> m v
getSelected' p bigul s = get bigul s >>= \v -> p s v >>= \b -> if b then return v else throwError $ ErrorInfo "fail to satisfy p."

getCaseV :: MonadError' ErrorInfo m => [(v -> m Bool, BiGUL m s v)] -> s -> m v
getCaseV [] s = throwError $ ErrorInfo "Get: caseV branch is empty"
getCaseV (branch@(p , bigul) : restBranches) s =
  catchBind (getSelected p bigul s)
            return
            (\e -> catchBind (getCaseV restBranches s) (\v -> p v >>= \b -> if b
                                                                            then throwError $ ErrorInfo "Get: caseV previous pattern matched."
                                                                            else return v)  (\e2 -> throwError $ ErrorInfo "failed."))

--catchBind (get bigul s)
--            (\v' -> return $ construct pat v')
--            (\e -> catchBind (getCaseV restBranches s) (\v -> catchBind (deconstruct pat v) (\v' -> throwError $ ErrorInfo "Get: caseV previous pattern matched.") (\e -> return v)) (\e2 -> throwError $ ErrorInfo "failed."))


getCaseSV :: MonadError' ErrorInfo m => [(s -> v -> m Bool , BiGUL m s v)] -> s -> m v
getCaseSV [] s = throwError $ ErrorInfo "Get: caseSV branch is empty"
getCaseSV (branch@(p , bigul) : restBranches) s =
  catchBind (getSelected' p bigul s)
            return
            (\e -> catchBind (getCaseSV restBranches s) (\v -> p s v >>= \b -> if b
                                                                               then throwError $ ErrorInfo "Get: caseV previous pattern matched."
                                                                               else return v)  (\e2 -> throwError $ ErrorInfo "failed."))


getAlign :: MonadError' ErrorInfo m =>
             (s -> m Bool)
          -> (s -> v -> m Bool)
          -> BiGUL m s v
          -> (v -> m s)
          -> (s -> m (Maybe s))
          -> [s]
          -> m [v]
getAlign sourceCond matchCond bigul create conceal ss =
  filterSourceList sourceCond ss >>= \(filteredSource, residualSource) ->
    mapM (getAndCheck bigul matchCond) filteredSource

getAndCheck :: MonadError' ErrorInfo m => BiGUL m s v -> (s -> v -> m Bool) -> s -> m v
getAndCheck bigul matchCond s = get bigul s >>= \v -> matchCond s v >>= \b -> if b then return v else throwError $ ErrorInfo "get: matchCond not satisfied."
