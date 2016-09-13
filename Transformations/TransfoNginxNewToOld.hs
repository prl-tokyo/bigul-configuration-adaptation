{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric  #-}

module TransfoNginxNewToOld where

---------
--IMPORTS
---------
----BiGUL imports
import Generics.BiGUL
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Control.Monad
import GHC.Generics

import qualified TypeFiles.NginxTypes as O
import qualified TypeFiles.NginxNewTypes as N
import TypeFiles.Common
import NginxDefaultValues
import TransfoNginx

transNginxNew :: MonadError' e m => DefaultValues -> BiGUL m N.NginxWebserver CommonWebserver
transNginxNew defaults = transNginxOldToNew `Compose` transNginx defaults

transNginxOldToNew :: MonadError' e m => BiGUL m N.NginxWebserver O.NginxWebserver
transNginxOldToNew = $(rearrAndUpdate [p| O.NginxWebserver {
    O.nDaemon = nDaemon,
    O.nErrorLog = nErrorLog,
    O.nEnv = nEnv,
    O.nInclude = nInclude,
    O.nLockFile = nLockFile,
    O.nPid = nPid,
    O.nSslEngine = nSslEngine,
    O.nThreadPool = nThreadPool,
    O.nTimerResolution = nTimerResolution,
    O.nUser = nUser,
    O.nWorkerCpuAffinity = nWorkerCpuAffinity,
    O.nWorkerPriority = nWorkerPriority,
    O.nWorkerProcesses = nWorkerProcesses,
    O.nWorkerRLimitCore = nWorkerRLimitCore,
    O.nWorkerRLimitNoFile = nWorkerRLimitNoFile,
    O.nWorkingDirectory = nWorkingDirectory,
    O.nEvents = nEvents,
    O.nHttp = nHttp
} |] [p| N.NginxWebserver {
    N.nnDaemon = nDaemon,
    N.nnErrorLog = nErrorLog,
    N.nnEnv = nEnv,
    N.nnInclude = nInclude,
    N.nnLockFile = nLockFile,
    N.nnPid = nPid,
    N.nnSslEngine = nSslEngine,
    N.nnThreadPool = nThreadPool,
    N.nnTimerResolution = nTimerResolution,
    N.nnUser = nUser,
    N.nnWorkerCpuAffinity = nWorkerCpuAffinity,
    N.nnWorkerPriority = nWorkerPriority,
    N.nnWorkerProcesses = nWorkerProcesses,
    N.nnWorkerRLimitCore = nWorkerRLimitCore,
    N.nnWorkerRLimitNoFile = nWorkerRLimitNoFile,
    N.nnWorkingDirectory = nWorkingDirectory,
    N.nnEvents = nEvents,
    N.nnHttp = nHttp
} |] [d| nErrorLog = errorLogBX;
         nDaemon = Replace;
         nEnv = Replace;
         nInclude = Replace;
         nLockFile = Replace;
         nPid = Replace;
         nSslEngine = Replace;
         nThreadPool = Replace;
         nTimerResolution = Replace;
         nUser = Replace;
         nWorkerCpuAffinity = Replace;
         nWorkerPriority = Replace;
         nWorkerProcesses = Replace;
         nWorkerRLimitCore = Replace;
         nWorkerRLimitNoFile = Replace;
         nWorkingDirectory = Replace;
         nEvents = Replace;
         nHttp = Replace
    |])
    where errorLogBX = Emb (return . fmap fst)
                           (\s v -> return (case s of
                               Nothing -> fmap (\l -> (l, "644")) v
                               Just (_, p) -> fmap (\l -> (l, p)) v))
