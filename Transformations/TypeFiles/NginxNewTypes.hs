{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric  #-}
module TypeFiles.NginxNewTypes where

import Generics.BiGUL
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Control.Monad
import GHC.Generics

import TypeFiles.NginxTypes hiding (NginxWebserver)

--Contexts
data NginxWebserver = NginxWebserver {
    nnDaemon :: Maybe Daemon, 
    nnErrorLog :: Maybe (ErrorLog, PermissionMode), 
    nnEnv :: Maybe [Env], 
    nnInclude :: Maybe [Include], 
    nnLockFile :: Maybe LockFile, 
    nnPid :: Maybe Pid, 
    nnSslEngine :: Maybe SslEngine, 
    nnThreadPool :: Maybe [ThreadPool], 
    nnTimerResolution :: Maybe TimerResolution, 
    nnUser :: Maybe User, 
    nnWorkerCpuAffinity :: Maybe WorkerCpuAffinity, 
    nnWorkerPriority :: Maybe WorkerPriority, 
    nnWorkerProcesses :: Maybe WorkerProcesses, 
    nnWorkerRLimitCore :: Maybe WorkerRlimitCore, 
    nnWorkerRLimitNoFile :: Maybe WorkerRlimitNofile, 
    nnWorkingDirectory :: Maybe WorkingDirectory, 
    nnEvents :: Maybe Events, 
    nnHttp :: Maybe Http
} deriving (Show,Eq)

type PermissionMode = String

deriveBiGULGeneric ''NginxWebserver

