{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric  #-}
module TypeFiles.Common where

import Generics.BiGUL
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import GHC.Generics

data CommonWebserver = CommonWebserver { 
    {-serving static content-}
    vRoot :: Root, 
    vIndex :: Index, 
    {-client connections-}
    vKeepaliveTimeout :: String, --Int
    vKeepaliveMaxRequests :: String, --Int 
    {-speed/quality-}
    vSendfile :: String, --Bool
    {-ssl-}
    {-
    vSSL :: String, --Bool
    vSSLCACertificate :: CACertificate, 
    vSSLCARevocationFile :: CARevocationFile, 
    vSSLCertificate :: Certificate, 
    vSSLCertificateKey :: CertificateKey, 
    vSSLCiphers :: Ciphers, 
    vSSLPreferServerCiphers :: PreferServerCiphers, 
    vSSLProtocols :: Protocols, 
    vSSLVerifyClient :: VerifyClient, 
    vSSLVerifyDepth :: VerifyDepth, 
    -}

    vServers :: [VServer] 
} deriving (Show, Eq)


data VServer = VServer { 
    vListen :: [Listen], 
    vServNames :: [ServerName], 
    {-serving static content-}
    vServRoot :: Root, 
    vServIndex :: Index, 
    {-client connections-}
    vServKeepaliveTimeout :: String, --Int
    vServKeepaliveMaxRequests :: String, --Int
    {-speed/quality-}
    vServSendfile :: String, --Bool
    {-ssl-}
    {-
    vServSSL :: String, --Bool
    vServSSLCACertificate :: CACertificate, 
    vServSSLCARevocationFile :: CARevocationFile, 
    vServSSLCertificate :: Certificate, 
    vServSSLCertificateKey :: CertificateKey, 
    vServSSLCiphers :: Ciphers, 
    vServSSLPreferServerCiphers :: PreferServerCiphers, 
    vServSSLProtocols :: Protocols, 
    vServSSLVerifyClient :: VerifyClient, 
    vServSSLVerifyDepth :: VerifyDepth, 
    -}

    vLocations :: [VLocation]
} deriving (Show, Eq)

data VLocation = VLocation {
    vLocationPath :: LocPath, 
    {-serving static content-}
    vLocIndex :: Index, 
    {-speed/quality-}
    vLocSendfile :: String --Bool

    {-vLocLocations :: [VLocation]-}
} deriving (Show, Eq)

type ID = String
type Pid = String 
type Listen = String
type ServerName = String
type LocPath = String --"path regex"
type ErrorPage = String
type Root = String
type Index = String
type CACertificate = String --"file" -N:SslClientCertificate -A:SSLCACertificateFile
type CARevocationFile = String --"file" -N:SslCrl -A:SSLCARevocationFile
type Certificate = String --"file" -N:SslCertificate -A:SSLCertificateFile
type CertificateKey = String --"file" -N:SslCertificateKey -A:SSLCertificateKeyFile
type Ciphers = String --N:SslCiphers A:SSLCipherSuite
type PreferServerCiphers = String --N:SslPreferServerCiphers A:SSLHonorCipherOrder
type Protocols = String 
type SessionTimeout = String --"seconds" N:SslSessionTimeout A:SSLSessionCacheTimeout
type VerifyClient = String --no|yes|optional|optional_no_ca
type VerifyDepth = String

deriveBiGULGeneric ''CommonWebserver
deriveBiGULGeneric ''VServer
deriveBiGULGeneric ''VLocation
