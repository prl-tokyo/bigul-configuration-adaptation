{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric  #-}

---------
--IMPORTS
---------
----BiGUL imports
import Generics.BiGUL
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Control.Monad
import GHC.Generics
--parser-printer imports
import NginxPrettyPrinter
import NginxSourceCreator
import TreeConfigNginxFiller
----nginx imports
import Nginx_output
import TypeFiles.NginxTypes
import NginxDefaultValues
import TypeFiles.Common

--source and view records defined as BiGUL types
deriveBiGULGeneric ''NginxWebserver
deriveBiGULGeneric ''Events
deriveBiGULGeneric ''Http
deriveBiGULGeneric ''Server
deriveBiGULGeneric ''Location

deriveBiGULGeneric ''CommonWebserver
deriveBiGULGeneric ''VServer
deriveBiGULGeneric ''VLocation

--importing view
nginxView' :: CommonWebserver
nginxView' = nginxOutput


-----------------
--TRANSFORMATIONS
-----------------
--global transformation
transNginx :: MonadError' e m => DefaultValues -> BiGUL m NginxWebserver CommonWebserver
transNginx defaults = $(rearrAndUpdate [p| CommonWebserver { 
    vRoot = root, 
    vIndex = index, 
    vKeepaliveTimeout = kaTimeout, 
    vKeepaliveMaxRequests = kaMaxRequests, 
    vSendfile = sendfile, 
    vSSL = ssl, 
    vSSLCACertificate = caCertif, 
    vSSLCARevocationFile = caRevocFile, 
    vSSLCertificate = certif, 
    vSSLCertificateKey = certifKey, 
    vSSLCiphers = ciphers, 
    vSSLPreferServerCiphers = preferServCiphers, 
    vSSLProtocols = protocols, 
    vSSLVerifyClient = verifyClient, 
    vSSLVerifyDepth = verifyDepth, 
    vServers = servers
}
 |] [p| NginxWebserver { 
    nHttp = Just Http { 
        hRoot = root, 
        hIndex = index, 
        hKeepaliveTimeout = kaTimeout, 
        hKeepaliveRequests = kaMaxRequests, 
        hSendFile = sendfile, 
        hSsl = ssl, 
        hSslCertificate = certif, 
        hSslCertificateKey = certifKey, 
        hSslCiphers = ciphers, 
        hSslClientCertificate = caCertif, 
        hSslCrl = caRevocFile, 
        hSslPreferServerCiphers = preferServCiphers, 
        hSslProtocols = protocols, 
        hSslVerifyClient = verifyClient, 
        hSslVerifyDepth = verifyDepth, 
        hServer = servers
    } 
}
 |] [d| root = addDefault (d_root defaults);
        index = addDefault (d_index defaults);
        kaTimeout = addDefault (d_keepalive_timeout defaults);
        kaMaxRequests = addDefault (d_keepalive_requests defaults);
        sendfile = addDefault (d_send_file defaults);
        ssl = addDefault (d_ssl defaults);
        caCertif = addDefault (d_ssl_client_certificate defaults);
        caRevocFile = addDefault (d_ssl_crl defaults);
        certif = addDefault (d_ssl_certificate defaults);
        certifKey = addDefault (d_ssl_certificate_key defaults);
        ciphers = addDefault (d_ssl_ciphers defaults);
        preferServCiphers = addDefault (d_ssl_prefered_server_ciphers defaults);
        protocols = addDefault (d_ssl_protocols defaults);
        verifyClient = verifyClientDefault (d_ssl_verify_client defaults);
        verifyDepth = addDefault (d_ssl_verify_depth defaults)
        servers = CaseV [ 
            $(branch  [p| [] |]) $ $(rearr [| \ []  -> Nothing |]) Replace, 
            $(branch  [p| (_:_) |]) $ (Compose ($(rearr [| \ x -> (Just x) |]) Replace) (transServer defaults))
                        ]
         |])


--servers transformation
transServer :: MonadError' e m => DefaultValues -> BiGUL m [Server] [VServer]
transServer defaults = Align
    --source condition
    (\ _ -> return True)
    --match
    --defines on which field the records will match between source and view
    (\ (Server {sServerName = (Just sName)} ) (VServer {vServNames = vName} ) -> return ((head sName) == (head vName)))
    --trans
    ($(rearrAndUpdate [p| VServer { 
        vListen = listen, 
        vServNames = servNames, 
        vServRoot = servRoot, 
        vServIndex = servIndex, 
        vServKeepaliveTimeout = servKaTimeout, 
        vServKeepaliveMaxRequests = servKaMaxRequests, 
        vServSendfile = servSendfile, 
        vServSSL = servSSL, 
        vServSSLCACertificate = servCaCertif, 
        vServSSLCARevocationFile = servCaRevocFile, 
        vServSSLCertificate = servCertif, 
        vServSSLCertificateKey = servCertifKey, 
        vServSSLCiphers = servCiphers, 
        vServSSLPreferServerCiphers = servPreferServCiphers, 
        vServSSLProtocols = servProtocols, 
        vServSSLVerifyClient = servVerifyClient, 
        vServSSLVerifyDepth = servVerifyDepth, 
        vLocations = locations
    } |] [p| Server { 
        sListen = listen, 
        sIndex = servIndex, 
        sRoot = servRoot, 
        sKeepaliveTimeout = servKaTimeout, 
        sKeepaliveRequests = servKaMaxRequests, 
        sSendFile = servSendfile, 
        sServerName = servNames,  
        sSsl = servSSL, 
        sSslCertificate = servCertif, 
        sSslCertificateKey = servCertifKey, 
        sSslCiphers = servCiphers, 
        sSslClientCertificate = servCaCertif, 
        sSslCrl = servCaRevocFile, 
        sSslPreferServerCiphers = servPreferServCiphers, 
        sSslProtocols = servProtocols, 
        sSslVerifyClient = servVerifyClient, 
        sSslVerifyDepth = servVerifyDepth, 
        sLocation = locations
    } |] [d| servRoot = addDefault (d_root defaults);
             servIndex = addDefault (d_index defaults);
             servKaTimeout = addDefault (d_keepalive_timeout defaults);
             servKaMaxRequests = addDefault (d_keepalive_requests defaults);
             servSendfile = addDefault (d_send_file defaults);
             servNames = addDefaultList (d_server_name defaults);
             listen = addDefaultList (d_listen defaults);
             servSSL = addDefault (d_ssl defaults);
             servCaCertif = addDefault (d_ssl_client_certificate defaults);
             servCaRevocFile = addDefault (d_ssl_crl defaults);
             servCertif = addDefault (d_ssl_certificate defaults);
             servCertifKey = addDefault (d_ssl_certificate_key defaults);
             servCiphers = addDefault (d_ssl_ciphers defaults);
             servPreferServCiphers = addDefault (d_ssl_prefered_server_ciphers defaults);
             servProtocols = addDefault (d_ssl_protocols defaults);
             servVerifyClient = verifyClientDefault (d_ssl_verify_client defaults);
             servVerifyDepth = addDefault (d_ssl_verify_depth defaults);
             locations = CaseV [ 
                $(branch  [p| [] |]) $ $(rearr [| \ []  -> Nothing |]) Replace, 
                $(branch  [p| (_:_) |]) $ (Compose ($(rearr [| \ x -> (Just x) |]) Replace) (transLocation defaults))
                               ] 
    |] ))
    --create
    --adds a new server to the source if a new one was added to the view
    (\ VServer { 
        vListen = listen, 
        vServNames = servNames, 
        vServRoot = servRoot, 
        vServIndex = servIndex, 
        vServKeepaliveTimeout = servKaTimeout, 
        vServKeepaliveMaxRequests = servKaMaxRequests, 
        vServSendfile = servSendfile, 
        vServSSL = servSSL, 
        vServSSLCACertificate = servCaCertif, 
        vServSSLCARevocationFile = servCaRevocFile, 
        vServSSLCertificate = servCertif, 
        vServSSLCertificateKey = servCertifKey, 
        vServSSLCiphers = servCiphers, 
        vServSSLPreferServerCiphers = servPreferServCiphers, 
        vServSSLProtocols = servProtocols, 
        vServSSLVerifyClient = servVerifyClient, 
        vServSSLVerifyDepth = servVerifyDepth, 
        vLocations = locations
    } -> return Server { 
        sListen = emptyListCheck listen, 
        sIndex = emptyCheck servIndex, 
        sRoot = emptyCheck servRoot, 
        sErrorPage = Nothing, 
        sKeepaliveDisable = Nothing, 
        sKeepaliveTimeout = emptyCheck servKaTimeout, 
        sKeepaliveRequests = emptyCheck servKaMaxRequests, 
        sSendFile = emptyCheck servSendfile, 
        sServerName = emptyListCheck servNames,  
        sAccessLog = Nothing, 
        sErrorLog = Nothing, 
        sGzip = Nothing, 
        sGzipCompLevel = Nothing, 
        sSsl = emptyCheck servSSL, 
        sSslCertificate = emptyCheck servCertif, 
        sSslCertificateKey = emptyCheck servCertifKey, 
        sSslCiphers = emptyCheck servCiphers, 
        sSslClientCertificate = emptyCheck servCaCertif, 
        sSslCrl = (emptyCheck servCaRevocFile), 
        sSslPreferServerCiphers = emptyCheck servPreferServCiphers, 
        sSslProtocols = emptyCheck servProtocols, 
        sSslSessionTimeout = Nothing, 
        sSslVerifyClient = emptyCheck servVerifyClient, 
        sSslVerifyDepth = emptyCheck servVerifyDepth, 
        sLocation = Nothing,
        sAccessRule = Nothing, 
        sAddHeader = Nothing, 
        sAio = Nothing, 
        sAuthBasic = Nothing, 
        sAuthBasicUserFile = Nothing, 
        sAutoindex = Nothing, 
        sAiExactSize = Nothing, 
        sAiFormat = Nothing, 
        sAiLocaltime = Nothing, 
        sAncientBrowser = Nothing, 
        sAncientBrowserValue = Nothing, 
        sModernBrowser = Nothing, 
        sModernBrowserValue = Nothing, 
        sCharset = Nothing, 
        sOverrideCharset = Nothing, 
        sSourceCharset = Nothing, 
        sCharsetType = Nothing, 
        sChunkedTransferEncoding = Nothing, 
        sClientHeaderBufferSize = Nothing, 
        sClientBodyBufferSize = Nothing, 
        sClientBodyInFileOnly = Nothing, 
        sClientBodyInSingleBuffer = Nothing, 
        sClientBodyTempPath = Nothing, 
        sClientBodyTimeout = Nothing, 
        sClientMaxBodySize = Nothing, 
        sClientHeaderTimeout = Nothing, 
        sConnectionPoolSize = Nothing, 
        sDefaultType = Nothing, 
        sDirectio = Nothing, 
        sDirectioAlignment = Nothing, 
        sDisableSymlinks = Nothing, 
        sEtag = Nothing, 
        sExpires = Nothing, 
        sGzipBuffers = Nothing, 
        sGzipDisable = Nothing, 
        sGzipMinLength = Nothing, 
        sGzipHttpVersion = Nothing, 
        sGzipProxied = Nothing, 
        sGzipTypes = Nothing, 
        sGzipVary = Nothing, 
        sIfModifiedSince = Nothing, 
        sIgnoreInvalidHeaders = Nothing, 
        sInclude = Nothing, 
        sLargeClientHeaderBuffers = Nothing, 
        sLimitConn = Nothing, 
        sLimitConnLogLevel = Nothing, 
        sLimitConnStatus = Nothing, 
        sLimitRate = Nothing, 
        sLimitRateAfter = Nothing, 
        sLimitReq = Nothing, 
        sLimitReqLogLevel = Nothing, 
        sLimitReqStatus = Nothing, 
        sLingeringClose = Nothing, 
        sLingeringTime = Nothing, 
        sLingeringTimeout = Nothing, 
        sLogNotFound = Nothing, 
        sLogSubrequest = Nothing, 
        sOpenLogFileCache = Nothing, 
        sMaxRanges = Nothing, 
        sMemcachedBind = Nothing, 
        sMemCachedBufferSize = Nothing, 
        sMemcachedConnectTimeout = Nothing, 
        sMemcachedForceRanges = Nothing, 
        sMemcachedGzipFlag = Nothing, 
        sMemcachedNextUpstream = Nothing, 
        sMemcachedNextUpstreamTimeout = Nothing, 
        sMemcachedNextUpstreamTries = Nothing, 
        sMemcachedReadTimeout = Nothing, 
        sMemcachedSendTimeout = Nothing, 
        sMergeSlashes = Nothing, 
        sMsiePadding = Nothing, 
        sMsieRefresh = Nothing, 
        sOpenFileCache = Nothing, 
        sOpenFileCacheErrors = Nothing, 
        sOpenFileCacheMinUses = Nothing, 
        sOpenFileCacheValid = Nothing, 
        sOutputBuffers = Nothing, 
        sPostponeOutput = Nothing, 
        sPortInRedirect = Nothing, 
        sReadAhead = Nothing, 
        sRecursiveErrorPages = Nothing, 
        sValidReferer = Nothing, 
        sRefererHashBucketSize = Nothing, 
        sRefererHashMaxSize = Nothing, 
        sRequestPoolSize = Nothing, 
        sResetTimedoutConnection = Nothing, 
        sResolver = Nothing, 
        sResolverTimeout = Nothing, 
        sSatisfy = Nothing, 
        sSendLowat = Nothing, 
        sSendTimeout = Nothing, 
        sSendFileMaxChunks = Nothing, 
        sServerNameInRedirect = Nothing, 
        sServerTokens = Nothing, 
        sSslBufferSize = Nothing, 
        sSslDhparam = Nothing, 
        sSslEcdhCurve = Nothing, 
        sSslPasswordFile = Nothing, 
        sSslSessionCache = Nothing, 
        sSslSessionTicketKey = Nothing, 
        sSslSessionTickets = Nothing, 
        sSslStapling = Nothing, 
        sSslStaplingFile = Nothing, 
        sSslStaplingResponder = Nothing, 
        sSslStaplingVerify = Nothing, 
        sSslTrustedCertificate = Nothing, 
        sTcpNodelay = Nothing, 
        sTcpNopush = Nothing, 
        sTryFiles = Nothing, 
        sTypes = Nothing, 
        sTypesHashBucketSize = Nothing, 
        sTypesHashMaxSize = Nothing, 
        sUnderscoresInHeaders = Nothing
    })
    -- conceal
    (\ _ -> return Nothing)

--locations transformation
transLocation :: MonadError' e m => DefaultValues -> BiGUL m [Location] [VLocation]
transLocation defaults = Align
    --source condition
    (\ _ -> return True)
    --match
    --defines on which field the records will match between source and view
    (\ (Location { lLocationPath = (Just sPath) } ) (VLocation { vLocationPath = vPath } ) -> return (sPath == vPath))
    --trans
    ($(rearrAndUpdate [p| VLocation {
        vLocationPath = locPath, 
        vLocIndex = locIndex, 
        vLocSendfile = locSendfile
    } |] [p| Location {
        lLocationPath = locPath, 
        lIndex = locIndex, 
        lSendFile = locSendfile
    } |] [d| locPath = $(rearr [| \ x -> (Just x)  |]) Replace;
             locIndex = addDefault (d_index defaults);
             locSendfile = addDefault (d_send_file defaults)
    |]))
    --create
    --adds a new location to the source if a new one was added to the view
    (\ VLocation {
        vLocationPath = locPath, 
        vLocIndex = locIndex, 
        vLocSendfile = locSendfile
    } -> return Location {
        lLocationPath = emptyCheck locPath, 
        lRoot = Nothing, 
        lIndex = emptyCheck locIndex, 
        lErrorPage = Nothing, 
        lKeepaliveDisable = Nothing, 
        lKeepaliveTimeout = Nothing, 
        lKeepaliveRequests = Nothing, 
        lSendFile = emptyCheck locSendfile, 
        lAccessLog = Nothing, 
        lErrorLog = Nothing, 
        lLocation = Nothing, 
        lAccessRule = Nothing, 
        lAddHeader = Nothing, 
        lAio = Nothing, 
        lAlias = Nothing, 
        lAuthBasic = Nothing, 
        lAuthBasicUserFile = Nothing, 
        lAutoindex = Nothing, 
        lAiExactSize = Nothing, 
        lAiFormat = Nothing, 
        lAiLocaltime = Nothing, 
        lAncientBrowser = Nothing, 
        lAncientBrowserValue = Nothing, 
        lModernBrowser = Nothing, 
        lModernBrowserValue = Nothing, 
        lCharset = Nothing, 
        lOverrideCharset = Nothing, 
        lSourceCharset = Nothing, 
        lCharsetType = Nothing, 
        lChunkedTransferEncoding = Nothing, 
        lClientBodyBufferSize = Nothing, 
        lClientBodyInFileOnly = Nothing, 
        lClientBodyInSingleBuffer = Nothing, 
        lClientBodyTempPath = Nothing, 
        lClientBodyTimeout = Nothing, 
        lClientMaxBodySize = Nothing, 
        lDefaultType = Nothing, 
        lDirectio = Nothing, 
        lDirectioAlignment = Nothing, 
        lDisableSymlinks = Nothing, 
        lEtag = Nothing, 
        lExpires = Nothing, 
        lGzip = Nothing, 
        lGzipBuffers = Nothing, 
        lGzipCompLevel = Nothing, 
        lGzipDisable = Nothing, 
        lGzipMinLength = Nothing, 
        lGzipHttpVersion = Nothing, 
        lGzipProxied = Nothing, 
        lGzipTypes = Nothing, 
        lGzipVary = Nothing, 
        lIfModifiedSince = Nothing, 
        lInclude = Nothing, 
        lInternal = Nothing, 
        lLimitConn = Nothing, 
        lLimitConnLogLevel = Nothing, 
        lLimitConnStatus = Nothing, 
        lLimitRate = Nothing, 
        lLimitRateAfter = Nothing, 
        lLimitReq = Nothing, 
        lLimitReqLogLevel = Nothing, 
        lLimitReqStatus = Nothing, 
        lLingeringClose = Nothing, 
        lLingeringTime = Nothing, 
        lLingeringTimeout = Nothing, 
        lLogNotFound = Nothing, 
        lLogSubrequest = Nothing, 
        lOpenLogFileCache = Nothing, 
        lMaxRanges = Nothing, 
        lMemcachedBind = Nothing, 
        lMemCachedBufferSize = Nothing, 
        lMemcachedConnectTimeout = Nothing, 
        lMemcachedForceRanges = Nothing, 
        lMemcachedGzipFlag = Nothing, 
        lMemcachedNextUpstream = Nothing, 
        lMemcachedNextUpstreamTimeout = Nothing, 
        lMemcachedNextUpstreamTries = Nothing, 
        lMemcachedReadTimeout = Nothing, 
        lMemcachedSendTimeout = Nothing, 
        lMemcachedPass = Nothing, 
        lMsiePadding = Nothing, 
        lMsieRefresh = Nothing, 
        lOpenFileCache = Nothing, 
        lOpenFileCacheErrors = Nothing, 
        lOpenFileCacheMinUses = Nothing, 
        lOpenFileCacheValid = Nothing, 
        lOutputBuffers = Nothing, 
        lPostponeOutput = Nothing, 
        lPortInRedirect = Nothing, 
        lReadAhead = Nothing, 
        lRecursiveErrorPages = Nothing, 
        lValidReferer = Nothing, 
        lRefererHashBucketSize = Nothing, 
        lRefererHashMaxSize = Nothing, 
        lResetTimedoutConnection = Nothing, 
        lResolver = Nothing, 
        lResolverTimeout = Nothing, 
        lSatisfy = Nothing, 
        lSendLowat = Nothing, 
        lSendTimeout = Nothing, 
        lSendFileMaxChunks = Nothing, 
        lServerNameInRedirect = Nothing, 
        lServerTokens = Nothing, 
        lTcpNodelay = Nothing, 
        lTcpNopush = Nothing, 
        lTryFiles = Nothing, 
        lTypes = Nothing, 
        lTypesHashBucketSize = Nothing, 
        lTypesHashMaxSize = Nothing 
    })
    -- conceal
    (\ _ -> return Nothing)


------------------------
--DEFAULT VALUES GESTION
------------------------
--defaults gestion for simple fields
addDefault :: MonadError' e m => String -> BiGUL m (Maybe String) String
addDefault def = CaseV [ ((return . (== def)), 
    (CaseS [ $(normal [p| Nothing |]) $ Rearr (RConst def) (EIn (ELeft (EConst ()))) Replace, 
             $(normal [p| (Just _) |]) $ $(rearr [| \ x -> (Just x) |]) Replace
           ])) ,
                         ((return . (/= def)), 
    ($(rearr [| \ x -> (Just x) |]) Replace) )
                       ]

--defaults gestion for list fields
addDefaultList :: MonadError' e m => String -> BiGUL m (Maybe [String]) [String]
addDefaultList def = CaseV [ ((return . (== [def])), 
    (CaseS [ $(normal [p| Nothing |]) $ Rearr (RConst [def]) (EIn (ELeft (EConst ()))) Replace, 
             $(normal [p| (Just _) |]) $ $(rearr [| \ x -> (Just x) |]) Replace
           ])) ,
                         ((return . (/= [def])), 
    ($(rearr [| \ x -> (Just x) |]) Replace) )
                           ]

--defaults gestion for the VerifyClient directive
verifyClientDefault :: MonadError' e m => String -> BiGUL m (Maybe String) String
verifyClientDefault def = CaseV [ ((return . (liftM2 (&&) (== def) (== "yes"))), 
    (CaseS [ $(normal [p| Nothing |]) $ Rearr (RConst def) (EIn (ELeft (EConst ()))) Replace, 
             $(normal [p| (Just _) |]) $ $(rearr [| \ "yes" -> (Just "on") |]) Replace
           ])),
                                  ((return . (liftM2 (&&) (== def) (== "no"))), 
    (CaseS [ $(normal [p| Nothing |]) $ Rearr (RConst def) (EIn (ELeft (EConst ()))) Replace, 
             $(normal [p| (Just _) |]) $ $(rearr [| \ "no" -> (Just "off") |]) Replace
           ])) ,
                                  ((return . (== def)), 
    (CaseS [ $(normal [p| Nothing |]) $ Rearr (RConst def) (EIn (ELeft (EConst ()))) Replace, 
             $(normal [p| (Just _) |]) $ $(rearr [| \ def -> (Just def) |]) Replace
           ])) ,
                                  ((return . (liftM2 (&&) (/= def) (== "yes"))), 
    ($(rearr [| \ "yes" -> (Just "on") |]) Replace) ),
                                  ((return . (liftM2 (&&) (/= def) (== "no"))), 
    ($(rearr [| \ "no" -> (Just "off") |]) Replace) ),
                                  ((return . (/= def)), 
    ($(rearr [| \ x -> (Just x) |]) Replace) )
                                ]


------------
--OPERATIONS
------------
--performs get and show extracted view in console
--does not override existing view
getNginx1 x = (catchBind (get (transNginx defaults) x) (\v -> Right (show v)) (\e -> Left e))
extractConfig = parseTreeNginx "nginx.conf" >>= \(Right tree) -> return (createSourceNginx tree) >>= return . getNginx1

--performs get and rewrites view file
extractConfigToFile = do
  content <- extractConfig
  case content of
    Left e -> putStrLn ("some error: " ++ show e)
    Right r -> writeFile "Nginx_output.hs" ("module Nginx_output where"++"\n"++"import TypeFiles.Common"++"\n"++"nginxOutput :: CommonWebserver"++"\n"++"nginxOutput = "++r)


--performs putback and show new source in console
--does not override existing source config file
putNginx1 x = catchBind (put (transNginx defaults) x nginxView') (Right . printNginx) Left
putbackConfig = parseTreeNginx "nginx.conf" >>= \(Right tree) -> return (createSourceNginx tree) >>= return . putNginx1

--performs putback and rewrites source config file
putbackConfigToFile = do
  content <- putbackConfig
  case content of
    Left e -> putStrLn ("some error: " ++ show e)
    Right r -> writeFile "nginx.conf" ((show r))


-------------------------------------
--OTHER FUNCTIONS FOR TESTING PURPOSE
-------------------------------------
testPut :: BiGUL (Either ErrorInfo) s v -> s -> v -> Either ErrorInfo s
testPut u s v = catchBind (put u s v) (\s' -> Right s') (\e -> Left e)

testGet :: BiGUL (Either ErrorInfo) s v -> s -> Either ErrorInfo v
testGet u s = catchBind (get u s) (\v' -> Right v') (\e -> Left e)

--demo function for showing source
showSource = parseTreeNginx "nginx.conf" >>= \(Right tree) -> return (createSourceNginx tree) >>= return . show


-----------------------
--OTHER ANNEX FUNCTIONS
-----------------------
--replaces an empty field by Nothing
--used when creating a new element in the source
emptyCheck :: String -> Maybe String
emptyCheck input = if (input == "") then Nothing else (Just input)

--replaces an empty list by Nothing
--used when creating a new element in the source
emptyListCheck :: [String] -> Maybe [String]
emptyListCheck input = if (input == []) then Nothing else (Just input) 


