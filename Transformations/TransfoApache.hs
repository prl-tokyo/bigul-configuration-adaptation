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
import ApachePrettyPrinter
import ApacheSourceCreator
import TreeConfigApacheFiller
----apache imports
import Apache_output
import TypeFiles.ApacheTypes
import ApacheDefaultValues
import TypeFiles.Common

--source and view records defined as BiGUL types
deriveBiGULGeneric ''ApacheWebserver
deriveBiGULGeneric ''VirtualHost
deriveBiGULGeneric ''Directory
deriveBiGULGeneric ''Location
deriveBiGULGeneric ''Files
deriveBiGULGeneric ''DirDirectives

deriveBiGULGeneric ''CommonWebserver
deriveBiGULGeneric ''VServer
deriveBiGULGeneric ''VLocation

--importing view
apacheView' :: CommonWebserver
apacheView' = apacheOutput

-----------------
--TRANSFORMATIONS
-----------------
--global transformation
transApache :: MonadError' e m => DefaultValues -> BiGUL m ApacheWebserver CommonWebserver
transApache defaults = $(rearrAndUpdate [p| CommonWebserver { 
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
 |] [p| ApacheWebserver { 
    aDocumentRoot = root, 
    aDirectoryIndex = index, 
    aEnableSendfile = sendfile, 
    aKeepAliveTimeout = kaTimeout, 
    aMaxKeepAliveRequests = kaMaxRequests, 
    aSSLEngine = ssl, 
    aSSLCACertificateFile = caCertif, 
    aSSLCARevocationFile = caRevocFile, 
    aSSLCertificateFile = certif, 
    aSSLCertificateKeyFile = certifKey, 
    aSSLCipherSuite = ciphers, 
    aSSLHonorCipherOrder = preferServCiphers, 
    aSSLProtocol = protocols, 
    aSSLVerifyClient = verifyClient, 
    aSSLVerifyDepth = verifyDepth, 
    aVirtualHosts = servers 
}
 |] [d| root = addDefault (d_DocumentRoot defaults);
        index = addDefault (d_DirectoryIndex defaults);
        kaTimeout = addDefault (d_KeepAliveTimeout defaults);
        kaMaxRequests = addDefault (d_MaxKeepAliveRequests defaults);
        sendfile = addDefault (d_EnableSendfile defaults);
        ssl = addDefault (d_SSLEngine defaults);
        caCertif = addDefault (d_SSLCACertificateFile defaults);
        caRevocFile = addDefault (d_SSLCARevocationFile defaults);
        certif = addDefault (d_SSLCertificateFile defaults);
        certifKey = addDefault (d_SSLCertificateKeyFile defaults);
        ciphers = addDefault (d_SSLCipherSuite defaults);
        preferServCiphers = addDefault (d_SSLHonorCipherOrder defaults);
        protocols = addDefault (d_SSLProtocol defaults);
        verifyClient = verifyClientDefault (d_SSLVerifyClient defaults);
        verifyDepth = addDefault (d_SSLVerifyDepth defaults)
        servers = CaseV [ 
            $(branch [p| [] |]) $ $(rearr [| \ [] -> Nothing |]) Replace, 
            $(branch [p| (_:_) |]) $ (Compose ($(rearr [| \ x -> (Just x) |]) Replace) (transServer defaults))
                        ]
         |])

--servers transformation
transServer :: MonadError' e m => DefaultValues -> BiGUL m [VirtualHost] [VServer]
transServer defaults = Align 
    --source condition
    (\ _ -> return True)
    --match
    --defines on which field the records will match between source and view
    (\ (VirtualHost {sServerName = (Just sName)}) (VServer {vServNames = vNames}) -> return ((head sName) == (head vNames)))
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
        vLocations = servLocations
    } |] [p| VirtualHost { 
        sVirtualHostAddress = listen, 
        sDocumentRoot = servRoot, 
        sEnableSendfile = servSendfile, 
        sKeepAliveTimeout = servKaTimeout, 
        sLocation = servLocations, 
        sMaxKeepAliveRequests = servKaMaxRequests, 
        sServerName = servNames, 
        sDirectoryIndex = servIndex, 
        sSSLEngine = servSSL, 
        sSSLCACertificateFile = servCaCertif, 
        sSSLCARevocationFile = servCaRevocFile, 
        sSSLCertificateFile = servCertif, 
        sSSLCertificateKeyFile = servCertifKey,  
        sSSLCipherSuite = servCiphers, 
        sSSLHonorCipherOrder = servPreferServCiphers, 
        sSSLProtocol = servProtocols, 
        sSSLVerifyClient = servVerifyClient, 
        sSSLVerifyDepth = servVerifyDepth
    } |] [d| servRoot = addDefault (d_DocumentRoot defaults);
             servIndex = addDefault (d_DirectoryIndex defaults);
             servKaTimeout = addDefault (d_KeepAliveTimeout defaults);
             servKaMaxRequests = addDefault (d_MaxKeepAliveRequests defaults);
             servSendfile = addDefault (d_EnableSendfile defaults);
             servNames = addDefaultList (d_ServerName defaults);
             listen = addDefaultList (d_Listen defaults);
             servSSL = addDefault (d_SSLEngine defaults);
             servCaCertif = addDefault (d_SSLCACertificateFile defaults);
             servCaRevocFile = addDefault (d_SSLCARevocationFile defaults);
             servCertif = addDefault (d_SSLCertificateFile defaults);
             servCertifKey = addDefault (d_SSLCertificateKeyFile defaults);
             servCiphers = addDefault (d_SSLCipherSuite defaults);
             servPreferServCiphers = addDefault (d_SSLHonorCipherOrder defaults);
             servProtocols = addDefault (d_SSLProtocol defaults);
             servVerifyClient = verifyClientDefault (d_SSLVerifyClient defaults);
             servVerifyDepth = addDefault (d_SSLVerifyDepth defaults);
             servLocations = CaseV [
                $(branch [p| [] |]) $ $(rearr [| \ [] -> Nothing |]) Replace,
                $(branch [p| (_:_) |]) $ (Compose ($(rearr [| \ x -> (Just x) |]) Replace) (transLocation defaults))
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
    } -> return VirtualHost { 
        sVirtualHostAddress = emptyListCheck listen, 
        sDirectory = Nothing, 
        sDocumentRoot = emptyCheck servRoot, 
        sEnableSendfile = emptyCheck servSendfile, 
        sErrorDocument = Nothing, 
        sErrorLog = Nothing,  
        sFiles = Nothing, 
        sKeepAlive = Nothing, 
        sKeepAliveTimeout = emptyCheck servKaTimeout, 
        sLocation = Nothing, 
        sMaxKeepAliveRequests = emptyCheck servKaMaxRequests, 
        sServerName = emptyListCheck servNames, 
        sDirectoryIndex = emptyCheck servIndex, 
        sCustomLog = Nothing, 
        sSSLEngine = emptyCheck servSSL, 
        sSSLCACertificateFile = emptyCheck servCaCertif, 
        sSSLCARevocationFile = emptyCheck servCaRevocFile, 
        sSSLCertificateFile = emptyCheck servCertif, 
        sSSLCertificateKeyFile = emptyCheck servCertifKey,  
        sSSLCipherSuite = emptyCheck servCiphers, 
        sSSLHonorCipherOrder = emptyCheck servPreferServCiphers, 
        sSSLProtocol = emptyCheck servProtocols, 
        sSSLSessionCacheTimeout = Nothing, 
        sSSLVerifyClient = emptyCheck servVerifyClient, 
        sSSLVerifyDepth = emptyCheck servVerifyDepth, 
        sAcceptPathInfo = Nothing, 
        sAccessFileName = Nothing, 
        sAddDefaultCharset = Nothing, 
        sAllowEncodedSlashes = Nothing, 
        sContentDigest = Nothing, 
        sDefine = Nothing, 
        sEnableMMAP = Nothing, 
        sError = Nothing, 
        sErrorLogFormat = Nothing, 
        sFileETag = Nothing, 
        sHostnameLookups = Nothing, 
        sInclude = Nothing, 
        sIncludeOptional = Nothing, 
        sLimitInternalRecursion = Nothing, 
        sLimitRequestBody = Nothing, 
        sLimitRequestFields = Nothing, 
        sLimitRequestFieldSize = Nothing, 
        sLimitRequestLine = Nothing, 
        sLimitXMLRequestBody = Nothing, 
        sLogLevel = Nothing, 
        sMaxRangesOverlaps = Nothing, 
        sMaxRangesReversals = Nothing, 
        sMaxRanges = Nothing, 
        sOptions = Nothing, 
        sProtocol = Nothing, 
        sRLimitCPU = Nothing, 
        sRLimitMEM = Nothing, 
        sRLimitNPROC = Nothing, 
        sServerAdmin = Nothing, 
        sServerPath = Nothing, 
        sServerSignature = Nothing, 
        sSetHandler = Nothing, 
        sSetInputFilter = Nothing, 
        sSetOutputFilter = Nothing, 
        sTimeOut = Nothing, 
        sTraceEnable = Nothing, 
        sUseCanonicalName = Nothing, 
        sUseCanonicalPhysicalPort = Nothing, 
        sAlias = Nothing, 
        sAliasMatch = Nothing, 
        sRedirect = Nothing, 
        sRedirectMatch = Nothing, 
        sRedirectPermanent = Nothing, 
        sRedirectTemp = Nothing, 
        sScriptAlias = Nothing, 
        sScriptAliasMatch = Nothing, 
        sAddAlt = Nothing, 
        sAddAltByEncoding = Nothing, 
        sAddAltByType = Nothing, 
        sAddDescription = Nothing, 
        sAddIcon = Nothing, 
        sAddIconByEncoding = Nothing, 
        sAddIconByType = Nothing, 
        sDefaultIcon = Nothing, 
        sHeaderName = Nothing, 
        sIndexHeadInsert = Nothing, 
        sIndexIgnore = Nothing, 
        sIndexIgnoreReset = Nothing, 
        sIndexOptions = Nothing, 
        sIndexOrderDefault = Nothing, 
        sIndexStyleSheet = Nothing, 
        sReadmeName = Nothing, 
        sScriptLog = Nothing, 
        sScriptLogBuffer = Nothing, 
        sScriptLogLength = Nothing, 
        sCGIDScriptTimeout = Nothing, 
        sDirectoryCheckHandler = Nothing, 
        sIndexRedirect = Nothing, 
        sDirectorySlash = Nothing, 
        sFallbackResource = Nothing, 
        sPassEnv = Nothing, 
        sSetEnv = Nothing, 
        sUnsetEnv = Nothing, 
        sAddOutputFilterByType = Nothing, 
        sFilterChain = Nothing, 
        sFilterDeclare = Nothing, 
        sFilterProtocol = Nothing, 
        sFilterProvider = Nothing, 
        sFilterTrace = Nothing, 
        sImapBase = Nothing, 
        sImapDefault = Nothing, 
        sImapMenu = Nothing, 
        sLogFormat = Nothing, 
        sTransferLog = Nothing, 
        sAddCharset = Nothing, 
        sAddEncoding = Nothing, 
        sAddHandler = Nothing, 
        sAddInputFilter = Nothing, 
        sAddLanguage = Nothing, 
        sAddOutputFilter = Nothing, 
        sAddType = Nothing, 
        sDefaultLanguage = Nothing, 
        sMultiviewsMatch = Nothing, 
        sRemoveCharset = Nothing, 
        sRemoveEncoding = Nothing, 
        sRemoveHandler = Nothing, 
        sRemoveInputFilter = Nothing, 
        sRemoveLanguage = Nothing, 
        sRemoveOutputFilter = Nothing, 
        sRemoveType = Nothing, 
        sCacheNegotiatedDocs = Nothing, 
        sForceLanguagePriority = Nothing, 
        sLanguagePriority = Nothing, 
        sReflectorHeader = Nothing, 
        sBrowserMatch = Nothing, 
        sBrowserMatchNoCase = Nothing, 
        sSetEnvIf = Nothing, 
        sSetEnvIfExpr = Nothing, 
        sSetEnvIfNoCase = Nothing, 
        sUserDir = Nothing, 
        sSSLCACertificatePath = Nothing, 
        sSSLCADNRequestFile = Nothing, 
        sSSLCADNRequestPath = Nothing, 
        sSSLCARevocationCheck = Nothing, 
        sSSLCARevocationPath = Nothing, 
        sSSLCertificateChainFile = Nothing, 
        sSSLCompression = Nothing, 
        sSSLInsecureRenegotiation = Nothing, 
        sSSLOCSPDefaultResponder = Nothing, 
        sSSLOCSPEnable = Nothing, 
        sSSLOCSPOverrideResponder = Nothing, 
        sSSLOCSPResponderTimeout = Nothing, 
        sSSLOCSPResponseMaxAge = Nothing, 
        sSSLOCSPResponseTimeSkew = Nothing, 
        sSSLOCSPUseRequestNonce = Nothing, 
        sSSLOpenSSLConfCmd = Nothing, 
        sSSLOptions = Nothing, 
        sSSLSessionTicketKeyFile = Nothing, 
        sSSLSessionTickets = Nothing, 
        sSSLSRPUnknownUserSeed = Nothing, 
        sSSLSRPVerifierFile = Nothing, 
        sSSLStaplingErrorCacheTimeout = Nothing, 
        sSSLStaplingFakeTryLater = Nothing, 
        sSSLStaplingForceURL = Nothing, 
        sSSLStaplingResponderTimeout = Nothing, 
        sSSLStaplingResponseMaxAge = Nothing, 
        sSSLStaplingResponseTimeSkew = Nothing, 
        sSSLStaplingReturnResponderErrors = Nothing, 
        sSSLStaplingStandardCacheTimeout = Nothing, 
        sSSLStrictSNIVHostCheck = Nothing, 
        sSSLUseStapling = Nothing
    })
    --conceal
    (\ _ -> return Nothing)


--locations transformation
transLocation :: MonadError' e m => DefaultValues -> BiGUL m [Location] [VLocation]
transLocation defaults = Align
    --source condition
    (\ _ -> return True)
    --match
    --defines on which field the records will match between source and view
    (\ (Location { lPath = (Just sPath) } ) (VLocation { vLocationPath = vPath } ) -> return (sPath == vPath))
    --trans
    ($(rearrAndUpdate [p| VLocation {
        vLocationPath = locPath, 
        vLocIndex = locIndex, 
        vLocSendfile = locSendfile
    } |] [p| Location {
        lPath = locPath, 
        lDirDirectives = Just DirDirectives {
            dDirectoryIndex = locIndex, 
            dEnableSendFile = locSendfile
        }
    } |] [d| locPath = $(rearr [| \ x -> (Just x) |]) Replace;
             locIndex = addDefault (d_DirectoryIndex defaults);
             locSendfile = addDefault (d_EnableSendfile defaults)
    |] ))
    --create
    --adds a new location to the source if a new one was added to the view
    (\ VLocation { 
        vLocationPath = locPath, 
        vLocIndex = locIndex, 
        vLocSendfile = locSendfile
    } -> return Location {
        lMatch = (Just False), 
        lPath = (Just locPath), 
        lDirDirectives = Just DirDirectives {
            dAcceptPathInfo = Nothing, 
            dAddDefaultCharset = Nothing, 
            dContentDigest = Nothing, 
            dDefine = Nothing, 
            dEnableMMAP = Nothing, 
            dEnableSendFile = emptyCheck locSendfile, 
            dError = Nothing, 
            dErrorDocument = Nothing, 
            dFileETag = Nothing, 
            dForceType = Nothing, 
            dHostnameLookups = Nothing, 
            dInclude = Nothing, 
            dIncludeOptional = Nothing, 
            dLimitRequestBody = Nothing, 
            dLimitXMLRequestBody = Nothing, 
            dLogLevel = Nothing, 
            dMaxRangeOverlaps = Nothing, 
            dMaxRangeReversals = Nothing, 
            dMaxRanges = Nothing, 
            dRLimitCPU = Nothing, 
            dRLimitMEM = Nothing, 
            dRLimitNPROC = Nothing, 
            dServerSignature = Nothing, 
            dSetHandler = Nothing, 
            dSetInputFilter = Nothing, 
            dSetOutputFilter = Nothing, 
            dUseCanonicalName = Nothing, 
            dUseCanonicalPhysicalPort = Nothing, 
            dRedirect = Nothing, 
            dRedirectMatch = Nothing, 
            dRedirectPermanent = Nothing, 
            dRedirectTemp = Nothing, 
            dAuthBasicAuthoritative = Nothing, 
            dAuthBasicFake = Nothing, 
            dAuthBasicProvider = Nothing, 
            dAuthBasicUseDigestAlgorithm = Nothing, 
            dAuthName = Nothing, 
            dAuthType = Nothing, 
            dAuthUserFile = Nothing, 
            dAuthMerging = Nothing, 
            dAuthzSendForbiddenOnFailure = Nothing, 
            dRequire = Nothing, 
            dAuthGroupFile = Nothing, 
            dAddAlt = Nothing, 
            dAddAltByEncoding = Nothing, 
            dAddAltByType = Nothing, 
            dAddDescription = Nothing, 
            dAddIcon = Nothing, 
            dAddIconByEncoding = Nothing, 
            dAddIconByType = Nothing, 
            dDefaultIcon = Nothing, 
            dHeaderName = Nothing, 
            dIndexHeadInsert = Nothing, 
            dIndexIgnore = Nothing, 
            dIndexIgnoreReset = Nothing, 
            dIndexOptions = Nothing, 
            dIndexOrderDefault = Nothing, 
            dIndexStyleSheet = Nothing, 
            dReadmeName = Nothing, 
            dCGIDScriptTimeout = Nothing, 
            dDirectoryCheckHandler = Nothing, 
            dDirectoryIndex = emptyCheck locIndex, 
            dIndexRedirect = Nothing, 
            dDirectorySlash = Nothing, 
            dFallbackResource = Nothing,
            dPassEnv = Nothing, 
            dSetEnv = Nothing, 
            dUnsetEnv = Nothing, 
            dAddOutputFilterByType = Nothing, 
            dFilterChain = Nothing, 
            dFilterDeclare = Nothing, 
            dFilterProtocol = Nothing, 
            dFilterProvider = Nothing, 
            dFilterTrace = Nothing, 
            dImapBase = Nothing, 
            dImapDefault = Nothing, 
            dImapMenu = Nothing, 
            dCustomLog = Nothing, 
            dLogFormat = Nothing, 
            dTransferLog = Nothing, 
            dAddCharset = Nothing, 
            dAddEncoding = Nothing, 
            dAddHandler = Nothing, 
            dAddInputFilter = Nothing, 
            dAddLanguage = Nothing, 
            dAddOutputFilter = Nothing, 
            dAddType = Nothing, 
            dDefaultLanguage = Nothing, 
            dModMimeUsePathInfo = Nothing, 
            dMultiviewsMatch = Nothing, 
            dRemoveCharset = Nothing, 
            dRemoveEncoding = Nothing, 
            dRemoveHandler = Nothing, 
            dRemoveInputFilter = Nothing, 
            dRemoveLanguage = Nothing, 
            dRemoveOutputFilter = Nothing, 
            dRemoveType = Nothing, 
            dForceLanguagePriority = Nothing, 
            dLanguagePriority = Nothing, 
            dReflectorHeader = Nothing, 
            dKeptBodySize = Nothing, 
            dBrowserMatch = Nothing, 
            dBrowserMatchNoCase = Nothing, 
            dSetEnvIf = Nothing, 
            dSetEnvIfExpr = Nothing, 
            dSetEnvIfNoCase = Nothing, 
            dSSLCipherSuite = Nothing, 
            dSSLOptions = Nothing, 
            dSSLRenegBufferSize = Nothing, 
            dSSLRequireSSL = Nothing, 
            dSSLUserName = Nothing, 
            dSSLVerifyClient = Nothing, 
            dSSLVerifyDepth = Nothing
        }, 
        lRequireCons = Nothing, 
        lOptions = Nothing 
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
             $(normal [p| (Just _) |]) $ $(rearr [| \ "yes" -> (Just "require") |]) Replace
           ])),
                                  ((return . (liftM2 (&&) (== def) (== "no"))), 
    (CaseS [ $(normal [p| Nothing |]) $ Rearr (RConst def) (EIn (ELeft (EConst ()))) Replace, 
             $(normal [p| (Just _) |]) $ $(rearr [| \ "no" -> (Just "none") |]) Replace
           ])) ,
                                  ((return . (== def)), 
    (CaseS [ $(normal [p| Nothing |]) $ Rearr (RConst def) (EIn (ELeft (EConst ()))) Replace, 
             $(normal [p| (Just _) |]) $ $(rearr [| \ def -> (Just def) |]) Replace
           ])) ,
                                  ((return . (liftM2 (&&) (/= def) (== "yes"))), 
    ($(rearr [| \ "yes" -> (Just "require") |]) Replace) ),
                                  ((return . (liftM2 (&&) (/= def) (== "no"))), 
    ($(rearr [| \ "no" -> (Just "none") |]) Replace) ),
                                  ((return . (/= def)), 
    ($(rearr [| \ x -> (Just x) |]) Replace) )
                                ]

------------
--OPERATIONS
------------
--performs get and show extracted view in console
--does not override existing view
getApache1 x = (catchBind (get (transApache defaults) x) (\v -> Right (show v)) (\e -> Left e))
extractConfig = parseTreeApache "apache.conf" >>= \(Right tree) -> return (createSourceApache tree) >>= return . getApache1

--performs get and rewrites view file
extractConfigToFile = do
  content <- extractConfig
  case content of
    Left e -> putStrLn ("some error: " ++ show e)
    Right r -> writeFile "Apache_output.hs" ("module Apache_output where"++"\n"++"import TypeFiles.Common"++"\n"++"apacheOutput :: CommonWebserver"++"\n"++"apacheOutput = "++r)


--performs putback and show new source in console
--does not override existing source config file
putApache1 x = catchBind (put (transApache defaults) x apacheView') (Right . printApache) Left
putbackConfig = parseTreeApache "apache.conf" >>= \(Right tree) -> return (createSourceApache tree) >>= return . putApache1

--performs putback and rewrites source config file
putbackConfigToFile = do
  content <- putbackConfig
  case content of
    Left e -> putStrLn ("some error: " ++ show e)
    Right r -> writeFile "apache.conf" ((show r))


-------------------------------------
--OTHER FUNCTIONS FOR TESTING PURPOSE
-------------------------------------
testPut :: BiGUL (Either ErrorInfo) s v -> s -> v -> Either ErrorInfo s
testPut u s v = catchBind (put u s v) (\s' -> Right s') (\e -> Left e)

testGet :: BiGUL (Either ErrorInfo) s v -> s -> Either ErrorInfo v
testGet u s = catchBind (get u s) (\v' -> Right v') (\e -> Left e)

--demo function for showing source
showSource = parseTreeApache "apache.conf" >>= \(Right tree) -> return (createSourceApache tree) >>= return . show

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


