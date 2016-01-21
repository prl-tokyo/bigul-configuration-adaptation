{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric  #-}
--Test transfo for webserver source--

--imports
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

import TypeFiles.Common
import ApacheDefaultValues

import Transfo_Apache_test_input

deriveBiGULGeneric ''ApacheWebserver
deriveBiGULGeneric ''VirtualHost
deriveBiGULGeneric ''Directory
deriveBiGULGeneric ''Location
deriveBiGULGeneric ''Files
deriveBiGULGeneric ''DirDirectives

deriveBiGULGeneric ''CommonWebserver
deriveBiGULGeneric ''VServer
deriveBiGULGeneric ''VLocation

--sample view for testing
apacheView' :: CommonWebserver
apacheView' = apacheOutput

apacheSource :: ApacheWebserver
apacheSource = apacheTestInput


--TRANSFORMATIONS
--global transformation
transApache :: MonadError' e m => DefaultValues -> BiGUL m ApacheWebserver CommonWebserver
transApache defaults = $(rearrAndUpdate [p| CommonWebserver { 
    {-serving static content-}
    vRoot = root, 
    vIndex = index, 
    {-client connections-}
    vKeepaliveTimeout = kaTimeout, 
    vKeepaliveMaxRequests = kaMaxRequests, 
    {-speed/quality-}
    vSendfile = sendfile, 
    {-ssl-}
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
    {-ssl-}
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
    {-servers-}
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
    (\ (VirtualHost {sServerName = (Just sName)}) (VServer {vServNames = vNames}) -> return ((head sName) == (head vNames)))
    --b
    ($(rearrAndUpdate [p| VServer {
        vListen = listen, 
        vServNames = servNames, 
        {-serving static content-}
        vServRoot = servRoot, 
        vServIndex = servIndex, 
        {-client connections-}
        vServKeepaliveTimeout = servKaTimeout, 
        vServKeepaliveMaxRequests = servKaMaxRequests, 
        {-speed/quality-}
        vServSendfile = servSendfile, 
        {-ssl-}
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
        --sDirectory = Nothing, 
        sDocumentRoot = servRoot, 
        sEnableSendfile = servSendfile, 
        --sFiles = Nothing, 
        sKeepAliveTimeout = servKaTimeout, 
        sLocation = servLocations, 
        sMaxKeepAliveRequests = servKaMaxRequests, 
        sServerName = servNames, 
        sDirectoryIndex = servIndex, 
        {-ssl-}
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
    (\ VServer {
        vListen = listen, 
        vServNames = servNames, 
        {-serving static content-}
        vServRoot = servRoot, 
        vServIndex = servIndex, 
        {-client connections-}
        vServKeepaliveTimeout = servKaTimeout, 
        vServKeepaliveMaxRequests = servKaMaxRequests, 
        {-speed/quality-}
        vServSendfile = servSendfile, 
        {-ssl-}
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
        {-ssl-}        
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
        {-alias-}
        sAlias = Nothing, 
        sAliasMatch = Nothing, 
        sRedirect = Nothing, 
        sRedirectMatch = Nothing, 
        sRedirectPermanent = Nothing, 
        sRedirectTemp = Nothing, 
        sScriptAlias = Nothing, 
        sScriptAliasMatch = Nothing, 
        {-autoindex-}
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
        {-cgi mod-}
        sScriptLog = Nothing, 
        sScriptLogBuffer = Nothing, 
        sScriptLogLength = Nothing, 
        {-cgid-}
        sCGIDScriptTimeout = Nothing, 
        {-dir-}
        sDirectoryCheckHandler = Nothing, 
        sIndexRedirect = Nothing, 
        sDirectorySlash = Nothing, 
        sFallbackResource = Nothing, 
        {-env-}
        sPassEnv = Nothing, 
        sSetEnv = Nothing, 
        sUnsetEnv = Nothing, 
        {-filter-}
        sAddOutputFilterByType = Nothing, 
        sFilterChain = Nothing, 
        sFilterDeclare = Nothing, 
        sFilterProtocol = Nothing, 
        sFilterProvider = Nothing, 
        sFilterTrace = Nothing, 
        {-imap-}
        sImapBase = Nothing, 
        sImapDefault = Nothing, 
        sImapMenu = Nothing, 
        {-logconf-}
        sLogFormat = Nothing, 
        sTransferLog = Nothing, 
        {-mime-}
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
        {-nego-}
        sCacheNegotiatedDocs = Nothing, 
        sForceLanguagePriority = Nothing, 
        sLanguagePriority = Nothing, 
        {-reflector-}
        sReflectorHeader = Nothing, 
        {-setenvif-}
        sBrowserMatch = Nothing, 
        sBrowserMatchNoCase = Nothing, 
        sSetEnvIf = Nothing, 
        sSetEnvIfExpr = Nothing, 
        sSetEnvIfNoCase = Nothing, 
        {-userdir-}
        sUserDir = Nothing, 
        {-ssl-}
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
    -- source condition
    (\ _ -> return True)
    -- match
    (\ (Location { lPath = (Just sPath) } ) (VLocation { vLocationPath = vPath } ) -> return (sPath == vPath))
    -- b
    ($(rearrAndUpdate [p| VLocation {
        vLocationPath = locPath, 
        {-serving static content-}
        vLocIndex = locIndex, 
        {-client connections-}
        {-speed/quality-}
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
    -- create
    (\ VLocation { 
        vLocationPath = locPath, 
        {-serving static content-}
        vLocIndex = locIndex, 
        {-client connections-} 
        {-speed/quality-}
        vLocSendfile = locSendfile
    } -> return Location {
        {-identifiers-}
        lMatch = (Just False), 
        lPath = (Just locPath), 
        {-directives-}
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


--DEFAULT VALUES GESTION
addDefault :: MonadError' e m => String -> BiGUL m (Maybe String) String
addDefault def = CaseV [ ((return . (== def)), 
    (CaseS [ $(normal [p| Nothing |]) $ Rearr (RConst def) (EIn (ELeft (EConst ()))) Replace, 
             $(normal [p| (Just _) |]) $ $(rearr [| \ x -> (Just x) |]) Replace
           ])) ,
                         ((return . (/= def)), 
    ($(rearr [| \ x -> (Just x) |]) Replace) )
                       ]

addDefaultList :: MonadError' e m => String -> BiGUL m (Maybe [String]) [String]
addDefaultList def = CaseV [ ((return . (== [def])), 
    (CaseS [ $(normal [p| Nothing |]) $ Rearr (RConst [def]) (EIn (ELeft (EConst ()))) Replace, 
             $(normal [p| (Just _) |]) $ $(rearr [| \ x -> (Just x) |]) Replace
           ])) ,
                         ((return . (/= [def])), 
    ($(rearr [| \ x -> (Just x) |]) Replace) )
                           ]

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


--functions for testing purpose
testPut :: BiGUL (Either ErrorInfo) s v -> s -> v -> Either ErrorInfo s
testPut u s v = catchBind (put u s v) (\s' -> Right s') (\e -> Left e)

testGet :: BiGUL (Either ErrorInfo) s v -> s -> Either ErrorInfo v
testGet u s = catchBind (get u s) (\v' -> Right v') (\e -> Left e)


--OPERATIONS
--extract configuration from file to view
getApache1 x = (catchBind (get (transApache defaults) x) (\v -> Right (show v)) (\e -> Left e))
extractConfig = parseTreeApache "apache.conf" >>= \(Right tree) -> return (createSourceApache tree) >>= return . getApache1

extractConfigToFile = do
  content <- extractConfig
  case content of
    Left e -> putStrLn ("some error: " ++ show e)
    Right r -> writeFile "Apache_output.hs" ("module Apache_output where"++"\n"++"import TypeFiles.Common"++"\n"++"apacheOutput :: CommonWebserver"++"\n"++"apacheOutput = "++r)


--put view back in source and print
putApache1 x = catchBind (put (transApache defaults) x apacheView') (Right . printApache) Left
putbackConfig = parseTreeApache "apache.conf" >>= \(Right tree) -> return (createSourceApache tree) >>= return . putApache1

putbackConfigToFile = do
  content <- putbackConfig
  case content of
    Left e -> putStrLn ("some error: " ++ show e)
    Right r -> writeFile "apache.conf" ((show r))


--demo function for showing source
showSource = parseTreeApache "apache.conf" >>= \(Right tree) -> return (createSourceApache tree) >>= return . show

putbackNoPrint = catchBind (put (transApache defaults) apacheSource apacheView') (Right . show) Left

--OTHER ANNEX FUNCTIONS
emptyCheck :: String -> Maybe String
emptyCheck input = if (input == "") then Nothing else (Just input)

emptyListCheck :: [String] -> Maybe [String]
emptyListCheck input = if (input == []) then Nothing else (Just input) 


