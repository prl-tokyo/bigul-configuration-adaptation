--structure tree for apache webserver 2.4.10
--only core, mpm prefork and built-in modules
module TypeFiles.ApacheTypes where

--contexts
data ApacheWebserver = ApacheWebserver { 
    {-core-}
    aAcceptFilter :: Maybe [AcceptFilter], 
    aAcceptPathInfo :: Maybe AcceptPathInfo, 
    aAccessFileName :: Maybe AccessFileName, 
    aAddDefaultCharset :: Maybe AddDefaultCharset, 
    aAllowEncodedSlashes :: Maybe AllowEncodedSlashes, 
    aContentDigest :: Maybe ContentDigest, 
    aDefaultRuntimeDir :: Maybe DefaultRuntimeDir, 
    aDefine :: Maybe [Define], 
    aDirectory :: Maybe [Directory], 
    aDocumentRoot :: Maybe DocumentRoot, 
    aEnableMMAP :: Maybe EnableMMAP, 
    aEnableSendfile :: Maybe EnableSendfile, 
    aError :: Maybe Error, 
    aErrorDocument :: Maybe [ErrorDocument], 
    aErrorLog :: Maybe ErrorLog, 
    aErrorLogFormat :: Maybe ErrorLogFormat, 
    aExtendedStatus :: Maybe ExtendedStatus, 
    aFileEtag :: Maybe FileETag, 
    aFiles :: Maybe [Files], 
    aHostnameLookups :: Maybe HostnameLookups, 
    aInclude :: Maybe [Include], 
    aIncludeOptional :: Maybe [IncludeOptional], 
    aKeepAlive :: Maybe KeepAlive, 
    aKeepAliveTimeout :: Maybe KeepAliveTimeout, 
    aLimitInternalRecursion :: Maybe LimitInternalRecursion, 
    aLimitRequestBody :: Maybe LimitRequestBody, 
    aLimitRequestFields :: Maybe LimitRequestFields, 
    aLimitRequestFieldSize :: Maybe LimitRequestFieldSize, 
    aLimitRequestLine :: Maybe LimitRequestLine, 
    aLimitXMLRequestBody :: Maybe LimitXMLRequestBody, 
    aLocation :: Maybe [Location], 
    aLogLevel :: Maybe LogLevel, 
    aMaxKeepAliveRequests :: Maybe MaxKeepAliveRequests, 
    aMaxRangeOverlaps :: Maybe MaxRangeOverlaps, 
    aMaxRangeReversals :: Maybe MaxRangeReversals, 
    aMaxRanges :: Maybe MaxRanges, 
    aMutex :: Maybe [Mutex], 
    aOptions :: Maybe Options, 
    aProtocol :: Maybe Protocol, 
    aRLimitCPU :: Maybe RLimitCPU, 
    aRLimitMEM :: Maybe RLimitMEM, 
    aRLimitNPROC :: Maybe RLimitNPROC, 
    aSeeRequestTail :: Maybe SeeRequestTail, 
    aServerAdmin :: Maybe ServerAdmin, 
    aServerName :: Maybe ServerName, 
    aServerRoot :: Maybe ServerRoot, 
    aServerSignature :: Maybe ServerSignature, 
    aServerTokens :: Maybe ServerTokens, 
    aSetHandler :: Maybe SetHandler, 
    aSetInputFilter :: Maybe SetInputFilter, 
    aSetOutputFilter :: Maybe SetOutputFilter, 
    aTimeOut :: Maybe TimeOut, 
    aTraceEnable :: Maybe TraceEnable, 
    aUnDefine :: Maybe [UnDefine], 
    aUseCanonicalName :: Maybe UseCanonicalName, 
    aUseCanonicalPhysicalPort :: Maybe UseCanonicalPhysicalPort, 
    {-mpm-}
    aGracefulShutdownTimeout :: Maybe GracefulShutdownTimeout, 
    aListen :: Maybe [Listen], 
    aListenBackLog :: Maybe ListenBackLog, 
    aMaxConnectionsPerChild :: Maybe MaxConnectionsPerChild, 
    aMaxMemFree :: Maybe MaxMemFree, 
    aMaxRequestWorkers :: Maybe MaxRequestWorkers, 
    aPidFile :: Maybe PidFile, 
    aReceiveBufferSize :: Maybe ReceiveBufferSize, 
    aScoreBoardFile :: Maybe ScoreBoardFile, 
    aSendBufferSize :: Maybe SendBufferSize, 
    aServerLimit :: Maybe ServerLimit, 
    aStartServers :: Maybe StartServers, 
    aMaxSpareServers :: Maybe MaxSpareServers, 
    aMinSpareServers :: Maybe MinSpareServers, 
    {-alias-}
    aAlias :: Maybe [Alias], 
    aAliasMatch :: Maybe [AliasMatch], 
    aRedirect :: Maybe [Redirect], 
    aRedirectMatch :: Maybe [RedirectMatch], 
    aRedirectPermanent :: Maybe [RedirectPermanent], 
    aRedirectTemp :: Maybe [RedirectTemp], 
    aScriptAlias :: Maybe [ScriptAlias], 
    aScriptAliasMatch :: Maybe [ScriptAliasMatch], 
    {-autoindex-}
    aAddAlt :: Maybe [AddAlt], 
    aAddAltByEncoding :: Maybe [AddAltByEncoding], 
    aAddAltByType :: Maybe [AddAltByType], 
    aAddDescription :: Maybe [AddDescription], 
    aAddIcon :: Maybe [AddIcon], 
    aAddIconByEncoding :: Maybe [AddIconByEncoding], 
    aAddIconByType :: Maybe [AddIconByType], 
    aDefaultIcon :: Maybe DefaultIcon, 
    aHeaderName :: Maybe HeaderName, 
    aIndexHeadInsert :: Maybe IndexHeadInsert, 
    aIndexIgnore :: Maybe [IndexIgnore], 
    aIndexIgnoreReset :: Maybe IndexIgnoreReset, 
    aIndexOptions :: Maybe [IndexOptions], 
    aIndexOrderDefault :: Maybe IndexOrderDefault, 
    aIndexStyleSheet :: Maybe IndexStyleSheet, 
    aReadmeName :: Maybe ReadmeName, 
    {-cgi-}
    aScriptLog :: Maybe ScriptLog, 
    aScriptLogBuffer :: Maybe ScriptLogBuffer, 
    aScriptLogLength :: Maybe ScriptLogLength, 
    {-cgid-}
    aScriptSock :: Maybe ScriptSock, 
    aCGIDScriptTimeout :: Maybe CGIDScriptTimeout, 
    {-dir-}
    aDirectoryCheckHandler :: Maybe DirectoryCheckHandler, 
    aDirectoryIndex :: Maybe DirectoryIndex, 
    aIndexRedirect :: Maybe IndexRedirect, 
    aDirectorySlash :: Maybe DirectorySlash, 
    aFallbackResource :: Maybe FallbackResource, 
    {-env-}
    aPassEnv :: Maybe PassEnv, 
    aSetEnv :: Maybe [SetEnv], 
    aUnsetEnv :: Maybe [UnsetEnv], 
    {-filter-}
    aAddOutputFilterByType :: Maybe [AddOutputFilterByType], 
    aFilterChain :: Maybe [FilterChain], 
    aFilterDeclare :: Maybe [FilterDeclare], 
    aFilterProtocol :: Maybe [FilterProtocol], 
    aFilterProvider :: Maybe [FilterProvider], 
    aFilterTrace :: Maybe [FilterTrace], 
    {-imap-}
    aImapBase :: Maybe ImapBase, 
    aImapDefault :: Maybe ImapDefault, 
    aImapMenu :: Maybe ImapMenu, 
    {-logconf-}
    aBufferedLogs :: Maybe BufferedLogs, 
    aCustomLog :: Maybe [CustomLog], 
    aLogFormat :: Maybe [LogFormat], 
    aTransferLog :: Maybe [TransferLog], 
    {-mime-}
    aAddCharset :: Maybe [AddCharset], 
    aAddEncoding :: Maybe [AddEncoding], 
    aAddHandler :: Maybe [AddHandler], 
    aAddInputFilter :: Maybe [AddInputFilter], 
    aAddLanguage :: Maybe [AddLanguage], 
    aAddOutputFilter :: Maybe [AddOutputFilter], 
    aAddType :: Maybe [AddType], 
    aDefaultLanguage :: Maybe DefaultLanguage, 
    aMultiviewsMatch :: Maybe MultiviewsMatch, 
    aTypesConfig :: Maybe TypesConfig, 
    {-nego-}
    aCacheNegotiatedDocs :: Maybe CacheNegotiatedDocs, 
    aForceLanguagePriority :: Maybe ForceLanguagePriority, 
    aLanguagePriority :: Maybe LanguagePriority, 
    {-reflector-}
    aReflectorHeader :: Maybe ReflectorHeader, 
    {-setenvif-}
    aBrowserMatch :: Maybe [BrowserMatch], 
    aBrowserMatchNoCase :: Maybe [BrowserMatchNoCase], 
    aSetEnvIf :: Maybe [SetEnvIf], 
    aSetEnvIfExpr :: Maybe [SetEnvIfExpr], 
    aSetEnvIfNoCase :: Maybe [SetEnvIfNoCase], 
    {-unixd-}
    aChrootDir :: Maybe ChrootDir, 
    aGroup :: Maybe Group, 
    aSuexec :: Maybe Suexec, 
    aUser :: Maybe User, 
    {-userdir-}
    aUserDir :: Maybe UserDir, 
    {-ssl-}
    aSSLCACertificateFile :: Maybe SSLCACertificateFile, 
    aSSLCACertificatePath :: Maybe SSLCACertificatePath, 
    aSSLCADNRequestFile :: Maybe SSLCADNRequestFile, 
    aSSLCADNRequestPath :: Maybe SSLCADNRequestPath, 
    aSSLCARevocationCheck :: Maybe SSLCARevocationCheck, 
    aSSLCARevocationFile :: Maybe SSLCARevocationFile, 
    aSSLCARevocationPath :: Maybe SSLCARevocationPath, 
    aSSLCertificateChainFile :: Maybe SSLCertificateChainFile, 
    aSSLCertificateFile :: Maybe SSLCertificateFile, 
    aSSLCertificateKeyFile :: Maybe SSLCertificateKeyFile, 
    aSSLCipherSuite :: Maybe SSLCipherSuite, 
    aSSLCompression :: Maybe SSLCompression, 
    aSSLEngine :: Maybe SSLEngine, 
    aSSLHonorCipherOrder :: Maybe SSLHonorCipherOrder, 
    aSSLInsecureRenegotiation :: Maybe SSLInsecureRenegotiation, 
    aSSLOCSPDefaultResponder :: Maybe SSLOCSPDefaultResponder, 
    aSSLOCSPEnable :: Maybe SSLOCSPEnable, 
    aSSLOCSPOverrideResponder :: Maybe SSLOCSPOverrideResponder, 
    aSSLOCSPResponderTimeout :: Maybe SSLOCSPResponderTimeout, 
    aSSLOCSPResponseMaxAge :: Maybe SSLOCSPResponseMaxAge, 
    aSSLOCSPResponseTimeSkew :: Maybe SSLOCSPResponseTimeSkew, 
    aSSLOCSPUseRequestNonce :: Maybe SSLOCSPUseRequestNonce, 
    aSSLOpenSSLConfCmd :: Maybe [SSLOpenSSLConfCmd], 
    aSSLOptions :: Maybe SSLOptions, 
    aSSLProtocol :: Maybe SSLProtocol, 
    aSSLSessionCacheTimeout :: Maybe SSLSessionCacheTimeout, 
    aSSLSessionTicketKeyFile :: Maybe SSLSessionTicketKeyFile, 
    aSSLSessionTickets :: Maybe SSLSessionTickets, 
    aSSLSRPUnknownUserSeed :: Maybe SSLSRPUnknownUserSeed, 
    aSSLSRPVerifierFile :: Maybe SSLSRPVerifierFile, 
    aSSLStaplingErrorCacheTimeout :: Maybe SSLStaplingErrorCacheTimeout, 
    aSSLStaplingFakeTryLater :: Maybe SSLStaplingFakeTryLater, 
    aSSLStaplingForceURL :: Maybe SSLStaplingForceURL, 
    aSSLStaplingResponderTimeout :: Maybe SSLStaplingResponderTimeout, 
    aSSLStaplingResponseMaxAge :: Maybe SSLStaplingResponseMaxAge, 
    aSSLStaplingResponseTimeSkew :: Maybe SSLStaplingResponseTimeSkew, 
    aSSLStaplingReturnResponderErrors :: Maybe SSLStaplingReturnResponderErrors, 
    aSSLStaplingStandardCacheTimeout :: Maybe SSLStaplingStandardCacheTimeout, 
    aSSLStrictSNIVHostCheck :: Maybe SSLStrictSNIVHostCheck, 
    aSSLUseStapling :: Maybe SSLUseStapling, 
    aSSLVerifyClient :: Maybe SSLVerifyClient, 
    aSSLVerifyDepth :: Maybe SSLVerifyDepth, 
    aSSLCryptoDevice :: Maybe SSLCryptoDevice, 
    aSSLFIPS :: Maybe SSLFIPS, 
    aSSLPassPhraseDialog :: Maybe SSLPassPhraseDialog,  
    aSSLRandomSeed :: Maybe SSLRandomSeed, 
    aSSLSessionCache :: Maybe SSLSessionCache, 
    aSSLStaplingCache :: Maybe SSLStaplingCache, 
    aSSLUserName :: Maybe SSLUserName, 
    {-servers-}
    aVirtualHosts :: Maybe [VirtualHost] 
} deriving (Show,Eq)

data VirtualHost = VirtualHost { 
    sVirtualHostAddress :: Maybe [VirtualHostAddress], 
    {-core-}
    sAcceptPathInfo :: Maybe AcceptPathInfo, 
    sAccessFileName :: Maybe AccessFileName, 
    sAddDefaultCharset :: Maybe AddDefaultCharset, 
    sAllowEncodedSlashes :: Maybe AllowEncodedSlashes, 
    sContentDigest :: Maybe ContentDigest, 
    sDefine :: Maybe [Define], 
    sDirectory :: Maybe [Directory], 
    sDocumentRoot :: Maybe DocumentRoot, 
    sEnableMMAP :: Maybe EnableMMAP, 
    sEnableSendfile :: Maybe EnableSendfile, 
    sError :: Maybe Error, 
    sErrorDocument :: Maybe [ErrorDocument], 
    sErrorLog :: Maybe ErrorLog, 
    sErrorLogFormat :: Maybe ErrorLogFormat, 
    sFileETag :: Maybe FileETag, 
    sFiles :: Maybe [Files], 
    sHostnameLookups :: Maybe HostnameLookups, 
    sInclude :: Maybe [Include], 
    sIncludeOptional :: Maybe [IncludeOptional], 
    sKeepAlive :: Maybe KeepAlive, 
    sKeepAliveTimeout :: Maybe KeepAliveTimeout, 
    sLimitInternalRecursion :: Maybe LimitInternalRecursion, 
    sLimitRequestBody :: Maybe LimitRequestBody, 
    sLimitRequestFields :: Maybe LimitRequestFields, 
    sLimitRequestFieldSize :: Maybe LimitRequestFieldSize, 
    sLimitRequestLine :: Maybe LimitRequestLine, 
    sLimitXMLRequestBody :: Maybe LimitXMLRequestBody, 
    sLocation :: Maybe [Location], 
    sLogLevel :: Maybe LogLevel, 
    sMaxKeepAliveRequests :: Maybe MaxKeepAliveRequests, 
    sMaxRangesOverlaps :: Maybe MaxRangeOverlaps, 
    sMaxRangesReversals :: Maybe MaxRangeReversals, 
    sMaxRanges :: Maybe MaxRanges, 
    sOptions :: Maybe Options, 
    sProtocol :: Maybe Protocol, 
    sRLimitCPU :: Maybe RLimitCPU, 
    sRLimitMEM :: Maybe RLimitMEM, 
    sRLimitNPROC :: Maybe RLimitNPROC, 
    sServerAdmin :: Maybe ServerAdmin, 
    sServerName :: Maybe [ServerName], 
    sServerPath :: Maybe ServerPath, 
    sServerSignature :: Maybe ServerSignature, 
    sSetHandler :: Maybe SetHandler, 
    sSetInputFilter :: Maybe SetInputFilter, 
    sSetOutputFilter :: Maybe SetOutputFilter, 
    sTimeOut :: Maybe TimeOut, 
    sTraceEnable :: Maybe TraceEnable, 
    sUseCanonicalName :: Maybe UseCanonicalName, 
    sUseCanonicalPhysicalPort :: Maybe UseCanonicalPhysicalPort, 
    {-alias-}
    sAlias :: Maybe [Alias], 
    sAliasMatch :: Maybe [AliasMatch], 
    sRedirect :: Maybe [Redirect], 
    sRedirectMatch :: Maybe [RedirectMatch], 
    sRedirectPermanent :: Maybe [RedirectPermanent], 
    sRedirectTemp :: Maybe [RedirectTemp], 
    sScriptAlias :: Maybe [ScriptAlias], 
    sScriptAliasMatch :: Maybe [ScriptAliasMatch], 
    {-autoindex-}
    sAddAlt :: Maybe [AddAlt], 
    sAddAltByEncoding :: Maybe [AddAltByEncoding], 
    sAddAltByType :: Maybe [AddAltByType], 
    sAddDescription :: Maybe [AddDescription], 
    sAddIcon :: Maybe [AddIcon], 
    sAddIconByEncoding :: Maybe [AddIconByEncoding], 
    sAddIconByType :: Maybe [AddIconByType], 
    sDefaultIcon :: Maybe DefaultIcon, 
    sHeaderName :: Maybe HeaderName, 
    sIndexHeadInsert :: Maybe IndexHeadInsert, 
    sIndexIgnore :: Maybe [IndexIgnore], 
    sIndexIgnoreReset :: Maybe IndexIgnoreReset, 
    sIndexOptions :: Maybe [IndexOptions], 
    sIndexOrderDefault :: Maybe IndexOrderDefault, 
    sIndexStyleSheet :: Maybe IndexStyleSheet, 
    sReadmeName :: Maybe ReadmeName, 
    {-cgi mod-}
    sScriptLog :: Maybe ScriptLog, 
    sScriptLogBuffer :: Maybe ScriptLogBuffer, 
    sScriptLogLength :: Maybe ScriptLogLength, 
    {-cgid-}
    sCGIDScriptTimeout :: Maybe CGIDScriptTimeout, 
    {-dir-}
    sDirectoryCheckHandler :: Maybe DirectoryCheckHandler, 
    sDirectoryIndex :: Maybe DirectoryIndex, 
    sIndexRedirect :: Maybe IndexRedirect, 
    sDirectorySlash :: Maybe DirectorySlash, 
    sFallbackResource :: Maybe FallbackResource, 
    {-env-}
    sPassEnv :: Maybe PassEnv, 
    sSetEnv :: Maybe [SetEnv], 
    sUnsetEnv :: Maybe [UnsetEnv], 
    {-filter-}
    sAddOutputFilterByType :: Maybe [AddOutputFilterByType], 
    sFilterChain :: Maybe [FilterChain], 
    sFilterDeclare :: Maybe [FilterDeclare], 
    sFilterProtocol :: Maybe [FilterProtocol], 
    sFilterProvider :: Maybe [FilterProvider], 
    sFilterTrace :: Maybe [FilterTrace], 
    {-imap-}
    sImapBase :: Maybe ImapBase, 
    sImapDefault :: Maybe ImapDefault, 
    sImapMenu :: Maybe ImapMenu, 
    {-logconf-}
    sCustomLog :: Maybe [CustomLog], 
    sLogFormat :: Maybe [LogFormat], 
    sTransferLog :: Maybe [TransferLog], 
    {-mime-}
    sAddCharset :: Maybe [AddCharset], 
    sAddEncoding :: Maybe [AddEncoding], 
    sAddHandler :: Maybe [AddHandler], 
    sAddInputFilter :: Maybe [AddInputFilter], 
    sAddLanguage :: Maybe [AddLanguage], 
    sAddOutputFilter :: Maybe [AddOutputFilter], 
    sAddType :: Maybe [AddType], 
    sDefaultLanguage :: Maybe DefaultLanguage, 
    sMultiviewsMatch :: Maybe MultiviewsMatch, 
    sRemoveCharset :: Maybe [RemoveCharset], 
    sRemoveEncoding :: Maybe [RemoveEncoding], 
    sRemoveHandler :: Maybe [RemoveHandler], 
    sRemoveInputFilter :: Maybe [RemoveInputFilter], 
    sRemoveLanguage :: Maybe [RemoveLanguage], 
    sRemoveOutputFilter :: Maybe [RemoveOutputFilter], 
    sRemoveType :: Maybe [RemoveType], 
    {-nego-}
    sCacheNegotiatedDocs :: Maybe CacheNegotiatedDocs, 
    sForceLanguagePriority :: Maybe ForceLanguagePriority, 
    sLanguagePriority :: Maybe LanguagePriority, 
    {-reflector-}
    sReflectorHeader :: Maybe ReflectorHeader, 
    {-setenvif-}
    sBrowserMatch :: Maybe [BrowserMatch], 
    sBrowserMatchNoCase :: Maybe [BrowserMatchNoCase], 
    sSetEnvIf :: Maybe [SetEnvIf], 
    sSetEnvIfExpr :: Maybe [SetEnvIfExpr], 
    sSetEnvIfNoCase :: Maybe [SetEnvIfNoCase], 
    {-userdir-}
    sUserDir :: Maybe UserDir, 
    {-ssl-}
    sSSLCACertificateFile :: Maybe SSLCACertificateFile, 
    sSSLCACertificatePath :: Maybe SSLCACertificatePath, 
    sSSLCADNRequestFile :: Maybe SSLCADNRequestFile, 
    sSSLCADNRequestPath :: Maybe SSLCADNRequestPath, 
    sSSLCARevocationCheck :: Maybe SSLCARevocationCheck, 
    sSSLCARevocationFile :: Maybe SSLCARevocationFile, 
    sSSLCARevocationPath :: Maybe SSLCARevocationPath, 
    sSSLCertificateChainFile :: Maybe SSLCertificateChainFile, 
    sSSLCertificateFile :: Maybe SSLCertificateFile, 
    sSSLCertificateKeyFile :: Maybe SSLCertificateKeyFile, 
    sSSLCipherSuite :: Maybe SSLCipherSuite, 
    sSSLCompression :: Maybe SSLCompression, 
    sSSLEngine :: Maybe SSLEngine, 
    sSSLHonorCipherOrder :: Maybe SSLHonorCipherOrder, 
    sSSLInsecureRenegotiation :: Maybe SSLInsecureRenegotiation, 
    sSSLOCSPDefaultResponder :: Maybe SSLOCSPDefaultResponder, 
    sSSLOCSPEnable :: Maybe SSLOCSPEnable, 
    sSSLOCSPOverrideResponder :: Maybe SSLOCSPOverrideResponder, 
    sSSLOCSPResponderTimeout :: Maybe SSLOCSPResponderTimeout, 
    sSSLOCSPResponseMaxAge :: Maybe SSLOCSPResponseMaxAge, 
    sSSLOCSPResponseTimeSkew :: Maybe SSLOCSPResponseTimeSkew, 
    sSSLOCSPUseRequestNonce :: Maybe SSLOCSPUseRequestNonce, 
    sSSLOpenSSLConfCmd :: Maybe [SSLOpenSSLConfCmd], 
    sSSLOptions :: Maybe SSLOptions, 
    sSSLProtocol :: Maybe SSLProtocol, 
    sSSLSessionCacheTimeout :: Maybe SSLSessionCacheTimeout, 
    sSSLSessionTicketKeyFile :: Maybe SSLSessionTicketKeyFile, 
    sSSLSessionTickets :: Maybe SSLSessionTickets, 
    sSSLSRPUnknownUserSeed :: Maybe SSLSRPUnknownUserSeed, 
    sSSLSRPVerifierFile :: Maybe SSLSRPVerifierFile, 
    sSSLStaplingErrorCacheTimeout :: Maybe SSLStaplingErrorCacheTimeout, 
    sSSLStaplingFakeTryLater :: Maybe SSLStaplingFakeTryLater, 
    sSSLStaplingForceURL :: Maybe SSLStaplingForceURL, 
    sSSLStaplingResponderTimeout :: Maybe SSLStaplingResponderTimeout, 
    sSSLStaplingResponseMaxAge :: Maybe SSLStaplingResponseMaxAge, 
    sSSLStaplingResponseTimeSkew :: Maybe SSLStaplingResponseTimeSkew, 
    sSSLStaplingReturnResponderErrors :: Maybe SSLStaplingReturnResponderErrors, 
    sSSLStaplingStandardCacheTimeout :: Maybe SSLStaplingStandardCacheTimeout, 
    sSSLStrictSNIVHostCheck :: Maybe SSLStrictSNIVHostCheck, 
    sSSLUseStapling :: Maybe SSLUseStapling, 
    sSSLVerifyClient :: Maybe SSLVerifyClient, 
    sSSLVerifyDepth :: Maybe SSLVerifyDepth 
} deriving (Show,Eq)

data Directory = Directory { 
    {-identifiers-}
    dMatch :: Maybe Match, 
    dPath :: Maybe Path, 
    {-directives-}
    dDirDirectives :: Maybe DirDirectives, 
    dRequireCons :: Maybe [RequireCons], 
    {-specific directory directives-}
    dAllowOverride :: Maybe AllowOverride, 
    dAllowOverrideList :: Maybe AllowOverrideList, 
    dOptions :: Maybe Options, 
    dFiles :: Maybe [Files] 
} deriving (Show,Eq)

data Location = Location { 
    {-identifiers-}
    lMatch :: Maybe Match, 
    lPath :: Maybe Path, 
    {-directives-}
    lDirDirectives :: Maybe DirDirectives, 
    lRequireCons :: Maybe [RequireCons], 
    {-specific location directives-}
    lOptions :: Maybe Options 
} deriving (Show,Eq)

data Files = Files { 
    {-identifiers-}
    fMatch :: Maybe Match, 
    fFileName :: Maybe FileName, 
    {-directives-}
    fDirDirectives :: Maybe DirDirectives,
    fRequireCons :: Maybe [RequireCons] 
} deriving (Show,Eq)

--data AIf = AIf IfType Condition DirDirectives Options deriving (Show)

data DirDirectives = DirDirectives { 
    {-core-}
    dAcceptPathInfo :: Maybe AcceptPathInfo, 
    dAddDefaultCharset :: Maybe AddDefaultCharset, 
    dContentDigest :: Maybe ContentDigest, 
    dDefine :: Maybe [Define], 
    dEnableMMAP :: Maybe EnableMMAP, 
    dEnableSendFile :: Maybe EnableSendfile, 
    dError :: Maybe Error, 
    dErrorDocument :: Maybe [ErrorDocument], 
    dFileETag :: Maybe FileETag, 
    dForceType :: Maybe ForceType, 
    dHostnameLookups :: Maybe HostnameLookups, 
    dInclude :: Maybe [Include], 
    dIncludeOptional :: Maybe [IncludeOptional], 
    dLimitRequestBody :: Maybe LimitRequestBody, 
    dLimitXMLRequestBody :: Maybe LimitXMLRequestBody, 
    dLogLevel :: Maybe LogLevel, 
    dMaxRangeOverlaps :: Maybe MaxRangeOverlaps, 
    dMaxRangeReversals :: Maybe MaxRangeReversals, 
    dMaxRanges :: Maybe MaxRanges, 
    dRLimitCPU :: Maybe RLimitCPU, 
    dRLimitMEM :: Maybe RLimitMEM, 
    dRLimitNPROC :: Maybe RLimitNPROC, 
    dServerSignature :: Maybe ServerSignature, 
    dSetHandler :: Maybe SetHandler, 
    dSetInputFilter :: Maybe SetInputFilter, 
    dSetOutputFilter :: Maybe SetOutputFilter, 
    dUseCanonicalName :: Maybe UseCanonicalName, 
    dUseCanonicalPhysicalPort :: Maybe UseCanonicalPhysicalPort, 
    {-alias-}
    dRedirect :: Maybe [Redirect], 
    dRedirectMatch :: Maybe [RedirectMatch], 
    dRedirectPermanent :: Maybe [RedirectPermanent], 
    dRedirectTemp :: Maybe [RedirectTemp], 
    {-auth-basic-}
    dAuthBasicAuthoritative :: Maybe AuthBasicAuthoritative, 
    dAuthBasicFake :: Maybe AuthBasicFake, 
    dAuthBasicProvider :: Maybe AuthBasicProvider, 
    dAuthBasicUseDigestAlgorithm :: Maybe AuthBasicUseDigestAlgorithm, 
    {-auth-form-}
    {-AAuthFormConf-} 
    {-authn-core-}
    dAuthName :: Maybe AuthName, 
    dAuthType :: Maybe AuthType, 
    {-authn-file-}
    dAuthUserFile :: Maybe AuthUserFile, 
    {-authz-core-}
    dAuthMerging :: Maybe AuthMerging, 
    dAuthzSendForbiddenOnFailure :: Maybe AuthzSendForbiddenOnFailure, 
    dRequire :: Maybe [Require], 
    {-authz-groupfile-}
    dAuthGroupFile :: Maybe AuthGroupFile, 
    {-autoindex-}
    dAddAlt :: Maybe [AddAlt], 
    dAddAltByEncoding :: Maybe [AddAltByEncoding], 
    dAddAltByType :: Maybe [AddAltByType], 
    dAddDescription :: Maybe [AddDescription], 
    dAddIcon :: Maybe [AddIcon], 
    dAddIconByEncoding :: Maybe [AddIconByEncoding], 
    dAddIconByType :: Maybe [AddIconByType], 
    dDefaultIcon :: Maybe DefaultIcon, 
    dHeaderName :: Maybe HeaderName, 
    dIndexHeadInsert :: Maybe IndexHeadInsert, 
    dIndexIgnore :: Maybe [IndexIgnore], 
    dIndexIgnoreReset :: Maybe IndexIgnoreReset, 
    dIndexOptions :: Maybe [IndexOptions], 
    dIndexOrderDefault :: Maybe IndexOrderDefault, 
    dIndexStyleSheet :: Maybe IndexStyleSheet, 
    dReadmeName :: Maybe ReadmeName, 
    {-cgid-}
    dCGIDScriptTimeout :: Maybe CGIDScriptTimeout, 
    {-dir-}
    dDirectoryCheckHandler :: Maybe DirectoryCheckHandler, 
    dDirectoryIndex :: Maybe DirectoryIndex, 
    dIndexRedirect :: Maybe IndexRedirect, 
    dDirectorySlash :: Maybe DirectorySlash, 
    dFallbackResource :: Maybe FallbackResource, 
    {-env-}
    dPassEnv :: Maybe PassEnv, 
    dSetEnv :: Maybe [SetEnv], 
    dUnsetEnv :: Maybe [UnsetEnv], 
    {-filter-}
    dAddOutputFilterByType :: Maybe [AddOutputFilterByType], 
    dFilterChain :: Maybe [FilterChain], 
    dFilterDeclare :: Maybe [FilterDeclare], 
    dFilterProtocol :: Maybe [FilterProtocol], 
    dFilterProvider :: Maybe [FilterProvider], 
    dFilterTrace :: Maybe [FilterTrace], 
    {-imap-}
    dImapBase :: Maybe ImapBase, 
    dImapDefault :: Maybe ImapDefault, 
    dImapMenu :: Maybe ImapMenu, 
    {-logconf-}
    dCustomLog :: Maybe [CustomLog], 
    dLogFormat :: Maybe [LogFormat], 
    dTransferLog :: Maybe [TransferLog], 
    {-mime-}
    dAddCharset :: Maybe [AddCharset], 
    dAddEncoding :: Maybe [AddEncoding], 
    dAddHandler :: Maybe [AddHandler], 
    dAddInputFilter :: Maybe [AddInputFilter], 
    dAddLanguage :: Maybe [AddLanguage], 
    dAddOutputFilter :: Maybe [AddOutputFilter], 
    dAddType :: Maybe [AddType], 
    dDefaultLanguage :: Maybe DefaultLanguage, 
    dModMimeUsePathInfo :: Maybe ModMimeUsePathInfo, 
    dMultiviewsMatch :: Maybe MultiviewsMatch, 
    dRemoveCharset :: Maybe [RemoveCharset], 
    dRemoveEncoding :: Maybe [RemoveEncoding], 
    dRemoveHandler :: Maybe [RemoveHandler], 
    dRemoveInputFilter :: Maybe [RemoveInputFilter], 
    dRemoveLanguage :: Maybe [RemoveLanguage], 
    dRemoveOutputFilter :: Maybe [RemoveOutputFilter], 
    dRemoveType :: Maybe [RemoveType], 
    {-nego-}
    dForceLanguagePriority :: Maybe ForceLanguagePriority, 
    dLanguagePriority :: Maybe LanguagePriority, 
    {-reflector-}
    dReflectorHeader :: Maybe ReflectorHeader, 
    {-request-}
    dKeptBodySize :: Maybe KeptBodySize, 
    {-setenvif-}
    dBrowserMatch :: Maybe [BrowserMatch], 
    dBrowserMatchNoCase :: Maybe [BrowserMatchNoCase], 
    dSetEnvIf :: Maybe [SetEnvIf], 
    dSetEnvIfExpr :: Maybe [SetEnvIfExpr], 
    dSetEnvIfNoCase :: Maybe [SetEnvIfNoCase], 
    {-ssl-}
    dSSLCipherSuite :: Maybe SSLCipherSuite, 
    dSSLOptions :: Maybe SSLOptions, 
    dSSLRenegBufferSize :: Maybe SSLRenegBufferSize, 
    dSSLRequireSSL :: Maybe SSLRequireSSL, 
    dSSLUserName :: Maybe SSLUserName, 
    dSSLVerifyClient :: Maybe SSLVerifyClient, 
    dSSLVerifyDepth :: Maybe SSLVerifyDepth 
} deriving (Show,Eq)

type Match = Bool
type Path = String
type FileName = String
--type IfType = String --If|IfDefine|IfModule|ElseIf|Else
--type Condition = String 
type VirtualHostAddress = String --"IpAddress"[:"port"] ["IpAddress"[:"port"]] ... 

--core mod
type ID = String
type AcceptFilter = String --"protocol"+"filter"
type AcceptPathInfo = String --On|Off|Default default:Default
type AccessFileName = String --"filename" ["filename"] ... default:.htaccess
type AddDefaultCharset = String --On|Off|"charset" default:off
type AllowEncodedSlashes = String --On|Off|NoDecode default:Off
type AllowOverride = String --All|None|"directive type" ["directive type"] ... default:None(since 2.3.9)
type AllowOverrideList = String --None|["directive"] default:None
type ContentDigest = String --On|Off default:Off 
type DefaultRuntimeDir = String --"directory path" default:DEFAULT_REL_RUNTIMEDIR
type Define = String --"var" ["value"]
type DocumentRoot = String --"directoryPath" --default:"/usr/local/apache/htdocs"
type EnableMMAP = String --On|Off default:On
type EnableSendfile = String --On|Off default:Off
type Error = String --"errorMessage"
type ErrorDocument = String --"errorCode"+"document or error message"
type ErrorLog = String --"filePath"|syslog[:"facility"] default:logs/error_log
type ErrorLogFormat = String --[connection|request] "format"
type ExtendedStatus = String --On|Off default:Off
type FileETag = String --None|All|[INode|MTime|Size] ... default:MTime Size
type ForceType = String --None|"mime-type"
type HostnameLookups = String --On|Off|Double default:Off
type Include = String --"filepath"|"directorypath"|"wildcard"
type IncludeOptional = String --"filepath"|"directorypath"|"wildcard"
type KeepAlive = String --On|Off default:On
type KeepAliveTimeout = String --"number"[ms] default:5
--data ALimit = ALimit Except [Method] ?[directive]? [ALimit] deriving (show)
type LimitInternalRecursion = String --"number" ["number"] default:10 10 (if only one number specified -> same for the two)
type LimitRequestBody = String --"octets number" default:0 (unlimited)
type LimitRequestFields = String --"number" default:100
type LimitRequestFieldSize = String --"octets number" default:8190
type LimitRequestLine = String --"octets number" default:8190
type LimitXMLRequestBody = String --"octets number" default:1000000
type LogLevel = String --["module"]"level" ["module":"level"] ... default:warn
type MaxKeepAliveRequests = String --"number" default:100
type MaxRangeOverlaps = String --default|unlimited|none|"number" default:20
type MaxRangeReversals = String --default|unlimited|none|"number" default:20
type MaxRanges = String --default|unlimited|none|"number" default:200
type Mutex = String --"mechanism" [default|"mutexName"] ... [OmitPID] default:default (mechanisms : default|none|posixsem|sysvsem|sem|pthread|fcntl:"mutexPath"|flock:"mutexPath"|file:"mutexPath")
type Options = String --[+|-]"option" [[+|-]"option"] ... default:FollowSymLinks (option : All|ExecCGI|FollowSymLinks|Includes|IncludesNOEXEC|Indexes|MultiViews|SymLinksIfOwnerMatch)
type Protocol = String --"protocol"
type RLimitCPU = String --"seconds|max" ["seconds"|max]
type RLimitMEM = String --"octets number"|max ["octets number"|max]
type RLimitNPROC = String --"number"|max ["number"|max]
type SeeRequestTail = String --On|Off default:Off
type ServerAdmin = String --"e-mail"|"url"
type ServerName = String --["protocol"://]"full domain name"[:"port"]
type ServerPath = String --"UrlPath"
type ServerRoot = String --"directoryPath" default:/usr/local/apache
type ServerSignature = String --On|Off|EMail default:off
type ServerTokens = String --Full|OS|Minimal|Minor|Major|Prod (from most to least information) Default:Full
type SetHandler = String --"Handler name"|None
type SetInputFilter = String --"filter" [;"filter"] ...
type SetOutputFilter = String --"filter" [;"filter"] ...
type TimeOut = String --"number seconds" default:60
type TraceEnable = String --On|Off|Extended default:on
type UnDefine = String --"var"
type UseCanonicalName = String --On|Off|DNS default:Off
type UseCanonicalPhysicalPort = String --On|Off default:Off
--end core

--prefork mpm
type GracefulShutdownTimeout = String --"number seconds" default:0
type Listen = String --["IpAddress":]"port" ["protocol"]
type ListenBackLog = String --"number" default:511
type MaxConnectionsPerChild = String --"number" default:0
type MaxMemFree = String --"number koctets" default:2048
type MaxRequestWorkers = String --"number" default:256
type PidFile = String --"file name" default:logs/httpd.pid
type ReceiveBufferSize = String --"number octets" default:0
type ScoreBoardFile = String --"file path" default:logs/apache_runtime_status
type SendBufferSize = String --"number octets" default:0
type ServerLimit = String --"number" default:256
type StartServers = String --"number" default:5
type MaxSpareServers = String --"number" default:10
type MinSpareServers = String --"number" default:5
--end mpm

--actions mod 
--not used

--alias mod (Provides for mapping different parts of the host filesystem in the document tree and for URL redirection)
type Alias = String --"URL path" "file path"|"directory path"
type AliasMatch = String --"regex" "file path"|"directory path"
type Redirect = String --["state"] "URL path" "URL"
type RedirectMatch = String --["state"] "regex" "URL"
type RedirectPermanent = String --"URL path" "URL"
type RedirectTemp = String --"URL path" "URL"
type ScriptAlias = String --"URL path" "file path"|"directory path"
type ScriptAliasMatch = String --"regex" "file path"|"directory path"
--end alias mod 

--auth-basic mod (Basic HTTP authentication)
type AuthBasicAuthoritative = String --On|Off default:On
type AuthBasicFake = String --Off|"username" ["password"]
type AuthBasicProvider = String --"provider" ["provider"] ... default:file
type AuthBasicUseDigestAlgorithm = String --Off|MD5 default:off
--end auth-basic mod

--authn-core mod (Core Authentication)
type AuthName = String --"auth-domain"
type AuthType = String --None|Basic|Digest|Form 
--end authn-core

--authn-file mod (User authentication using text files)
type AuthUserFile = String --"filePath"
--end authn-file mod

--authz-core mod (Core Authorization)
type AuthMerging = String --Off|And|Or default:Off
type AuthzSendForbiddenOnFailure = String --On|Off default:Off
type Require = String --[not] "entityName" ["entityName"] ...
data RequireCons = RequireCons { 
    rConsType :: Maybe String, --All|Any|None (not actual directive, just used here to differentiate between RequireAll, ReqireAny and RequireNone)
    rRequire :: Maybe [Require], 
    rRequireCons :: Maybe [RequireCons] 
} deriving (Show,Eq)
--data RequireAll = RequireAll [RequireAll] [RequireAny] [RequireNone] [Require] deriving (Show) -- <RequireAll> ... </RequireAll>
--data RequireAny = RequireAny [RequireAll] [RequireAny] [RequireNone] [Require] deriving (Show) -- <RequireAny> ... </RequireAny>
--data RequireNone = RequireNone [RequireAll] [RequireAny] [RequireNone] [Require] deriving (Show) -- <RequireNone> ... </RequireNone>
--end authz-core mod

--authz-groupfile mod (Group authorization using plaintext files)
type AuthGroupFile = String --"file-path"
--end authz-groupfile mod

--authz_host mod
--no directives

--authz_user mod
--no directives

--autoindex mod (Generates directory indexes automatically)
type AddAlt = String --"string" "file" ["file"] ...
type AddAltByEncoding = String --"string" "mime-encoding" ["mime-encoding"] ...
type AddAltByType = String --"string" "mime-type" ["mime-type"] ...
type AddDescription = String --"string" "file" ["file"] ...
type AddIcon = String --"icon" "name" ["name"] ...
type AddIconByEncoding = String --"icon" "mime-encoding" ["mime-encoding"] ...
type AddIconByType = String --"icon" "mime-type" ["mime-type"] ...
type DefaultIcon = String --"url-path"
type HeaderName = String --"filename"
type IndexHeadInsert = String --"string to insert in <head>"
type IndexIgnore = String --"file" ["file"] ... default:"."
type IndexIgnoreReset = String --ON|OFF, actually Bool but can be undefined, no default
type IndexOptions = String --[+|-]"option" [[+|-]"option"] ... 
type IndexOrderDefault = String --Ascending|Descending Name|Date|Size|Description default:Ascending Name
type IndexStyleSheet = String --"url-path" 
type ReadmeName = String --"filename"
--end autoindex mod

--cgi mod (Execution of CGI scripts)
type ScriptLog = String --"filepath" 
type ScriptLogBuffer = String --"number bytes" default:1024
type ScriptLogLength = String --"number bytes" default:10385760
--end cgi mod

--cgid mod (Execution of CGI scripts using an external CGI daemon)
type CGIDScriptTimeout = String --"time" default:"value of Timeout directive"
type ScriptSock = String --"filepath" default:cgisock
--end cgid mod

--deflate mod
--not considered for now

--dir mod (Provides for "trailing slash" redirects and serving directory index files)
type DirectoryCheckHandler = String --On|Off default:Off
type DirectoryIndex = String --disabled|"local-url" ["local-url"] ... default:index.html
type IndexRedirect = String --On|Off|Permanent|temp|seeother|"3xx-code" default:Off
type DirectorySlash = String --On|Off default:On
type FallbackResource = String --disabled|"local-url" default:disabled
--end dir mod

--env mod (Modifies the environment which is passed to CGI scripts and SSI pages)
type PassEnv = String --"env-var" ["env-var"] ... 
type SetEnv = String --"env-var" ["value"]
type UnsetEnv = String --"env-var" ["env-var"] ...
--end env mod

--filter mod
type AddOutputFilterByType = String --"filter"[;"filter"...] "media-type" ["media-type"] ...
type FilterChain = String --[+=-@!]"filter-name" ...
type FilterDeclare = String --"filter-name" ["type"]
type FilterProtocol = String --"filter-name" ["provider-name"] "proto-flags"
type FilterProvider = String --"filter-name" "provider-name" "expression"
type FilterTrace = String --"filter-name" "level"
--end filter mod

--imap mod
type ImapBase = String --map|referer|"url" default:http://"servername"/
type ImapDefault = String --error|nocontent|map|referer|"url" default:nocontent
type ImapMenu = String --none|formatted|semiformatted|unformatted default:formatted
--end imap mod

--include mod (Server-parsed html documents)
--not used?

--log config mod
type BufferedLogs = String --On|Off default:Off
type CustomLog = String --"file"|"pipe" "format"|"nickname" [env=[!]"environment-variable"|expr="expression"]
type LogFormat = String --"format"|"nickname" ["nickname"] default:"%h %l %u %t \"%r\" %>s %b"
type TransferLog = String --"file"|"pipe"
--end log config mod

--macro mod
--not considered

--mime mod
type AddCharset = String --"charset" "extension" ["extension"] ...
type AddEncoding = String --"encoding" "extension" ["extension"] ...
type AddHandler = String --"handler-name" "extension" ["extension"] ...
type AddInputFilter = String --"filter"[;"filter"...] "extension" ["extension"] ... 
type AddLanguage = String --"language-tag" "extension" ["extension"] ...
type AddOutputFilter = String --"filter"[;"filter"] "extension" ["extension"] ...
type AddType = String --"media-type" "extension" ["extension"] ...

type DefaultLanguage = String --"language-tag"
type ModMimeUsePathInfo = String --On|Off default:Off
type MultiviewsMatch = String --Any|NegotiatedOnly|Filters|Handlers [Handlers|Filters] default:NegotiatedOnly

type RemoveCharset = String --"extension" ["extension"] ...
type RemoveEncoding = String --"extension" ["extension"] ...
type RemoveHandler = String --"extension" ["extension"] ...
type RemoveInputFilter = String --"extension" ["extension"] ...
type RemoveLanguage = String --"extension" ["extension"] ...
type RemoveOutputFilter = String --"extension" ["extension"] ...
type RemoveType = String --"extension" ["extension"] ...

type TypesConfig = String --"filepath" default:conf/mime.types
--end mime mod

--negotiation mod
type CacheNegotiatedDocs = String --On|Off default:Off
type ForceLanguagePriority = String --None|Prefer|Fallback [Prefer|Fallback] default:Prefer
type LanguagePriority = String --"mime-lang" ["mime-lang"] ...
--end negotiation mod

--several proxy mods
--not considered for now

--reflector mod
type ReflectorHeader = String --"inputheader" ["outputheader"]
--end reflector mod

--remoteIP mod
--not considered for now (only if proxy)

--request mod
type KeptBodySize = String --"max size in bytes" default:0
--end request mod

--setenvif mod
type BrowserMatch = String --"regex" [!]"env-var"[="value"] [[!]"env-var"[="value"]] ... 
type BrowserMatchNoCase = String --"regex" [!]"env-var"[="value"] [[!]"env-var"[="value"]] ... 
type SetEnvIf = String --"attribute" "regex" [!]"env-var"[="value"] [[!]"env-var"[="value"]] ... 
type SetEnvIfExpr = String --"expr" "regex" [!]"env-var"[="value"] [[!]"env-var"[="value"]] ... 
type SetEnvIfNoCase = String --"attribute" "regex" [!]"env-var"[="value"] [[!]"env-var"[="value"]] ... 
--end setenvif mod

--ssl mod
type SSLCACertificateFile = String --"filepath"
type SSLCACertificatePath = String --"directorypath"
type SSLCADNRequestFile = String --"filepath"
type SSLCADNRequestPath = String --"directorypath"
type SSLCARevocationCheck = String --chain|leaf|none default:none
type SSLCARevocationFile = String --"filepath"
type SSLCARevocationPath = String --"directorypath"
type SSLCertificateChainFile = String --"filepath"
type SSLCertificateFile = String --"filepath"
type SSLCertificateKeyFile = String --"filepath"
type SSLCipherSuite = String --"algorithms" default:DEFAULT (depends on openssl version)
type SSLCompression = String --On|Off default:off
type SSLCryptoDevice = String --"engine" default:builtin
type SSLEngine = String --on|off|optional default:off
type SSLFIPS = String --On|Off default:off
type SSLHonorCipherOrder = String --On|Off default:off
type SSLInsecureRenegotiation = String --On|Off default:off
type SSLOCSPDefaultResponder = String --"uri"
type SSLOCSPEnable = String --On|Off default:off
type SSLOCSPOverrideResponder = String --On|Off default:off
type SSLOCSPResponderTimeout = String --"number seconds" default:10
type SSLOCSPResponseMaxAge = String --"number seconds" default:-1
type SSLOCSPResponseTimeSkew = String --"number seconds" default:300
type SSLOCSPUseRequestNonce = String --On|Off default:on
type SSLOpenSSLConfCmd = String --"command" "value" 
type SSLOptions = String --[+|-]"option" ... 
type SSLPassPhraseDialog = String --"type" default:builtin
type SSLProtocol = String --[+|-]"protocole" ... default:all
type SSLRandomSeed = String --"context" "source" ["number"]
type SSLRenegBufferSize = String --"size" default:131072
type SSLRequireSSL = String --no value, just write SSLRequireSSL to enable
type SSLSessionCache = String --"type" default:none
type SSLSessionCacheTimeout = String --"number seconds" default:300
type SSLSessionTicketKeyFile = String --"filepath"
type SSLSessionTickets = String --On|Off default:on
type SSLSRPUnknownUserSeed = String --"secret-string"
type SSLSRPVerifierFile = String --"filepath"
type SSLStaplingCache = String --"type"
type SSLStaplingErrorCacheTimeout = String --"number seconds" default:600
type SSLStaplingFakeTryLater = String --On|Off default:on
type SSLStaplingForceURL = String --"uri" 
type SSLStaplingResponderTimeout = String --"number seconds" default:10
type SSLStaplingResponseMaxAge = String --"number seconds" default:-1
type SSLStaplingResponseTimeSkew = String --"number seconds" default:300
type SSLStaplingReturnResponderErrors = String --On|Off default:on
type SSLStaplingStandardCacheTimeout = String --"number seconds" default:3600
type SSLStrictSNIVHostCheck = String --On|Off default:off
type SSLUserName = String --"var-name" 
type SSLUseStapling = String --On|Off default:off
type SSLVerifyClient = String --none|optional|require|optional_no_ca default:none
type SSLVerifyDepth = String --"number" default:1
--end ssl mod

--status mod
--no directives

--unixd mod
type ChrootDir = String --"directory path"
type Group = String --"groupName"|#"number"
type Suexec = String --On|Off default:On if suexec binary exists with proper owner and mode, Off otherwise
type User = String --"unix-user"|#"number"
--end unixd mod

--userdir mod
type UserDir = String --"directory-filename" ["directory-filename"] ...
--end userdir mod


