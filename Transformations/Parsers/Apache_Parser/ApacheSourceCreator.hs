module ApacheSourceCreator where

--{{{ Modules import
import TypeFiles.ApacheTypes
import TreeConfigApacheFiller
import TypesAndFunctions
--}}}

--{{{ Haskell import
import Text.Peggy
import Control.Monad
import Control.Monad.Reader
import Data.List.Split
--}}}

--{{{ Function that will contruct a Directory list by returning nothing if no Directory context found and returning a list of Directory if found
getDirectories :: TreeFile -> [String] ->Maybe [Directory]
getDirectories tree path =  if (isInChildren tree "Directory") then
	Just (map getDirectoryValues (giveTree [tree] path)) 
	else
	Nothing
--}}}

--{{{ Function that will construct a Directory type using the server node (will always be current node)
getDirectoryValues :: TreeFile -> Directory
getDirectoryValues tree = Directory {
	dMatch = if ((getValue tree "Match") == Just "True") then
		Just True else 
		Just False,
	dPath = getValue tree "Path",
	dDirDirectives = getDirDirectives tree,
	dRequireCons = getRequireCons tree ["Directory","Require"],
	dAllowOverride = getValue tree "AllowOverride",
	dAllowOverrideList = getValue tree "AllowOverrideList",
	dOptions = getValue tree "Options",
	dFiles = getFiles tree ["Directory","Files"]
}
--}}}

--{{{ Function that will contruct a RequireCons list by returning nothing if no Require context found and returning a list of RequireCons if found
getRequireCons :: TreeFile -> [String] -> Maybe [RequireCons]
getRequireCons tree path= if (isInChildren tree "Require") then
	Just (map getRequireConsValues (giveTree [tree] path)) 
	else
	Nothing
--}}}

--{{{ Function that will construct a RequireCons type using the server node (will always be current node)
getRequireConsValues :: TreeFile -> RequireCons
getRequireConsValues tree = RequireCons {
	rConsType = getValue tree "Path",
	rRequire = getValueList tree "Require",
	rRequireCons = getRequireCons tree ["Require","Require"]	
}
--}}}

--{{{ Function that will construct a DirDirectives type in the current node
getDirDirectives :: TreeFile -> Maybe DirDirectives
getDirDirectives tree = Just DirDirectives {
	dAcceptPathInfo = getValue tree "AcceptPathInfo",
	dAddDefaultCharset = getValue tree "AddDefaultCharset",
	dContentDigest = getValue tree "ContentDigest",
	dDefine = getValueList tree "Define",
	dEnableMMAP = getValue tree "EnableMMAP",
	dEnableSendFile = getValue tree "EnableSendFile",
	dError = getValue tree "Error",
	dErrorDocument = getValueList tree "ErrorDocument",
	dFileETag = getValue tree "FileETag",
	dForceType = getValue tree "ForceType",
	dHostnameLookups = getValue tree "HostnameLookups",
	dInclude = getValueList tree "Include",
	dIncludeOptional = getValueList tree "IncludeOptional",
	dLimitRequestBody = getValue tree "LimitRequestBody",
	dLimitXMLRequestBody = getValue tree "LimitXMLRequestBody",
	dLogLevel = getValue tree "LogLevel",
	dMaxRangeOverlaps = getValue tree "MaxRangeOverlaps",
	dMaxRangeReversals = getValue tree "MaxRangeReversals",
	dMaxRanges = getValue tree "MaxRanges",
	dRLimitCPU = getValue tree "RLimitCPU",
	dRLimitMEM = getValue tree "RLimitMEM",
	dRLimitNPROC = getValue tree "RLimitNPROC",
	dServerSignature = getValue tree "ServerSignature",
	dSetHandler = getValue tree "SetHandler",
	dSetInputFilter = getValue tree "SetInputFilter",
	dSetOutputFilter = getValue tree "SetOutputFilter",
	dUseCanonicalName = getValue tree "UseCanonicalName",
	dUseCanonicalPhysicalPort = getValue tree "UseCanonicalPhysicalPort",
	dRedirect = getValueList tree "Redirect",
	dRedirectMatch = getValueList tree "RedirectMatch",
	dRedirectPermanent = getValueList tree "RedirectPermanent",
	dRedirectTemp = getValueList tree "RedirectTemp",
	dAuthBasicAuthoritative = getValue tree "AuthBasicAuthoritative",
	dAuthBasicFake = getValue tree "AuthBasicFake",
	dAuthBasicProvider = getValue tree "AuthBasicProvider",
	dAuthBasicUseDigestAlgorithm = getValue tree "AuthBasicUseDigestAlgorithm",
	dAuthName = getValue tree "AuthName",
	dAuthType = getValue tree "AuthType",
	dAuthUserFile = getValue tree "AuthUserFile",
	dAuthMerging = getValue tree "AuthMerging",
	dAuthzSendForbiddenOnFailure = getValue tree "AuthzSendForbiddenOnFailure",
	dRequire = getValueList tree "Require",
	dAuthGroupFile = getValue tree "AuthGroupFile",
	dAddAlt = getValueList tree "AddAlt",
	dAddAltByEncoding = getValueList tree "AddAltByEncoding",
	dAddAltByType = getValueList tree "AddAltByType",
	dAddDescription = getValueList tree "AddDescription",
	dAddIcon = getValueList tree "AddIcon",
	dAddIconByEncoding = getValueList tree "AddIconByEncoding",
	dAddIconByType = getValueList tree "AddIconByType",
	dDefaultIcon = getValue tree "DefaultIcon",
	dHeaderName = getValue tree "HeaderName",
	dIndexHeadInsert = getValue tree "IndexHeadInsert",
	dIndexIgnore = getValueList tree "IndexIgnore",
	dIndexIgnoreReset = getValue tree "IndexIgnoreReset",
	dIndexOptions = getValueList tree "IndexOptions",
	dIndexOrderDefault = getValue tree "IndexOrderDefault",
	dIndexStyleSheet = getValue tree "IndexStyleSheet",
	dReadmeName = getValue tree "ReadmeName",
	dCGIDScriptTimeout = getValue tree "CGIDScriptTimeout",
	dDirectoryCheckHandler = getValue tree "DirectoryCheckHandler",
	dDirectoryIndex = getValue tree "DirectoryIndex",
	dIndexRedirect = getValue tree "IndexRedirect",
	dDirectorySlash = getValue tree "DirectorySlash",
	dFallbackResource = getValue tree "FallbackResource",
	dPassEnv = getValue tree "PassEnv",
	dSetEnv = getValueList tree "SetEnv",
	dUnsetEnv = getValueList tree "UnsetEnv",
	dAddOutputFilterByType = getValueList tree "AddOutputFilterByType",
	dFilterChain = getValueList tree "FilterChain",
	dFilterDeclare = getValueList tree "FilterDeclare",
	dFilterProtocol = getValueList tree "FilterProtocol",
	dFilterProvider = getValueList tree "FilterProvider",
	dFilterTrace = getValueList tree "FilterTrace",
	dImapBase = getValue tree "ImapBase",
	dImapDefault = getValue tree "ImapDefault",
	dImapMenu = getValue tree "ImapMenu",
	dCustomLog = getValueList tree "CustomLog",
	dLogFormat = getValueList tree "LogFormat",
	dTransferLog = getValueList tree "TransferLog",
	dAddCharset = getValueList tree "AddCharset",
	dAddEncoding = getValueList tree "AddEncoding",
	dAddHandler = getValueList tree "AddHandler",
	dAddInputFilter = getValueList tree "AddInputFilter",
	dAddLanguage = getValueList tree "AddLanguage",
	dAddOutputFilter = getValueList tree "AddOutputFilter",
	dAddType = getValueList tree "AddTypes",
	dDefaultLanguage = getValue tree "DefaultLanguage",
	dModMimeUsePathInfo = getValue tree "ModMimeUsePathInfo",
	dMultiviewsMatch = getValue tree "MultiviewsMatch",
	dRemoveCharset = getValueList tree "RemoveCharset",
	dRemoveEncoding = getValueList tree "RemoveEncoding",
	dRemoveHandler = getValueList tree "RemoveHandler",
	dRemoveInputFilter = getValueList tree "RemoveInputFilter",
	dRemoveLanguage = getValueList tree "RemoveLanguage",
	dRemoveOutputFilter = getValueList tree "RemoveOutputFilter",
	dRemoveType = getValueList tree "RemoveType",
	dForceLanguagePriority = getValue tree "ForceLanguagePriority",
	dLanguagePriority = getValue tree "LanguagePriority",
	dReflectorHeader = getValue tree "ReflectorHeader",
	dKeptBodySize = getValue tree "KeptBodySize",
	dBrowserMatch = getValueList tree "BrowserMatch",
	dBrowserMatchNoCase = getValueList tree "BrowserMatchNoCase",
	dSetEnvIf = getValueList tree "SetEnvIf",
	dSetEnvIfExpr = getValueList tree "SetEnvIfExpr",
	dSetEnvIfNoCase = getValueList tree "SetEnvIfNoCase",
	dSSLCipherSuite = getValue tree "SSLCipherSuite",
	dSSLOptions = getValue tree "SSLOptions",
	dSSLRenegBufferSize = getValue tree "SSLRenegBufferSize",
	dSSLRequireSSL = getValue tree "SSLRequireSSL",
	dSSLUserName = getValue tree "SSLUserName",
	dSSLVerifyClient = getValue tree "SSLVerifyClient",
	dSSLVerifyDepth = getValue tree "SSLVerifyDepth"
}
--}}}

--{{{ Function that will contruct a Files list by returning nothing if no Files context found and returning a list of Files if found
getFiles :: TreeFile -> [String] -> Maybe [Files]
getFiles tree path=  if (isInChildren tree "Files") then
	Just (map getFilesValues (giveTree [tree] path)) 
	else
	Nothing
--}}}

--{{{ Function that will construct a Files type using the server node (will always be current node)
getFilesValues :: TreeFile -> Files
getFilesValues tree = Files {
	fMatch = if ((getValue tree "Match") == Just "True") then
		Just True else 
		Just False,
	fFileName = getValue tree "Path",
	fDirDirectives = getDirDirectives tree,
	fRequireCons = getRequireCons tree ["Files","Require"]
}
--}}}

--{{{ Function that will contruct a Location list by returning nothing if no Location context found and returning a list of Location if found
getLocations :: TreeFile -> [String] -> Maybe [Location]
getLocations tree path = if (isInChildren tree "Location") then
	Just (map getLocationValues (giveTree [tree] path)) 
	else
	Nothing
--}}}

--{{{ Function that will construct a Location type using the server node (will always be current node)
getLocationValues :: TreeFile -> Location
getLocationValues tree = Location {
	lMatch= if ((getValue tree "Match") == Just "True") then
		Just True else 
		Just False,
	lPath = getValue tree "Path",
	lDirDirectives = getDirDirectives tree,
	lRequireCons = getRequireCons tree ["Location","Require"],
	lOptions = getValue tree "Options"
}
--}}}

--{{{ Function that will contruct a VirtualHost list by returning nothing if no VirtualHost context found and returning a list of VirtualHost if found
getVirtualHosts :: TreeFile -> Maybe [VirtualHost]
getVirtualHosts tree =  if (isInChildren tree "VirtualHost") then
	Just (map getVirtualHostValues (giveTree [tree] ["root","VirtualHost"])) 
	else
	Nothing
--}}}

--{{{ Function that will construct a VirtualHost type using the server node (will always be current node)
getVirtualHostValues :: TreeFile -> VirtualHost
getVirtualHostValues tree = VirtualHost {
	sVirtualHostAddress = Just (splitOn " " (noMaybe (getValue tree "Path"))),
	sAcceptPathInfo = getValue tree "AcceptPathInfo",
	sAccessFileName = getValue tree "AccessFileName",
	sAddDefaultCharset = getValue tree "AddDefaultCharset",
	sAllowEncodedSlashes = getValue tree "AllowEncodedSlashes",
	sContentDigest = getValue tree "ContentDigest",
	sDefine = getValueList tree "Define",
	sDirectory = getDirectories tree ["VirtualHost","Directory"],
	sDocumentRoot = getValue tree "DocumentRoot",
	sEnableMMAP = getValue tree "EnableMMAP",
	sEnableSendfile = getValue tree "EnableSendfile",
	sError = getValue tree "Error",
	sErrorDocument = getValueList tree "ErrorDocument",
	sErrorLog = getValue tree "ErrorLog",
	sErrorLogFormat = getValue tree "ErrorLogFormat",
	sFileETag = getValue tree "FileEtag",
	sFiles = getFiles tree ["VirtualHost","Files"],
	sHostnameLookups = getValue tree "HostnameLookups",
	sInclude = getValueList tree "Include",
	sIncludeOptional = getValueList tree "IncludeOptional",
	sKeepAlive = getValue tree "KeepAlive",
	sKeepAliveTimeout = getValue tree "KeepAliveTimeout",
	sLimitInternalRecursion = getValue tree "LimitInternalRecursion",
	sLimitRequestBody = getValue tree "LimitRequestBody",
	sLimitRequestFields = getValue tree "LimitRequestFields",
	sLimitRequestFieldSize = getValue tree "LimitRequestFieldSize",
	sLimitRequestLine = getValue tree "LimitRequestLine",
	sLimitXMLRequestBody = getValue tree "LimitXMLRequestBody",
	sLocation = getLocations tree ["VirtualHost","Location"],
	sLogLevel = getValue tree "LofLevel",
	sMaxKeepAliveRequests = getValue tree "MaxKeepAliveRequests",
	sMaxRangesOverlaps = getValue tree "MaxRangesOverlaps",
	sMaxRangesReversals = getValue tree "MaxRangesReversals",
	sMaxRanges = getValue tree "MaxRanges",
	sOptions = getValue tree "Options",
	sProtocol = getValue tree "Protocol",
	sRLimitCPU = getValue tree "RLimitCPU",
	sRLimitMEM = getValue tree "RLimitMEM",
	sRLimitNPROC = getValue tree "RlimitNPROC",
	sServerAdmin = getValue tree "ServerAdmin",
	sServerName = combineMaybeList (getValueList tree "ServerName") (getValueList tree "ServerAlias"),
	sServerPath = getValue tree "ServerPath",
	sServerSignature = getValue tree "ServerSignature",
	sSetHandler = getValue tree "SetHandler",
	sSetInputFilter = getValue tree "SetInputFilter",
	sSetOutputFilter = getValue tree "SetOutputFilter",
	sTimeOut = getValue tree "TimeOut",
	sTraceEnable = getValue tree "TraceEnable",
	sUseCanonicalName = getValue tree "UseCanonicalName",
	sUseCanonicalPhysicalPort = getValue tree "UseCanonicalPhysicalPort",
	sAlias = getValueList tree "Alias",
	sAliasMatch = getValueList tree "AliasMatch",
	sRedirect = getValueList tree "Redirect",
	sRedirectMatch = getValueList tree "RedirectMatch",
	sRedirectPermanent = getValueList tree "RedirectPermanent",
	sRedirectTemp = getValueList tree "RedirectTemp",
	sScriptAlias = getValueList tree "ScriptAlias",
	sScriptAliasMatch = getValueList tree "ScriptAliasMatch",
	sAddAlt = getValueList tree "AddAlt",
	sAddAltByEncoding = getValueList tree "AddAltByEncoding",
	sAddAltByType = getValueList tree "AddAltByType",
	sAddDescription = getValueList tree "AddDescription",
	sAddIcon = getValueList tree "AddIcon",
	sAddIconByEncoding = getValueList tree "AddIconByEncoding",
	sAddIconByType = getValueList tree "AddIconByType",
	sDefaultIcon = getValue tree "DefaultIcon",
	sHeaderName = getValue tree "HeaderName",
	sIndexHeadInsert = getValue tree "IndexHeadInsert",
	sIndexIgnore = getValueList tree "IndexIgnore",
	sIndexIgnoreReset = getValue tree "IndexIgnoreReset",
	sIndexOptions = getValueList tree "IndexOptions",
	sIndexOrderDefault = getValue tree "IndexOrderDefault",
	sIndexStyleSheet = getValue tree "IndexStyleSheet",
	sReadmeName = getValue tree "ReadmeName",
	sScriptLog = getValue tree "ScriptLog",
	sScriptLogBuffer = getValue tree "ScriptLogBuffer",
	sScriptLogLength = getValue tree "ScriptLogLength",
	sCGIDScriptTimeout = getValue tree "CGIDScriptTimeout",
	sDirectoryCheckHandler = getValue tree "DirectoryCheckHandler",
	sDirectoryIndex = getValue tree "DirectoryIndex",
	sIndexRedirect = getValue tree "IndexRedirect",
	sDirectorySlash = getValue tree "DirectorySlash",
	sFallbackResource = getValue tree "FallbackResource",
	sPassEnv = getValue tree "PassEnv",
	sSetEnv = getValueList tree "SetEnv",
	sUnsetEnv = getValueList tree "UnsetEnv",
	sAddOutputFilterByType = getValueList tree "AddOutputFilterByType",
	sFilterChain = getValueList tree "FilterChain",
	sFilterDeclare = getValueList tree "FilterDeclare",
	sFilterProtocol = getValueList tree "FilterProtocol",
	sFilterProvider = getValueList tree "FilterProvider",
	sFilterTrace = getValueList tree "FilterTrace",
	sImapBase = getValue tree "ImapBase",
	sImapDefault = getValue tree "ImapDefault",
	sImapMenu = getValue tree "ImapMenu",
	sCustomLog = getValueList tree "CustomLog",
	sLogFormat = getValueList tree "LogFormat",
	sTransferLog = getValueList tree "TransferLog",
	sAddCharset = getValueList tree "AddCharset",
	sAddEncoding = getValueList tree "AddEncoding",
	sAddHandler = getValueList tree "AddHandler",
	sAddInputFilter = getValueList tree "AddInputFilter",
	sAddLanguage = getValueList tree "AddLanguage",
	sAddOutputFilter = getValueList tree "AddOutputFilter",
	sAddType = getValueList tree "AddType",
	sDefaultLanguage = getValue tree "DefaultLanguage",
	sMultiviewsMatch = getValue tree "MultiviewsMatch",
	sRemoveCharset = getValueList tree "RemoveCharset",
	sRemoveEncoding = getValueList tree "RemoveEncoding",
	sRemoveHandler = getValueList tree "RemoveHandler",
	sRemoveInputFilter = getValueList tree "RemoveInputFilter",
	sRemoveLanguage = getValueList tree "RemoveLanguage",
	sRemoveOutputFilter = getValueList tree "RemoveOutputFilter",
	sRemoveType = getValueList tree "RemoveType",
	sCacheNegotiatedDocs = getValue tree "CacheNegotiatedDocs",
	sForceLanguagePriority = getValue tree "ForceLanguagePriority",
	sLanguagePriority = getValue tree "LanguagePriority",
	sReflectorHeader = getValue tree "ReflectorHeader",
	sBrowserMatch = getValueList tree "BrowserMatch",
	sBrowserMatchNoCase = getValueList tree "BrowserMatchNoCase",
	sSetEnvIf = getValueList tree "SetEnvIf",
	sSetEnvIfExpr = getValueList tree "SetEnvIfExpr",
	sSetEnvIfNoCase = getValueList tree "SetEnvIfNoCase",
	sUserDir = getValue tree "UserDir",
	sSSLCACertificateFile = getValue tree "SSLCACertificateFile",
	sSSLCACertificatePath = getValue tree "SSLCACertificatePath",
	sSSLCADNRequestFile = getValue tree "SSLCADNRequestFile",
	sSSLCADNRequestPath = getValue tree "SSLCADNRequestPath",
	sSSLCARevocationCheck = getValue tree "SSLCARevocationCheck",
	sSSLCARevocationFile = getValue tree "SSLCARevocationFile",
	sSSLCARevocationPath = getValue tree "SSLCARevocationPath",
	sSSLCertificateChainFile = getValue tree "SSLCertificateChainFile",
	sSSLCertificateFile = getValue tree "SSLCertificateFile",
	sSSLCertificateKeyFile = getValue tree "SSLCertificateKeyFile",
	sSSLCipherSuite = getValue tree "SSLCipherSuite",
	sSSLCompression = getValue tree "SSLCompression",
	sSSLEngine = getValue tree "SSLEngine",
	sSSLHonorCipherOrder = getValue tree "SSLHonorCipherOrder",
	sSSLInsecureRenegotiation = getValue tree "SSLInsecureRenegotiation",
	sSSLOCSPDefaultResponder = getValue tree "SSLOCSPDefaultResponder",
	sSSLOCSPEnable = getValue tree "SSLOCSPEnable",
	sSSLOCSPOverrideResponder = getValue tree "SSLOCSPOverrideResponder",
	sSSLOCSPResponderTimeout = getValue tree "SSLOCSPResponderTimeout",
	sSSLOCSPResponseMaxAge = getValue tree "SSLOCSPResponseMaxAge",
	sSSLOCSPResponseTimeSkew = getValue tree "SSLOCSPResponseTimeSkew",
	sSSLOCSPUseRequestNonce = getValue tree "SSLOCSPUseRequestNonce",
	sSSLOpenSSLConfCmd = getValueList tree "SSLOpenSSLConfCmd",
	sSSLOptions = getValue tree "SSLOptions",
	sSSLProtocol = getValue tree "SSProtocol",
	sSSLSessionCacheTimeout = getValue tree "SSLSessionCacheTimeout",
	sSSLSessionTicketKeyFile = getValue tree "SSLSessionTicketKeyFile",
	sSSLSessionTickets = getValue tree "SSLSessionTickets",
	sSSLSRPUnknownUserSeed = getValue tree "SSLSRPUnknownUserSeed",
	sSSLSRPVerifierFile = getValue tree "SSLSRPVerifierFile",
	sSSLStaplingErrorCacheTimeout = getValue tree "SSLStaplingErrorCacheTimeout",
	sSSLStaplingFakeTryLater = getValue tree "SSLStaplingFakeTryLater",
	sSSLStaplingForceURL = getValue tree "SSLStaplingForceURL",
	sSSLStaplingResponderTimeout = getValue tree "SSLStaplingResponderTimeout",
	sSSLStaplingResponseMaxAge = getValue tree "SSLStaplingResponseMaxAge",
	sSSLStaplingResponseTimeSkew = getValue tree "SSLStaplingResponseTimeSkew",
	sSSLStaplingReturnResponderErrors = getValue tree "SSLStaplingResponderErrors",
	sSSLStaplingStandardCacheTimeout = getValue tree "SSLStaplingStandardCacheTimeout",
	sSSLStrictSNIVHostCheck = getValue tree "SSLStrictSNIVHostCheck",
	sSSLUseStapling = getValue tree "SSLUseStapling",
	sSSLVerifyClient = getValue tree "SSLVerifyClient",
	sSSLVerifyDepth = getValue tree "SSLVerifyDepth" 
}
--}}}

--{{{ Function that will put the Tree into the source type
createSourceApache :: TreeFile -> ApacheWebserver
createSourceApache tree = ApacheWebserver {
	aAcceptFilter = getValueList tree "AcceptFilter",
	aAcceptPathInfo = getValue tree "AcceptPathInfo",
	aAccessFileName = getValue tree "AccessFileName",
	aAddDefaultCharset = getValue tree "AddDefaultCharset",
	aAllowEncodedSlashes = getValue tree "AllowEncodedSlashes",
	aContentDigest = getValue tree "ContentDigest",
	aDefaultRuntimeDir = getValue tree "DefaultRuntimeDir",
	aDefine = getValueList tree "Define",
	aDirectory = getDirectories tree ["root","Directory"],
	aDocumentRoot = getValue tree "DocumentRoot",
	aEnableMMAP = getValue tree "EnableMMAP",
	aEnableSendfile = getValue tree "EnableSendFile",
	aError = getValue tree "Error",
	aErrorDocument = getValueList tree "ErrorDocument",
	aErrorLog = getValue tree "ErrorLog",
	aErrorLogFormat = getValue tree "ErrorLogFormat",
	aExtendedStatus = getValue tree "ExtendedStatus",
	aFileEtag = getValue tree "FileEtag",
	aFiles = getFiles tree ["root","Files"],
	aHostnameLookups = getValue tree "HostnameLookups",
	aInclude = getValueList tree "Include",
	aIncludeOptional = getValueList tree "IncludeOptional",
	aKeepAlive = getValue tree "KeepAlive",
	aKeepAliveTimeout = getValue tree "KeepAliveTimeout",
	aLimitInternalRecursion = getValue tree "LimitInternalRecursion",
	aLimitRequestBody = getValue tree "LimitRequestBody",
	aLimitRequestFields = getValue tree "LimitRequestFields",
	aLimitRequestFieldSize = getValue tree "LimitRequestFieldSize",
	aLimitRequestLine = getValue tree "LimitRequestLine",
	aLimitXMLRequestBody = getValue tree "LimitXMLRequestBody",
	aLocation = getLocations tree ["root","Location"],
	aLogLevel = getValue tree "LogLevel",
	aMaxKeepAliveRequests = getValue tree "MaxKeepAliveRequests",
	aMaxRangeOverlaps = getValue tree "MaxRangeOverlaps",
	aMaxRangeReversals = getValue tree "MaxRangeReversals",
	aMaxRanges = getValue tree "MaxRange",
	aMutex = getValueList tree "Mutex",
	aOptions = getValue tree "Options",
	aProtocol = getValue tree "Protocol",
	aRLimitCPU = getValue tree "RLimitCPU",
	aRLimitMEM = getValue tree "RLimitMEM",
	aRLimitNPROC = getValue tree "RLimitNPROC",
	aSeeRequestTail = getValue tree "SeeRequestTail",
	aServerAdmin = getValue tree "ServerAdmin",
	aServerName = getValue tree "ServerName",
	aServerRoot = getValue tree "ServerRoot",
	aServerSignature = getValue tree "ServerSignature",
	aServerTokens = getValue tree "ServerTokens",
	aSetHandler = getValue tree "SetHandler",
	aSetInputFilter = getValue tree "SetInputFilter",
	aSetOutputFilter = getValue tree "SetOutputFilter",
	aTimeOut = getValue tree "Timeout",
	aTraceEnable = getValue tree "TraceEnable",
	aUnDefine = getValueList tree "UnDefine",
	aUseCanonicalName = getValue tree "UseCanonicalName",
	aUseCanonicalPhysicalPort = getValue tree "UseCanonicalPhysicalPort",
	aGracefulShutdownTimeout = getValue tree "GracefulShutdownTimeout",
	aListen = getValueList tree "Listen",
	aListenBackLog = getValue tree "ListenBackLog",
	aMaxConnectionsPerChild = getValue tree "MaxConnectionsPerChild",
	aMaxMemFree = getValue tree "MaxMemFree",
	aMaxRequestWorkers = getValue tree "MaxRequestWorkers",
	aPidFile = getValue tree "PidFile",
	aReceiveBufferSize = getValue tree "ReceiveBufferSize",
	aScoreBoardFile = getValue tree "ScoreBoardFile",
	aSendBufferSize = getValue tree "SendBufferSize",
	aServerLimit = getValue tree "ServerLimit",
	aStartServers = getValue tree "StartServers",
	aMaxSpareServers = getValue tree "MaxSpareServers",
	aMinSpareServers = getValue tree "MinSpareServers",
	aAlias = getValueList tree "Alias",
	aAliasMatch = getValueList tree "AliasMatch",
	aRedirect = getValueList tree "Redirect",
	aRedirectMatch = getValueList tree "RedirectMatch",
	aRedirectPermanent = getValueList tree "RedirectPermanent",
	aRedirectTemp = getValueList tree "RedirectTemp",
	aScriptAlias = getValueList tree "ScriptAlias",
	aScriptAliasMatch = getValueList tree "ScriptAliasMatch",
	aAddAlt = getValueList tree "AddAlt",
	aAddAltByEncoding = getValueList tree "AddAltByEncoding",
	aAddAltByType = getValueList tree "AddAltByType",
	aAddDescription = getValueList tree "AddDescription",
	aAddIcon = getValueList tree "AddIcon",
	aAddIconByEncoding = getValueList tree "AddIconByEncoding",
	aAddIconByType = getValueList tree "AddIconByType",
	aDefaultIcon = getValue tree "DefaultIcon",
	aHeaderName = getValue tree "HeaderName",
	aIndexHeadInsert = getValue tree "IndexHeadInsert",
	aIndexIgnore = getValueList tree "IndexIgnore",
	aIndexIgnoreReset = getValue tree "IndexIgnoreReset",
	aIndexOptions = getValueList tree "IndexOptions",
	aIndexOrderDefault = getValue tree "IndexOrderDefault",
	aIndexStyleSheet = getValue tree "IndexStyleSheet",
	aReadmeName = getValue tree "ReadmeName",
	aScriptLog = getValue tree "ScriptLog",
	aScriptLogBuffer = getValue tree "ScriptLogBuffer",
	aScriptLogLength = getValue tree "ScriptLogLength",
	aScriptSock = getValue tree "ScriptSock",
	aCGIDScriptTimeout = getValue tree "CGIDScriptTimeout",
	aDirectoryCheckHandler = getValue tree "DirectoryCheckHandler",
	aDirectoryIndex = getValue tree "DirectoryIndex",
	aIndexRedirect = getValue tree "IndexRedirect",
	aDirectorySlash = getValue tree "DirectorySlash",
	aFallbackResource = getValue tree "FallbackResource",
	aPassEnv = getValue tree "PassEnv",
	aSetEnv = getValueList tree "SetEnv",
	aUnsetEnv = getValueList tree "UnsetEnv",
	aAddOutputFilterByType = getValueList tree "AddOutputFilterByType",
	aFilterChain = getValueList tree "FilterChain",
	aFilterDeclare = getValueList tree "FilterDeclare",
	aFilterProtocol = getValueList tree "FilterProtocol",
	aFilterProvider = getValueList tree "FilterProvider",
	aFilterTrace = getValueList tree "FilterTrace",
	aImapBase = getValue tree "ImapBase",
	aImapDefault = getValue tree "ImapDefault",
	aImapMenu = getValue tree "ImapMenu",
	aBufferedLogs = getValue tree "BufferedLogs",
	aCustomLog = getValueList tree "CustomLog",
	aLogFormat = getValueList tree "LogFormat",
	aTransferLog = getValueList tree "TransferLog",
	aAddCharset = getValueList tree "AddCharset",
	aAddEncoding = getValueList tree "AddEncoding",
	aAddHandler = getValueList tree "AddHandler",
	aAddInputFilter = getValueList tree "AddInputFilter",
	aAddLanguage = getValueList tree "AddLanguage",
	aAddOutputFilter = getValueList tree "OutputFilter",
	aAddType = getValueList tree "AddType",
	aDefaultLanguage = getValue tree "DefaultLanguage",
	aMultiviewsMatch = getValue tree "MultiviewsMatch",
	aTypesConfig = getValue tree "TypesConfig",
	aCacheNegotiatedDocs = getValue tree "CacheNegotiateDocs",
	aForceLanguagePriority = getValue tree "ForceLanguagePriority",
	aLanguagePriority = getValue tree "LanguagePriority",
	aReflectorHeader = getValue tree "ReflectorHeader",
	aBrowserMatch = getValueList tree "BrowserMatch",
	aBrowserMatchNoCase = getValueList tree "BrowserMatchNoCase",
	aSetEnvIf = getValueList tree "SetEnvIf",
	aSetEnvIfExpr = getValueList tree "SetEnvIfExpr",
	aSetEnvIfNoCase = getValueList tree "SetEnvIfNoCase",
	aChrootDir = getValue tree "ChrootDir",
	aGroup = getValue tree "Group",
	aSuexec = getValue tree "Suexec",
	aUser = getValue tree "User",
	aUserDir = getValue tree "UserDir",
	aSSLCACertificateFile = getValue tree "SSLCACertificateFile",
	aSSLCACertificatePath = getValue tree "SSLCACertificatePath",
	aSSLCADNRequestFile = getValue tree "SSLCADNRequestFile",
	aSSLCADNRequestPath = getValue tree "SSLCADNRequestPath",
	aSSLCARevocationCheck = getValue tree "SSLCARevocationCheck",
	aSSLCARevocationFile = getValue tree "SSLCARevocationFile",
	aSSLCARevocationPath = getValue tree "SSLCARevocationPath",
	aSSLCertificateChainFile = getValue tree "SSLCertificateChainFile",
	aSSLCertificateFile = getValue tree "SSLCertificateFile",
	aSSLCertificateKeyFile = getValue tree "SSLCertificateKeyFile",
	aSSLCipherSuite = getValue tree "SSLCipherSuite",
	aSSLCompression = getValue tree "SSLCompression",
	aSSLEngine = getValue tree "SSLEngine",
	aSSLHonorCipherOrder = getValue tree "SSLHonorCipherOrder",
	aSSLInsecureRenegotiation = getValue tree "SSLInsecureRenegotiation",
	aSSLOCSPDefaultResponder = getValue tree "SSLOCSPDefaultResponder",
	aSSLOCSPEnable = getValue tree "SSLOCSPEnable",
	aSSLOCSPOverrideResponder = getValue tree "SSLOCSPOverrideResponder",
	aSSLOCSPResponderTimeout = getValue tree "SSLOCSPResponderTimeout",
	aSSLOCSPResponseMaxAge = getValue tree "SSLOCSPResponderMaxAge",
	aSSLOCSPResponseTimeSkew = getValue tree "SSLOCSPResponseTimeSkew",
	aSSLOCSPUseRequestNonce = getValue tree "SSLOCSPUseRequestNonce",
	aSSLOpenSSLConfCmd = getValueList tree "SSLOpenSSLconfCmd",
	aSSLOptions = getValue tree "SSLOptions",
	aSSLProtocol = getValue tree "SSLProtocol",
	aSSLSessionCacheTimeout = getValue tree "SSLSessionCacheTimeout",
	aSSLSessionTicketKeyFile = getValue tree "SSLSessionTicketKeyFile",
	aSSLSessionTickets = getValue tree "SSLSessionTickets",
	aSSLSRPUnknownUserSeed = getValue tree "SSLSRUnknownUserSeed",
	aSSLSRPVerifierFile = getValue tree "SSLSRVerifierFile",
	aSSLStaplingErrorCacheTimeout = getValue tree "SSLStaplingErrorCacheTimeout",
	aSSLStaplingFakeTryLater = getValue tree "SSLStaplingFakeTryLater",
	aSSLStaplingForceURL = getValue tree "SSLStaplingForceURL",
	aSSLStaplingResponderTimeout = getValue tree "SSLStaplingResponderTimeout",
	aSSLStaplingResponseMaxAge = getValue tree "SSLStaplingResponseMaxAge",
	aSSLStaplingResponseTimeSkew = getValue tree "SSLStaplingResponseTimeSkew",
	aSSLStaplingReturnResponderErrors = getValue tree "SSLStaplingReturnResponderErrors",
	aSSLStaplingStandardCacheTimeout = getValue tree "SSLStaplingStandardCacheTimeout",
	aSSLStrictSNIVHostCheck = getValue tree "SSLStrictSNIVHostCheck",
	aSSLUseStapling = getValue tree "SSLUseStapling",
	aSSLVerifyClient = getValue tree "SSLVerifyClient",
	aSSLVerifyDepth = getValue tree "SSLVerifyDepth",
	aSSLCryptoDevice = getValue tree "SSLCryptoDevice",
	aSSLFIPS = getValue tree "SSLFIPS",
	aSSLPassPhraseDialog = getValue tree "SSLPassPhraseDialog",
	aSSLRandomSeed = getValue tree "SSLRandomSeed",
	aSSLSessionCache = getValue tree "SSLSessionCache",
	aSSLStaplingCache = getValue tree "SSLStaplingCache",
	aSSLUserName = getValue tree "SSLUserName",
	aVirtualHosts = getVirtualHosts tree
}
--}}}

--{{{ Function that combine 2 lists with the maybe monad
combineMaybeList :: Maybe [a] -> Maybe [a] -> Maybe [a]
combineMaybeList Nothing Nothing = Nothing
combineMaybeList Nothing list = list
combineMaybeList list Nothing = list
combineMaybeList (Just list1) (Just list2) = Just (list1++list2)
--}}}

--{{{ Function that print the resulting source
printSourceApache :: IO ()
printSourceApache = parseTreeApache "apache.conf" >>= \(Right tree) -> print (createSourceApache tree)
--}}}
