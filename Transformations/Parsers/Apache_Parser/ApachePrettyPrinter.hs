module ApachePrettyPrinter where

--{{{ Modules imports
import TreeConfigApacheFiller
import ApacheSourceCreator
import TypeFiles.ApacheTypes
import TypesAndFunctions
--}}}

--{{{ Haskell imports
import Text.PrettyPrint
--}}}

--{{{ Function that will pretty print the ApacheWebserver
printApache :: ApacheWebserver -> Doc
printApache source = printMaybe
	(printListInstruction "AcceptFilter")
	(aAcceptFilter source)
	$$ printMaybe
	(printSimpleInstruction "AcceptPathInfo")
	(aAcceptPathInfo source)
	$$ printMaybe
	(printSimpleInstruction "AccessFileName")
	(aAccessFileName source)
	$$ printMaybe
	(printSimpleInstruction "AddDefaultCharset")
	(aAddDefaultCharset source)
	$$ printMaybe
	(printSimpleInstruction "AllowEncodedSlashes")
	(aAllowEncodedSlashes source)
	$$ printMaybe
	(printSimpleInstruction "ContentDigest")
	(aContentDigest source)
	$$ printMaybe
	(printSimpleInstruction "DefaultRuntimeDir")
	(aDefaultRuntimeDir source)
	$$ printMaybe
	(printListInstruction "Define")
	(aDefine source)
	$$ printMaybe
	(printSimpleInstruction "DocumentRoot")
	(aDocumentRoot source)
	$$ printMaybe
	(printSimpleInstruction "EnableMMAP")
	(aEnableMMAP source)
	$$ printMaybe
	(printSimpleInstruction "EnableSendfile")
	(aEnableSendfile source)
	$$ printMaybe
	(printSimpleInstruction "Error")
	(aError source)
	$$ printMaybe
	(printListInstruction "ErrorDocument")
	(aErrorDocument source)
	$$ printMaybe
	(printSimpleInstruction "ErrorLog")
	(aErrorLog source)
	$$ printMaybe
	(printSimpleInstruction "ErrorLogFormat")
	(aErrorLogFormat source)
	$$ printMaybe
	(printSimpleInstruction "ExtendedStatus")
	(aExtendedStatus source)
	$$ printMaybe
	(printSimpleInstruction "FileEtag")
	(aFileEtag source)
	$$ printMaybe
	(printSimpleInstruction "HostnameLookups")
	(aHostnameLookups source)
	$$ printMaybe
	(printListInstruction "Include")
	(aInclude source)
	$$ printMaybe
	(printListInstruction "IncludeOptional")
	(aIncludeOptional source)
	$$ printMaybe
	(printSimpleInstruction "KeepAlive")
	(aKeepAlive source)
	$$ printMaybe
	(printSimpleInstruction "KeepAliveTimeout")
	(aKeepAliveTimeout source)
	$$ printMaybe
	(printSimpleInstruction "LimitInternalRecursion")
	(aLimitInternalRecursion source)
	$$ printMaybe
	(printSimpleInstruction "LimitRequestBody")
	(aLimitRequestBody source)
	$$ printMaybe
	(printSimpleInstruction "LimitRequestFields")
	(aLimitRequestFields source)
	$$ printMaybe
	(printSimpleInstruction "LimitRequestFieldSize")
	(aLimitRequestFieldSize source)
	$$ printMaybe
	(printSimpleInstruction "LimitRequestLine")
	(aLimitRequestLine source)
	$$ printMaybe
	(printSimpleInstruction "LimitXMLRequestBody")
	(aLimitXMLRequestBody source)
	$$ printMaybe
	(printSimpleInstruction "LogLevel")
	(aLogLevel source)
	$$ printMaybe
	(printSimpleInstruction "MaxKeepAliveRequests")
	(aMaxKeepAliveRequests source)
	$$ printMaybe
	(printSimpleInstruction "MaxRangeOverlaps")
	(aMaxRangeOverlaps source)
	$$ printMaybe
	(printSimpleInstruction "MaxRangeReversals")
	(aMaxRangeReversals source)
	$$ printMaybe
	(printSimpleInstruction "MaxRanges")
	(aMaxRanges source)
	$$ printMaybe
	(printListInstruction "Mutex")
	(aMutex source)
	$$ printMaybe
	(printSimpleInstruction "Options")
	(aOptions source)
	$$ printMaybe
	(printSimpleInstruction "Protocol")
	(aProtocol source)
	$$ printMaybe
	(printSimpleInstruction "RLimitCPU")
	(aRLimitCPU source)
	$$ printMaybe
	(printSimpleInstruction "RLimitMEM")
	(aRLimitMEM source)
	$$ printMaybe
	(printSimpleInstruction "RLimitNPROC")
	(aRLimitNPROC source)
	$$ printMaybe
	(printSimpleInstruction "SeeRequestTail")
	(aSeeRequestTail source)
	$$ printMaybe
	(printSimpleInstruction "ServerAdmin")
	(aServerAdmin source)
	$$ printMaybe
	(printSimpleInstruction "ServerName")
	(aServerName source)
	$$ printMaybe
	(printSimpleInstruction "ServerRoot")
	(aServerRoot source)
	$$ printMaybe
	(printSimpleInstruction "ServerSignature")
	(aServerSignature source)
	$$ printMaybe
	(printSimpleInstruction "ServerTokens")
	(aServerTokens source)
	$$ printMaybe
	(printSimpleInstruction "SetHandler")
	(aSetHandler source)
	$$ printMaybe
	(printSimpleInstruction "SetInputFilter")
	(aSetInputFilter source)
	$$ printMaybe
	(printSimpleInstruction "SetOutputFilter")
	(aSetOutputFilter source)
	$$ printMaybe
	(printSimpleInstruction "TimeOut")
	(aTimeOut source)
	$$ printMaybe
	(printSimpleInstruction "TraceEnable")
	(aTraceEnable source)
	$$ printMaybe
	(printListInstruction "UnDefine")
	(aUnDefine source)
	$$ printMaybe
	(printSimpleInstruction "UseCanonicalName")
	(aUseCanonicalName source)
	$$ printMaybe
	(printSimpleInstruction "UseCanonicalPhysicalPort")
	(aUseCanonicalPhysicalPort source)
	$$ printMaybe
	(printSimpleInstruction "GracefulShutDownTimeout")
	(aGracefulShutdownTimeout source)
	$$ printMaybe
	(printListInstruction "Listen")
	(aListen source)
	$$ printMaybe
	(printSimpleInstruction "ListenBackLog")
	(aListenBackLog source)
	$$ printMaybe
	(printSimpleInstruction "MaxConnectionsPerChild")
	(aMaxConnectionsPerChild source)
	$$ printMaybe
	(printSimpleInstruction "MaxMemFree")
	(aMaxMemFree source)
	$$ printMaybe
	(printSimpleInstruction "MaxRequestWorkers")
	(aMaxRequestWorkers source)
	$$ printMaybe
	(printSimpleInstruction "PidFile")
	(aPidFile source)
	$$ printMaybe
	(printSimpleInstruction "ReceiveBufferSize")
	(aReceiveBufferSize source)
	$$ printMaybe
	(printSimpleInstruction "ScoreBoardFile")
	(aScoreBoardFile source)
	$$ printMaybe
	(printSimpleInstruction "SendBufferSize")
	(aSendBufferSize source)
	$$ printMaybe
	(printSimpleInstruction "ServerLimit")
	(aServerLimit source)
	$$ printMaybe
	(printSimpleInstruction "StartServers")
	(aStartServers source)
	$$ printMaybe
	(printSimpleInstruction "MaxSpareServers")
	(aMaxSpareServers source)
	$$ printMaybe
	(printSimpleInstruction "MinSpareServers")
	(aMinSpareServers source)
	$$ printMaybe
	(printListInstruction "Alias")
	(aAlias source)
	$$ printMaybe
	(printListInstruction "AliasMatch")
	(aAliasMatch source)
	$$ printMaybe
	(printListInstruction "Redirect")
	(aRedirect source)
	$$ printMaybe
	(printListInstruction "RedirectMatch")
	(aRedirectMatch source)
	$$ printMaybe
	(printListInstruction "RedirectPermanent")
	(aRedirectPermanent source)
	$$ printMaybe
	(printListInstruction "RedirectTemp")
	(aRedirectTemp source)
	$$ printMaybe
	(printListInstruction "ScriptAlias")
	(aScriptAlias source)
	$$ printMaybe
	(printListInstruction "ScriptAliasMatch")
	(aScriptAliasMatch source)
	$$ printMaybe
	(printListInstruction "AddAlt")
	(aAddAlt source)
	$$ printMaybe
	(printListInstruction "AddAltByEncoding")
	(aAddAltByEncoding source)
	$$ printMaybe
	(printListInstruction "AddAltByType")
	(aAddAltByType source)
	$$ printMaybe
	(printListInstruction "AddDescription")
	(aAddDescription source)
	$$ printMaybe
	(printListInstruction "AddIcon")
	(aAddIcon source)
	$$ printMaybe
	(printListInstruction "AddIconByEncoding")
	(aAddIconByEncoding source)
	$$ printMaybe
	(printListInstruction "AddIconByType")
	(aAddIconByType source)
	$$ printMaybe
	(printSimpleInstruction "DefaultIcon")
	(aDefaultIcon source)
	$$ printMaybe
	(printSimpleInstruction "HeaderName")
	(aHeaderName source)
	$$ printMaybe
	(printSimpleInstruction "IndexHeadInsert")
	(aIndexHeadInsert source)
	$$ printMaybe
	(printListInstruction "IndexIgnore")
	(aIndexIgnore source)
	$$ printMaybe
	(printSimpleInstruction "IndexIgnoreReset")
	(aIndexIgnoreReset source)
	$$ printMaybe
	(printListInstruction "IndexOptions")
	(aIndexOptions source)
	$$ printMaybe
	(printSimpleInstruction "IndexOrderDefault")
	(aIndexOrderDefault source)
	$$ printMaybe
	(printSimpleInstruction "IndexStyleSheet")
	(aIndexStyleSheet source)
	$$ printMaybe
	(printSimpleInstruction "ReadmeName")
	(aReadmeName source)
	$$ printMaybe
	(printSimpleInstruction "ScriptLog")
	(aScriptLog source)
	$$ printMaybe
	(printSimpleInstruction "ScriptLogBuffer")
	(aScriptLogBuffer source)
	$$ printMaybe
	(printSimpleInstruction "ScriptLogLength")
	(aScriptLogLength source)
	$$ printMaybe
	(printSimpleInstruction "ScriptSock")
	(aScriptSock source)
	$$ printMaybe
	(printSimpleInstruction "CGIDScriptTimeout")
	(aCGIDScriptTimeout source)
	$$ printMaybe
	(printSimpleInstruction "DirectoryCheckHandler")
	(aDirectoryCheckHandler source)
	$$ printMaybe
	(printSimpleInstruction "DirectoryIndex")
	(aDirectoryIndex source)
	$$ printMaybe
	(printSimpleInstruction "IndexRedirect")
	(aIndexRedirect source)
	$$ printMaybe
	(printSimpleInstruction "DirectorySlash")
	(aDirectorySlash source)
	$$ printMaybe
	(printSimpleInstruction "FallbackResource")
	(aFallbackResource source)
	$$ printMaybe
	(printSimpleInstruction "PassEnv")
	(aPassEnv source)
	$$ printMaybe
	(printListInstruction "SetEnv")
	(aSetEnv source)
	$$ printMaybe
	(printListInstruction "UnsetEnv")
	(aUnsetEnv source)
	$$ printMaybe
	(printListInstruction "AddOutputFilterByType")
	(aAddOutputFilterByType source)
	$$ printMaybe
	(printListInstruction "FilterChain")
	(aFilterChain source)
	$$ printMaybe
	(printListInstruction "FilterDeclare")
	(aFilterDeclare source)
	$$ printMaybe
	(printListInstruction "FilterProtocol")
	(aFilterProtocol source)
	$$ printMaybe
	(printListInstruction "FilterProvider")
	(aFilterProvider source)
	$$ printMaybe
	(printListInstruction "FilterTrace")
	(aFilterTrace source)
	$$ printMaybe
	(printSimpleInstruction "ImapBase")
	(aImapBase source)
	$$ printMaybe
	(printSimpleInstruction "ImapDefault")
	(aImapDefault source)
	$$ printMaybe
	(printSimpleInstruction "ImapMenu")
	(aImapMenu source)
	$$ printMaybe
	(printSimpleInstruction "BufferedLogs")
	(aBufferedLogs source)
	$$ printMaybe
	(printListInstruction "CustomLog")
	(aCustomLog source)
	$$ printMaybe
	(printListInstruction "LogFormat")
	(aLogFormat source)
	$$ printMaybe
	(printListInstruction "TransferLog")
	(aTransferLog source)
	$$ printMaybe
	(printListInstruction "AddCharset")
	(aAddCharset source)
	$$ printMaybe
	(printListInstruction "AddEncoding")
	(aAddEncoding source)
	$$ printMaybe
	(printListInstruction "AddHandler")
	(aAddHandler source)
	$$ printMaybe
	(printListInstruction "AddInputFilter")
	(aAddInputFilter source)
	$$ printMaybe
	(printListInstruction "AddLanguage")
	(aAddLanguage source)
	$$ printMaybe
	(printListInstruction "AddOutputFilter")
	(aAddOutputFilter source)
	$$ printMaybe
	(printListInstruction "AddType")
	(aAddType source)
	$$ printMaybe
	(printSimpleInstruction "DefaultLanguage")
	(aDefaultLanguage source)
	$$ printMaybe
	(printSimpleInstruction "MultiviewsMatch")
	(aMultiviewsMatch source)
	$$ printMaybe
	(printSimpleInstruction "TypesConfig")
	(aTypesConfig source)
	$$ printMaybe
	(printSimpleInstruction "CacheNegotiatedDocs")
	(aCacheNegotiatedDocs source)
	$$ printMaybe
	(printSimpleInstruction "ForceLanguagePriority")
	(aForceLanguagePriority source)
	$$ printMaybe
	(printSimpleInstruction "LanguagePriority")
	(aLanguagePriority source)
	$$ printMaybe
	(printSimpleInstruction "ReflectorHeader")
	(aReflectorHeader source)
	$$ printMaybe
	(printListInstruction "BrowserMatch")
	(aBrowserMatch source)
	$$ printMaybe
	(printListInstruction "BrowserMatchNoCase")
	(aBrowserMatchNoCase source)
	$$ printMaybe
	(printListInstruction "SetEnvIf")
	(aSetEnvIf source)
	$$ printMaybe
	(printListInstruction "SetEnvIfExpr")
	(aSetEnvIfExpr source)
	$$ printMaybe
	(printListInstruction "SetEnvIfNoCase")
	(aSetEnvIfNoCase source)
	$$ printMaybe
	(printSimpleInstruction "ChrootDir")
	(aChrootDir source)
	$$ printMaybe
	(printSimpleInstruction "Group")
	(aGroup source)
	$$ printMaybe
	(printSimpleInstruction "Suexec")
	(aSuexec source)
	$$ printMaybe
	(printSimpleInstruction "User")
	(aUser source)
	$$ printMaybe
	(printSimpleInstruction "UserDir")
	(aUserDir source)
	$$ printMaybe
	(printSimpleInstruction "SSLCACertificateFile")
	(aSSLCACertificateFile source)
	$$ printMaybe
	(printSimpleInstruction "SSLCACertificatePath")
	(aSSLCACertificatePath source)
	$$ printMaybe
	(printSimpleInstruction "SSLCADNRequestFile")
	(aSSLCADNRequestFile source)
	$$ printMaybe
	(printSimpleInstruction "SSLCADNRequestPath")
	(aSSLCADNRequestPath source)
	$$ printMaybe
	(printSimpleInstruction "SSLCARevocationCheck")
	(aSSLCARevocationCheck source)
	$$ printMaybe
	(printSimpleInstruction "SSLCARevocationFile")
	(aSSLCARevocationFile source)
	$$ printMaybe
	(printSimpleInstruction "SSLCARevocationPath")
	(aSSLCARevocationPath source)
	$$ printMaybe
	(printSimpleInstruction "SSLCertificateChainFile")
	(aSSLCertificateChainFile source)
	$$ printMaybe
	(printSimpleInstruction "SSLCertificateFile")
	(aSSLCertificateFile source)
	$$ printMaybe
	(printSimpleInstruction "SSLCertificateKeyFile")
	(aSSLCertificateKeyFile source)
	$$ printMaybe
	(printSimpleInstruction "SSLCipherSuite")
	(aSSLCipherSuite source)
	$$ printMaybe
	(printSimpleInstruction "SSLCompression")
	(aSSLCompression source)
	$$ printMaybe
	(printSimpleInstruction "SSLEngine")
	(aSSLEngine source)
	$$ printMaybe
	(printSimpleInstruction "SSLHonorCipherOrder")
	(aSSLHonorCipherOrder source)
	$$ printMaybe
	(printSimpleInstruction "SSLInsecureRenegotiation")
	(aSSLInsecureRenegotiation source)
	$$ printMaybe
	(printSimpleInstruction "SSLOCSPDefaultResponder")
	(aSSLOCSPDefaultResponder source)
	$$ printMaybe
	(printSimpleInstruction "SSLOCSPEnable")
	(aSSLOCSPEnable source)
	$$ printMaybe
	(printSimpleInstruction "SSLOCSPOverrideResponder")
	(aSSLOCSPOverrideResponder source)
	$$ printMaybe
	(printSimpleInstruction "SSLOCSPResponderTImeout")
	(aSSLOCSPResponderTimeout source)
	$$ printMaybe
	(printSimpleInstruction "SSLOCSPResponseMaxAge")
	(aSSLOCSPResponseMaxAge source)
	$$ printMaybe
	(printSimpleInstruction "SSLOCSPResponseTimeSkew")
	(aSSLOCSPResponseTimeSkew source)
	$$ printMaybe
	(printSimpleInstruction "SSLOCSPUserRequestNonce")
	(aSSLOCSPUseRequestNonce source)
	$$ printMaybe
	(printListInstruction "SSLOpenSSLConfCmd")
	(aSSLOpenSSLConfCmd source)
	$$ printMaybe
	(printSimpleInstruction "SSLOptions")
	(aSSLOptions source)
	$$ printMaybe
	(printSimpleInstruction "SSLProtocol")
	(aSSLProtocol source)
	$$ printMaybe
	(printSimpleInstruction "SSLSessionCacheTimeout")
	(aSSLSessionCacheTimeout source)
	$$ printMaybe
	(printSimpleInstruction "SSLSessionTicketKeyFile")
	(aSSLSessionTicketKeyFile source)
	$$ printMaybe
	(printSimpleInstruction "SSLSessionTickets")
	(aSSLSessionTickets source)
	$$ printMaybe
	(printSimpleInstruction "SSLSRPUnknownUserSeed")
	(aSSLSRPUnknownUserSeed source)
	$$ printMaybe
	(printSimpleInstruction "SSLSRPVerifierFile")
	(aSSLSRPVerifierFile source)
	$$ printMaybe
	(printSimpleInstruction "SSLStaplingErrorCacheTimeout")
	(aSSLStaplingErrorCacheTimeout source)
	$$ printMaybe
	(printSimpleInstruction "SSLStaplingFakeTryLater")
	(aSSLStaplingFakeTryLater source)
	$$ printMaybe
	(printSimpleInstruction "SSLStaplingForceURL")
	(aSSLStaplingForceURL source)
	$$ printMaybe
	(printSimpleInstruction "SSLStaplingResponderTimeout")
	(aSSLStaplingResponderTimeout source)
	$$ printMaybe
	(printSimpleInstruction "SSLStaplingResponseMaxAge")
	(aSSLStaplingResponseMaxAge source)
	$$ printMaybe
	(printSimpleInstruction "SSLStaplingResponseTimeSkew")
	(aSSLStaplingResponseTimeSkew source)
	$$ printMaybe
	(printSimpleInstruction "SSLStaplingReturnResponderErrors")
	(aSSLStaplingReturnResponderErrors source)
	$$ printMaybe
	(printSimpleInstruction "SSLStaplingStandardCacheTimeout")
	(aSSLStaplingStandardCacheTimeout source)
	$$ printMaybe
	(printSimpleInstruction "SSLStrictSNIVHostCheck")
	(aSSLStrictSNIVHostCheck source)
	$$ printMaybe
	(printSimpleInstruction "SSLUseStapling")
	(aSSLUseStapling source)
	$$ printMaybe
	(printSimpleInstruction "SSLVerifyClient")
	(aSSLVerifyClient source)
	$$ printMaybe
	(printSimpleInstruction "SSLVerifyDepth")
	(aSSLVerifyDepth source)
	$$ printMaybe
	(printSimpleInstruction "SSLCryptoDevice")
	(aSSLCryptoDevice source)
	$$ printMaybe
	(printSimpleInstruction "SSLFIPS")
	(aSSLFIPS source)
	$$ printMaybe
	(printSimpleInstruction "SSLPassPhraseDialog")
	(aSSLPassPhraseDialog source)
	$$ printMaybe
	(printSimpleInstruction "SSLRandomSeed")
	(aSSLRandomSeed source)
	$$ printMaybe
	(printSimpleInstruction "SSLSessionCache")
	(aSSLSessionCache source)
	$$ printMaybe
	(printSimpleInstruction "SSLStaplingCache")
	(aSSLStaplingCache source)
	$$ printMaybe
	(printSimpleInstruction "SSLUserName")
	(aSSLUserName source)
	$$ printMaybe
	printDirectories
	(aDirectory source)
	$$ printMaybe
	printFiles
	(aFiles source)
	$$ printMaybe
	printLocations
	(aLocation source)
	$$ printMaybe
	printVirtualHosts
	(aVirtualHosts source)
--}}}

--{{{ Function that will print a list of Directory context
printDirectories :: [Directory] -> Doc
printDirectories [] = empty
printDirectories (x:xs) = printDirectory x $$ printDirectories xs
--}}}

--{{{ Function that will print a Directory context
printDirectory :: Directory -> Doc
printDirectory source = text "<Directory" <> 
	case (noMaybe (dMatch source)) of
	True -> (text "Match " <> text (noMaybe (dPath source)) <> text ">")
	False ->  (text " " <> text (noMaybe (dPath source)) <> text ">")
	$$ nest 5 (printMaybe
	printDirDirectives
	(dDirDirectives source))
	$$ nest 5 (printMaybe
	printRequireCons
	(dRequireCons source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "AllowOverride")
	(dAllowOverride source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "AllowOverrideList")
	(dAllowOverrideList source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "Options")
	(dOptions source))
	$$ nest 5 (printMaybe
	printFiles
	(dFiles source))
	$$ text "</Directory>"
--}}}

--{{{ Function that will print a DirDirective context
printDirDirectives :: DirDirectives -> Doc
printDirDirectives source =  printMaybe
	(printSimpleInstruction "AcceptPathInfo")
	(dAcceptPathInfo source)
	$$ printMaybe
	(printSimpleInstruction "AddDefaultCharset")
	(dAddDefaultCharset source)
	$$ printMaybe
	(printSimpleInstruction "ContentDigest")
	(dContentDigest source)
	$$ printMaybe
	(printListInstruction "Define")
	(dDefine source)
	$$ printMaybe
	(printSimpleInstruction "EnableMMAP")
	(dEnableMMAP source)
	$$ printMaybe
	(printSimpleInstruction "EnableSendFile")
	(dEnableSendFile source)
	$$ printMaybe
	(printSimpleInstruction "Error")
	(dError source)
	$$ printMaybe
	(printListInstruction "ErrorDocument")
	(dErrorDocument source)
	$$ printMaybe
	(printSimpleInstruction "FileETag")
	(dFileETag source)
	$$ printMaybe
	(printSimpleInstruction "ForceType")
	(dForceType source)
	$$ printMaybe
	(printSimpleInstruction "HostnameLookups")
	(dHostnameLookups source)
	$$ printMaybe
	(printListInstruction "Include")
	(dInclude source)
	$$ printMaybe
	(printListInstruction "IncludeOptional")
	(dIncludeOptional source)
	$$ printMaybe
	(printSimpleInstruction "LimitRequestBody")
	(dLimitRequestBody source)
	$$ printMaybe
	(printSimpleInstruction "LimitXMLRequestBody")
	(dLimitXMLRequestBody source)
	$$ printMaybe
	(printSimpleInstruction "LogLevel")
	(dLogLevel source)
	$$ printMaybe
	(printSimpleInstruction "MaxRangeOverlaps")
	(dMaxRangeOverlaps source)
	$$ printMaybe
	(printSimpleInstruction "MaxRangeReversals")
	(dMaxRangeReversals source)
	$$ printMaybe
	(printSimpleInstruction "MaxRanges")
	(dMaxRanges source)
	$$ printMaybe
	(printSimpleInstruction "RLimitCPU")
	(dRLimitCPU source)
	$$ printMaybe	
	(printSimpleInstruction "RLimitMEM")
	(dRLimitMEM source)
	$$ printMaybe
	(printSimpleInstruction "RLimitNPROC")
	(dRLimitNPROC source)
	$$ printMaybe
	(printSimpleInstruction "ServerSignature")
	(dServerSignature source)
	$$ printMaybe
	(printSimpleInstruction "SetHandler")
	(dSetHandler source)
	$$ printMaybe
	(printSimpleInstruction "SetInputFilter")
	(dSetInputFilter source)
	$$ printMaybe
	(printSimpleInstruction "SetOutputFilter")
	(dSetOutputFilter source)
	$$ printMaybe
	(printSimpleInstruction "UseCanonicalName")
	(dUseCanonicalName source)
	$$ printMaybe
	(printSimpleInstruction "UseCanonicalPhysicalPort")
	(dUseCanonicalPhysicalPort source)
	$$ printMaybe
	(printListInstruction "Redirect")
	(dRedirect source)
	$$ printMaybe
	(printListInstruction "RedirectMatch")
	(dRedirectMatch source)
	$$ printMaybe
	(printListInstruction "RedirectPermanent")
	(dRedirectPermanent source)
	$$ printMaybe
	(printListInstruction "RedirectTemp")
	(dRedirectTemp source)
	$$ printMaybe
	(printSimpleInstruction "AuthBasicAuthoritative")
	(dAuthBasicAuthoritative source)
	$$ printMaybe
	(printSimpleInstruction "AuthBasicFake")
	(dAuthBasicFake source)
	$$ printMaybe
	(printSimpleInstruction "AuthBasicProvider")
	(dAuthBasicProvider source)
	$$ printMaybe
	(printSimpleInstruction "AuthBasicUseDigestAlgorithm")
	(dAuthBasicUseDigestAlgorithm source)
	$$ printMaybe
	(printSimpleInstruction "AuthName")
	(dAuthName source)
	$$ printMaybe
	(printSimpleInstruction "AuthType")
	(dAuthType source)
	$$ printMaybe
	(printSimpleInstruction "AuthUserFile")
	(dAuthUserFile source)
	$$ printMaybe
	(printSimpleInstruction "AuthMerging")
	(dAuthMerging source)
	$$ printMaybe
	(printSimpleInstruction "AuthzSendForbiddenOnFailure")
	(dAuthzSendForbiddenOnFailure source)
	$$ printMaybe
	(printListInstruction "Require")
	(dRequire source)
	$$ printMaybe
	(printSimpleInstruction "AuthGroupFile")
	(dAuthGroupFile source)
	$$ printMaybe
	(printListInstruction "AddAlt")
	(dAddAlt source)
	$$ printMaybe
	(printListInstruction "AddAltByEncoding")
	(dAddAltByEncoding source)
	$$ printMaybe
	(printListInstruction "AddAltByType")
	(dAddAltByType source)
	$$ printMaybe
	(printListInstruction "AddDescription")
	(dAddDescription source)
	$$ printMaybe
	(printListInstruction "AddIcon")
	(dAddIcon source)
	$$ printMaybe
	(printListInstruction "AddIconByEncoding")
	(dAddIconByEncoding source)
	$$ printMaybe
	(printListInstruction "AddIconByType")
	(dAddIconByType source)
	$$ printMaybe
	(printSimpleInstruction "DefaultIcon")
	(dDefaultIcon source)
	$$ printMaybe
	(printSimpleInstruction "HeaderName")
	(dHeaderName source)
	$$ printMaybe
	(printSimpleInstruction "IndexHeadInsert")
	(dIndexHeadInsert source)
	$$ printMaybe
	(printListInstruction "IndexIgnore")
	(dIndexIgnore source)
	$$ printMaybe
	(printSimpleInstruction "IndexIgnoreReset")
	(dIndexIgnoreReset source)
	$$ printMaybe
	(printListInstruction "IndexOptions")
	(dIndexOptions source)
	$$ printMaybe
	(printSimpleInstruction "IndexOrderDefault")
	(dIndexOrderDefault source)
	$$ printMaybe
	(printSimpleInstruction "IndexStyleSheet")
	(dIndexStyleSheet source)
	$$ printMaybe
	(printSimpleInstruction "ReadmeName")
	(dReadmeName source)
	$$ printMaybe
	(printSimpleInstruction "CGIDScript")
	(dCGIDScriptTimeout source)
	$$ printMaybe
	(printSimpleInstruction "DirectoryCheckHandler")
	(dDirectoryCheckHandler source)
	$$ printMaybe
	(printSimpleInstruction "DirectoryIndex")
	(dDirectoryIndex source)
	$$ printMaybe
	(printSimpleInstruction "IndexRedirect")
	(dIndexRedirect source)
	$$ printMaybe
	(printSimpleInstruction "DirectorySlash")
	(dDirectorySlash source)
	$$ printMaybe
	(printSimpleInstruction "FallbackResource")
	(dFallbackResource source)
	$$ printMaybe
	(printSimpleInstruction "PassEnv")
	(dPassEnv source)
	$$ printMaybe
	(printListInstruction "SetEnv")
	(dSetEnv source)
	$$ printMaybe
	(printListInstruction "UnsetEnv")
	(dUnsetEnv source)
	$$ printMaybe
	(printListInstruction "AddOutputFilterByType")
	(dAddOutputFilterByType source)
	$$ printMaybe
	(printListInstruction "FilterChain")
	(dFilterChain source)
	$$ printMaybe
	(printListInstruction "FilterDeclare")
	(dFilterDeclare source)
	$$ printMaybe
	(printListInstruction "FilterProtocol")
	(dFilterProtocol source)
	$$ printMaybe
	(printListInstruction "FilterProvider")
	(dFilterProvider source)
	$$ printMaybe
	(printListInstruction "FilterTrace")
	(dFilterTrace source)
	$$ printMaybe
	(printSimpleInstruction "ImapBase")
	(dImapBase source)
	$$ printMaybe
	(printSimpleInstruction "ImapDefault")
	(dImapDefault source)
	$$ printMaybe
	(printSimpleInstruction "ImapMenu")
	(dImapMenu source)
	$$ printMaybe
	(printListInstruction "CustomLog") (dCustomLog source)
	$$ printMaybe
	(printListInstruction "LogFormat")
	(dLogFormat source)
	$$ printMaybe
	(printListInstruction "TransferLog")
	(dTransferLog source)
	$$ printMaybe
	(printListInstruction "AddCharset")
	(dAddCharset source)
	$$ printMaybe
	(printListInstruction "AddEncoding")
	(dAddEncoding source)
	$$ printMaybe
	(printListInstruction "AddHandler")
	(dAddHandler source)
	$$ printMaybe
	(printListInstruction "AddInputFilter")
	(dAddInputFilter source)
	$$ printMaybe
	(printListInstruction "AddLanguage")
	(dAddLanguage source)
	$$ printMaybe
	(printListInstruction "AddOutputFilter")
	(dAddOutputFilter source)
	$$ printMaybe
	(printListInstruction "AddType")
	(dAddType source)
	$$ printMaybe
	(printSimpleInstruction "DefaultLanguage")
	(dDefaultLanguage source)
	$$ printMaybe
	(printSimpleInstruction "ModMimeUsePathInfo")
	(dModMimeUsePathInfo source)
	$$ printMaybe
	(printSimpleInstruction "MultiviewsMatch")
	(dMultiviewsMatch source)
	$$ printMaybe
	(printListInstruction "RemoveCharset")
	(dRemoveCharset source)
	$$ printMaybe
	(printListInstruction "RemoveEncoding")
	(dRemoveEncoding source)
	$$ printMaybe
	(printListInstruction "RemoveHandler")
	(dRemoveHandler source)
	$$ printMaybe
	(printListInstruction "RemoveInputFilter")
	(dRemoveInputFilter source)
	$$ printMaybe
	(printListInstruction "RemoveLanguage")
	(dRemoveLanguage source)
	$$ printMaybe
	(printListInstruction "RemoveOutputFilter")
	(dRemoveOutputFilter source)
	$$ printMaybe
	(printListInstruction "RemoveType")
	(dRemoveType source)
	$$ printMaybe
	(printSimpleInstruction "ForceLanguagePriority")
	(dForceLanguagePriority source)
	$$ printMaybe
	(printSimpleInstruction "LanguagePriority")
	(dLanguagePriority source)
	$$ printMaybe
	(printSimpleInstruction "ReflectorHeader")
	(dReflectorHeader source)
	$$ printMaybe
	(printSimpleInstruction "KeptBodySize")
	(dKeptBodySize source)
	$$ printMaybe
	(printListInstruction "BrowserMatch")
	(dBrowserMatch source)
	$$ printMaybe
	(printListInstruction "BrowserMatchNoCase")
	(dBrowserMatchNoCase source)
	$$ printMaybe
	(printListInstruction "SetEnvIf")
	(dSetEnvIf source)
	$$ printMaybe
	(printListInstruction "SetEnvIfExpr")
	(dSetEnvIfExpr source)
	$$ printMaybe
	(printListInstruction "SetEnvIfNoCase")
	(dSetEnvIfNoCase source)
	$$ printMaybe
	(printSimpleInstruction "SSLCipherSuite")
	(dSSLCipherSuite source)
	$$ printMaybe
	(printSimpleInstruction "SSLOptions")
	(dSSLOptions source)
	$$ printMaybe
	(printSimpleInstruction "SSLRenegBufferSize")
	(dSSLRenegBufferSize source)
	$$ printMaybe
	(printSimpleInstruction "SSLRequireSSL")
	(dSSLRequireSSL source)
	$$ printMaybe
	(printSimpleInstruction "SSLUserName")
	(dSSLUserName source)
	$$ printMaybe
	(printSimpleInstruction "SSLVerifyClient")
	(dSSLVerifyClient source)
	$$ printMaybe
	(printSimpleInstruction "SSLVerifyDepth")
	(dSSLVerifyDepth source)
--}}}

--{{{ Function that will print a list of RequireCons context
printRequireCons :: [RequireCons] -> Doc
printRequireCons [] = empty
printRequireCons (x:xs) = printRequiresCons x $$ printRequireCons xs
--}}}

--{{{ Function that will print a RequireCons context
printRequiresCons :: RequireCons -> Doc
printRequiresCons source = text "<Require" <> 
	case (noMaybe (rConsType source)) of
		"All" -> text "All>"
		"Any" -> text "Any>"
		"None" -> text "None>"
	$$ nest 5 (printMaybe
	(printListInstruction "Require")
	(rRequire source))
	$$ nest 5 (printMaybe
	printRequireCons
	(rRequireCons source))
	$$ text "</Require>"
--}}}

--{{{ Function that prints a list of Files context
printFiles :: [Files] -> Doc
printFiles [] = empty
printFiles (x:xs) = printFile x $$ printFiles xs
--}}}

--{{{ Function that will print a Files context
printFile :: Files -> Doc
printFile source = text "<Files" <>
	case (noMaybe (fMatch source)) of
	True -> (text "Match " <> text (noMaybe (fFileName source)) <> text ">")
	False ->  (text " " <> text (noMaybe (fFileName source)) <> text ">")
	$$ nest 5 (printMaybe
	printDirDirectives
	(fDirDirectives source))
	$$ nest 5 (printMaybe
	printRequireCons
	(fRequireCons source))
	$$ text "</Files>"
--}}}

--{{{ Function that prints a list of Location context
printLocations :: [Location] -> Doc
printLocations [] = empty
printLocations (x:xs) = printLocation x $$ printLocations xs
--}}}

--{{{ Function that prints a Location context
printLocation :: Location -> Doc
printLocation source = text "<Location" <> 
	case (noMaybe (lMatch source)) of
	True -> (text "Match " <> text (noMaybe (lPath source)) <> text ">")
	False ->  (text " " <> text (noMaybe (lPath source)) <> text ">")
	$$ nest 5 (printMaybe
	printDirDirectives
	(lDirDirectives source))
	$$ nest 5 (printMaybe
	printRequireCons
	(lRequireCons source))
 	$$ nest 5 (printMaybe
	(printSimpleInstruction "Options")
	(lOptions source))
	$$ text "</Location>"
--}}}

--{{{ Function that prints a list of VirtualHost context
printVirtualHosts :: [VirtualHost] -> Doc
printVirtualHosts [] = empty
printVirtualHosts (x:xs) = printVirtualHost x $$ printVirtualHosts xs
--}}}

--{{{ Function that prints a VirtualHost context
printVirtualHost :: VirtualHost -> Doc
printVirtualHost source = text "<VirtualHost "
	<> hsep (map text (noMaybe (sVirtualHostAddress source))) <> text ">"
	$$ nest 5 (printMaybe
	(printSimpleInstruction "AcceptPathInfo")
	(sAcceptPathInfo source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "AccessFileName")
	(sAccessFileName source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "AddDefaultCharset")
	(sAddDefaultCharset source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "AllowEncodedSlashes")
	(sAllowEncodedSlashes source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ContentDigest")
	(sContentDigest source))
	$$ nest 5 (printMaybe
	(printListInstruction "Define")
	(sDefine source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "DocumentRoot")
	(sDocumentRoot source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "EnableMMAP")
	(sEnableMMAP source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "EnableSendfile")
	(sEnableSendfile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "Error")
	(sError source))
	$$ nest 5 (printMaybe
	(printListInstruction "ErrorDocument")
	(sErrorDocument source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ErrorLog")
	(sErrorLog source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ErrorLogFormat")
	(sErrorLogFormat source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "FileETag")
	(sFileETag source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "HostnameLookups")
	(sHostnameLookups source))
	$$ nest 5 (printMaybe
	(printListInstruction "Include")
	(sInclude source))
	$$ nest 5 (printMaybe
	(printListInstruction "IncludeOptional")
	(sIncludeOptional source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "KeepAlive")
	(sKeepAlive source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "KeepAliveTimeout")
	(sKeepAliveTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "LimitInternalRecursion")
	(sLimitInternalRecursion source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "LimitRequestBody")
	(sLimitRequestBody source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "LimitRequestFields")
	(sLimitRequestFields source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "LimitRequestFieldSize")
	(sLimitRequestFieldSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "LimitRequestLine")
	(sLimitRequestLine source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "LimitXMLRequestBody")
	(sLimitXMLRequestBody source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "LogLevel")
	(sLogLevel source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "MaxKeepAliveRequests")
	(sMaxKeepAliveRequests source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "MaxRangesOverlaps")
	(sMaxRangesOverlaps source))
	$$ nest 5
	(printMaybe (printSimpleInstruction "MaxRangesReversals")
	(sMaxRangesReversals source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "MaxRanges")
	(sMaxRanges source))
	$$ nest 5
	(printMaybe (printSimpleInstruction "Options")
	(sOptions source))
	$$ nest 5
	(printMaybe (printSimpleInstruction "Protocol")
	(sProtocol source))
	$$ nest 5
	(printMaybe (printSimpleInstruction "RLimitCPU")
	(sRLimitCPU source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "RLimitMEM")
	(sRLimitMEM source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "RLimitNPROC")
	(sRLimitNPROC source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ServerAdmin")
	(sServerAdmin source))
	-- ServerName and ServerAlias instructions are represented in the same list.
	-- ServerName is in the first position of the list and ServerAlias are after.
	-- The pretty print has to take care of that.
	$$ nest 5 (if ((sServerName source) /= Nothing) then 
		printSimpleInstruction "ServerName" (head (noMaybe
		(sServerName source))) else 
		empty)
	$$ nest 5 (if ((sServerName source) /= Nothing) then 
		printListInstruction "ServerAlias" (tail (noMaybe
		(sServerName source))) else 
		empty)
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ServerPath")
	(sServerPath source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ServerSignature")
	(sServerSignature source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SetHandler")
	(sSetHandler source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SetInputFilter")
	(sSetInputFilter source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SetOutputFilter")
	(sSetOutputFilter source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "TimeOut")
	(sTimeOut source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "TraceEnable")
	(sTraceEnable source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "UseCanonicalName")
	(sUseCanonicalName source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "UseCanonicalPhysicalPort")
	(sUseCanonicalPhysicalPort source))
	$$ nest 5 (printMaybe
	(printListInstruction "Alias")
	(sAlias source))
	$$ nest 5 (printMaybe
	(printListInstruction "AliasMatch")
	(sAliasMatch source))
	$$ nest 5 (printMaybe
	(printListInstruction "Redirect")
	(sRedirect source))
	$$ nest 5 (printMaybe
	(printListInstruction "RedirectMatch")
	(sRedirectMatch source))
	$$ nest 5 (printMaybe
	(printListInstruction "RedirectPermanent")
	(sRedirectPermanent source))
	$$ nest 5 (printMaybe
	(printListInstruction "RedirectTemp")
	(sRedirectTemp source))
	$$ nest 5 (printMaybe
	(printListInstruction "ScriptAlias")
	(sScriptAlias source))
	$$ nest 5 (printMaybe
	(printListInstruction "ScriptAliasMatch")
	(sScriptAliasMatch source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddAlt")
	(sAddAlt source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddAltByEncoding")
	(sAddAltByEncoding source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddAltByType")
	(sAddAltByType source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddDescription")
	(sAddDescription source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddIcon")
	(sAddIcon source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddIconByEncoding")
	(sAddIconByEncoding source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddIconByType")
	(sAddIconByType source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "DefaultIcon")
	(sDefaultIcon source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "HeaderName")
	(sHeaderName source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "IndexHeadInsert")
	(sIndexHeadInsert source))
	$$ nest 5 (printMaybe
	(printListInstruction "IndexIgnore")
	(sIndexIgnore source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "IndexIgnoreReset")
	(sIndexIgnoreReset source))
	$$ nest 5 (printMaybe
	(printListInstruction "IndexOptions")
	(sIndexOptions source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "IndexOrderDefault")
	(sIndexOrderDefault source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "IndexStyleSheet")
	(sIndexStyleSheet source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ReadmeName")
	(sReadmeName source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ScriptLog")
	(sScriptLog source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ScriptLogBuffer")
	(sScriptLogBuffer source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ScriptLogLength")
	(sScriptLogLength source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "CGIDScriptTimeout")
	(sCGIDScriptTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "DirectoryCheckHandler")
	(sDirectoryCheckHandler source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "DirectoryIndex")
	(sDirectoryIndex source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "IndexRedirect")
	(sIndexRedirect source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "DirectorySlash")
	(sDirectorySlash source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "FallbackResource")
	(sFallbackResource source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "PassEnv")
	(sPassEnv source))
	$$ nest 5 (printMaybe
	(printListInstruction "SetEnv")
	(sSetEnv source))
	$$ nest 5 (printMaybe
	(printListInstruction "UnsetEnv")
	(sUnsetEnv source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddOutputFilterByType")
	(sAddOutputFilterByType source))
	$$ nest 5 (printMaybe
	(printListInstruction "FilterChain")
	(sFilterChain source))
	$$ nest 5 (printMaybe
	(printListInstruction "FilterDeclare")
	(sFilterDeclare source))
	$$ nest 5 (printMaybe
	(printListInstruction "FilterProtocol")
	(sFilterProtocol source))
	$$ nest 5 (printMaybe
	(printListInstruction "FilterProvider")
	(sFilterProvider source))
	$$ nest 5 (printMaybe
	(printListInstruction "FilterTrace")
	(sFilterTrace source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ImapBase")
	(sImapBase source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ImapDefault")
	(sImapDefault source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ImapMenu")
	(sImapMenu source))
	$$ nest 5 (printMaybe
	(printListInstruction "CustomLog")
	(sCustomLog source))
	$$ nest 5 (printMaybe
	(printListInstruction "LogFormat")
	(sLogFormat source))
	$$ nest 5 (printMaybe
	(printListInstruction "TransferLog")
	(sTransferLog source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddCharset")
	(sAddCharset source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddEncoding")
	(sAddEncoding source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddHandler")
	(sAddHandler source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddInputFilter")
	(sAddInputFilter source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddLanguage")
	(sAddLanguage source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddOutputFilter")
	(sAddOutputFilter source))
	$$ nest 5 (printMaybe
	(printListInstruction "AddType")
	(sAddType source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "DefaultLanguage")
	(sDefaultLanguage source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "MultiviewsMatch")
	(sMultiviewsMatch source))
	$$ nest 5 (printMaybe
	(printListInstruction "RemoveCharset")
	(sRemoveCharset source))
	$$ nest 5 (printMaybe
	(printListInstruction "RemoveEncoding")
	(sRemoveEncoding source))
	$$ nest 5 (printMaybe
	(printListInstruction "RemoveHandler")
	(sRemoveHandler source))
	$$ nest 5 (printMaybe
	(printListInstruction "RemoveInputFilter")
	(sRemoveInputFilter source))
	$$ nest 5 (printMaybe
	(printListInstruction "RemoveLanguage")
	(sRemoveLanguage source))
	$$ nest 5 (printMaybe
	(printListInstruction "RemoveOutputFilter")
	(sRemoveOutputFilter source))
	$$ nest 5 (printMaybe
	(printListInstruction "RemoveType")
	(sRemoveType source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "CacheNegotiatedDocs")
	(sCacheNegotiatedDocs source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ForceLanguagePriority")
	(sForceLanguagePriority source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "LanguagePriority")
	(sLanguagePriority source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ReflectorHeader")
	(sReflectorHeader source))
	$$ nest 5 (printMaybe
	(printListInstruction "BrowserMatch")
	(sBrowserMatch source))
	$$ nest 5 (printMaybe
	(printListInstruction "BrowserMatchNoCase")
	(sBrowserMatchNoCase source))
	$$ nest 5 (printMaybe
	(printListInstruction "SetEnvIf")
	(sSetEnvIf source))
	$$ nest 5 (printMaybe
	(printListInstruction "SetEnvIfExpr")
	(sSetEnvIfExpr source))
	$$ nest 5 (printMaybe
	(printListInstruction "SetEnvIfNoCase")
	(sSetEnvIfNoCase source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "UserDir")
	(sUserDir source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCACertificateFile")
	(sSSLCACertificateFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCACertificatePath")
	(sSSLCACertificatePath source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCADNRequestFile")
	(sSSLCADNRequestFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCADNRequestPath")
	(sSSLCADNRequestPath source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCARevocationCheck")
	(sSSLCARevocationCheck source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCARevocationFile")
	(sSSLCARevocationFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCARevocationPath")
	(sSSLCARevocationPath source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCertificateChainFile")
	(sSSLCertificateChainFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCertificateFile")
	(sSSLCertificateFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCertificateKeyFile")
	(sSSLCertificateKeyFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCipherSuite")
	(sSSLCipherSuite source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLCompression")
	(sSSLCompression source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLEngine")
	(sSSLEngine source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLHonorCipherOrder")
	(sSSLHonorCipherOrder source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLInsecureRenegotiation")
	(sSSLInsecureRenegotiation source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLOCSPDefaultResponder")
	(sSSLOCSPDefaultResponder source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLOCSPEnable")
	(sSSLOCSPEnable source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLOCSPOverrideResponder")
	(sSSLOCSPOverrideResponder source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLOCSPResponderTImeout")
	(sSSLOCSPResponderTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLOCSPResponseMaxAge")
	(sSSLOCSPResponseMaxAge source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLOCSPResponseTimeSkew")
	(sSSLOCSPResponseTimeSkew source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLOCSPUserRequestNonce")
	(sSSLOCSPUseRequestNonce source))
	$$ nest 5 (printMaybe
	(printListInstruction "SSLOpenSSLConfCmd")
	(sSSLOpenSSLConfCmd source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLOptions")
	(sSSLOptions source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLProtocol")
	(sSSLProtocol source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLSessionCacheTimeout")
	(sSSLSessionCacheTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLSessionTicketKeyFile")
	(sSSLSessionTicketKeyFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLSessionTickets")
	(sSSLSessionTickets source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLSRPUnknownUserSeed")
	(sSSLSRPUnknownUserSeed source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLSRPVerifierFile")
	(sSSLSRPVerifierFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLStaplingErrorCacheTimeout")
	(sSSLStaplingErrorCacheTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLStaplingFakeTryLater")
	(sSSLStaplingFakeTryLater source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLStaplingForceURL")
	(sSSLStaplingForceURL source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLStaplingResponderTimeout")
	(sSSLStaplingResponderTimeout source))
	$$ nest 5
	(printMaybe (printSimpleInstruction "SSLStaplingResponseMaxAge")
	(sSSLStaplingResponseMaxAge source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLStaplingResponseTimeSkew")
	(sSSLStaplingResponseTimeSkew source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLStaplingReturnResponderErrors")
	(sSSLStaplingReturnResponderErrors source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLStaplingStandardCacheTimeout")
	(sSSLStaplingStandardCacheTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLStrictSNIVHostCheck")
	(sSSLStrictSNIVHostCheck source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLUseStapling")
	(sSSLUseStapling source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLVerifyClient")
	(sSSLVerifyClient source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "SSLVerifyDepth")
	(sSSLVerifyDepth source))
	$$ nest 5 (printMaybe
	printDirectories
	(sDirectory source))
	$$ nest 5 (printMaybe
	printLocations
	(sLocation source))
	$$ text "</VirtualHost>" 
--}}}

--{{{ Functionn that prints a simple instruction
printSimpleInstruction :: String -> String -> Doc
printSimpleInstruction instruction value = text instruction <+> text value
--}}}

--{{{ Function that prints a list of the same instructions from a list of values
printListInstruction :: String -> [String] -> Doc
printListInstruction instruction [] = empty
printListInstruction instruction (x:xs) = printSimpleInstruction instruction x
	$$ printListInstruction instruction xs
--}}}

--{{{ Functions that prints a Apache source
printApacheConf :: IO ()
printApacheConf = parseTreeApache "apache.conf" >>= \(Right tree) -> print
	(printApache (createSourceApache tree))
--}}}
