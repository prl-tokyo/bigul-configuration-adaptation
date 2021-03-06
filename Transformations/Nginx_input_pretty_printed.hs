module Nginx_input_pretty_printed where
import TypeFiles.NginxTypes
nginxTestInput :: NginxWebserver
nginxTestInput = NginxWebserver { 
    nDaemon = Nothing, 
    nErrorLog = Nothing, 
    nEnv = Nothing, 
    nInclude = Nothing, 
    nLockFile = Nothing, 
    nPid = Just "/run/nginx.pid", 
    nSslEngine = Nothing, 
    nThreadPool = Nothing, 
    nTimerResolution = Nothing, 
    nUser = Just "www-data", 
    nWorkerCpuAffinity = Nothing, 
    nWorkerPriority = Nothing, 
    nWorkerProcesses = Just "4", 
    nWorkerRLimitCore = Nothing, 
    nWorkerRLimitNoFile = Nothing, 
    nWorkingDirectory = Nothing, 
    nEvents = Just Events { 
        eAcceptMutex = Nothing, 
        eAcceptMutexDelay = Nothing, 
        eInclude = Nothing, 
        eMultiAccept = Nothing, 
        eUse = Nothing, 
        eWorkerAioRequests = Nothing, 
        eWorkerConnections = Just "768"
    }, 
    nHttp = Just Http { 
        hAccessRule = Nothing, 
        hAddHeader = Nothing, 
        hAio = Nothing, 
        hAuthBasic = Nothing, 
        hAuthBasicUserFile = Nothing, 
        hAutoindex = Nothing, 
        hAiExactSize = Nothing, 
        hAiFormat = Nothing, 
        hAiLocaltime = Nothing, 
        hAncientBrowser = Nothing, 
        hAncientBrowserValue = Nothing, 
        hModernBrowser = Nothing, 
        hModernBrowserValue = Nothing, 
        hCharset = Nothing, 
        hOverrideCharset = Nothing, 
        hSourceCharset = Nothing, 
        hCharsetMap = Nothing, 
        hCharsetType = Nothing, 
        hChunkedTransferEncoding = Nothing, 
        hClientHeaderBufferSize = Nothing, 
        hClientBodyBufferSize = Nothing, 
        hClientBodyInFileOnly = Nothing, 
        hClientBodyInSingleBuffer = Nothing, 
        hClientBodyTempPath = Nothing, 
        hClientBodyTimeout = Nothing, 
        hClientMaxBodySize = Nothing, 
        hClientHeaderTimeout = Nothing, 
        hConnectionPoolSize = Nothing, 
        hDefaultType = Nothing, 
        hDirectio = Nothing, 
        hDirectioAlignment = Nothing, 
        hDisableSymlinks = Nothing, 
        hErrorPage = Nothing, 
        hEtag = Nothing, 
        hExpires = Nothing, 
        hGzip = Just "on",
        hGzipBuffers = Nothing, 
        hGzipCompLevel = Nothing, 
        hGzipDisable = Nothing, 
        hGzipMinLength = Nothing, 
        hGzipHttpVersion = Nothing, 
        hGzipProxied = Nothing, 
        hGzipTypes = Nothing, 
        hGzipVary = Nothing, 
        hIfModifiedSince = Nothing, 
        hIgnoreInvalidHeaders = Nothing, 
        hInclude = Nothing, 
        hIndex = Just "index.html index.htm", 
        hKeepaliveDisable = Nothing, 
        hKeepaliveRequests = Nothing, 
        hKeepaliveTimeout = Just "65",
        hLargeClientHeaderBuffers = Nothing, 
        hLimitConn = Nothing, 
        hLimitConnLogLevel = Nothing, 
        hLimitConnStatus = Nothing, 
        hLimitConnZone = Nothing, 
        hLimitRate = Nothing, 
        hLimitRateAfter = Nothing, 
        hLimitReq = Nothing, 
        hLimitReqLogLevel = Nothing, 
        hLimitReqStatus = Nothing, 
        hLimitReqZone = Nothing, 
        hLingeringClose = Nothing, 
        hLingeringTime = Nothing, 
        hLingeringTimeout = Nothing, 
        hAccessLog = Just "/var/log/nginx/access.log", 
        hErrorLog = Just "/var/log/nginx/error.log", 
        hLogNotFound = Nothing, 
        hLogSubrequest = Nothing, 
        hOpenLogFileCache = Nothing, 
        hLogFormat = Nothing, 
        hMap = Nothing, 
        hMapHashBucketSize = Nothing, 
        hMapHashMaxSize = Nothing, 
        hMaxRanges = Nothing, 
        hMemcachedBind = Nothing, 
        hMemCachedBufferSize = Nothing, 
        hMemcachedConnectTimeout = Nothing, 
        hMemcachedForceRanges = Nothing, 
        hMemcachedGzipFlag = Nothing, 
        hMemcachedNextUpstream = Nothing, 
        hMemcachedNextUpstreamTimeout = Nothing, 
        hMemcachedNextUpstreamTries = Nothing, 
        hMemcachedReadTimeout = Nothing, 
        hMemcachedSendTimeout = Nothing, 
        hMergeSlashes = Nothing, 
        hMsiePadding = Nothing, 
        hMsieRefresh = Nothing, 
        hOpenFileCache = Nothing, 
        hOpenFileCacheErrors = Nothing, 
        hOpenFileCacheMinUses = Nothing, 
        hOpenFileCacheValid = Nothing, 
        hOutputBuffers = Nothing, 
        hPostponeOutput = Nothing, 
        hPortInRedirect = Nothing, 
        hReadAhead = Nothing, 
        hRecursiveErrorPages = Nothing, 
        hRequestPoolSize = Nothing, 
        hResetTimedoutConnection = Nothing, 
        hResolver = Nothing, 
        hResolverTimeout = Nothing, 
        hRoot = Nothing, 
        hSatisfy = Nothing, 
        hSendLowat = Nothing, 
        hSendTimeout = Nothing, 
        hSendFile = Just "on",
        hSendFileMaxChunks = Nothing, 
        hServer = Just [
            Server { 
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
                sErrorPage = Nothing, 
                sEtag = Nothing, 
                sExpires = Nothing,
                sGzip = Nothing, 
                sGzipBuffers = Nothing, 
                sGzipCompLevel = Nothing, 
                sGzipDisable = Nothing, 
                sGzipMinLength = Nothing, 
                sGzipHttpVersion = Nothing, 
                sGzipProxied = Nothing, 
                sGzipTypes = Nothing, 
                sGzipVary = Nothing, 
                sIfModifiedSince = Nothing, 
                sIgnoreInvalidHeaders = Nothing, 
                sInclude = Nothing, 
                sIndex = Nothing, 
                sKeepaliveDisable = Nothing, 
                sKeepaliveRequests = Nothing, 
                sKeepaliveTimeout = Nothing, 
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
                sListen = Just ["80"], 
                sLocation = Just [
                    Location { 
                        lLocationPath = Nothing, 
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
                        lErrorPage = Nothing, 
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
                        lIndex = Nothing, 
                        lInternal = Nothing, 
                        lKeepaliveDisable = Nothing, 
                        lKeepaliveRequests = Nothing, 
                        lKeepaliveTimeout = Nothing, 
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
                        lLocation = Nothing, 
                        lAccessLog = Nothing, 
                        lErrorLog = Nothing, 
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
                        lRoot = Nothing, 
                        lSatisfy = Nothing, 
                        lSendLowat = Nothing, 
                        lSendTimeout = Nothing, 
                        lSendFile = Nothing, 
                        lSendFileMaxChunks = Nothing, 
                        lServerNameInRedirect = Nothing, 
                        lServerTokens = Nothing, 
                        lTcpNodelay = Nothing, 
                        lTcpNopush = Nothing, 
                        lTryFiles = Just "$uri $uri/ =404", 
                        lTypes = Nothing, 
                        lTypesHashBucketSize = Nothing, 
                        lTypesHashMaxSize = Nothing
                    }
                ], 
                sAccessLog = Nothing, 
                sErrorLog = Nothing, 
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
                sRoot = Just "/var/www/html", 
                sSatisfy = Nothing, 
                sSendLowat = Nothing, 
                sSendTimeout = Nothing, 
                sSendFile = Nothing, 
                sSendFileMaxChunks = Nothing, 
                sServerName = Just ["example.com"], 
                sServerNameInRedirect = Nothing, 
                sServerTokens = Nothing, 
                sSsl = Nothing, 
                sSslBufferSize = Nothing, 
                sSslCertificate = Nothing, 
                sSslCertificateKey = Nothing, 
                sSslCiphers = Nothing, 
                sSslClientCertificate = Nothing, 
                sSslCrl = Nothing, 
                sSslDhparam = Nothing, 
                sSslEcdhCurve = Nothing, 
                sSslPasswordFile = Nothing, 
                sSslPreferServerCiphers = Nothing, 
                sSslProtocols = Nothing, 
                sSslSessionCache = Nothing, 
                sSslSessionTicketKey = Nothing, 
                sSslSessionTickets = Nothing, 
                sSslSessionTimeout = Nothing, 
                sSslStapling = Nothing, 
                sSslStaplingFile = Nothing, 
                sSslStaplingResponder = Nothing, 
                sSslStaplingVerify = Nothing, 
                sSslTrustedCertificate = Nothing, 
                sSslVerifyClient = Nothing, 
                sSslVerifyDepth = Nothing, 
                sTcpNodelay = Nothing, 
                sTcpNopush = Nothing, 
                sTryFiles = Nothing, 
                sTypes = Nothing, 
                sTypesHashBucketSize = Nothing, 
                sTypesHashMaxSize = Nothing, 
                sUnderscoresInHeaders = Nothing
            },
            Server { 
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
                sErrorPage = Nothing, 
                sEtag = Nothing, 
                sExpires = Nothing,
                sGzip = Nothing, 
                sGzipBuffers = Nothing, 
                sGzipCompLevel = Nothing, 
                sGzipDisable = Nothing, 
                sGzipMinLength = Nothing, 
                sGzipHttpVersion = Nothing, 
                sGzipProxied = Nothing, 
                sGzipTypes = Nothing, 
                sGzipVary = Nothing, 
                sIfModifiedSince = Nothing, 
                sIgnoreInvalidHeaders = Nothing, 
                sInclude = Nothing, 
                sIndex = Nothing, 
                sKeepaliveDisable = Nothing, 
                sKeepaliveRequests = Nothing, 
                sKeepaliveTimeout = Nothing, 
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
                sListen = Just ["443"], 
                sLocation = Nothing,
                sAccessLog = Nothing, 
                sErrorLog = Nothing, 
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
                sRoot = Just "/var/www/html_secure", 
                sSatisfy = Nothing, 
                sSendLowat = Nothing, 
                sSendTimeout = Nothing, 
                sSendFile = Nothing, 
                sSendFileMaxChunks = Nothing, 
                sServerName = Just ["example.com"], 
                sServerNameInRedirect = Nothing, 
                sServerTokens = Nothing, 
                sSsl = Just "on", 
                sSslBufferSize = Nothing, 
                sSslCertificate = Just "/etc/nginx/certs/cert.pem", 
                sSslCertificateKey = Just "/etc/nginx/certs/cert.key",
                sSslCiphers = Nothing, 
                sSslClientCertificate = Nothing, 
                sSslCrl = Nothing, 
                sSslDhparam = Nothing, 
                sSslEcdhCurve = Nothing, 
                sSslPasswordFile = Nothing, 
                sSslPreferServerCiphers = Nothing, 
                sSslProtocols = Nothing, 
                sSslSessionCache = Nothing, 
                sSslSessionTicketKey = Nothing, 
                sSslSessionTickets = Nothing, 
                sSslSessionTimeout = Nothing, 
                sSslStapling = Nothing, 
                sSslStaplingFile = Nothing, 
                sSslStaplingResponder = Nothing, 
                sSslStaplingVerify = Nothing, 
                sSslTrustedCertificate = Nothing, 
                sSslVerifyClient = Nothing, 
                sSslVerifyDepth = Nothing, 
                sTcpNodelay = Nothing, 
                sTcpNopush = Nothing, 
                sTryFiles = Nothing, 
                sTypes = Nothing, 
                sTypesHashBucketSize = Nothing, 
                sTypesHashMaxSize = Nothing, 
                sUnderscoresInHeaders = Nothing
            }
        ], 
        hServerNameInRedirect = Nothing, 
        hServerNamesHashBucketSize = Nothing, 
        hServerNamesHashMaxSize = Nothing, 
        hServerTokens = Nothing, 
        hSsl = Nothing, 
        hSslBufferSize = Nothing, 
        hSslCertificate = Nothing, 
        hSslCertificateKey = Nothing, 
        hSslCiphers = Nothing, 
        hSslClientCertificate = Nothing, 
        hSslCrl = Nothing, 
        hSslDhparam = Nothing, 
        hSslEcdhCurve = Nothing, 
        hSslPasswordFile = Nothing, 
        hSslPreferServerCiphers = Just "on", 
        hSslProtocols = Just "TLSv1 TLSv1.1 TLSv1.2",
        hSslSessionCache = Nothing, 
        hSslSessionTicketKey = Nothing, 
        hSslSessionTickets = Nothing, 
        hSslSessionTimeout = Nothing, 
        hSslStapling = Nothing, 
        hSslStaplingFile = Nothing, 
        hSslStaplingResponder = Nothing, 
        hSslStaplingVerify = Nothing, 
        hSslTrustedCertificate = Nothing, 
        hSslVerifyClient = Nothing, 
        hSslVerifyDepth = Nothing, 
        hTcpNodelay = Just "on",
        hTcpNopush = Just "on", 
        hTypes = Nothing, 
        hTypesHashBucketSize = Nothing, 
        hTypesHashMaxSize = Just "2048", 
        hUnderscoreInHeaders = Nothing, 
        hVariablesHashBucketSize = Nothing, 
        hVariablesHashMaxSize = Nothing
    } 
}















{-
NginxWebserver { 
    nID = "1", 
    nDaemon = "on", 
    nErrorLog = "logs/error.log error", 
    nEnv = ["TZ"], 
    nInclude = [], 
    nLockFile = "logs/nginx.lock", 
    nPid = "/run/nginx.pid", 
    nSslEngine = Nothing, 
    nThreadPool = ["default threads=32 max_queue=65536"], 
    nTimerResolution = "", 
    nUser = "www-data", 
    nWorkerCpuAffinity = "", 
    nWorkerPriority = "0", 
    nWorkerProcesses = "4", 
    nWorkerRLimitCore = "", 
    nWorkerRLimitNoFile = "", 
    nWorkingDirectory = "", 
    nEvents = Events { 
        eAcceptMutex = "on", 
        eAcceptMutexDelay = "500ms", 
        eInclude = [], 
        eMultiAccept = "off", 
        eUse = "", 
        eWorkerAioRequests = "32", 
        eWorkerConnections = "768" 
    }, 
    nHttp = Http { 
        hAccessRule = [], 
        hAddHeader = [], 
        hAio = "off", 
        hAuthBasic = "off", 
        hAuthBasicUserFile = Nothing, 
        hAutoIndexConf = (NAutoindexConf False True "html" False), 
        hBrowserConf = (NBrowserConf [] "1" [] "1"), 
        hCharsetConf = (NCharsetConf "off" False ""), 
        hCharsetMap = [], 
        hCharsetType = "", 
        hChunkedTransferEncoding = True, 
        hClientHeaderBufferSize = "1k", 
        hClientBodyConf = (NClientBodyConf "16k" "off" False "client_body_temp" "60s" "1m"), 
        hClientHeaderTimeout = "60s", 
        hConnectionPoolSize = 256, 
        hDefaultType = "application/octet-stream", 
        hDirectio = "off", 
        hDirectioAlignment = 512, 
        hDisableSymlinks = "off", 
        hErrorPage = [], 
        hEtag = True, 
        hExpires = "off", 
        {-FastcgiCachePath NFastcgiConf -}
        hGzipConf = (NGzipConf True "16 8k" 1 "msie6" 20 1.1 "off" "text/html" False), 
        hIfModifiedSince = "exact", 
        hIgnoreInvalidHeaders = True, 
        hInclude = [], 
        hIndex = "index.html", 
        hKeepaliveConf = (NKeepaliveConf "msie6" 100 "65"), 
        hLargeClientHeaderBuffers = "4 8k", 
        hLimitConnConf = (NLimitConnConf [] "error" 503), 
        hLimitConnZone = [], 
        hLimitRate = "0", 
        hLimitRateAfter = "0", 
        hLimitReqConf = (NLimitReqConf [] "error" 503), 
        hLimitReqZone = [], 
        hLingeringConf = (NLingeringConf "on" "30s" "5s"), 
        hLogConf = (NLogConf "/var/log/nginx/access.log" "/var/log/nginx/error.log" True False "off"), 
        hLogFormat = [], hMapConf = (NMapConf [] "64" "2048"), hMaxRanges = "", hMemcachedConf = (NMemcachedConf "" "8k" "60s" False "" "error timeout" "0" 0 "60s" "60s"), hMergeSlashes = True, hMsiePadding = True, hMsieRefresh = False, hOpenFileCacheConf = (NOpenFileCacheConf "off" False 1 "60s"), hOutputConf = (NOutputConf "2 32k" "1460"), hPortInRedirect = True, {-ProxyCachePath NProxyConf -}hReadAhead = "0", hRecursiveErrorPages = False, hRequestPoolSize = "4k", hResetTimedoutConnection = False, hResolver = "", hResolverTimeout = "30s", hRoot = "html", hSatisfy = "all", hSendLowat = "0", hSendTimeout = "60s", hSendFile = True, hSendFileMaxChunks = "0"
    , hServer = 
    [
    Server { sAccessRule = [], sAddHeader = [], sAio = "off", sAuthConf = (NAuthConf "off" ""), sAutoIndexConf = (NAutoindexConf False True "html" False), sBrowserConf = (NBrowserConf [] "1" [] "1"), sCharsetConf = (NCharsetConf "off" False ""), sCharsetType = "", sChunkedTransferEncoding = True, sClientHeaderBufferSize = "1k", sClientBodyConf = (NClientBodyConf "16k" "off" False "client_body_temp" "60s" "1m"), sClientHeaderTimeout = "60s", sConnectionPoolSize = 256, sDefaultType = "application/octet-stream", sDirectio = "off", sDirectioAlignment = 512, sDisableSymlinks = "off", sErrorPage = [], sEtag = True, sExpires = "off", {-NFastcgiConf -}sGzipConf = (NGzipConf False "16 8k" 1 "" 20 1.1 "off" "text/html" False), sIfModifiedSince = "exact", sIgnoreInvalidHeaders = True, sInclude = [], sIndex = "index.html", sKeepaliveConf = (NKeepaliveConf "msie6" 100 "75s"), sLargeClientHeaderBuffers = "4 8k", sLimitConnConf = (NLimitConnConf [] "error" 503), sLimitRate = "0", sLimitRateAfter = "0", sLimitReqConf = (NLimitReqConf [] "error" 503), sLingeringConf = (NLingeringConf "on" "30s" "5s"), sListen = ["80 default_server", "[=]:80 default_server"]
        , sLocation = [
        Location { lLocationPath = "/", lAccessRule = [], lAddHeader = [], lAio = "off", lAlias = "", lAuthConf = (NAuthConf "off" ""), lAutoIndexConf = (NAutoindexConf False True "html" False), lBrowserConf = (NBrowserConf [] "1" [] "1"), lCharsetConf = (NCharsetConf "off" False ""), lCharsetType = "", lChunkedTransferEncoding = True, lClientBodyConf = (NClientBodyConf "16k" "off" False "client_body_temp" "60s" "1m"), lDefaultType = "application/octet-stream", lDirectio = "off", lDirectioAlignment = 512, lDisableSymlinks = "off", lEmptyGif = False, lErrorPage = [], lEtag = True, lExpires = "off", {-NFastcgiConf FastcgiPass FastcgiSplitPathInfo -}lGzipConf = (NGzipConf False "16 8k" 1 "" 20 1.1 "off" "text/html" False), lIfModifiedSince = "exact", lInclude = [], lIndex = "index.html", lInternal = False, lKeepaliveConf = (NKeepaliveConf "msie6" 100 "75s"), lLimitConnConf = (NLimitConnConf [] "error" 503), lLimitExcept = [], lLimitRate = "0", lLimitRateAfter = "0", lLimitReqConf = (NLimitReqConf [] "error" 503), lLingeringConf = (NLingeringConf "on" "30s" "5s"), lLocation = [], lLogConf = (NLogConf "" "" True False "off"), lMaxRanges = "", lMemcachedConf = (NMemcachedConf "" "8k" "60s" False "" "error timeout" "0" 0 "60s" "60s"), lMemcachedPass = "", lMsiePadding = True, lMsieRefresh = False, lOpenFileCacheConf = (NOpenFileCacheConf "off" False 1 "60s"), lOutputConf = (NOutputConf "2 32k" "1460"), lPortInRedirect = True, {-NProxyConf ProxyPass -}lReadAhead = "0", lRecursiveErrorPages = False, lRefererConf = (NRefererConf [] "64" "2048"), lResetTimedoutConnection = False, lResolver = "", lResolverTimeout = "30s", lRoot = "html", lSatisfy = "all", lSendLowat = "0", lSendTimeout = "60s", lSendFile = False, lSendFileMaxChunks = "0", lServerNameInRedirect = False, lServerTokens = True, lTcpNodelay = True, lTcpNopush = False, lTryFiles = "$uri $uri/ =404", lTypeConf = (NTypeConf [("text/html", ["html"]), ("image/gif", ["gif"]), ("image/jpeg", ["jpg"])] "64" "1024") }
        ]
, sLogConf = (NLogConf "" "" True False "off"), sMaxRanges = "", sMemcachedConf = (NMemcachedConf "" "8k" "60s" False "" "error timeout" "0" 0 "60s" "60s"), sMergeSlashes = True, sMsiePadding = True, sMsieRefresh = False, sOpenFileCacheConf = (NOpenFileCacheConf "off" False 1 "60s"), sOutputConf = (NOutputConf "2 32k" "1460"), sPortInRedirect = True, {-NProxyConf -}sReadAhead = "0", sRecursiveErrorPages = False, sRefererConf = (NRefererConf [] "64" "2048"), sRequestPoolSize = "4k", sResetTimedoutConnection = False, sResolver = "", sResolverTimeout = "30s", sRoot = "/var/www/html", sSatisfy = "all", sSendLowat = "0", sSendTimeout = "60s", sSendFile = False, sSendFileMaxChunks = "0", sServerName = "_", sServerNameInRedirect = False, sServerTokens = True, sSslConf = (NSslConf False "16k" "" "" "HIGH:!aNULL:!MD5" "" "" "" "prime256v1" "" False "" "none" [] True "5m" False "" "" False "" "off" 1), sTcpNodelay = True, sTcpNopush = False, sTryFiles = "", sTypeConf = (NTypeConf [("text/html", ["html"]), ("image/gif", ["gif"]), ("image/jpeg", ["jpg"])] "64" "1024"), sUnderscoresInHeaders = False }
    ]
, hServerNameInRedirect = False, hServerNamesHashBucketSize = "64", hServerNamesHashMaxSize = "512", hServerTokens = True, hSslConf = (NSslConf False "16k" "" "" "HIGH:!aNULL:!MD5" "" "" "" "prime256v1" "" True "TLSv1 TLSv1.1 TLSv1.2" "none" [] True "5m" False "" "" False "" "off" 1), hTcpNodelay = True, hTcpNopush = True, hTypeConf = (NTypeConf [("text/html", ["html"]), ("image/gif", ["gif"]), ("image/jpeg", ["jpg"])] "64" "1024"), hUnderscoreInHeaders = False, hVariablesHashBucketSize = "64", hVariablesHashMaxSize = "1024" }
}
-}

