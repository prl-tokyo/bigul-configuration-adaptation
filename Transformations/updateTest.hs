{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric  #-}

----BiGUL imports
import Generics.BiGUL
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Control.Monad
import GHC.Generics

import TransfoNginxNewToOld
import TypeFiles.NginxNewTypes as N
import TypeFiles.NginxTypes 
import TypeFiles.Common
import NginxDefaultValues

common :: Either ErrorInfo CommonWebserver
common = get (transNginxNew defaults) nginxServerNew

nginxServerNew = N.NginxWebserver { nnDaemon = Nothing,
        nnErrorLog = Just ("/var/log/nginx/error.log", "644"),
        nnEnv = Nothing,
        nnInclude = Nothing,
        nnLockFile = Nothing,
        nnPid = Just "/run/nginx.pid",
        nnSslEngine = Nothing,
        nnThreadPool = Nothing,
        nnTimerResolution = Nothing,
        nnUser = Just "www-data",
        nnWorkerCpuAffinity = Nothing,
        nnWorkerPriority = Nothing,
        nnWorkerProcesses = Just "4",
        nnWorkerRLimitCore = Nothing,
        nnWorkerRLimitNoFile = Nothing,
        nnWorkingDirectory = Nothing,
        nnEvents = Just (Events {eAcceptMutex = Nothing,
                        eAcceptMutexDelay = Nothing,
                        eInclude = Nothing,
                        eMultiAccept = Nothing,
                        eUse = Nothing,
                        eWorkerAioRequests = Nothing,
                        eWorkerConnections = Just "768"}),
        nnHttp = Just (Http {hAccessRule = Nothing,
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
                        hSendFile = Nothing,
                        hSendFileMaxChunks = Nothing,
                        hServer = Just [Server {sAccessRule = Nothing,
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
                                sLocation = Just [Location {lLocationPath = Just "/",
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
                                        lTypesHashMaxSize = Nothing}],
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
                                sUnderscoresInHeaders = Nothing},Server {sAccessRule = Nothing,
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
                                        sServerName = Just ["example2.com"],
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
                                        sUnderscoresInHeaders = Nothing}],
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
                                hVariablesHashMaxSize = Nothing})}
