{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric  #-}
module TypeFiles.NginxTypes where


import Generics.BiGUL
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Control.Monad
import GHC.Generics

--Contexts
data NginxWebserver = NginxWebserver { 
    nDaemon :: Maybe Daemon, 
    nErrorLog :: Maybe ErrorLog, 
    nEnv :: Maybe [Env], 
    nInclude :: Maybe [Include], 
    nLockFile :: Maybe LockFile, 
    nPid :: Maybe Pid, 
    nSslEngine :: Maybe SslEngine, 
    nThreadPool :: Maybe [ThreadPool], 
    nTimerResolution :: Maybe TimerResolution, 
    nUser :: Maybe User, 
    nWorkerCpuAffinity :: Maybe WorkerCpuAffinity, 
    nWorkerPriority :: Maybe WorkerPriority, 
    nWorkerProcesses :: Maybe WorkerProcesses, 
    nWorkerRLimitCore :: Maybe WorkerRlimitCore, 
    nWorkerRLimitNoFile :: Maybe WorkerRlimitNofile, 
    nWorkingDirectory :: Maybe WorkingDirectory, 
    nEvents :: Maybe Events, 
    nHttp :: Maybe Http 
} deriving (Show,Eq)

data Events = Events { 
    eAcceptMutex :: Maybe AcceptMutex, 
    eAcceptMutexDelay :: Maybe AcceptMutexDelay, 
    eInclude :: Maybe [Include], 
    eMultiAccept :: Maybe MultiAccept, 
    eUse :: Maybe Use, 
    eWorkerAioRequests :: Maybe WorkerAioRequests, 
    eWorkerConnections :: Maybe WorkerConnections 
} deriving (Show,Eq)

data Http = Http { 
    hAccessRule :: Maybe [AccessRule], 
    hAddHeader :: Maybe [AddHeader], 
    hAio :: Maybe Aio, 
    hAuthBasic :: Maybe AuthBasic, 
    hAuthBasicUserFile :: Maybe AuthBasicUserFile, 
    hAutoindex :: Maybe Autoindex, 
    hAiExactSize :: Maybe AiExactSize, 
    hAiFormat :: Maybe AiFormat, 
    hAiLocaltime :: Maybe AiLocaltime, 
    hAncientBrowser :: Maybe [AncientBrowser], 
    hAncientBrowserValue :: Maybe AncientBrowserValue, 
    hModernBrowser :: Maybe [ModernBrowser], 
    hModernBrowserValue :: Maybe ModernBrowserValue, 
    hCharset :: Maybe Charset, 
    hOverrideCharset :: Maybe OverrideCharset, 
    hSourceCharset :: Maybe SourceCharset, 
    hCharsetMap :: Maybe [CharsetMap], 
    hCharsetType :: Maybe CharsetType, 
    hChunkedTransferEncoding :: Maybe ChunkedTransferEncoding, 
    hClientHeaderBufferSize :: Maybe ClientHeaderBufferSize, 
    hClientBodyBufferSize :: Maybe ClientBodyBufferSize, 
    hClientBodyInFileOnly :: Maybe ClientBodyInFileOnly, 
    hClientBodyInSingleBuffer :: Maybe ClientBodyInSingleBuffer, 
    hClientBodyTempPath :: Maybe ClientBodyTempPath, 
    hClientBodyTimeout :: Maybe ClientBodyTimeout, 
    hClientMaxBodySize :: Maybe ClientMaxBodySize, 
    hClientHeaderTimeout :: Maybe ClientHeaderTimeout, 
    hConnectionPoolSize :: Maybe ConnectionPoolSize, 
    hDefaultType :: Maybe DefaultType, 
    hDirectio :: Maybe Directio, 
    hDirectioAlignment :: Maybe DirectioAlignment, 
    hDisableSymlinks :: Maybe DisableSymlinks, 
    hErrorPage :: Maybe [ErrorPage], 
    hEtag :: Maybe Etag, 
    hExpires :: Maybe Expires, 
    {-FastcgiCachePath NFastcgiConf -}
    hGzip :: Maybe Gzip, 
    hGzipBuffers :: Maybe GzipBuffers, 
    hGzipCompLevel :: Maybe GzipCompLevel, 
    hGzipDisable :: Maybe GzipDisable, 
    hGzipMinLength :: Maybe GzipMinLength, 
    hGzipHttpVersion :: Maybe GzipHttpVersion, 
    hGzipProxied :: Maybe GzipProxied, 
    hGzipTypes :: Maybe GzipTypes, 
    hGzipVary :: Maybe GzipVary, 
    hIfModifiedSince :: Maybe IfModifiedSince, 
    hIgnoreInvalidHeaders :: Maybe IgnoreInvalidHeaders, 
    hInclude :: Maybe [Include], 
    hIndex :: Maybe Index, 
    hKeepaliveDisable :: Maybe KeepaliveDisable, 
    hKeepaliveRequests :: Maybe KeepaliveRequests, 
    hKeepaliveTimeout :: Maybe KeepaliveTimeout, 
    hLargeClientHeaderBuffers :: Maybe LargeClientHeaderBuffers, 
    hLimitConn :: Maybe [LimitConn], 
    hLimitConnLogLevel :: Maybe LimitConnLogLevel, 
    hLimitConnStatus :: Maybe LimitConnStatus, 
    hLimitConnZone :: Maybe [LimitConnZone], 
    hLimitRate :: Maybe LimitRate, 
    hLimitRateAfter :: Maybe LimitRateAfter, 
    hLimitReq :: Maybe [LimitReq], 
    hLimitReqLogLevel :: Maybe LimitReqLogLevel, 
    hLimitReqStatus :: Maybe LimitReqStatus, 
    hLimitReqZone :: Maybe [LimitReqZone], 
    hLingeringClose :: Maybe LingeringClose, 
    hLingeringTime :: Maybe LingeringTime, 
    hLingeringTimeout :: Maybe LingeringTimeout, 
    hAccessLog :: Maybe AccessLog, 
    hErrorLog :: Maybe ErrorLog, 
    hLogNotFound :: Maybe LogNotFound, 
    hLogSubrequest :: Maybe LogSubrequest, 
    hOpenLogFileCache :: Maybe OpenLogFileCache, 
    hLogFormat :: Maybe [LogFormat], 
    hMap :: Maybe [Map], 
    hMapHashBucketSize :: Maybe MapHashBucketSize, 
    hMapHashMaxSize :: Maybe MapHashMaxSize, 
    hMaxRanges :: Maybe MaxRanges, 
    hMemcachedBind :: Maybe MemcachedBind, 
    hMemCachedBufferSize :: Maybe MemCachedBufferSize, 
    hMemcachedConnectTimeout :: Maybe MemcachedConnectTimeout, 
    hMemcachedForceRanges :: Maybe MemcachedForceRanges, 
    hMemcachedGzipFlag :: Maybe MemcachedGzipFlag, 
    hMemcachedNextUpstream :: Maybe MemcachedNextUpstream, 
    hMemcachedNextUpstreamTimeout :: Maybe MemcachedNextUpstreamTimeout, 
    hMemcachedNextUpstreamTries :: Maybe MemcachedNextUpstreamTries, 
    hMemcachedReadTimeout :: Maybe MemcachedReadTimeout, 
    hMemcachedSendTimeout :: Maybe MemcachedSendTimeout, 
    hMergeSlashes :: Maybe MergeSlashes, 
    hMsiePadding :: Maybe MsiePadding, 
    hMsieRefresh :: Maybe MsieRefresh, 
    hOpenFileCache :: Maybe OpenFileCache, 
    hOpenFileCacheErrors :: Maybe OpenFileCacheErrors, 
    hOpenFileCacheMinUses :: Maybe OpenFileCacheMinUses, 
    hOpenFileCacheValid :: Maybe OpenFileCacheValid, 
    hOutputBuffers :: Maybe OutputBuffers, 
    hPostponeOutput :: Maybe PostponeOutput, 
    hPortInRedirect :: Maybe PortInRedirect, 
    {-ProxyCachePath NProxyConf -}
    hReadAhead :: Maybe ReadAhead, 
    hRecursiveErrorPages :: Maybe RecursiveErrorPages, 
    hRequestPoolSize :: Maybe RequestPoolSize, 
    hResetTimedoutConnection :: Maybe ResetTimedoutConnection, 
    hResolver :: Maybe Resolver, 
    hResolverTimeout :: Maybe ResolverTimeout, 
    hRoot :: Maybe Root, 
    hSatisfy :: Maybe Satisfy, 
    hSendLowat :: Maybe SendLowat, 
    hSendTimeout :: Maybe SendTimeout, 
    hSendFile :: Maybe SendFile, 
    hSendFileMaxChunks :: Maybe SendFileMaxChunks, 
    hServer :: Maybe [Server], 
    hServerNameInRedirect :: Maybe ServerNameInRedirect, 
    hServerNamesHashBucketSize :: Maybe ServerNamesHashBucketSize, 
    hServerNamesHashMaxSize :: Maybe ServerNamesHashMaxSize, 
    hServerTokens :: Maybe ServerTokens, 
    hSsl :: Maybe Ssl, 
    hSslBufferSize :: Maybe SslBufferSize, 
    hSslCertificate :: Maybe SslCertificate, 
    hSslCertificateKey :: Maybe SslCertificateKey, 
    hSslCiphers :: Maybe SslCiphers, 
    hSslClientCertificate :: Maybe SslClientCertificate, 
    hSslCrl :: Maybe SslCrl, 
    hSslDhparam :: Maybe SslDhparam, 
    hSslEcdhCurve :: Maybe SslEcdhCurve, 
    hSslPasswordFile :: Maybe SslPasswordFile, 
    hSslPreferServerCiphers :: Maybe SslPreferServerCiphers, 
    hSslProtocols :: Maybe SslProtocols, 
    hSslSessionCache :: Maybe SslSessionCache, 
    hSslSessionTicketKey :: Maybe [SslSessionTicketKey], 
    hSslSessionTickets :: Maybe SslSessionTickets, 
    hSslSessionTimeout :: Maybe SslSessionTimeout, 
    hSslStapling :: Maybe SslStapling, 
    hSslStaplingFile :: Maybe SslStaplingFile, 
    hSslStaplingResponder :: Maybe SslStaplingResponder, 
    hSslStaplingVerify :: Maybe SslStaplingVerify, 
    hSslTrustedCertificate :: Maybe SslTrustedCertificate, 
    hSslVerifyClient :: Maybe SslVerifyClient, 
    hSslVerifyDepth :: Maybe SslVerifyDepth, 
    hTcpNodelay :: Maybe TcpNodelay, 
    hTcpNopush :: Maybe TcpNopush, 
    hTypes :: Maybe Types, 
    hTypesHashBucketSize :: Maybe TypesHashBucketSize, 
    hTypesHashMaxSize :: Maybe TypesHashMaxSize, 
    hUnderscoreInHeaders :: Maybe UnderscoresInHeaders, 
    hVariablesHashBucketSize :: Maybe VariablesHashBucketSize, 
    hVariablesHashMaxSize :: Maybe VariablesHashMaxSize 
} deriving (Show,Eq)

data Server = Server { 
    sAccessRule :: Maybe [AccessRule], 
    sAddHeader :: Maybe [AddHeader], 
    sAio :: Maybe Aio, 
    sAuthBasic :: Maybe AuthBasic, 
    sAuthBasicUserFile :: Maybe AuthBasicUserFile, 
    sAutoindex :: Maybe Autoindex, 
    sAiExactSize :: Maybe AiExactSize, 
    sAiFormat :: Maybe AiFormat, 
    sAiLocaltime :: Maybe AiLocaltime, 
    sAncientBrowser :: Maybe [AncientBrowser], 
    sAncientBrowserValue :: Maybe AncientBrowserValue, 
    sModernBrowser :: Maybe [ModernBrowser], 
    sModernBrowserValue :: Maybe ModernBrowserValue, 
    sCharset :: Maybe Charset, 
    sOverrideCharset :: Maybe OverrideCharset, 
    sSourceCharset :: Maybe SourceCharset, 
    sCharsetType :: Maybe CharsetType, 
    sChunkedTransferEncoding :: Maybe ChunkedTransferEncoding, 
    sClientHeaderBufferSize :: Maybe ClientHeaderBufferSize, 
    sClientBodyBufferSize :: Maybe ClientBodyBufferSize, 
    sClientBodyInFileOnly :: Maybe ClientBodyInFileOnly, 
    sClientBodyInSingleBuffer :: Maybe ClientBodyInSingleBuffer, 
    sClientBodyTempPath :: Maybe ClientBodyTempPath, 
    sClientBodyTimeout :: Maybe ClientBodyTimeout, 
    sClientMaxBodySize :: Maybe ClientMaxBodySize, 
    sClientHeaderTimeout :: Maybe ClientHeaderTimeout, 
    sConnectionPoolSize :: Maybe ConnectionPoolSize, 
    sDefaultType :: Maybe DefaultType, 
    sDirectio :: Maybe Directio, 
    sDirectioAlignment :: Maybe DirectioAlignment, 
    sDisableSymlinks :: Maybe DisableSymlinks, 
    sErrorPage :: Maybe [ErrorPage], 
    sEtag :: Maybe Etag, 
    sExpires :: Maybe Expires, 
    sGzip :: Maybe Gzip, 
    sGzipBuffers :: Maybe GzipBuffers, 
    sGzipCompLevel :: Maybe GzipCompLevel, 
    sGzipDisable :: Maybe GzipDisable, 
    sGzipMinLength :: Maybe GzipMinLength, 
    sGzipHttpVersion :: Maybe GzipHttpVersion, 
    sGzipProxied :: Maybe GzipProxied, 
    sGzipTypes :: Maybe GzipTypes, 
    sGzipVary :: Maybe GzipVary, 
    sIfModifiedSince :: Maybe IfModifiedSince, 
    sIgnoreInvalidHeaders :: Maybe IgnoreInvalidHeaders, 
    sInclude :: Maybe [Include], 
    sIndex :: Maybe Index, 
    sKeepaliveDisable :: Maybe KeepaliveDisable, 
    sKeepaliveRequests :: Maybe KeepaliveRequests, 
    sKeepaliveTimeout :: Maybe KeepaliveTimeout, 
    sLargeClientHeaderBuffers :: Maybe LargeClientHeaderBuffers, 
    sLimitConn :: Maybe [LimitConn], 
    sLimitConnLogLevel :: Maybe LimitConnLogLevel, 
    sLimitConnStatus :: Maybe LimitConnStatus, 
    sLimitRate :: Maybe LimitRate, 
    sLimitRateAfter :: Maybe LimitRateAfter, 
    sLimitReq :: Maybe [LimitReq], 
    sLimitReqLogLevel :: Maybe LimitReqLogLevel, 
    sLimitReqStatus :: Maybe LimitReqStatus, 
    sLingeringClose :: Maybe LingeringClose, 
    sLingeringTime :: Maybe LingeringTime, 
    sLingeringTimeout :: Maybe LingeringTimeout, 
    sListen :: Maybe [Listen], 
    sLocation :: Maybe [Location], 
    sAccessLog :: Maybe AccessLog, 
    sErrorLog :: Maybe ErrorLog, 
    sLogNotFound :: Maybe LogNotFound, 
    sLogSubrequest :: Maybe LogSubrequest, 
    sOpenLogFileCache :: Maybe OpenLogFileCache, 
    sMaxRanges :: Maybe MaxRanges, 
    sMemcachedBind :: Maybe MemcachedBind, 
    sMemCachedBufferSize :: Maybe MemCachedBufferSize, 
    sMemcachedConnectTimeout :: Maybe MemcachedConnectTimeout, 
    sMemcachedForceRanges :: Maybe MemcachedForceRanges, 
    sMemcachedGzipFlag :: Maybe MemcachedGzipFlag, 
    sMemcachedNextUpstream :: Maybe MemcachedNextUpstream, 
    sMemcachedNextUpstreamTimeout :: Maybe MemcachedNextUpstreamTimeout, 
    sMemcachedNextUpstreamTries :: Maybe MemcachedNextUpstreamTries, 
    sMemcachedReadTimeout :: Maybe MemcachedReadTimeout, 
    sMemcachedSendTimeout :: Maybe MemcachedSendTimeout, 
    sMergeSlashes :: Maybe MergeSlashes, 
    sMsiePadding :: Maybe MsiePadding, 
    sMsieRefresh :: Maybe MsieRefresh, 
    sOpenFileCache :: Maybe OpenFileCache, 
    sOpenFileCacheErrors :: Maybe OpenFileCacheErrors, 
    sOpenFileCacheMinUses :: Maybe OpenFileCacheMinUses, 
    sOpenFileCacheValid :: Maybe OpenFileCacheValid, 
    sOutputBuffers :: Maybe OutputBuffers, 
    sPostponeOutput :: Maybe PostponeOutput, 
    sPortInRedirect :: Maybe PortInRedirect, 
    sReadAhead :: Maybe ReadAhead, 
    sRecursiveErrorPages :: Maybe RecursiveErrorPages, 
    sValidReferer :: Maybe [ValidReferer], 
    sRefererHashBucketSize :: Maybe RefererHashBucketSize, 
    sRefererHashMaxSize :: Maybe RefererHashMaxSize, 
    sRequestPoolSize :: Maybe RequestPoolSize, 
    sResetTimedoutConnection :: Maybe ResetTimedoutConnection, 
    sResolver :: Maybe Resolver, 
    sResolverTimeout :: Maybe ResolverTimeout, 
    sRoot :: Maybe Root, 
    sSatisfy :: Maybe Satisfy, 
    sSendLowat :: Maybe SendLowat, 
    sSendTimeout :: Maybe SendTimeout, 
    sSendFile :: Maybe SendFile, 
    sSendFileMaxChunks :: Maybe SendFileMaxChunks, 
    sServerName :: Maybe [ServerName], 
    sServerNameInRedirect :: Maybe ServerNameInRedirect, 
    sServerTokens :: Maybe ServerTokens, 
    sSsl :: Maybe Ssl, 
    sSslBufferSize :: Maybe SslBufferSize, 
    sSslCertificate :: Maybe SslCertificate, 
    sSslCertificateKey :: Maybe SslCertificateKey, 
    sSslCiphers :: Maybe SslCiphers, 
    sSslClientCertificate :: Maybe SslClientCertificate, 
    sSslCrl :: Maybe SslCrl, 
    sSslDhparam :: Maybe SslDhparam, 
    sSslEcdhCurve :: Maybe SslEcdhCurve, 
    sSslPasswordFile :: Maybe SslPasswordFile, 
    sSslPreferServerCiphers :: Maybe SslPreferServerCiphers, 
    sSslProtocols :: Maybe SslProtocols, 
    sSslSessionCache :: Maybe SslSessionCache, 
    sSslSessionTicketKey :: Maybe [SslSessionTicketKey], 
    sSslSessionTickets :: Maybe SslSessionTickets, 
    sSslSessionTimeout :: Maybe SslSessionTimeout, 
    sSslStapling :: Maybe SslStapling, 
    sSslStaplingFile :: Maybe SslStaplingFile, 
    sSslStaplingResponder :: Maybe SslStaplingResponder, 
    sSslStaplingVerify :: Maybe SslStaplingVerify, 
    sSslTrustedCertificate :: Maybe SslTrustedCertificate, 
    sSslVerifyClient :: Maybe SslVerifyClient, 
    sSslVerifyDepth :: Maybe SslVerifyDepth, 
    sTcpNodelay :: Maybe TcpNodelay, 
    sTcpNopush :: Maybe TcpNopush, 
    sTryFiles :: Maybe TryFiles, 
    sTypes :: Maybe Types, 
    sTypesHashBucketSize :: Maybe TypesHashBucketSize, 
    sTypesHashMaxSize :: Maybe TypesHashMaxSize, 
    sUnderscoresInHeaders :: Maybe UnderscoresInHeaders 
} deriving (Show, Eq) --N: server{}

data Location = Location { 
    lLocationPath :: Maybe LocationPath, 
    lAccessRule :: Maybe [AccessRule], 
    lAddHeader :: Maybe [AddHeader], 
    lAio :: Maybe Aio, 
    lAlias :: Maybe Alias, 
    lAuthBasic :: Maybe AuthBasic, 
    lAuthBasicUserFile :: Maybe AuthBasicUserFile, 
    lAutoindex :: Maybe Autoindex, 
    lAiExactSize :: Maybe AiExactSize, 
    lAiFormat :: Maybe AiFormat, 
    lAiLocaltime :: Maybe AiLocaltime, 
    lAncientBrowser :: Maybe [AncientBrowser], 
    lAncientBrowserValue :: Maybe AncientBrowserValue, 
    lModernBrowser :: Maybe [ModernBrowser], 
    lModernBrowserValue :: Maybe ModernBrowserValue, 
    lCharset :: Maybe Charset, 
    lOverrideCharset :: Maybe OverrideCharset, 
    lSourceCharset :: Maybe SourceCharset, 
    lCharsetType :: Maybe CharsetType, 
    lChunkedTransferEncoding :: Maybe ChunkedTransferEncoding, 
    lClientBodyBufferSize :: Maybe ClientBodyBufferSize, 
    lClientBodyInFileOnly :: Maybe ClientBodyInFileOnly, 
    lClientBodyInSingleBuffer :: Maybe ClientBodyInSingleBuffer, 
    lClientBodyTempPath :: Maybe ClientBodyTempPath, 
    lClientBodyTimeout :: Maybe ClientBodyTimeout, 
    lClientMaxBodySize :: Maybe ClientMaxBodySize, 
    lDefaultType :: Maybe DefaultType, 
    lDirectio :: Maybe Directio, 
    lDirectioAlignment :: Maybe DirectioAlignment, 
    lDisableSymlinks :: Maybe DisableSymlinks, 
    lErrorPage :: Maybe [ErrorPage], 
    lEtag :: Maybe Etag, 
    lExpires :: Maybe Expires, 
    {-NFastcgiConf FastcgiPass FastcgiSplitPathInfo -}
    lGzip :: Maybe Gzip, 
    lGzipBuffers :: Maybe GzipBuffers, 
    lGzipCompLevel :: Maybe GzipCompLevel, 
    lGzipDisable :: Maybe GzipDisable, 
    lGzipMinLength :: Maybe GzipMinLength, 
    lGzipHttpVersion :: Maybe GzipHttpVersion, 
    lGzipProxied :: Maybe GzipProxied, 
    lGzipTypes :: Maybe GzipTypes, 
    lGzipVary :: Maybe GzipVary, 
    lIfModifiedSince :: Maybe IfModifiedSince, 
    lInclude :: Maybe [Include], 
    lIndex :: Maybe Index, 
    lInternal :: Maybe Internal, 
    lKeepaliveDisable :: Maybe KeepaliveDisable, 
    lKeepaliveRequests :: Maybe KeepaliveRequests, 
    lKeepaliveTimeout :: Maybe KeepaliveTimeout, 
    lLimitConn :: Maybe [LimitConn], 
    lLimitConnLogLevel :: Maybe LimitConnLogLevel, 
    lLimitConnStatus :: Maybe LimitConnStatus, 
    {-lLimitExcept :: Maybe [LimitExcept], -}
    lLimitRate :: Maybe LimitRate, 
    lLimitRateAfter :: Maybe LimitRateAfter, 
    lLimitReq :: Maybe [LimitReq], 
    lLimitReqLogLevel :: Maybe LimitReqLogLevel, 
    lLimitReqStatus :: Maybe LimitReqStatus, 
    lLingeringClose :: Maybe LingeringClose, 
    lLingeringTime :: Maybe LingeringTime, 
    lLingeringTimeout :: Maybe LingeringTimeout, 
    lLocation :: Maybe [Location], 
    lAccessLog :: Maybe AccessLog, 
    lErrorLog :: Maybe ErrorLog, 
    lLogNotFound :: Maybe LogNotFound, 
    lLogSubrequest :: Maybe LogSubrequest, 
    lOpenLogFileCache :: Maybe OpenLogFileCache, 
    lMaxRanges :: Maybe MaxRanges, 
    lMemcachedBind :: Maybe MemcachedBind, 
    lMemCachedBufferSize :: Maybe MemCachedBufferSize, 
    lMemcachedConnectTimeout :: Maybe MemcachedConnectTimeout, 
    lMemcachedForceRanges :: Maybe MemcachedForceRanges, 
    lMemcachedGzipFlag :: Maybe MemcachedGzipFlag, 
    lMemcachedNextUpstream :: Maybe MemcachedNextUpstream, 
    lMemcachedNextUpstreamTimeout :: Maybe MemcachedNextUpstreamTimeout, 
    lMemcachedNextUpstreamTries :: Maybe MemcachedNextUpstreamTries, 
    lMemcachedReadTimeout :: Maybe MemcachedReadTimeout, 
    lMemcachedSendTimeout :: Maybe MemcachedSendTimeout, 
    lMemcachedPass :: Maybe MemcachedPass, 
    lMsiePadding :: Maybe MsiePadding, 
    lMsieRefresh :: Maybe MsieRefresh, 
    lOpenFileCache :: Maybe OpenFileCache, 
    lOpenFileCacheErrors :: Maybe OpenFileCacheErrors, 
    lOpenFileCacheMinUses :: Maybe OpenFileCacheMinUses, 
    lOpenFileCacheValid :: Maybe OpenFileCacheValid, 
    lOutputBuffers :: Maybe OutputBuffers, 
    lPostponeOutput :: Maybe PostponeOutput, 
    lPortInRedirect :: Maybe PortInRedirect, 
    {-NProxyConf ProxyPass -}
    lReadAhead :: Maybe ReadAhead, 
    lRecursiveErrorPages :: Maybe RecursiveErrorPages, 
    lValidReferer :: Maybe [ValidReferer], 
    lRefererHashBucketSize :: Maybe RefererHashBucketSize, 
    lRefererHashMaxSize :: Maybe RefererHashMaxSize, 
    lResetTimedoutConnection :: Maybe ResetTimedoutConnection, 
    lResolver :: Maybe Resolver, 
    lResolverTimeout :: Maybe ResolverTimeout, 
    lRoot :: Maybe Root, 
    lSatisfy :: Maybe Satisfy, 
    lSendLowat :: Maybe SendLowat, 
    lSendTimeout :: Maybe SendTimeout, 
    lSendFile :: Maybe SendFile, 
    lSendFileMaxChunks :: Maybe SendFileMaxChunks, 
    lServerNameInRedirect :: Maybe ServerNameInRedirect, 
    lServerTokens :: Maybe ServerTokens, 
    lTcpNodelay :: Maybe TcpNodelay, 
    lTcpNopush :: Maybe TcpNopush, 
    lTryFiles :: Maybe TryFiles, 
    lTypes :: Maybe Types, 
    lTypesHashBucketSize :: Maybe TypesHashBucketSize, 
    lTypesHashMaxSize :: Maybe TypesHashMaxSize 
} deriving (Show, Eq) 

type LocationPath = String --[ = | ~ | ~* | ^~ ] "uri"

--Core functionality
type AcceptMutex = String --on|off default:on
type AcceptMutexDelay = String --"time" default:500ms
type Daemon = String --on|off default:on
type ErrorLog = String --"file"|stderr|syslog:server="address"[,"parameter"="value"]|memory:"size" [debug|info|notice|warn|error|crit|alert|emerg] default:logs/error.log error;
type Env = String --"variable"[="value"] default:TZ
type Include = String 
type LockFile = String --"file" default:logs/nginx.lock
type MultiAccept = String --on|off default:off
type Pid = String --"file" default:nginx.pid
type SslEngine = String --"device"
type ThreadPool = String --"name" threads="number" [max_queue="number"] default:default threads=32 max_queue=65536
type TimerResolution = String --"time" 
type Use = String --kqueue|rtsig|epoll|/dev/poll|select|poll
type User = String --"user"["group"] default:nobody nobody
type WorkerAioRequests = String --"number" default:32
type WorkerConnections = String --"number" default:512
type WorkerCpuAffinity = String --"cpumask" ... ex:0010 0101
type WorkerPriority = String --"number" default:0
type WorkerProcesses = String --"number"|auto default:1
type WorkerRlimitCore = String --"size"
type WorkerRlimitNofile = String --String with no default value
type WorkingDirectory = String --"directory"
--end core functionality
 
--core module directives
type Aio = String --on|off|threads="poolPath" default:off
type Alias = String --alias "path" 
type ChunkedTransferEncoding = String --on|off default:on
type ClientBodyBufferSize = String --"size" default:16k
type ClientBodyInFileOnly = String --on|clean|off default:off
type ClientBodyInSingleBuffer = String --on|off default:off
type ClientBodyTempPath = String --"path" default:client_body_temp
type ClientBodyTimeout = String --"time" default:60s
type ClientMaxBodySize = String --"size" default:1m
type ClientHeaderBufferSize = String --"size" default:1k
type ClientHeaderTimeout = String --"time" default:60s
type ConnectionPoolSize = String --"size" default:256
type DefaultType = String --"mime-type" default:text/plain
type Directio = String --"size"|off default:off
type DirectioAlignment = String --"size" default:512
type DisableSymlinks = String --off|on [from="part"]|if_not_owner [from="part"] default:off
type ErrorPage = String --"code" ... [=["response"]] "uri"
type Etag = String --on|off default:on
type IfModifiedSince = String --off|exact|before default:exact
type IgnoreInvalidHeaders = String --on|off default:on
type Internal = String --just 'internal;' not 'internal on|off', leave empty if off
type KeepaliveDisable = String --none|"browser" ... default:msie6
type KeepaliveRequests = String --"number" default:100
type KeepaliveTimeout = String --"time" ["time"] default:75s
type LargeClientHeaderBuffers = String --"number"+"size" default:4 8k
type LimitRate = String --"size" default:0
type LimitRateAfter = String --"size" default:0
type LingeringClose = String --off|on|always default:on
type LingeringTime = String --"time" default:30s
type LingeringTimeout = String --"time" default:5s
type Listen = String --"address"[:"port"] [default_server] [ssl] [http2 | spdy] [proxy_protocol] [setfib=number] [fastopen=number] [backlog=number] [rcvbuf=size] [sndbuf=size] [accept_filter=filter] [deferred] [bind] [ipv6only=on|off] [reuseport] [so_keepalive=on|off|[keepidle]:[keepintvl]:[keepcnt]]
type LogNotFound = String --on|off default:on
type LogSubrequest = String --on|off default:off
type MaxRanges = String --"number" no default
type MergeSlashes = String --on|off default:on
type MsiePadding = String --on|off default:on
type MsieRefresh = String --on|off default:off
--data LimitExcept = LimitExcept { [Method] AccessLog [AccessRule] AuthBasic :: AuthBasic, AuthBasicUserFile :: AuthBasicUserFile } {-ProxyPass -}deriving (Show) --limit_except "method" ... {...}
type Method = String --GET|POST|PUT|DELETE|...
type OpenFileCache = String --off|max="number" [inactive="time"] default:off
type OpenFileCacheErrors = String --on|off default:off
type OpenFileCacheMinUses = String --"number" default:1
type OpenFileCacheValid = String --"time" default:60s
type OutputBuffers = String --"number"+"size" default:2 32k
type PostponeOutput = String --"size" default:1460
type PortInRedirect = String --on|off default:on
type ReadAhead = String --"size" default:0
type RecursiveErrorPages = String --on|off default:off
type RequestPoolSize = String --"size" default:4k
type ResetTimedoutConnection = String --on|off default:off
type Resolver = String --"address" ... [valid="time"] [ipv6=on|off]
type ResolverTimeout = String --"time" default:30s
type Root = String --"path" default:html
type Satisfy = String --all|any default:all
type SendLowat = String --"size" default:0
type SendTimeout = String --"time" default:60s
type SendFile = String --on|off default:off
type SendFileMaxChunks = String --"size" default:0
type ServerName = String --default:""
type ServerNameInRedirect = String --on|off default:off
type ServerNamesHashBucketSize = String --"size" default:64
type ServerNamesHashMaxSize = String --"size" default:512
type ServerTokens = String --on|off default:on
type TcpNodelay = String --on|off default:on
type TcpNopush = String --on|off default:off
type TryFiles = String --"file" ... ("Uri"|="code")
type Types = String --{"Filetype" "FileExtension" ... ; ...}
--type Type = (FileType, [FileExtension]) --default:types {text/html html; image/gif gif; image/jpeg jpg;}
type FileType = String
type FileExtension = String
type TypesHashBucketSize = String --"size" default:64
type TypesHashMaxSize = String --"size" default:1024
type UnderscoresInHeaders = String --on|off default:off
type VariablesHashBucketSize = String --"size" default:64
type VariablesHashMaxSize = String --"size" default:1024
--end core module directives

--access module directives
type AccessRule = (String,String) --(allow|deny, "address"|all)
--end access module directives

--auth basic module directives
type AuthBasic = String --off|"string" default:off
type AuthBasicUserFile = String --"file"
--end auth basic module directives

--autoindex module directives
type Autoindex = String --on|off default:off
type AiExactSize = String --on|off default:on
type AiFormat = String --html|xml|json|jsonp default:html
type AiLocaltime = String --on|off default:off
--end autoindex module directives

--browser module directives
type AncientBrowser = String --ex: Lynx, netscape4...
type AncientBrowserValue = String --default:1
type ModernBrowser = String --msie|gecko|opera|safari|konqueror "version"
type ModernBrowserValue = String --default:1
--end browser module directives

--charset module directives
type Charset = String --"charset"|off default:off
type OverrideCharset = String --on|off default:off
type SourceCharset = String --"charset"
type CharsetType = String --"mime-type" ex:text/html,text/plain...
type CharsetMap = String
--data CharsetMap = CharsetMap CharsetFrom CharsetTo [(CharCode,CharCode)] deriving (Show) --charset_map "charset1" "charset2" {"code1a" "code1b"; "code2a" "code2b"; ...}
type CharsetFrom = String
type CharsetTo = String
type CharCode = String
--end charset module directives

--fastcgi module directives
{-data NFastcgiConf = NFastcgiConf FastcgiBind FastcgiBufferSize FastcgiBuffering FastcgiBuffers FastcgiBusyBuffersSize NFastcgiCacheConf FastcgiCatchStderr FastcgiConnectTimeout FastcgiForceRanges [FastcgiHideHeader] FastcgiIgnoreClientAbort FastcgiIgnoreHeaders FastcgiIndex FastcgiInterceptErrors FastcgiKeepConn FastcgiLimitRate FastcgiMaxTempFileSize FastcgiNextUpstream FastcgiNextUpstreamTimeout FastcgiNextUpstreamTries [FastcgiNoCache] [FastcgiParam] [FastcgiPassHeader] FastcgiPassRequestBody FastcgiPassRequestHeader FastcgiReadTimeout FastcgiRequestBuffering FastcgiSendLowat FastcgiSendTimeout FastcgiStore FastcgiStoreAccess FastcgiTempFileWriteSize FastcgiTempPath deriving (Show)
type FastcgiBind = String --"address"|off
type FastcgiBufferSize = String --"size" default:4k|8k
type FastcgiBuffering = String --default:on
type FastcgiBuffers = String --"number" "size" default:8 4k|8k
type FastcgiBusyBuffersSize = String --"size" default:8k|16k
type FastcgiCatchStderr = String
type FastcgiConnectTimeout = String --"time" default:60s
type FastcgiForceRanges = String --default:off
type FastcgiHideHeader = String --"header field" 
type FastcgiIgnoreClientAbort = String --default:off
type FastcgiIgnoreHeaders = String --"field"...
type FastcgiIndex = String --"filename"
type FastcgiInterceptErrors = String --default:off
type FastcgiKeepConn = String --default:off
type FastcgiLimitRate = String --"rate" default:0
type FastcgiMaxTempFileSize = String --"size" default:1024m
type FastcgiNextUpstream = String --error|timeout|invalid_header|http_500|http_503|http_403|http_404|off ... default:"error timeout"
type FastcgiNextUpstreamTimeout = String --"time" default:0
type FastcgiNextUpstreamTries = String --default:0
type FastcgiNoCache = String --"condition"
type FastcgiParam = String --"param" "value"
type FastcgiPassHeader = String --"header field"
type FastcgiPassRequestBody = String --default:on
type FastcgiPassRequestHeader = String --default:on
type FastcgiReadTimeout = String --"time" default:60s
type FastcgiRequestBuffering = String --default:on
type FastcgiSendLowat = String --"size" default:0
type FastcgiSendTimeout = String --"time" default:60s
type FastcgiStore = String --on|off|"string" default:off
type FastcgiStoreAccess = String --"user:permissions" ... default:user:rw
type FastcgiTempFileWriteSize = String --"size" default:8k|16k
type FastcgiTempPath = String --"path"[level1 [level2 [level3]]] default:fastcgi_temp
data NFastcgiCacheConf = NFastcgiCacheConf FastcgiCache [FastcgiCacheBypass] FastcgiCacheKey FastcgiCacheLock FastcgiCacheLockAge FastcgiCacheLockTimeout [FastcgiCacheMethod] FastcgiCacheMinUses FastcgiCachePurge FastcgiCacheRevalidate FastcgiCacheUseStale [FastcgiCacheValid] deriving (Show)
type FastcgiCache = String --"zone"|off default:off
type FastcgiCacheBypass = String --"condition"
type FastcgiCacheKey = String 
type FastcgiCacheLock = String --default:off
type FastcgiCacheLockAge = String --"time" default:5s
type FastcgiCacheLockTimeout = String --"time" default:5s
type FastcgiCacheMethod = String --GET|POST|HEAD default:GET+HEAD
type FastcgiCacheMinUses = String --default:1
type FastcgiCachePurge = String --"condition" ...
type FastcgiCacheRevalidate = String --on|off default:off
type FastcgiCacheUseStale = String --error|timeout|invalid_header|updating|http_500|http_503|http_403|http_404|off ... default:off
type FastcgiCacheValid = String --"Code" ... "time" 
type FastcgiPass = String --"address"[:"port"] 
type FastcgiSplitPathInfo = String --"regex"
type FastcgiCachePath = String --"Path" + options...-}
--end fastcgi module directives

--gzip module directives
type Gzip = String --on|off default:off
type GzipBuffers = String --"number"+"size" default:16 8k
type GzipCompLevel = String --"number" default:1
type GzipDisable = String --"regex" 
type GzipMinLength = String --"number" default:20
type GzipHttpVersion = String --1.0|1.1 default:1.1
type GzipProxied = String --off|expired|no-cache|no-store|private|no_last_modified|no_etag|auth|any ... default:off
type GzipTypes = String --"mime-types" ... default:text/html
type GzipVary = String --on|off default:off
--end gzip module directives

--headers module directives
type AddHeader = String --"name" "value" [always]
type Expires = String --[modified] "time"|epoch|max|off default:off
--end headers module directives

--index module directives
type Index = String --"file" ... default:index.html
--end index module directives

--limit conn module directives
type LimitConn = String --"zone" "number" 
type LimitConnLogLevel = String --info|notice|warn|error default:error
type LimitConnStatus = String --"status code" default:503
type LimitConnZone = String --"key" zone="name":"size" 
--end limit conn module directives

--limit req module directives
type LimitReq = String --zone="name" [burst="number"] [nodelay]
type LimitReqLogLevel = String --info|notice|warn|error default:error
type LimitReqStatus = String --"status code" default:503
type LimitReqZone = String --"Key" zone="name":"size" rate="rate"
--end limit req module directives

--log module directives
type AccessLog = String --"path" ["format" [buffer="size" [flush="time"]] [if="condition"]]; | "path" "format" gzip[="level"] [buffer="size"] [flush="time"] [if="condition"]; | syslog:server="address"[,"parameter"="value"] ["format" [if="condition"]]; | off default:logs/access.log combined 
type OpenLogFileCache = String --max="N" [inactive="time"] [min_uses="N"] [valid="time"];|off default:off
type LogFormat = String --"name" string ...
--end log module directives

--map module directives
type Map = String
--data NMap = NMap String Var DefaultValue HostNames [(SourceValue,OutputValue)] [Include] deriving (Show) --map "mapName" "var" {[hostnames;] [default "value"] "SourceValue+OutputValue" ... [Include "file"] ...}
type Var = String
type DefaultValue = String
type HostNames = String
type SourceValue = String
type OutputValue = String
type MapHashBucketSize = String --"size" default:64
type MapHashMaxSize = String --"size" default:2048
--end map module directives

--memcached module directives
type MemcachedBind = String --"address"|off
type MemCachedBufferSize = String --"size" default:8k
type MemcachedConnectTimeout = String --"time" default:60s
type MemcachedForceRanges = String --on|off default:off
type MemcachedGzipFlag = String --"flag"
type MemcachedNextUpstream = String --error|timeout|invalid_response|not_found|off ... default:error timeout
type MemcachedNextUpstreamTimeout = String --"time" default:0
type MemcachedNextUpstreamTries = String --"number" default:0
type MemcachedPass = String --"address"
type MemcachedReadTimeout = String --"time" default:60s
type MemcachedSendTimeout = String --"time" default:60s
--end memcached module directives

--proxy module directives
{-data NProxyConf = NProxyConf ProxyBind ProxyBufferSize ProxyBuffering ProxyBuffers ProxyBusyBufferSize ProxyCache [ProxyCacheBypass] ProxyCacheKey ProxyCacheLock ProxyCacheLockAge ProxyCacheLockTimeout ProxyCacheMethods ProxyCacheMinUses ProxyCachePurge ProxyCacheRevalidate ProxyCacheUseStale [ProxyCacheValid] ProxyConnectTimeout [ProxyCookieDomain] [ProxyCookiePath] ProxyForceRanges ProxyHeadersHashBucketSize ProxyHeadersHashMaxSize [ProxyHideHeader] ProxyHttpVersion ProxyIgnoreClientAbort ProxyIgnoreHeaders ProxyInterceptErrors ProxyLimitRate ProxyMaxTempFileSize ProxyMethod ProxyNextUpstream ProxyNextUpstreamTimeout ProxyNextUpstreamTries [ProxyNoCache] [ProxyPassHeader] ProxyPassRequestBody ProxyPassRequestHeaders ProxyReadTimeout [ProxyRedirect] ProxyRequestBuffering ProxySendLowat ProxySendTimeout ProxySetBody [ProxySetHeader] ProxySslCertificate ProxySslCertificateKey ProxySslCiphers ProxySslCrl ProxySslName ProxySslPasswordFile ProxySslServerName ProxySslSessionReuse [ProxySslProtocol] ProxySslTrustedCertificate ProxySslVerify ProxySslVerifyDepth ProxyStore [ProxyStoreAccess] ProxyTempFileWriteSize ProxyTempPath deriving (Show)
type ProxyBind = String --"address"|off
type ProxyBufferSize = String --"size" default:4k|8k
type ProxyBuffering = String --default:on
type ProxyBuffers = (String,String) --("number","size") default:8 4k|8k
type ProxyBusyBufferSize = String --"size" default:8k|16k
type ProxyCache = String --"zone"|off default:off
type ProxyCacheBypass = String --"string" ...
type ProxyCacheKey = String --default:$scheme$proxy_host$request_uri;
type ProxyCacheLock = String --default:off
type ProxyCacheLockAge = String --"time" default:5s
type ProxyCacheLockTimeout = String --"time" default:5s
type ProxyCacheMethods = String --GET|HEAD|POST ... default:GET HEAD
type ProxyCacheMinUses = String --default:1
type ProxyCachePurge = String --"string" ...
type ProxyCacheRevalidate = String --default:off
type ProxyCacheUseStale = String --error|timeout|invalid_header|updating|http_500|http_502|http_503|http_504|http_403|http_404|off ... default:off
type ProxyCacheValid = ([ResponseCode],String) --(["code"],"time")
type ProxyConnectTimeout = String --"time" default:60s
type ProxyCookieDomain = String --"domain"+"replacement"|off default:off
type ProxyCookiePath = String --"domain"+"replacement"|off default:off
type ProxyForceRanges = String --default:off
type ProxyHeadersHashBucketSize = String --"size" default:64
type ProxyHeadersHashMaxSize = String --"size" default:512
type ProxyHideHeader = String --"field"
type ProxyHttpVersion = Float --1.0|1.1 default:1.0
type ProxyIgnoreClientAbort = String --default:off
type ProxyIgnoreHeaders = String --"field" ... 
type ProxyInterceptErrors = String --default:off
type ProxyLimitRate = String --"rate" default:0
type ProxyMaxTempFileSize = String --"size" default:1024m
type ProxyMethod = String --"method"
type ProxyNextUpstream = String --error|timeout|invalid_header|http_500|http_502|http_503|http_504|http_403|http_404|off ... default:error timeout
type ProxyNextUpstreamTimeout = String --"time" default:0
type ProxyNextUpstreamTries = String --default:0
type ProxyNoCache = String --"string" ... 
type ProxyPassHeader = String --"field"
type ProxyPassRequestBody = String --default:on
type ProxyPassRequestHeaders = String --default:on
type ProxyReadTimeout = String --"time" default:60s
type ProxyRedirect = String --default|off|"redirect"+"replacement" default:default
type ProxyRequestBuffering = String --default:on
type ProxySendLowat = String --"size" default:0
type ProxySendTimeout = String --"time" default:60s
type ProxySetBody = String --"value"
type ProxySetHeader = (String, String) --("field","value") default:Host $proxy_host + Connection close
type ProxySslCertificate = String --"file"
type ProxySslCertificateKey = String --"file"
type ProxySslCiphers = String --"ciphers" default:DEFAULT
type ProxySslCrl = String --"file"
type ProxySslName = String --"name" default:$proxy_host
type ProxySslPasswordFile = String --"file"
type ProxySslServerName = String --default:off
type ProxySslSessionReuse = String --default:on
type ProxySslProtocol = String --SSLv2|SSLv3|TLSv1|TLSv1.1|TLSv1.2 default:TLSv1 TLSv1.1 TLSv1.2;
type ProxySslTrustedCertificate = String --"file"
type ProxySslVerify = String --default:off
type ProxySslVerifyDepth = String --default:1
type ProxyStore = String --on|off|"string" default:off
type ProxyStoreAccess = String --"users":"permissions" default:user:rw
type ProxyTempFileWriteSize = String --"size" default:8k|16k
type ProxyTempPath = String --"path" ["levels"]

type ProxyCachePath = String --"path" [levels="levels"] [use_temp_path=on|off] keys_zone="name":"size" [inactive="time"] [max_size="size"] [loader_files="number"] [loader_sleep="time"] [loader_threshold="time"] [purger=on|off] [purger_files="number"] [purger_sleep="time"] [purger_threshold="time"];

type ProxyPass = String --"url"-}
--end proxy module directives

--referer module directives
type ValidReferer = String --none|blocked|server_names|"string" ...
type RefererHashBucketSize = String --"size" default:64
type RefererHashMaxSize = String --"size" default:2048
--end referer module directives

--rewrite module directives
{-data NIfServer = NIfServer Condition Break Return [NRewrite] RewriteLog [Set] UninitializedVariableWarn deriving (Show)
data NIfLocation = NIfLocation Condition AccessLog [AddHeader] Break Charset :: Charset, OverrideCharset :: OverrideCharset, SourceCharset :: SourceCharset [ErrorPage] Expires FastcgiPass GzipState LimitRate LimitRateAfter MemcachedPass ProxyPass Return [NRewrite] RewriteLog Root SendFile [Set] UninitializedVariableWarn deriving (Show)
type Condition = String
type Break = String --must be in a certain place in the code, cant really be represented here...
type Return = String --"code"+["text"] | "code"+"url" | "url"
data NRewrite = NRewrite Regex Replacement [Flag] deriving (Show)
type Regex = String --regular expression
type Replacement = String
type Flag = String --last|break|redirect|permanent
type RewriteLog = String
type Set = (OldValue, NewValue)
type OldValue = String
type NewValue = String
type UninitializedVariableWarn = String-}
--end rewrite module directives

--scgi module directives
--not used for now
--end scgi module directives

--split clients module directives
--not considered
--end split clients module directives

--ssi module directives
--not considered
--end ssi module directives

--ssl module directives
type Ssl = String --on|off default:off
type SslBufferSize = String --"size" default:16k
type SslCertificate = String --"file"
type SslCertificateKey = String --"file"
type SslCiphers = String --"ciphers"(openSsl library format) default:HIGH:!aNULL:!MD5
type SslClientCertificate = String --"file" 
type SslCrl = String --"file"
type SslDhparam = String --"file"
type SslEcdhCurve = String --"curve" default:prime256v1
type SslPasswordFile = String --"file"
type SslPreferServerCiphers = String --on|off default:off
type SslProtocols = String --[SSLv2] [SSLv3] [TLSv1] [TLSv1.1] [TLSv1.2] default:TLSv1 TLSv1.1 TLSv1.2
type SslSessionCache = String --off|none|[builtin[:"size"]] [shared:"name":"size"] default:none
type SslSessionTicketKey = String --"file"
type SslSessionTickets = String --on|off default:on
type SslSessionTimeout = String --"time" default:5m
type SslStapling = String --on|off default:off
type SslStaplingFile = String --"file"
type SslStaplingResponder = String --"url"
type SslStaplingVerify = String --on|off default:off
type SslTrustedCertificate = String --"file"
type SslVerifyClient = String --on|off|optional|optional_no_ca default:off
type SslVerifyDepth = String --"number" default:1
--end ssl module directives

--upstream module directives
--not considered
--end upstream module directives

--userid module directives
--not considered
--end userid module directives

--uwsgi module directives
--not used for now
--end uwsgi module directives


--only considered nginx http server, not mail or stream.
--only considered built-in modules.

--syntax from this file to nginx directives : 
----directives never begin by a capital letter
----capital letters in this file types stand for underscore separating words
----ex : ErrorLog -> error_log
--source and view records defined as BiGUL types
--
deriveBiGULGeneric ''NginxWebserver
deriveBiGULGeneric ''Events
deriveBiGULGeneric ''Http
deriveBiGULGeneric ''Server
deriveBiGULGeneric ''Location
