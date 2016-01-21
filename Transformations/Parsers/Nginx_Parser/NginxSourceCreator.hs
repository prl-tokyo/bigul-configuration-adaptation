module NginxSourceCreator where

--{{{ Modules import
import TreeConfigNginxFiller
import TypesAndFunctions
--}}}

--{{{ Haskell import
import Text.Peggy
import TypeFiles.NginxTypes
import Control.Monad
import Control.Monad.Reader
--}}}

--{{{ Function that will construct an AccessRule list if values of accessRule
--found and put nothing if not
getAccessRules :: TreeFile -> Maybe [AccessRule]
getAccessRules (TreeFile name instructionList treeFileList) = 
	if ((elem "allow" (map fst instructionList)) 
	|| (elem "deny" (map fst instructionList))) then
		Just (filter ((liftM2 (||) (=="allow") (=="deny")) . fst) 
		instructionList) 
	else
		Nothing
--}}}

--{{{ Function that will contruct an nEvents type by returning nothing if no
--event context found and returning a Event type if found
getEvents :: TreeFile -> Maybe Events
getEvents tree = if (isInChildren tree "events") then
	Just Events {
		eAcceptMutex = getValue 
		(head (giveTree [tree] ["root","events"]))
		"accept_mutex",
		eAcceptMutexDelay = getValue 
		(head (giveTree [tree] ["root","events"]))
		"accept_mutex_delay",
		eInclude = getValueList 
		(head (giveTree [tree] ["root","events"]))
		"include",
		eMultiAccept = getValue 
		(head (giveTree [tree] ["root","events"]))
		"multi_accept",
		eUse = getValue 
		(head (giveTree [tree] ["root","events"]))
		"use",
		eWorkerAioRequests = getValue 
		(head (giveTree [tree] ["root","events"]))
		"worker_aio_requests",
		eWorkerConnections = getValue 
		(head (giveTree [tree] ["root","events"]))
		"worker_connections"
	} else
		Nothing
--}}}

--{{{ Function that will contruct an nHttp type by returning nothing if no Http
--context found and returning a Http type if found
getHttp :: TreeFile -> Maybe Http
getHttp tree = if (isInChildren tree "http") then
	Just Http {
		hAccessRule = getAccessRules
		(head (giveTree [tree] ["root","http"])), 
		hAddHeader = getValueList
		(head (giveTree [tree] ["root","http"]))
		"add_header",
		hAio = getValue
		(head (giveTree [tree] ["root","http"]))
		"aio",
		hAuthBasic = getValue
		(head (giveTree [tree] ["root","http"]))
		"auth_basic",
		hAuthBasicUserFile = getValue
		(head (giveTree [tree] ["root","http"]))
		"auth_basic_user_file",
		hAutoindex = getValue
		(head (giveTree [tree] ["root","http"]))
		"autoindex",
		hAiExactSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"ai_exact_size",
		hAiFormat = getValue
		(head (giveTree [tree] ["root","http"]))
		"ai_format",
		hAiLocaltime = getValue
		(head (giveTree [tree] ["root","http"]))
		"ai_localtime",
		hAncientBrowser = getValueList
		(head (giveTree [tree] ["root","http"]))
		"ancient_browser",
		hAncientBrowserValue = getValue
		(head (giveTree [tree] ["root","http"]))
		"ancient_browser_value",
		hModernBrowser = getValueList
		(head (giveTree [tree] ["root","http"]))
		"modern_browser",
		hModernBrowserValue = getValue
		(head (giveTree [tree] ["root","http"]))
		"modern_browser_value",
		hCharset = getValue
		(head (giveTree [tree] ["root","http"]))
		"charset",
		hOverrideCharset = getValue
		(head (giveTree [tree] ["root","http"]))
		"override_charset",
		hSourceCharset = getValue
		(head (giveTree [tree] ["root","http"]))
		"source_charset",
		hCharsetMap = getValueList
		(head (giveTree [tree] ["root","http"]))
		"charset_map",
		hCharsetType = getValue
		(head (giveTree [tree] ["root","http"]))
		"charset_type",
		hChunkedTransferEncoding = getValue
		(head (giveTree [tree] ["root","http"]))
		"chunked_transfer_encoding",
   		hClientHeaderBufferSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"client_header_buffer_size",
    		hClientBodyBufferSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"client_body_buffer_size",
    		hClientBodyInFileOnly = getValue
		(head (giveTree [tree] ["root","http"]))
		"client_body_in_file_only",
		hClientBodyInSingleBuffer = getValue
		(head (giveTree [tree] ["root","http"]))
		"client_body_in_single_buffer",
    		hClientBodyTempPath = getValue
		(head (giveTree [tree] ["root","http"]))
		"client_body_temp_path",
    		hClientBodyTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"client_body_timeout",
    		hClientMaxBodySize = getValue
		(head (giveTree [tree] ["root","http"]))
		"client_max_body_size",
    		hClientHeaderTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"client_header_timeout",
		hConnectionPoolSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"connection_pool_size",
		hDefaultType = getValue
		(head (giveTree [tree] ["root","http"]))
		"default_type",
		hDirectio = getValue
		(head (giveTree [tree] ["root","http"]))
		"directio",
		hDirectioAlignment = getValue
		(head (giveTree [tree] ["root","http"]))
		"directio_alignment",
		hDisableSymlinks = getValue
		(head (giveTree [tree] ["root","http"]))
		"disable_symlinks",
		hErrorPage = getValueList
		(head (giveTree [tree] ["root","http"]))
		"error_page",
		hEtag = getValue
		(head (giveTree [tree] ["root","http"]))
		"etag",
		hExpires = getValue
		(head (giveTree [tree] ["root","http"]))
		"expires",
		hGzip = getValue
		(head (giveTree [tree] ["root","http"]))
		"gzip",
		hGzipBuffers = getValue
		(head (giveTree [tree] ["root","http"]))
		"gzip_buffers",
		hGzipCompLevel = getValue
		(head (giveTree [tree] ["root","http"]))
		"gzip_comp_level",
		hGzipDisable = getValue
		(head (giveTree [tree] ["root","http"]))
		"gzip_disable",
		hGzipMinLength = getValue
		(head (giveTree [tree] ["root","http"]))
		"gzip_min_length",
		hGzipHttpVersion = getValue
		(head (giveTree [tree] ["root","http"]))
		"gzip_http_version",
		hGzipProxied = getValue
		(head (giveTree [tree] ["root","http"]))
		"gzip_proxied",
		hGzipTypes = getValue
		(head (giveTree [tree] ["root","http"]))
		"gzip_types",
		hGzipVary = getValue
		(head (giveTree [tree] ["root","http"]))
		"gzip_vary",
		hIfModifiedSince = getValue
		(head (giveTree [tree] ["root","http"]))
		"if_modified_since",
		hIgnoreInvalidHeaders = getValue
		(head (giveTree [tree] ["root","http"]))
		"ignore_invalid_headers",
		hInclude = getValueList
		(head (giveTree [tree] ["root","http"]))
		"include",
		hIndex = getValue
		(head (giveTree [tree] ["root","http"]))
		"index",
		hKeepaliveDisable = getValue
		(head (giveTree [tree] ["root","http"]))
		"keepalive_disable",
		hKeepaliveRequests = getValue
		(head (giveTree [tree] ["root","http"]))
		"keepalive_requests",
		hKeepaliveTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"keepalive_timeout",
		hLargeClientHeaderBuffers = getValue
		(head (giveTree [tree] ["root","http"]))
		"large_client_header_buffers",
		hLimitConn = getValueList
		(head (giveTree [tree] ["root","http"]))
		"limit_conn",
		hLimitConnLogLevel = getValue
		(head (giveTree [tree] ["root","http"]))
		"limit_conn_log_level",
		hLimitConnStatus = getValue
		(head (giveTree [tree] ["root","http"]))
		"limit_conn_status",
		hLimitConnZone = getValueList
		(head (giveTree [tree] ["root","http"]))
		"limit_conn_zone",
		hLimitRate = getValue
		(head (giveTree [tree] ["root","http"]))
		"limit_rate",
		hLimitRateAfter = getValue
		(head (giveTree [tree] ["root","http"]))
		"limit_rate_after",
		hLimitReq = getValueList
		(head (giveTree [tree] ["root","http"]))
		"limit_req",
		hLimitReqLogLevel = getValue
		(head (giveTree [tree] ["root","http"]))
		"limit_req_log_level",
		hLimitReqStatus = getValue
		(head (giveTree [tree] ["root","http"]))
		"limit_req_status",
		hLimitReqZone = getValueList
		(head (giveTree [tree] ["root","http"]))
		"limit_req_zone",
		hLingeringClose = getValue
		(head (giveTree [tree] ["root","http"]))
		"lingering_close",
		hLingeringTime = getValue
		(head (giveTree [tree] ["root","http"]))
		"lingering_time",
		hLingeringTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"lingering_timeout",
		hAccessLog = getValue
		(head (giveTree [tree] ["root","http"]))
		"access_log",
		hErrorLog = getValue
		(head (giveTree [tree] ["root","http"]))
		"error_log",
		hLogNotFound = getValue
		(head (giveTree [tree] ["root","http"]))
		"log_not_found",
		hLogSubrequest = getValue
		(head (giveTree [tree] ["root","http"]))
		"log_subrequest",
		hOpenLogFileCache = getValue
		(head (giveTree [tree] ["root","http"]))
		"open_log_file_cache",
		hLogFormat = getValueList
		(head (giveTree [tree] ["root","http"]))
		"log_format",
		hMap =  getValueList
		(head (giveTree [tree] ["root","http"]))
		"map",
		hMapHashBucketSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"map_hash_bucket_size", 
		hMapHashMaxSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"map_hash_max_size",
		hMaxRanges = getValue
		(head (giveTree [tree] ["root","http"]))
		"max_ranges",
		hMemcachedBind = getValue
		(head (giveTree [tree] ["root","http"]))
		"memcached_bind",
		hMemCachedBufferSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"memcached_buffer_size",
		hMemcachedConnectTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"memcached_connect_timeout",
		hMemcachedForceRanges = getValue
		(head (giveTree [tree] ["root","http"]))
		"memcached_force_ranges",
		hMemcachedGzipFlag = getValue
		(head (giveTree [tree] ["root","http"]))
		"memcached_gzip_flag",
		hMemcachedNextUpstream = getValue
		(head (giveTree [tree] ["root","http"]))
		"memcached_next_upstream",
		hMemcachedNextUpstreamTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"memcached_next_upstream_timeout",
		hMemcachedNextUpstreamTries = getValue
		(head (giveTree [tree] ["root","http"]))
		"memcached_next_upstream_tries",
		hMemcachedReadTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"memcached_read_timeout",
		hMemcachedSendTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"memcached_send_timeout",
		hMergeSlashes = getValue
		(head (giveTree [tree] ["root","http"]))
		"merge_slashed",
		hMsiePadding = getValue
		(head (giveTree [tree] ["root","http"]))
		"msie_padding",
		hMsieRefresh = getValue
		(head (giveTree [tree] ["root","http"]))
		"msie_refresh",
		hOpenFileCache = getValue
		(head (giveTree [tree] ["root","http"]))
		"open_file_cache",
		hOpenFileCacheErrors = getValue
		(head (giveTree [tree] ["root","http"]))
		"open_file_cache_errors",
		hOpenFileCacheMinUses = getValue
		(head (giveTree [tree] ["root","http"]))
		"open_file_cache_min_uses",
		hOpenFileCacheValid = getValue
		(head (giveTree [tree] ["root","http"]))
		"open_file_cache_valid",
		hOutputBuffers = getValue
		(head (giveTree [tree] ["root","http"]))
		"output_buffers",
		hPostponeOutput = getValue
		(head (giveTree [tree] ["root","http"]))
		"postpone_output",
		hPortInRedirect = getValue
		(head (giveTree [tree] ["root","http"]))
		"port_in_redirect",
		hReadAhead = getValue
		(head (giveTree [tree] ["root","http"]))
		"read_ahead",
		hRecursiveErrorPages = getValue
		(head (giveTree [tree] ["root","http"]))
		"recursive_error_pages",
		hRequestPoolSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"request_pool_size",
		hResetTimedoutConnection = getValue
		(head (giveTree [tree] ["root","http"]))
		"reset_timedout_connection",
		hResolver = getValue
		(head (giveTree [tree] ["root","http"]))
		"resolver",
		hResolverTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"resolver_timeout",
		hRoot = getValue
		(head (giveTree [tree] ["root","http"]))
		"root",
		hSatisfy = getValue
		(head (giveTree [tree] ["root","http"]))
		"satisfy",
		hSendLowat = getValue
		(head (giveTree [tree] ["root","http"]))
		"send_lowat",
		hSendTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"send_timeout",
		hSendFile = getValue
		(head (giveTree [tree] ["root","http"]))
		"sendfile",
		hSendFileMaxChunks = getValue
		(head (giveTree [tree] ["root","http"]))
		"send_file_max_chunks",
		hServer = getServers
		tree,
		hServerNameInRedirect = getValue
		(head (giveTree [tree] ["root","http"]))
		"server_name_in_redirect",
		hServerNamesHashBucketSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"server_names_hash_bucket_size",
		hServerNamesHashMaxSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"server_names_hash_max_size",
		hServerTokens = getValue
		(head (giveTree [tree] ["root","http"]))
		"server_tokens",
		hSsl = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl",
		hSslBufferSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_buffer_size",
		hSslCertificate = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_certificate",
		hSslCertificateKey = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_certificate_key",
		hSslCiphers = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_ciphers",
		hSslClientCertificate = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_client_certificate",
		hSslCrl = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_crl",
		hSslDhparam = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_dhparam",
		hSslEcdhCurve = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_ecdh_curve",
		hSslPasswordFile = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_password_file",
		hSslPreferServerCiphers = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_prefer_server_ciphers",
		hSslProtocols = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_protocols",
		hSslSessionCache = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_session_cache",
		hSslSessionTicketKey = getValueList
		(head (giveTree [tree] ["root","http"]))
		"ssl_session_ticket_key",
		hSslSessionTickets = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_session_tickets",
		hSslSessionTimeout = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_session_timeout", 
		hSslStapling = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_stapling",
		hSslStaplingFile = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_stapling_file",
		hSslStaplingResponder = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_stapling_responder",
		hSslStaplingVerify = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_stapling_verify",
		hSslTrustedCertificate = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_trusted_certificate",
		hSslVerifyClient = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_verify_client",
		hSslVerifyDepth = getValue
		(head (giveTree [tree] ["root","http"]))
		"ssl_verify_depth",
		hTcpNodelay = getValue
		(head (giveTree [tree] ["root","http"]))
		"tcp_nodelay",
		hTcpNopush = getValue
		(head (giveTree [tree] ["root","http"]))
		"tcp_nopush",
		hTypes = getValue
		(head (giveTree [tree] ["root","http"]))
		"types",
		hTypesHashBucketSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"types_hash_bucket_size",
		hTypesHashMaxSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"types_hash_max_size",
		hUnderscoreInHeaders = getValue
		(head (giveTree [tree] ["root","http"]))
		"underscore_in_headers",
		hVariablesHashBucketSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"variables_hash_bucket_size",
		hVariablesHashMaxSize = getValue
		(head (giveTree [tree] ["root","http"]))
		"variables_hash_max_size"
	} else
		Nothing
--}}}

--{{{ Function that will contruct a hServer type by returning nothing
--if no Server context found and returning a list of Server if found
getServers :: TreeFile -> Maybe [Server]
getServers tree =  
	if (isInChildren (head (giveTree [tree] ["root","http"])) "server") then
		Just (map getServerValues
		(giveTree [tree] ["root","http","server"])) 
	else
		Nothing
--}}}

--{{{ Function that will construct a Server type using the server node
--(will always be current node)
getServerValues :: TreeFile -> Server
getServerValues tree = Server {
	sAccessRule = getAccessRules
	tree,
	sAddHeader = getValueList
	tree
	"add_header",
	sAio = getValue
	tree
	"aio",
	sAuthBasic = getValue
	tree
	"auth_basic",
	sAuthBasicUserFile = getValue
	tree
	"auth_basic_user_file",
	sAutoindex = getValue
	tree
	"auto_index",
	sAiExactSize = getValue
	tree
	"ai_exact_size",
	sAiFormat = getValue
	tree
	"ai_format",
	sAiLocaltime = getValue
	tree
	"ai_localtime",
	sAncientBrowser = getValueList
	tree
	"ancient_browser",
	sAncientBrowserValue = getValue
	tree
	"ancient_browser_value",
	sModernBrowser = getValueList
	tree
	"modern_browser",
	sModernBrowserValue = getValue
	tree
	"modern_browser_value",
	sCharset = getValue
	tree
	"charset",
	sOverrideCharset = getValue
	tree
	"override_charset",
	sSourceCharset = getValue
	tree
	"source_charset",
	sCharsetType = getValue
	tree
	"charset_type",
	sChunkedTransferEncoding = getValue
	tree
	"chunked_transfer_encoding",
	sClientHeaderBufferSize = getValue
	tree
	"client_header_buffer_size",
	sClientBodyBufferSize = getValue
	tree
	"client_body_buffer_size",
	sClientBodyInFileOnly = getValue
	tree
	"client_body_in_file_only",
	sClientBodyInSingleBuffer = getValue
	tree
	"client_body_in_single_buffer",
	sClientBodyTempPath = getValue
	tree
	"client_body_temp_path",
	sClientBodyTimeout = getValue
	tree
	"client_body_timeout",
	sClientMaxBodySize = getValue
	tree
	"client_max_body_size",
	sClientHeaderTimeout = getValue
	tree
	"client_header_timeout",
	sConnectionPoolSize = getValue
	tree
	"connection_pool_size",
	sDefaultType = getValue
	tree
	"default_type",
	sDirectio = getValue
	tree
	"diretio",
	sDirectioAlignment = getValue
	tree
	"directio_alignment",
	sDisableSymlinks = getValue
	tree
	"disable_symlinks",
	sErrorPage = getValueList
	tree
	"error_page",
	sEtag = getValue
	tree
	"etag",
	sExpires = getValue
	tree
	"expires",
	sGzip = getValue
	tree
	"gzip",
	sGzipBuffers = getValue
	tree
	"gzip_buffers", 
	sGzipCompLevel = getValue
	tree
	"gzip_comp_level",
	sGzipDisable = getValue
	tree
	"gzip_disable",
	sGzipMinLength = getValue
	tree
	"gzip_min_length",
	sGzipHttpVersion = getValue
	tree
	"gzip_http_version",
	sGzipProxied = getValue
	tree
	"gzip_proxied",
	sGzipTypes = getValue
	tree
	"gzip_types",
	sGzipVary = getValue
	tree
	"gzip_vary",
	sIfModifiedSince = getValue
	tree
	"if_modified_since",
	sIgnoreInvalidHeaders = getValue
	tree
	"ignore_invalid_headers",
	sInclude = getValueList
	tree
	"include",
	sIndex = getValue
	tree
	"index",
	sKeepaliveDisable = getValue
	tree
	"keepalive_disable",
	sKeepaliveRequests = getValue
	tree
	"keepalive_requests",
	sKeepaliveTimeout = getValue
	tree
	"keepalive_timeout",
	sLargeClientHeaderBuffers = getValue
	tree
	"large_client_header_buffers",
	sLimitConn = getValueList
	tree
	"limit_conn",
	sLimitConnLogLevel = getValue
	tree
	"limit_conn_log_level",
	sLimitConnStatus = getValue
	tree
	"limit_conn_status",
	sLimitRate = getValue
	tree
	"limit_rate",
	sLimitRateAfter = getValue
	tree
	"limit_rate_after",
	sLimitReq = getValueList	
	tree
	"limit_req",
	sLimitReqLogLevel = getValue
	tree
	"limit_req_log_level",
	sLimitReqStatus = getValue
	tree
	"limit_req_status",
	sLingeringClose = getValue
	tree
	"lingering_close",
	sLingeringTime = getValue
	tree
	"lingering_time",
	sLingeringTimeout = getValue
	tree
	"lingering_timeout", 
	sListen = getValueList
	tree
	"listen",
	sLocation = getLocationsNginx
	tree
	["server","location"],
	sAccessLog = getValue
	tree
	"access_log",
	sErrorLog = getValue
	tree
	"error_log",
	sLogNotFound = getValue
	tree
	"log_not_found",
	sLogSubrequest = getValue
	tree
	"log_subrequest",
	sOpenLogFileCache = getValue
	tree
	"open_log_file_cache",
	sMaxRanges = getValue
	tree
	"max_ranges",
	sMemcachedBind = getValue
	tree
	"memcached_bind",
	sMemCachedBufferSize = getValue
	tree
	"memcached_buffer_size",
	sMemcachedConnectTimeout = getValue
	tree
	"memcached_connect_timeout", 
	sMemcachedForceRanges = getValue
	tree
	"memcached_force_ranges",
	sMemcachedGzipFlag = getValue
	tree
	"memcached_gzip_flag",
	sMemcachedNextUpstream = getValue
	tree
	"memcached_next_upstream",
	sMemcachedNextUpstreamTimeout = getValue
	tree
	"memcached_next_upstream_tmeout",
	sMemcachedNextUpstreamTries = getValue
	tree
	"memcached_next_upstream_tries",
	sMemcachedReadTimeout = getValue
	tree
	"memcached_read_timeout",
	sMemcachedSendTimeout = getValue
	tree
	"memcached_send_timeout",
	sMergeSlashes = getValue
	tree
	"merge_slashed",
	sMsiePadding = getValue
	tree
	"msie_padding",
	sMsieRefresh = getValue
	tree
	"msie_refresh",
	sOpenFileCache = getValue
	tree
	"open_file_cache",
	sOpenFileCacheErrors = getValue
	tree
	"open_file_cache_errors",
	sOpenFileCacheMinUses = getValue
	tree
	"open_file_cache_min_uses",
	sOpenFileCacheValid = getValue
	tree
	"open_file_cache_valid",
	sOutputBuffers = getValue
	tree
	"output_buffers",
	sPostponeOutput = getValue
	tree
	"postpone_output",
	sPortInRedirect = getValue
	tree
	"port_in_redirect",
	sReadAhead = getValue
	tree
	"read_ahead",
	sRecursiveErrorPages = getValue
	tree
	"recursive_error_pages",
	sValidReferer = getValueList
	tree
	"valid_referer",
	sRefererHashBucketSize = getValue
	tree
	"referer_hash_bucket_size",
	sRefererHashMaxSize = getValue
	tree
	"referer_hash_max_size",
	sRequestPoolSize = getValue
	tree
	"request_pool_size",
	sResetTimedoutConnection = getValue
	tree
	"reset_timedout_connection", 
	sResolver = getValue
	tree
	"resolver",
	sResolverTimeout = getValue
	tree
	"resolver_timeout",
	sRoot = getValue
	tree
	"root",
	sSatisfy = getValue
	tree
	"satisfy",
	sSendLowat = getValue
	tree
	"send_lowat",
	sSendTimeout = getValue
	tree
	"send_timeout",
	sSendFile = getValue
	tree
	"send_file",
	sSendFileMaxChunks = getValue
	tree
	"send_file_max_chunks",
	sServerName = getValueList
	tree
	"server_name",
	sServerNameInRedirect = getValue
	tree
	"server_name_in_redirect", 
	sServerTokens = getValue
	tree
	"server_tokens",
	sSsl = getValue
	tree
	"ssl",
	sSslBufferSize = getValue
	tree
	"ssl_buffer_size",
	sSslCertificate = getValue
	tree
	"ssl_certificate",
	sSslCertificateKey = getValue
	tree
	"ssl_certificate_key",
	sSslCiphers = getValue
	tree
	"ssl_ciphers",
	sSslClientCertificate = getValue
	tree
	"ssl_client_certificate",
	sSslCrl = getValue
	tree
	"ssl_crl",
	sSslDhparam = getValue
	tree
	"ssl_dhparam",
	sSslEcdhCurve = getValue
	tree
	"ssl_ecdh_curve",
	sSslPasswordFile = getValue
	tree
	"ssl_password_file",
	sSslPreferServerCiphers = getValue
	tree
	"ssl_prefer_server_ciphers",
	sSslProtocols = getValue
	tree
	"ssl_protocols",
	sSslSessionCache = getValue
	tree
	"ssl_session_cache",
	sSslSessionTicketKey = getValueList
	tree
	"ssl_session_ticket_key",
	sSslSessionTickets = getValue
	tree
	"ssl_session_tickets",
	sSslSessionTimeout = getValue
	tree
	"ssl_session_timeout",
	sSslStapling = getValue
	tree
	"ssl_stapling",
	sSslStaplingFile = getValue
	tree
	"ssl_stapling_file",
	sSslStaplingResponder = getValue
	tree
	"ssl_stapling_responder",
	sSslStaplingVerify = getValue
	tree
	"ssl_stapling_verify",
	sSslTrustedCertificate = getValue
	tree
	"ssl_trusted_certificate",
	sSslVerifyClient = getValue
	tree
	"ssl_verify_client",
	sSslVerifyDepth = getValue
	tree
	"ssl_verify_depth",
	sTcpNodelay = getValue
	tree
	"tcp_nodelay",
	sTcpNopush = getValue
	tree
	"tcp_nopush",
	sTryFiles = getValue
	tree
	"try_files",
	sTypes = getValue
	tree
	"types", 
	sTypesHashBucketSize = getValue
	tree
	"types_hash_bucket_size",
	sTypesHashMaxSize = getValue
	tree
	"types_hash_max_size",
	sUnderscoresInHeaders = getValue
	tree
	"underscores_in_headers"
	}
--}}}

--{{{ Function that will contruct a sLocation type by returning nothing if no
--Location context found and returning a list of Location if found
getLocationsNginx :: TreeFile -> [String] -> Maybe [Location]
getLocationsNginx tree path =
	if (isInChildren tree "location") then
		Just (map getLocationValuesNginx (giveTree [tree] path)) 
	else
		Nothing
--}}}

--{{{ Function that will construct a Location type using the server node
--(will always be current node)
getLocationValuesNginx :: TreeFile -> Location
getLocationValuesNginx tree = Location {
	lLocationPath = getValue
	tree
	"location_path",
	lAccessRule = getAccessRules
	tree,
	lAddHeader = getValueList
	tree
	"add_header",
	lAio = getValue
	tree
	"aio",
	lAlias = getValue
	tree
	"alias",
	lAuthBasic = getValue
	tree
	"auth_basic",
	lAuthBasicUserFile = getValue
	tree
	"auth_basic_user_file",
	lAutoindex = getValue
	tree
	"auto_index",
	lAiExactSize = getValue
	tree
	"ai_exact_size",
	lAiFormat = getValue
	tree
	"ai_format",
	lAiLocaltime = getValue
	tree
	"ai_localtime",
	lAncientBrowser = getValueList
	tree
	"ancient_browser",
	lAncientBrowserValue = getValue
	tree
	"ancient_browser_value",
	lModernBrowser = getValueList
	tree
	"modern_browser",
	lModernBrowserValue = getValue
	tree
	"modern_browser_value",
	lCharset = getValue
	tree
	"charset",
	lOverrideCharset = getValue
	tree
	"override_charset",
	lSourceCharset = getValue
	tree
	"source_charset",
	lCharsetType = getValue
	tree
	"charset_type",
	lChunkedTransferEncoding = getValue
	tree
	"chunked_transfer_encoding",
	lClientBodyBufferSize = getValue
	tree
	"client_body_buffer_size",
	lClientBodyInFileOnly = getValue
	tree
	"client_body_in_file_only",
	lClientBodyInSingleBuffer = getValue
	tree
	"client_body_in_single_buffer",
	lClientBodyTempPath = getValue
	tree
	"client_body_temp_path",
	lClientBodyTimeout = getValue
	tree
	"client_body_timeout",
	lClientMaxBodySize = getValue
	tree
	"client_max_body_size",
	lDefaultType = getValue
	tree
	"default_type",
	lDirectio = getValue
	tree
	"directio",
	lDirectioAlignment = getValue
	tree
	"directio_alignment",
	lDisableSymlinks = getValue
	tree
	"disable_symlinks",
	lErrorPage = getValueList
	tree
	"error_page",
	lEtag = getValue
	tree
	"etag",
	lExpires = getValue
	tree
	"expires",
	lGzip = getValue
	tree
	"gzip",
	lGzipBuffers = getValue
	tree
	"gzip_buffers",
	lGzipCompLevel = getValue
	tree
	"gzip_comp_level",
	lGzipDisable = getValue
	tree
	"gzip_disable",
	lGzipMinLength = getValue
	tree
	"gzip_min_length",
	lGzipHttpVersion = getValue
	tree
	"gzip_http_version",
	lGzipProxied = getValue
	tree
	"gzip_proxied",
	lGzipTypes = getValue
	tree
	"gzip_types",
	lGzipVary = getValue
	tree
	"gzip_vary",
	lIfModifiedSince = getValue
	tree
	"if_modified_since",
	lInclude = getValueList
	tree
	"include",
	lIndex = getValue
	tree
	"index",
	lInternal = getValue
	tree
	"internal",
	lKeepaliveDisable = getValue
	tree
	"keepalive_disable",
	lKeepaliveRequests = getValue
	tree
	"keepalive_requests",
	lKeepaliveTimeout = getValue
	tree
	"keepalive_timeout",
	lLimitConn = getValueList
	tree
	"limit_conn",
	lLimitConnLogLevel = getValue
	tree
	"limit_conn_log_level",
	lLimitConnStatus = getValue
	tree
	"limit_conn_status",
	lLimitRate = getValue
	tree
	"limit_rate",
	lLimitRateAfter = getValue
	tree
	"limit_rate_after",
	lLimitReq = getValueList
	tree
	"limit_req",
	lLimitReqLogLevel = getValue
	tree
	"limit_req_log_level",
	lLimitReqStatus = getValue
	tree
	"limit_req_status",
	lLingeringClose = getValue
	tree
	"lingering_close",
	lLingeringTime = getValue
	tree
	"lingering_time",
	lLingeringTimeout = getValue
	tree
	"lingering_timeout",
	lLocation = getLocationsNginx
	tree
	["location","location"],
	lAccessLog = getValue
	tree
	"access_log",
	lErrorLog = getValue
	tree
	"error_log",
	lLogNotFound = getValue
	tree
	"log_not_found",
	lLogSubrequest = getValue
	tree
	"log_subrequest",
	lOpenLogFileCache = getValue
	tree
	"open_log_file_cache",
	lMaxRanges = getValue
	tree
	"max_ranges",
	lMemcachedBind = getValue
	tree
	"memcached_bind",
	lMemCachedBufferSize = getValue
	tree
	"memcached_buffer_size",
	lMemcachedConnectTimeout = getValue
	tree
	"memcached_connect_timeout",
	lMemcachedForceRanges = getValue
	tree
	"memcached_force_ranges",
	lMemcachedGzipFlag = getValue
	tree
	"memcached_gzip_flag",
	lMemcachedNextUpstream = getValue
	tree
	"memcached_next_upstream",
	lMemcachedNextUpstreamTimeout = getValue
	tree
	"memcached_next_upstream_timeout",
	lMemcachedNextUpstreamTries = getValue
	tree
	"memcached_next_upstream_tries",
	lMemcachedReadTimeout = getValue
	tree
	"memcached_read_timeout",
	lMemcachedSendTimeout = getValue
	tree
	"memcached_send_timeout",
	lMemcachedPass = getValue
	tree
	"memcached_pass",
	lMsiePadding = getValue
	tree
	"msie_padding",
	lMsieRefresh = getValue
	tree
	"msie_refresh",
	lOpenFileCache = getValue
	tree
	"open_file_cache",
	lOpenFileCacheErrors = getValue
	tree
	"open_file_cache_errors",
	lOpenFileCacheMinUses = getValue
	tree
	"open_file_cache_min_uses",
	lOpenFileCacheValid = getValue
	tree
	"open_file_cache_valid",
	lOutputBuffers = getValue
	tree
	"output_buffers",
	lPostponeOutput = getValue
	tree
	"postpone_output",
	lPortInRedirect = getValue
	tree
	"port_in_redirect",
	lReadAhead = getValue
	tree
	"read_ahead",
	lRecursiveErrorPages = getValue
	tree
	"recursive_error_pages",
	lValidReferer = getValueList
	tree
	"valid_referer",
	lRefererHashBucketSize = getValue
	tree
	"referer_hash_bucket_size",
	lRefererHashMaxSize = getValue
	tree
	"referer_hash_max_size",
	lResetTimedoutConnection = getValue
	tree
	"reset_timedout_connection", 
	lResolver = getValue
	tree
	"resolver",
	lResolverTimeout = getValue
	tree
	"resolver_timeout",
	lRoot = getValue
	tree
	"root",
	lSatisfy = getValue
	tree
	"satisfy",
	lSendLowat = getValue
	tree
	"send_lowat",
	lSendTimeout = getValue
	tree
	"send_timeout",
	lSendFile = getValue
	tree
	"send_file",
	lSendFileMaxChunks = getValue
	tree
	"send_file_max_chunks",
	lServerNameInRedirect = getValue
	tree
	"server_name_in_redirect",
	lServerTokens = getValue
	tree
	"server_tokens",
	lTcpNodelay = getValue
	tree
	"tcp_nodelay",
	lTcpNopush = getValue
	tree
	"tcp_nopush",
	lTryFiles = getValue
	tree
	"try_files",
	lTypes = getValue
	tree
	"types",
	lTypesHashBucketSize = getValue
	tree
	"types_hash_bucket_size",
	lTypesHashMaxSize = getValue
	tree
	"types_hash_max_size"
	}
--}}}

--{{{ Function that will put the Tree into the source type
createSourceNginx :: TreeFile -> NginxWebserver
createSourceNginx tree = NginxWebserver {
	nDaemon = getValue
	tree
	"daemon",
	nErrorLog = getValue
	tree
	"error_log",
	nEnv = getValueList
	tree
	"env",
	nInclude = getValueList
	tree
	"include",
	nLockFile = getValue
	tree
	"lock_file",
	nPid = getValue
	tree
	"pid",
	nSslEngine = getValue
	tree
	"ssl_engine",
	nThreadPool = getValueList
	tree
	"thread_pool",
	nTimerResolution = getValue
	tree
	"timer_resolution",
	nUser = getValue
	tree
	"user",
	nWorkerCpuAffinity = getValue
	tree
	"worker_cpu_affinity",
	nWorkerPriority = getValue
	tree
	"worker_priority",
	nWorkerProcesses = getValue
	tree
	"worker_processes",
	nWorkerRLimitCore = getValue
	tree
	"worker_rlimit_core",
	nWorkerRLimitNoFile = getValue
	tree
	"worker_rlimit_nofile",
	nWorkingDirectory = getValue
	tree
	"working_directory",
	nEvents = getEvents
	tree, 
	nHttp = getHttp
	tree
}
--}}}

--{{{ Function that print the resulting source
printSourceNginx :: IO ()
printSourceNginx = parseTreeNginx "nginx.conf" >>= \(Right tree) -> print 
	(createSourceNginx tree)
--}}}
