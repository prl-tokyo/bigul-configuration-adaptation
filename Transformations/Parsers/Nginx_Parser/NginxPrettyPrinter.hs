module NginxPrettyPrinter where

--{{{ Modules imports
import TreeConfigNginxFiller
import NginxSourceCreator
import TypeFiles.NginxTypes
import TypesAndFunctions
--}}}

--{{{ Haskell imports
import Text.PrettyPrint
---}}}

--{{{ Function that will pretty print the NginxWebserver
printNginx :: NginxWebserver -> Doc
printNginx source = printMaybe
	(printSimpleInstruction "daemon")
	(nDaemon source)
	$$ printMaybe
	(printSimpleInstruction "error_log")
	(nErrorLog source)
	$$ printMaybe
	(printListInstruction "env")
	(nEnv source)
	$$ printMaybe
	(printListInstruction "include")
	(nInclude source)
	$$ printMaybe
	(printSimpleInstruction "lock_file")
	(nLockFile source)
	$$ printMaybe
	(printSimpleInstruction "pid")
	(nPid source)
	$$ printMaybe
	(printSimpleInstruction "ssl_engine")
	(nSslEngine source)
	$$ printMaybe
	(printListInstruction "thread_pool")
	(nThreadPool source)
	$$ printMaybe
	(printSimpleInstruction "timer_resolution")
	(nTimerResolution source)
	$$ printMaybe
	(printSimpleInstruction "user")
	(nUser source)
	$$ printMaybe
	(printSimpleInstruction "worker_cpu_affinity")
	(nWorkerCpuAffinity source)
	$$ printMaybe
	(printSimpleInstruction "worker_priority")
	(nWorkerPriority source)
	$$ printMaybe
	(printSimpleInstruction "worker_processes")
	(nWorkerProcesses source)
	$$ printMaybe
	(printSimpleInstruction "worker_rlimit_nofile")
	(nWorkerRLimitNoFile source)
	$$ printMaybe
	(printSimpleInstruction "working_directory")
	(nWorkingDirectory source)
	$$ printMaybe
	printEvents
	(nEvents source)
	$$ printMaybe
	printHttp
	(nHttp source)
--}}}
	
--{{{ Function that will print the events context
printEvents :: Events -> Doc
printEvents source = text "events {"
	$$ nest 5 (printMaybe 
	(printSimpleInstruction "accept_mutex")
	(eAcceptMutex source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "accept_mutex_delay")
	(eAcceptMutexDelay source))
	$$ nest 5 (printMaybe
	(printListInstruction "include")
	(eInclude source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "multi_accept")
	(eMultiAccept source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "use")
	(eUse source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "worker_aio_requests")
	(eWorkerAioRequests source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "worker_connections")
	(eWorkerConnections source))
	$$ text "}"
--}}}

--{{{ Function that will print the http context
printHttp :: Http -> Doc
printHttp source = text "http {"
	$$ nest 5 (printMaybe
	printAccessRules
	(hAccessRule source))
	$$ nest 5 (printMaybe
	(printListInstruction "add_header")
	(hAddHeader source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "aio")
	(hAio source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "auth_basic")
	(hAuthBasic source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "auth_basic_user_file")
	(hAuthBasicUserFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "autoindex")
	(hAutoindex source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ai_exact_size")
	(hAiExactSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ai_format")
	(hAiFormat source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ai_localtime")
	(hAiLocaltime source))
	$$ nest 5 (printMaybe
	(printListInstruction "ancient_browser")
	(hAncientBrowser source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ancient_browser_value")
	(hAncientBrowserValue source))
	$$ nest 5 (printMaybe
	(printListInstruction "modern_browser")
	(hModernBrowser source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "modern_browser_value")
	(hModernBrowserValue source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "charset")
	(hCharset source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "override_charset")
	(hOverrideCharset source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "source_charset")
	(hSourceCharset source))
	$$ nest 5 (printMaybe
	(printListInstruction "charset_map")
	(hCharsetMap source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "charset_type")
	(hCharsetType source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "chunked_transfer_encoding")
	(hChunkedTransferEncoding source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_header_buffer_size")
	(hClientHeaderBufferSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_buffer_size")
	(hClientBodyBufferSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_in_file_only")
	(hClientBodyInFileOnly source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_in_single_buffer")
	(hClientBodyInSingleBuffer source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_temp_path")
	(hClientBodyTempPath source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_timeout")
	(hClientBodyTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_max_body_size")
	(hClientMaxBodySize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_header_timeout")
	(hClientHeaderTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "connection_pool_size")
	(hConnectionPoolSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "default_type")
	(hDefaultType source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "directio")
	(hDirectio source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "directio_alignment")
	(hDirectioAlignment source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "disable_symplinks")	
	(hDisableSymlinks source))
	$$ nest 5 (printMaybe
	(printListInstruction "error_page")
	(hErrorPage source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "etag")
	(hEtag source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "expires")
	(hExpires source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip")
	(hGzip source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_buffers")
	(hGzipBuffers source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_comp_level")
	(hGzipCompLevel source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_disable")
	(hGzipDisable source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_min_length")
	(hGzipMinLength source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_http_version")
	(hGzipHttpVersion source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_proxied")
	(hGzipProxied source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_types")
	(hGzipTypes source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_vary")
	(hGzipVary source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "if_modified_since")
	(hIfModifiedSince source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ingore_invalid_headers")
	(hIgnoreInvalidHeaders source))
	$$ nest 5 (printMaybe
	(printListInstruction "include")
	(hInclude source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "index")
	(hIndex source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "keepalive_disable")
	(hKeepaliveDisable source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "keepalive_requests")
	(hKeepaliveRequests source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "keepalive_timeout")
	(hKeepaliveTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "large_client_header_buffers")
	(hLargeClientHeaderBuffers source))
	$$ nest 5 (printMaybe
	(printListInstruction "limit_conn")
	(hLimitConn source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_conn_log_level")
	(hLimitConnLogLevel source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_conn_status")
	(hLimitConnStatus source))
	$$ nest 5 (printMaybe
	(printListInstruction "limit_conn_zone")
	(hLimitConnZone source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_rate")
	(hLimitRate source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_rate_after")
	(hLimitRateAfter source))
	$$ nest 5 (printMaybe
	(printListInstruction "limit_req")
	(hLimitReq source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_req_log_level")
	(hLimitReqLogLevel source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_req_status")
	(hLimitReqStatus source))
	$$ nest 5 (printMaybe
	(printListInstruction "limit_req_zone")
	(hLimitReqZone source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "lingering_close")
	(hLingeringClose source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "lingering_time")
	(hLingeringTime source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "lingering_timeout")
	(hLingeringTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "access_log")
	(hAccessLog source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "error_log")
	(hErrorLog source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "log_not_found")
	(hLogNotFound source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "log_subrequest")
	(hLogSubrequest source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_log_file_cache")
	(hOpenLogFileCache source))
	$$ nest 5 (printMaybe
	(printListInstruction "log_format")
	(hLogFormat source))
	$$ nest 5 (printMaybe
	(printListInstruction "map")
	(hMap source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "map_hash_bucket_size")
	(hMapHashBucketSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "map_hash_max_size")
	(hMapHashMaxSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "max_ranges")
	(hMaxRanges source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_bind")
	(hMemcachedBind source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_buffer_size")
	(hMemCachedBufferSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_connect_timeout")
	(hMemcachedConnectTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_force_ranges")
	(hMemcachedForceRanges source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_gzip_flag")
	(hMemcachedGzipFlag source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_next_upstream")
	(hMemcachedNextUpstream source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memached_next_upstream_timeout")
	(hMemcachedNextUpstreamTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_next_upstream_tries")
	(hMemcachedNextUpstreamTries source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_read_timeout")
	(hMemcachedReadTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_send_timeout")
	(hMemcachedSendTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "merge_slashes")
	(hMergeSlashes source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "msie_padding")
	(hMsiePadding source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "msie_refresh")
	(hMsieRefresh source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache")
	(hOpenFileCache source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache_errors")
	(hOpenFileCacheErrors source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache_min_uses")
	(hOpenFileCacheMinUses source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache_valid")
	(hOpenFileCacheValid source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "output_buffers")
	(hOutputBuffers source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "postpone_output")
	(hPostponeOutput source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "port_in_redirect")
	(hPortInRedirect source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "read_ahead")
	(hReadAhead source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "recursive_error_pages")
	(hRecursiveErrorPages source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "request_pool_size")
	(hRequestPoolSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "reset_timedout_connection")
	(hResetTimedoutConnection source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "resolver")
	(hResolver source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "resolver_timeout")
	(hResolverTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "root")
	(hRoot source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "satisfy")
	(hSatisfy source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_lowat")
	(hSendLowat source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_timeout")
	(hSendTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_file")
	(hSendFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_file_max_chunks")
	(hSendFileMaxChunks source))	
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_rate_after")
	(hLimitRateAfter source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "server_name_in_redirect")
	(hServerNameInRedirect source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "server_names_hash_bucket_size")
	(hServerNamesHashBucketSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "server_tokens")
	(hServerTokens source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl")
	(hSsl source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_buffer_size")
	(hSslBufferSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_certificate")
	(hSslCertificate source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_certificate_key")
	(hSslCertificateKey source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_ciphers")
	(hSslCiphers source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_client_certificate")
	(hSslClientCertificate source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_crl")
	(hSslCrl source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_dhparam")
	(hSslDhparam source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_ecdh_curve")
	(hSslEcdhCurve source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_password_file")
	(hSslPasswordFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_prefer_server_ciphers")
	(hSslPreferServerCiphers source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_protocols")
	(hSslProtocols source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_session_cache")
	(hSslSessionCache source))
	$$ nest 5 (printMaybe
	(printListInstruction "ssl_session_ticket_key")
	(hSslSessionTicketKey source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_session_tickets")
	(hSslSessionTickets source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_session_timeout")
	(hSslSessionTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_stapling")
	(hSslStapling source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_stapling_file")
	(hSslStaplingFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_stapling_responder")
	(hSslStaplingResponder source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_stapling_verify")
	(hSslStaplingVerify source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_trusted_certificate")
	(hSslTrustedCertificate source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_verify_client")
	(hSslVerifyClient source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_verify_depth")
	(hSslVerifyDepth source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "tcp_nodelay")
	(hTcpNodelay source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "tcp_nopush")
	(hTcpNopush source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "types")
	(hTypes source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "types_hash_bucket_size")
	(hTypesHashBucketSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "types_hash_max_size")
	(hTypesHashMaxSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "underscore_in_headers")
	(hUnderscoreInHeaders source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "variables_hash_bucket_size")
	(hVariablesHashBucketSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "variables_hash_max_size")
	(hVariablesHashMaxSize source))
	$$ nest 5 (printMaybe
	printServers
	(hServer source))
	$$ text "}"
--}}}

--{{{ Function that will print a list of servers context
printServers :: [Server] -> Doc
printServers [] = empty
printServers (x:xs) = printServer x $$ printServers xs
--}}}

--{{{ Function that will print a server context
printServer :: Server -> Doc
printServer source = text "server {"
	$$ nest 5 (printMaybe
	printAccessRules
	(sAccessRule source))
	$$ nest 5 (printMaybe
	(printListInstruction "add_header")
	(sAddHeader source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "aio")
	(sAio source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "auth_basic")
	(sAuthBasic source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "auth_basic_user_file")
	(sAuthBasicUserFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "autoindex")
	(sAutoindex source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ai_exact_size")
	(sAiExactSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ai_format")
	(sAiFormat source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ai_localtime")
	(sAiLocaltime source))
	$$ nest 5 (printMaybe
	(printListInstruction "ancient_browser")
	(sAncientBrowser source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ancient_browser_value")
	(sAncientBrowserValue source))
	$$ nest 5 (printMaybe
	(printListInstruction "modern_browser")
	(sModernBrowser source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "modern_browser_value")
	(sModernBrowserValue source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "charset")
	(sCharset source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "override_charset")
	(sOverrideCharset source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "source_charset")
	(sSourceCharset source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "charset_type")
	(sCharsetType source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "chunked_transfer_encoding")
	(sChunkedTransferEncoding source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_header_buffer_size")
	(sClientHeaderBufferSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_buffer_size")
	(sClientBodyBufferSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_in_file_only")
	(sClientBodyInFileOnly source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_in_single_buffer")
	(sClientBodyInSingleBuffer source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_temp_path")
	(sClientBodyTempPath source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_timeout")
	(sClientBodyTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_max_body_size")
	(sClientMaxBodySize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_header_timeout")
	(sClientHeaderTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "connection_pool_size")
	(sConnectionPoolSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "default_type") (sDefaultType source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "directio")
	(sDirectio source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "directio_alignment")
	(sDirectioAlignment source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "disable_symplinks")
	(sDisableSymlinks source))
	$$ nest 5 (printMaybe
	(printListInstruction "error_page")
	(sErrorPage source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "etag")
	(sEtag source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "expires")
	(sExpires source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip")
	(sGzip source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_buffers")
	(sGzipBuffers source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_comp_level")
	(sGzipCompLevel source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_disable")
	(sGzipDisable source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_min_length")
	(sGzipMinLength source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_http_version")
	(sGzipHttpVersion source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_proxied")
	(sGzipProxied source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_types")
	(sGzipTypes source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_vary")
	(sGzipVary source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "if_modified_since")
	(sIfModifiedSince source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ingore_invalid_headers")
	(sIgnoreInvalidHeaders source))
	$$ nest 5 (printMaybe
	(printListInstruction "include")
	(sInclude source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "index")
	(sIndex source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "keepalive_disable")
	(sKeepaliveDisable source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "keepalive_requests")
	(sKeepaliveRequests source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "keepalive_timeout")
	(sKeepaliveTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "large_client_header_buffers")
	(sLargeClientHeaderBuffers source))
	$$ nest 5 (printMaybe
	(printListInstruction "limit_conn")
	(sLimitConn source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_conn_log_level")
	(sLimitConnLogLevel source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_conn_status")
	(sLimitConnStatus source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_rate")
	(sLimitRate source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_rate_after")
	(sLimitRateAfter source))
	$$ nest 5 (printMaybe
	(printListInstruction "limit_req")
	(sLimitReq source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_req_log_level")
	(sLimitReqLogLevel source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_req_status")
	(sLimitReqStatus source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "lingering_close")
	(sLingeringClose source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "lingering_time")
	(sLingeringTime source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "lingering_timeout")
	(sLingeringTimeout source))
	$$ nest 5 (printMaybe
	(printListInstruction "listen")
	(sListen source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "access_log")
	(sAccessLog source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "error_log")
	(sErrorLog source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "log_not_found")
	(sLogNotFound source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "log_subrequest")
	(sLogSubrequest source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_log_file_cache")
	(sOpenLogFileCache source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "max_ranges")
	(sMaxRanges source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_bind")
	(sMemcachedBind source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_buffer_size")
	(sMemCachedBufferSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_connect_timeout")
	(sMemcachedConnectTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_force_ranges")
	(sMemcachedForceRanges source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_gzip_flag")
	(sMemcachedGzipFlag source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_next_upstream")
	(sMemcachedNextUpstream source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memached_next_upstream_timeout")
	(sMemcachedNextUpstreamTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_next_upstream_tries")
	(sMemcachedNextUpstreamTries source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_read_timeout")
	(sMemcachedReadTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_send_timeout")
	(sMemcachedSendTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "merge_slashes")
	(sMergeSlashes source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "msie_padding")
	(sMsiePadding source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "msie_refresh")
	(sMsieRefresh source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache")
	(sOpenFileCache source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache_errors")
	(sOpenFileCacheErrors source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache_min_uses")
	(sOpenFileCacheMinUses source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache_valid")
	(sOpenFileCacheValid source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "output_buffers")
	(sOutputBuffers source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "postpone_output")
	(sPostponeOutput source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "port_in_redirect")
	(sPortInRedirect source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "read_ahead")
	(sReadAhead source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "recursive_error_pages")
	(sRecursiveErrorPages source))
	$$ nest 5 (printMaybe
	(printListInstruction "valid_referer")
	(sValidReferer source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "referer_hash_bucket_size")
	(sRefererHashBucketSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "referer_hash_max_size")
	(sRefererHashMaxSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "request_pool_size")
	(sRequestPoolSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "reset_timedout_connection")
	(sResetTimedoutConnection source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "resolver")
	(sResolver source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "resolver_timeout")
	(sResolverTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "root")
	(sRoot source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "satisfy")
	(sSatisfy source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_lowat")
	(sSendLowat source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_timeout")
	(sSendTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_file")
	(sSendFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_file_max_chunks")
	(sSendFileMaxChunks source))	
	$$ nest 5 (printMaybe
	(printListInstruction "server_name")
	(sServerName source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "server_name_in_redirect")
	(sServerNameInRedirect source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "server_tokens")
	(sServerTokens source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl")
	(sSsl source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_buffer_size")
	(sSslBufferSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_certificate")
	(sSslCertificate source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_certificate_key")
	(sSslCertificateKey source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_ciphers")
	(sSslCiphers source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_client_certificate")
	(sSslClientCertificate source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_crl")
	(sSslCrl source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_dhparam")
	(sSslDhparam source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_ecdh_curve")
	(sSslEcdhCurve source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_password_file")
	(sSslPasswordFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_prefer_server_ciphers")
	(sSslPreferServerCiphers source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_protocols")
	(sSslProtocols source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_session_cache")
	(sSslSessionCache source))
	$$ nest 5 (printMaybe
	(printListInstruction "ssl_session_ticket_key")
	(sSslSessionTicketKey source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_session_tickets")
	(sSslSessionTickets source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_session_timeout")
	(sSslSessionTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_stapling")
	(sSslStapling source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_stapling_file")
	(sSslStaplingFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_stapling_responder")
	(sSslStaplingResponder source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_stapling_verify")
	(sSslStaplingVerify source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_trusted_certificate")
	(sSslTrustedCertificate source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_verify_client")
	(sSslVerifyClient source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ssl_verify_depth")
	(sSslVerifyDepth source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "tcp_nodelay")
	(sTcpNodelay source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "tcp_nopush")
	(sTcpNopush source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "try_files")
	(sTryFiles source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "types")
	(sTypes source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "types_hash_bucket_size")
	(sTypesHashBucketSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "types_hash_max_size")
	(sTypesHashMaxSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "underscore_in_headers")
	(sUnderscoresInHeaders source))
	$$ nest 5 (printMaybe
	printLocations
	(sLocation source))
	$$ text "}"
--}}}

--{{{ Function that will print a list of location context
printLocations :: [Location] -> Doc
printLocations [] = empty
printLocations (x:xs) = printLocation x $$ printLocations xs
--}}}

--{{{ Function that will print a server context
printLocation :: Location -> Doc
printLocation source = text "location" <+> 
	(printMaybe
	printValueInstruction
	(lLocationPath source)) <+>
	text "{"
	$$ nest 5 (printMaybe
	printAccessRules
	(lAccessRule source))
	$$ nest 5 (printMaybe
	(printListInstruction "add_header")
	(lAddHeader source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "aio")
	(lAio source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "auth_basic")
	(lAuthBasic source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "auth_basic_user_file")
	(lAuthBasicUserFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "autoindex")
	(lAutoindex source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ai_exact_size")
	(lAiExactSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ai_format")
	(lAiFormat source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ai_localtime")
	(lAiLocaltime source))
	$$ nest 5 (printMaybe
	(printListInstruction "ancient_browser")
	(lAncientBrowser source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "ancient_browser_value")
	(lAncientBrowserValue source))
	$$ nest 5 (printMaybe
	(printListInstruction "modern_browser")
	(lModernBrowser source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "modern_browser_value")
	(lModernBrowserValue source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "charset")
	(lCharset source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "override_charset")
	(lOverrideCharset source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "source_charset")
	(lSourceCharset source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "charset_type")
	(lCharsetType source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "chunked_transfer_encoding")
	(lChunkedTransferEncoding source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_buffer_size")
	(lClientBodyBufferSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_in_file_only")
	(lClientBodyInFileOnly source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_in_single_buffer")
	(lClientBodyInSingleBuffer source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_temp_path")
	(lClientBodyTempPath source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_body_timeout")
	(lClientBodyTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "client_max_body_size")
	(lClientMaxBodySize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "default_type")
	(lDefaultType source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "directio")
	(lDirectio source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "directio_alignment")
	(lDirectioAlignment source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "disable_symplinks")
	(lDisableSymlinks source))
	$$ nest 5 (printMaybe
	(printListInstruction "error_page")
	(lErrorPage source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "etag")
	(lEtag source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "expires")
	(lExpires source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip")
	(lGzip source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_buffers")
	(lGzipBuffers source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_comp_level")
	(lGzipCompLevel source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_disable")
	(lGzipDisable source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_min_length")
	(lGzipMinLength source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_http_version")
	(lGzipHttpVersion source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_proxied")
	(lGzipProxied source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_types")
	(lGzipTypes source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "gzip_vary")
	(lGzipVary source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "if_modified_since")
	(lIfModifiedSince source))
	$$ nest 5 (printMaybe
	(printListInstruction "include")
	(lInclude source))
	$$ nest 5 (printMaybe
	(printSingleInstruction "internal")
	(lInternal source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "keepalive_disable")
	(lKeepaliveDisable source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "keepalive_requests")
	(lKeepaliveRequests source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "keepalive_timeout")
	(lKeepaliveTimeout source))
	$$ nest 5 (printMaybe
	(printListInstruction "limit_conn")
	(lLimitConn source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_conn_log_level")
	(lLimitConnLogLevel source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_conn_status")
	(lLimitConnStatus source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_rate")
	(lLimitRate source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_rate_after")
	(lLimitRateAfter source))
	$$ nest 5 (printMaybe
	(printListInstruction "limit_req")
	(lLimitReq source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_req_log_level")
	(lLimitReqLogLevel source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "limit_req_status")
	(lLimitReqStatus source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "lingering_close")
	(lLingeringClose source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "lingering_time")
	(lLingeringTime source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "lingering_timeout")
	(lLingeringTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "access_log")
	(lAccessLog source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "error_log")
	(lErrorLog source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "log_not_found")
	(lLogNotFound source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "log_subrequest")
	(lLogSubrequest source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_log_file_cache")
	(lOpenLogFileCache source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "max_ranges")
	(lMaxRanges source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_bind")
	(lMemcachedBind source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_buffer_size")
	(lMemCachedBufferSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_connect_timeout")
	(lMemcachedConnectTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_force_ranges")
	(lMemcachedForceRanges source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_gzip_flag")
	(lMemcachedGzipFlag source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_next_upstream")
	(lMemcachedNextUpstream source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memached_next_upstream_timeout")
	(lMemcachedNextUpstreamTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_next_upstream_tries")
	(lMemcachedNextUpstreamTries source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_read_timeout")
	(lMemcachedReadTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_send_timeout")
	(lMemcachedSendTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "memcached_pass")
	(lMemcachedPass source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "msie_padding")
	(lMsiePadding source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "msie_refresh")
	(lMsieRefresh source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache")
	(lOpenFileCache source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache_errors")
	(lOpenFileCacheErrors source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache_min_uses")
	(lOpenFileCacheMinUses source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "open_file_cache_valid")
	(lOpenFileCacheValid source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "output_buffers")
	(lOutputBuffers source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "postpone_output")
	(lPostponeOutput source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "port_in_redirect")
	(lPortInRedirect source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "read_ahead")
	(lReadAhead source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "recursive_error_pages")
	(lRecursiveErrorPages source))
	$$ nest 5 (printMaybe
	(printListInstruction "valid_referer")
	(lValidReferer source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "referer_hash_bucket_size")
	(lRefererHashBucketSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "referer_hash_max_size")
	(lRefererHashMaxSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "reset_timedout_connection")
	(lResetTimedoutConnection source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "resolver")
	(lResolver source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "resolver_timeout")
	(lResolverTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "root")
	(lRoot source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "satisfy")
	(lSatisfy source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_lowat")
	(lSendLowat source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_timeout")
	(lSendTimeout source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_file")
	(lSendFile source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "send_file_max_chunks")
	(lSendFileMaxChunks source))	
	$$ nest 5 (printMaybe
	(printSimpleInstruction "server_name_in_redirect")
	(lServerNameInRedirect source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "server_tokens")
	(lServerTokens source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "tcp_nodelay")
	(lTcpNodelay source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "tcp_nopush")
	(lTcpNopush source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "try_files")
	(lTryFiles source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "types")
	(lTypes source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "types_hash_bucket_size")
	(lTypesHashBucketSize source))
	$$ nest 5 (printMaybe
	(printSimpleInstruction "types_hash_max_size")
	(lTypesHashMaxSize source))
	$$ nest 5 (printMaybe
	printLocations
	(lLocation source))
	$$ text "}"
--}}}

--{{{ Function that will print the access rules (allow and deny)
printAccessRules :: [(String,String)] -> Doc
printAccessRules [] = empty
printAccessRules (x:xs) =
	text (fst x) <+> text (snd x) <> semi $$ printAccessRules xs
--}}}

--{{{ Function that just prints the instruction
printSingleInstruction :: String -> String -> Doc
printSingleInstruction instruction value = text instruction <> semi
--}}}

--{{{ Functionn that prints a simple instruction
printSimpleInstruction :: String -> String -> Doc
printSimpleInstruction instruction value = text instruction <+> text value <> semi
--}}}

--{{{ Function that prints a list of the same instructions from a list of values
printListInstruction :: String -> [String] -> Doc
printListInstruction instruction [] = empty
printListInstruction instruction (x:xs) =
	printSimpleInstruction instruction x 
	$$ printListInstruction instruction xs
--}}}

--{{{ Function that prints a Nginx source
printNginxConf :: IO ()
printNginxConf = parseTreeNginx "nginx.conf" >>= \(Right tree) -> print 
	(printNginx (createSourceNginx tree))
--}}}
