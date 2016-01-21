module Nginx_output where
import TypeFiles.Common
nginxOutput :: CommonWebserver
nginxOutput = CommonWebserver {vRoot = "html", vIndex = "index.html index.htm", vKeepaliveTimeout = "65", vKeepaliveMaxRequests = "100", vSendfile = "off", vSSL = "off", vSSLCACertificate = "", vSSLCARevocationFile = "", vSSLCertificate = "", vSSLCertificateKey = "", vSSLCiphers = "HIGH:!aNULL:!MD5", vSSLPreferServerCiphers = "on", vSSLProtocols = "TLSv1 TLSv1.1 TLSv1.2", vSSLVerifyClient = "", vSSLVerifyDepth = "1", vServers = [VServer {vListen = ["*:80"], vServNames = ["example.com"], vServRoot = "/var/www/html", vServIndex = "index.html", vServKeepaliveTimeout = "75s", vServKeepaliveMaxRequests = "100", vServSendfile = "off", vServSSL = "off", vServSSLCACertificate = "", vServSSLCARevocationFile = "", vServSSLCertificate = "", vServSSLCertificateKey = "", vServSSLCiphers = "HIGH:!aNULL:!MD5", vServSSLPreferServerCiphers = "off", vServSSLProtocols = "TLSv1 TLSv1.1 TLSv1.2", vServSSLVerifyClient = "", vServSSLVerifyDepth = "1", vLocations = [VLocation {vLocationPath = "/", vLocIndex = "index.html", vLocSendfile = "off"}]},VServer {vListen = ["443"], vServNames = ["otherexample.com"], vServRoot = "/var/www/html_secure", vServIndex = "index.html", vServKeepaliveTimeout = "75s", vServKeepaliveMaxRequests = "100", vServSendfile = "off", vServSSL = "on", vServSSLCACertificate = "", vServSSLCARevocationFile = "", vServSSLCertificate = "/etc/nginx/certs/cert.pem", vServSSLCertificateKey = "/etc/nginx/certs/cert.key", vServSSLCiphers = "HIGH:!aNULL:!MD5", vServSSLPreferServerCiphers = "off", vServSSLProtocols = "TLSv1 TLSv1.1 TLSv1.2", vServSSLVerifyClient = "yes", vServSSLVerifyDepth = "1", vLocations = []}]}