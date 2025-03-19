# Web Server Application

## Overview
This is an OTP application that implements a web server in Erlang. It handles HTTP requests using a TCP listener and provides various HTTP-related functionalities.

## Architecture
The application follows a standard OTP architecture with:
- Core modules for application and supervision
- HTTP handlers for parsing and responding to HTTP requests
- TCP modules for handling network connections
- Metrics for monitoring performance
- Utilities for common functions

## Modules
- `web_server_app` - Main application module
- `web_server_sup` - Top-level supervisor
- HTTP modules:
  - `web_server_http_parser` - Parses HTTP requests
  - `web_server_http_socket_writer` - Writes responses to sockets
  - `web_server_http_cache` - Caches responses
  - `web_server_http_io` - Handles I/O operations
  - `web_server_http_get` - Processes GET requests
- TCP modules:
  - `web_server_tcp_listener` - Listens for incoming connections
  - `web_server_tcp_acceptor` - Accepts connections
- Metrics:
  - `metrics` - Collects and reports performance metrics
- Utils:
  - `process_utils` - Utilities for process management 