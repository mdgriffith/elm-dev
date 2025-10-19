/*!
elm-dev-proxy — a tiny TCP stdio proxy

Pipes stdin to a TCP socket at `<host>:<port>` and mirrors everything
received from the socket to stdout. Useful for bridging tools that speak
over stdio with services that listen on TCP.

Connection behavior:
- Resolves `<host>` and retries connecting for ~5 seconds (50 × 100ms)
- Exits with code 2 on usage error, 1 on connection failure
- Uses `TCP_NODELAY` to reduce buffering

Usage:

```sh
elm-dev-proxy <host> <port>
```

Examples:

- Send a quick message to a local TCP server and print the response
  (in another terminal, run `nc -l 4000` or `ncat -l 4000`):
  ```sh
  echo "hello" | elm-dev-proxy 127.0.0.1 4000
  ```

- Interactive session; type to send, Ctrl-D to close stdin:
  ```sh
  elm-dev-proxy localhost 8000
  ```

- Pipe a binary request and capture the binary response:
  ```sh
  elm-dev-proxy 10.0.0.5 1234 < request.bin > response.bin
  ```

Notes:
- This is a raw TCP bridge, not HTTP-aware and not TLS-encrypted.
- If multiple addresses resolve for `<host>`, it tries each until one connects.
*/

use std::env;
use std::fs::OpenOptions;
use std::io::{self, Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{SystemTime, UNIX_EPOCH};

// Optional binary-safe logger; drive via code-level toggle for now
const LOG_ENABLED: bool = false; // set to true to enable proxy I/O logging
const LOG_PATH: &str = "/tmp/elm-dev-proxy.log"; // customize path if needed

fn make_logger() -> Option<Arc<Mutex<std::fs::File>>> {
    if !LOG_ENABLED {
        return None;
    }
    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(LOG_PATH)
        .ok()?;
    Some(Arc::new(Mutex::new(file)))
}

fn log_bytes(logger: &Option<Arc<Mutex<std::fs::File>>>, dir: &str, bytes: &[u8]) {
    if let Some(l) = logger {
        if let Ok(mut f) = l.lock() {
            let ts = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis())
                .unwrap_or(0);
            let _ = write!(f, "[{}] {} {} bytes\n", ts, dir, bytes.len());
            // hex dump on one line (binary-safe)
            for b in bytes {
                let _ = write!(f, "{:02x}", b);
            }
            let _ = write!(f, "\n");
            // utf8 preview (escaped), with lossy fallback
            match std::str::from_utf8(bytes) {
                Ok(s) => {
                    let escaped = s.replace('\r', "\\r").replace('\n', "\\n");
                    let _ = write!(f, "utf8: {}\n", escaped);
                }
                Err(_) => {
                    let lossy = String::from_utf8_lossy(bytes);
                    let escaped = lossy.replace('\r', "\\r").replace('\n', "\\n");
                    let _ = write!(f, "utf8(lossy): {}\n", escaped);
                }
            }
            let _ = f.flush();
        }
    }
}

fn log_line(logger: &Option<Arc<Mutex<std::fs::File>>>, msg: &str) {
    if let Some(l) = logger {
        if let Ok(mut f) = l.lock() {
            let ts = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis())
                .unwrap_or(0);
            let _ = writeln!(f, "[{}] {}", ts, msg);
            let _ = f.flush();
        }
    }
}

fn looks_like_content_length_frame(bytes: &[u8]) -> bool {
    // Fast check: contains "Content-Length:" (case-insensitive) before a double newline
    if bytes.is_empty() {
        return false;
    }
    let upper = bytes
        .iter()
        .map(|b| b.to_ascii_uppercase())
        .collect::<Vec<u8>>();
    if let Some(hdr_end) = find_double_crlf_or_lf(&upper) {
        let headers = &upper[..hdr_end];
        return memmem(&headers, b"CONTENT-LENGTH:");
    }
    false
}

fn find_double_crlf_or_lf(bytes: &[u8]) -> Option<usize> {
    // Find end of headers: either "\r\n\r\n" or "\n\n"
    let mut i = 0;
    while i + 1 < bytes.len() {
        if i + 3 < bytes.len() && &bytes[i..i + 4] == b"\r\n\r\n" {
            return Some(i + 4);
        }
        if &bytes[i..i + 2] == b"\n\n" {
            return Some(i + 2);
        }
        i += 1;
    }
    None
}

fn memmem(hay: &[u8], needle: &[u8]) -> bool {
    if needle.is_empty() {
        return true;
    }
    hay.windows(needle.len()).any(|w| w == needle)
}

fn trim_trailing_newlines(bytes: &[u8]) -> &[u8] {
    let mut end = bytes.len();
    while end > 0 {
        let b = bytes[end - 1];
        if b == b'\n' || b == b'\r' {
            end -= 1;
        } else {
            break;
        }
    }
    &bytes[..end]
}

fn first_non_ws(bytes: &[u8]) -> Option<u8> {
    for &b in bytes {
        if !matches!(b, b' ' | b'\t' | b'\r' | b'\n') {
            return Some(b);
        }
    }
    None
}

fn frame_if_needed(bytes: &[u8]) -> Vec<u8> {
    // If already framed with Content-Length, pass through.
    if looks_like_content_length_frame(bytes) {
        return bytes.to_vec();
    }
    // If this chunk looks like a single JSON-RPC message line (starts with { or [), wrap it.
    match first_non_ws(bytes) {
        Some(b'{') | Some(b'[') => {
            let payload = trim_trailing_newlines(bytes);
            let len = payload.len();
            let mut out = Vec::with_capacity(len + 64);
            out.extend_from_slice(format!("Content-Length: {}\r\n\r\n", len).as_bytes());
            out.extend_from_slice(payload);
            out
        }
        _ => bytes.to_vec(),
    }
}

fn parse_content_length(headers: &[u8]) -> Option<usize> {
    // Split on lines and look for Content-Length: <n> (case-insensitive)
    let mut start = 0usize;
    while start < headers.len() {
        // find end of line
        let mut end = start;
        while end < headers.len() && headers[end] != b'\n' {
            end += 1;
        }
        let line = &headers[start..end];
        // advance start to next line
        start = if end < headers.len() { end + 1 } else { end };

        // trim CR at end
        let line = if !line.is_empty() && *line.last().unwrap() == b'\r' {
            &line[..line.len() - 1]
        } else {
            line
        };
        // find ':'
        if let Some(colon) = line.iter().position(|&b| b == b':') {
            let (name, value_raw) = line.split_at(colon);
            // skip ':' and spaces
            let value = value_raw
                .iter()
                .skip(1)
                .skip_while(|&&b| b == b' ')
                .copied()
                .collect::<Vec<u8>>();
            // lowercase name for compare
            let mut name_lc = name.to_vec();
            for b in &mut name_lc {
                *b = b.to_ascii_lowercase();
            }
            if name_lc == b"content-length" {
                if let Ok(s) = std::str::from_utf8(&value) {
                    if let Ok(n) = s.trim().parse::<usize>() {
                        return Some(n);
                    }
                }
            }
        }
    }
    None
}

fn extract_framed_payload(buffer: &mut Vec<u8>) -> Option<Vec<u8>> {
    // Find header terminator
    let header_end = find_double_crlf_or_lf(buffer.as_slice())?;
    let headers = &buffer[..header_end];
    let len = parse_content_length(headers)?;
    if buffer.len() < header_end + len {
        return None;
    }
    let payload = buffer[header_end..header_end + len].to_vec();
    buffer.drain(0..header_end + len);
    Some(payload)
}

fn main() {
    let logger = make_logger();
    let mut args = env::args().skip(1);
    let host = args.next().unwrap_or_else(|| "127.0.0.1".to_string());
    let port: u16 = args.next().and_then(|p| p.parse().ok()).unwrap_or(0);
    if port == 0 {
        eprintln!("usage: elm-dev-proxy <host> <port>");
        std::process::exit(2);
    }

    // Resolve and connect with simple retry/backoff
    let addr_iter = (host.as_str(), port)
        .to_socket_addrs()
        .expect("invalid host/port");

    let mut last_err = None;
    let mut stream_opt = None;
    for _ in 0..50 {
        for addr in addr_iter.clone() {
            match TcpStream::connect(addr) {
                Ok(s) => {
                    stream_opt = Some(s);
                    break;
                }
                Err(e) => {
                    last_err = Some(e);
                }
            }
        }
        if stream_opt.is_some() {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }

    let mut stream = match stream_opt {
        Some(s) => s,
        None => {
            eprintln!("elm-dev-proxy: failed to connect: {:?}", last_err);
            std::process::exit(1);
        }
    };

    // Make tcp unbuffered-ish
    stream.set_nodelay(true).ok();

    let mut stream_clone = match stream.try_clone() {
        Ok(s) => s,
        Err(e) => {
            eprintln!("elm-dev-proxy: failed to clone stream: {}", e);
            std::process::exit(1);
        }
    };

    // Thread: stdin -> socket
    let logger_in = logger.clone();
    let reframe_mode = Arc::new(Mutex::new(None::<bool>)); // None until first stdin message decides
    let reframe_mode_in = Arc::clone(&reframe_mode);
    thread::spawn(move || {
        let mut stdin = io::stdin();
        let mut buf = [0u8; 8192];
        loop {
            match stdin.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                    log_bytes(&logger_in, "stdin->tcp", &buf[..n]);
                    // Decide reframe mode on first non-empty stdin chunk
                    let mut mode_guard = reframe_mode_in.lock().unwrap();
                    if mode_guard.is_none() {
                        let decide_reframe = !looks_like_content_length_frame(&buf[..n])
                            && matches!(first_non_ws(&buf[..n]), Some(b'{') | Some(b'['));
                        *mode_guard = Some(decide_reframe);
                        if decide_reframe {
                            log_line(
                                &logger_in,
                                "mode: reframe enabled (stdin without Content-Length)",
                            );
                        } else {
                            log_line(&logger_in, "mode: pass-through (stdin uses Content-Length)");
                        }
                    }
                    let reframe = mode_guard.unwrap_or(false);
                    drop(mode_guard);
                    let framed = if reframe {
                        frame_if_needed(&buf[..n])
                    } else {
                        (&buf[..n]).to_vec()
                    };
                    if let Err(_) = stream.write_all(&framed) {
                        break;
                    }
                    let _ = stream.flush();
                }
                Err(_) => break,
            }
        }
    });

    // Main thread: socket -> stdout
    let mut stdout = io::stdout();
    let mut buf = [0u8; 8192];
    let mut acc: Vec<u8> = Vec::new();
    loop {
        match stream_clone.read(&mut buf) {
            Ok(0) => break,
            Ok(n) => {
                // Check current mode
                let reframe = *reframe_mode.lock().unwrap() == Some(true);
                if reframe {
                    acc.extend_from_slice(&buf[..n]);
                    // Try to extract all complete frames
                    while let Some(payload) = extract_framed_payload(&mut acc) {
                        log_bytes(&logger, "tcp->stdout (stripped)", &payload);
                        if let Err(_) = stdout.write_all(&payload) {
                            break;
                        }
                        // ensure newline for NDJSON-style consumers
                        let _ = stdout.write_all(b"\n");
                        let _ = stdout.flush();
                    }
                } else {
                    log_bytes(&logger, "tcp->stdout", &buf[..n]);
                    if let Err(_) = stdout.write_all(&buf[..n]) {
                        break;
                    }
                    let _ = stdout.flush();
                }
            }
            Err(_) => break,
        }
    }

    // Exit immediately so no dangling proxies remain after server closes.
    std::process::exit(0);
}
