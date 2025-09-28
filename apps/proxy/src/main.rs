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
use std::io::{self, Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::thread;

fn main() {
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
    let stdin_to_sock = thread::spawn(move || {
        let mut stdin = io::stdin();
        let mut buf = [0u8; 8192];
        loop {
            match stdin.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                    if let Err(_) = stream.write_all(&buf[..n]) {
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
    loop {
        match stream_clone.read(&mut buf) {
            Ok(0) => break,
            Ok(n) => {
                if let Err(_) = stdout.write_all(&buf[..n]) {
                    break;
                }
                let _ = stdout.flush();
            }
            Err(_) => break,
        }
    }

    let _ = stdin_to_sock.join();
}
