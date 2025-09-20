// Learn more about Tauri commands at https://tauri.app/develop/calling-rust/
use serde::{Deserialize, Serialize};
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};
use tauri::Emitter;
use tauri::Manager;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Endpoint {
    pub domain: String,
    pub port: u16,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DaemonStatus {
    pub pid: i32,
    pub version: String,
    pub lsp: Endpoint,
    pub mcp: Endpoint,
    pub http: Endpoint,
}

#[derive(Default)]
struct AppState {
    status: Mutex<Option<DaemonStatus>>,
}

fn fetch_daemon_status() -> Option<DaemonStatus> {
    let output = Command::new("elm-dev")
        .args(["daemon", "start"]) // prints a single JSON object to stdout
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .ok()?;

    // Be tolerant of any noise: try each non-empty line, last one wins
    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut last_ok: Option<DaemonStatus> = None;
    for line in stdout.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if let Ok(parsed) = serde_json::from_str::<DaemonStatus>(trimmed) {
            last_ok = Some(parsed);
        }
    }

    // If nothing parsed line-by-line, try whole buffer
    if last_ok.is_none() {
        if let Ok(parsed) = serde_json::from_str::<DaemonStatus>(&stdout) {
            last_ok = Some(parsed);
        }
    }

    last_ok
}

#[tauri::command]
fn get_daemon_status(state: tauri::State<Arc<AppState>>) -> Option<DaemonStatus> {
    state.status.lock().ok().and_then(|g| g.clone())
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .manage(Arc::new(AppState::default()))
        .invoke_handler(tauri::generate_handler![get_daemon_status])
        .setup(|app| {
            let handle = app.handle().clone();
            // Clone the inner Arc so it is owned and 'static for the thread
            let state_arc: Arc<AppState> = app.state::<Arc<AppState>>().inner().clone();
            std::thread::spawn(move || {
                if let Some(status) = fetch_daemon_status() {
                    if let Ok(mut guard) = state_arc.status.lock() {
                        *guard = Some(status.clone());
                    }
                    let _ = handle.emit("daemon://status", status);
                }
            });
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
