// Learn more about Tauri commands at https://tauri.app/develop/calling-rust/
use serde::{Deserialize, Serialize};
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tauri::menu::*;
use tauri::Emitter;
use tauri::Manager;
use tauri::{PhysicalPosition, PhysicalSize, Position, Size};
use tauri_plugin_global_shortcut::{Code, GlobalShortcutExt, Modifiers, Shortcut};

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
        .args(["dev", "start"]) // prints a single JSON object to stdout
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
        .plugin(tauri_plugin_positioner::init())
        .plugin(tauri_plugin_opener::init())
        .plugin(tauri_plugin_http::init())
        .plugin(tauri_plugin_global_shortcut::Builder::new().build())
        .manage(Arc::new(AppState::default()))
        .invoke_handler(tauri::generate_handler![get_daemon_status])
        .setup(|app| {
            // System tray with Quit option
            let handle_for_menu = app.handle();
            let quit =
                MenuItem::with_id(handle_for_menu, "quit", "Quit Elm Dev", true, None::<&str>)?;
            let tray_menu = tauri::menu::MenuBuilder::new(handle_for_menu)
                .separator()
                .item(&quit)
                .build()?;

            tauri::tray::TrayIconBuilder::with_id("elm-dev-tray")
                .title("Elm Dev")
                .menu(&tray_menu)
                .on_menu_event(|app, event| match event.id.0.as_str() {
                    "quit" => {
                        app.exit(0);
                    }
                    _ => {}
                })
                .on_tray_icon_event(|tray_handle, event| {
                    tauri_plugin_positioner::on_tray_event(tray_handle.app_handle(), &event);
                })
                .build(app)?;

            // Size and position the main window: half screen width, full screen height, aligned right
            if let Some(main) = app.get_webview_window("main") {
                // Best-effort: ignore errors positioning on startup
                if let Ok(Some(monitor)) = main.current_monitor() {
                    let screen = monitor.size();
                    let target_width = screen.width / 2;
                    let target_height = screen.height;
                    let _ = main.set_size(Size::Physical(PhysicalSize {
                        width: target_width,
                        height: target_height,
                    }));
                    // Align to the right edge (x = total_width - target_width), top = 0
                    let _ = main.set_position(Position::Physical(PhysicalPosition {
                        x: (screen.width.saturating_sub(target_width)) as i32,
                        y: 0,
                    }));
                    // Keep window always on top
                    let _ = main.set_always_on_top(true);
                }
            }

            // Register global shortcut: mac Option+1 (Alt+1) toggles window visibility
            {
                let handle = app.handle().clone();
                let target = Shortcut::new(Some(Modifiers::ALT), Code::Digit1);
                let gs = handle.global_shortcut();
                // Use a separate clone inside the callback to avoid moving the borrowed handle
                let handle_for_cb = handle.clone();
                eprintln!("Attempting to bind global shortcut Alt+1");
                // Simple debounce to avoid double toggle if both press and release fire
                let last_toggle = Arc::new(Mutex::new(None::<Instant>));
                let last_toggle_cb = last_toggle.clone();
                if let Err(err) = gs.on_shortcut(target.clone(), move |_app, _shortcut, _event| {
                    let now = Instant::now();
                    let mut guard = last_toggle_cb.lock().unwrap();
                    if let Some(prev) = *guard {
                        if now.duration_since(prev) < Duration::from_millis(200) {
                            // Ignore rapid second invocation
                            return;
                        }
                    }
                    *guard = Some(now);

                    if let Some(win) = handle_for_cb.get_webview_window("main") {
                        if let Ok(visible) = win.is_visible() {
                            if visible {
                                let _ = win.hide();
                            } else {
                                let _ = win.show();
                                let _ = win.set_focus();
                            }
                        }
                    }
                }) {
                    eprintln!("Global shortcut on_shortcut failed: {:?}", err);
                }
            }

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
