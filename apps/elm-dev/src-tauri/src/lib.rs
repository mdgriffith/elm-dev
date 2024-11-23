use tauri::menu::*;
use tauri::Manager;
use tauri_plugin_positioner::{Position, WindowExt};

// Learn more about Tauri commands at https://tauri.app/v1/guides/features/command
#[tauri::command]
fn greet(name: &str) -> String {
    format!("Hello, {}! You've been greeted from Rust!", name)
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    // here `"quit".to_string()` defines the menu item id, and the second parameter is the menu item label.

    tauri::Builder::default()
        .plugin(tauri_plugin_positioner::init())
        .plugin(tauri_plugin_http::init())
        .plugin(tauri_plugin_os::init())
        .plugin(tauri_plugin_shell::init())
        .invoke_handler(tauri::generate_handler![greet])
        .setup(|app| {
            let handle = app.handle();
            let quit = MenuItem::with_id(handle, "quit", "Quit Elm Dev", true, None::<&str>)?;
            let tray_menu = tauri::menu::MenuBuilder::new(handle)
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

            let win = app.get_webview_window("main").unwrap();
            let maybe_monitor = win.current_monitor().unwrap();

            let height = match maybe_monitor {
                None => 900,
                Some(monitor) => monitor.size().height - 60,
            };

            // Set window position and size.
            let _ = win.as_ref().window().move_window(Position::TopLeft);
            let _ = win
                .as_ref()
                .window()
                .set_size(tauri::Size::Physical(tauri::PhysicalSize {
                    width: 1200,
                    height: height,
                }));

            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
