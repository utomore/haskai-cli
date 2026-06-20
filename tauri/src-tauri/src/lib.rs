use std::process::Stdio;
use tauri::State;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStdin, ChildStdout};
use tokio::sync::Mutex;

struct HaskellProcess {
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
    _child: Child,
}

struct AppState(Mutex<Option<HaskellProcess>>);

fn haskell_binary_path() -> String {
    let exe_dir = std::env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(|d| d.to_path_buf()))
        .unwrap_or_default();

    let candidates = [
        exe_dir.join("haskai-server.exe"),
        exe_dir.join("haskai-server"),
        // Development path: cabal build output
        std::path::PathBuf::from(
            "dist-newstyle/build/x86_64-windows/ghc-9.14.1/haskai-cli-0.1.0.0/x/haskai-server/build/haskai-server/haskai-server.exe"
        ),
    ];

    for c in &candidates {
        if c.exists() {
            return c.to_string_lossy().into_owned();
        }
    }

    "haskai-server".to_string()
}

async fn spawn_haskell() -> Result<HaskellProcess, String> {
    let bin = haskell_binary_path();
    let mut child = tokio::process::Command::new(&bin)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|e| format!("無法啟動 Haskell 後端 ({bin}): {e}"))?;

    let stdin = child.stdin.take().ok_or("stdin unavailable")?;
    let stdout = BufReader::new(child.stdout.take().ok_or("stdout unavailable")?);

    Ok(HaskellProcess { stdin, stdout, _child: child })
}

#[tauri::command]
async fn chat(message: String, state: State<'_, AppState>) -> Result<String, String> {
    let request = serde_json::json!({ "message": message });
    let mut line = serde_json::to_string(&request).map_err(|e| e.to_string())?;
    line.push('\n');

    let mut guard = state.0.lock().await;

    if guard.is_none() {
        *guard = Some(spawn_haskell().await?);
    }

    let proc = guard.as_mut().unwrap();

    proc.stdin
        .write_all(line.as_bytes())
        .await
        .map_err(|e| format!("寫入失敗: {e}"))?;
    proc.stdin.flush().await.map_err(|e| e.to_string())?;

    let mut response_line = String::new();
    proc.stdout
        .read_line(&mut response_line)
        .await
        .map_err(|e| format!("讀取失敗: {e}"))?;

    let v: serde_json::Value =
        serde_json::from_str(response_line.trim()).map_err(|e| e.to_string())?;

    v["response"]
        .as_str()
        .map(|s| s.to_string())
        .ok_or_else(|| "回應格式錯誤".to_string())
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .manage(AppState(Mutex::new(None)))
        .invoke_handler(tauri::generate_handler![chat])
        .run(tauri::generate_context!())
        .expect("Tauri 啟動失敗");
}
