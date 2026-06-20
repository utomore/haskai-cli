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

const SERVER_CABAL_REL: &str =
    "dist-newstyle/build/x86_64-windows/ghc-9.14.1/haskai-cli-0.1.0.0\
     /x/haskai-server/build/haskai-server/haskai-server.exe";

fn haskell_binary_path() -> String {
    let exe_dir = std::env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(|d| d.to_path_buf()))
        .unwrap_or_default();

    // Walk up from desktop/src-tauri/target/debug/ to project root
    let project_root_from_exe = exe_dir
        .ancestors()
        .find(|p| p.join("haskai-cli.cabal").exists())
        .map(|p| p.to_path_buf());

    // Also try current_dir and its parents
    let project_root_from_cwd = std::env::current_dir()
        .ok()
        .and_then(|cwd| {
            cwd.ancestors()
                .find(|p| p.join("haskai-cli.cabal").exists())
                .map(|p| p.to_path_buf())
        });

    let mut candidates: Vec<std::path::PathBuf> = vec![
        exe_dir.join("haskai-server.exe"),
        exe_dir.join("haskai-server"),
    ];

    for root in [project_root_from_exe, project_root_from_cwd].into_iter().flatten() {
        candidates.push(root.join(SERVER_CABAL_REL));
    }

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

/// Send any input (chat or /command) to the Haskell server.
/// Returns the raw JSON response object from the server.
#[tauri::command]
async fn send_input(input: String, state: State<'_, AppState>) -> Result<serde_json::Value, String> {
    let request = serde_json::json!({ "input": input });
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

    serde_json::from_str(response_line.trim()).map_err(|e| e.to_string())
}

/// Get the list of available models from the Haskell server.
#[tauri::command]
async fn get_models(state: State<'_, AppState>) -> Result<serde_json::Value, String> {
    send_input("/models".to_string(), state).await
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .manage(AppState(Mutex::new(None)))
        .invoke_handler(tauri::generate_handler![send_input, get_models])
        .run(tauri::generate_context!())
        .expect("Tauri 啟動失敗");
}
