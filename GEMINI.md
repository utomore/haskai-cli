# haskai-cli 專案指南

`haskai-cli` 是一個使用 Haskell 撰寫的 AI CLI 工具，旨在提供類似 `gemini-cli` 或 `claude-code` 的終端互動體驗。

## 專案概述
- **目標**: 建立一個基於終端的 AI Agent 工具。
- **核心架構**:
  - **TUI 層**: 使用 `brick` 構建互動介面。
  - **網路層**: 使用 `wreq` 處理 API 請求。
  - **數據處理**: 使用 `aeson` 進行 JSON 解析。
  - **命令解析**: 使用 `optparse-applicative` 處理 CLI 參數。

## 技術棧
- **語言**: Haskell (GHC 2024 / GHC 9.12.2)
- **構建工具**: `cabal` + `hpack`
- **主要依賴**: `base`, `aeson`, `wreq`, `brick`, `optparse-applicative`

## 構建與執行
本專案採用 `hpack` 管理專案配置，**請勿直接修改 `.cabal` 檔案**。

### 常用指令
- **同步配置**: `hpack` (修改 `package.yaml` 後執行，或由 `cabal` 自動觸發)
- **構建專案**: `cabal build`
- **執行程式**: `cabal run haskai-cli`
- **啟動 REPL**: `cabal repl`
- **清理構建**: `cabal clean`

### 測試 (TODO)
目前尚未建立測試套件。

## 開發規範
1. **數據與邏輯分離**: 盡量使用純函數式思惟，將副作用 (Side Effects) 推到系統邊際處理。
2. **配置管理**: 所有的專案配置更改都應在 `package.yaml` 中進行，`hpack` 會自動更新 `haskai-cli.cabal`。
3. **語言特性**: 預設使用 `GHC2024`，編譯選項開啟 `-Wall`。
4. **編譯器版本**: 建議使用 `GHC 9.12.2` 以確保相容性。
