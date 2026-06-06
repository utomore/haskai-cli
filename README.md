# HaskAI CLI (`haskai-cli`)

An interactive AI CLI Tool written in Haskell that leverages local **Ollama** models with **streaming responses** and **project-local memory management**.

## Features
- **OpenAI-Compatible Streaming**: Streams model output word-by-word via the Ollama `/v1/chat/completions` API endpoint.
- **Model Selector**: Checks for downloaded models via `/v1/models` on startup and presents an interactive selection list.
- **Project-Local Memory**: Stores long-term memories (user facts) in `.config/memory.json` relative to the project directory, so it doesn't affect your global system configuration.
- **Interactive Console**: Built with `haskeline` for input history, arrow keys navigation, and line-editing.
- **Colored Output**: Uses terminal colors to separate the User prompts and Assistant responses clearly.
- **Slash Commands**:
  - `/help` — Show the command help menu.
  - `/memories` — List all currently saved long-term memories.
  - `/remember <fact>` — Save a new fact to the local memory file (e.g. `/remember I prefer writing Haskell code`).
  - `/forget <index>` — Delete a memory by its list index.
  - `/exit` or `/quit` — Exit the application.

---

## Setup & Run

### Prerequisites
1. **GHC & Cabal**: Ensure Haskell is installed (GHC 9.x or newer recommended).
2. **Ollama**: Make sure Ollama is installed and running:
   ```bash
   ollama serve
   ```
3. **Model**: Download a model, for example `gemma4:12b`:
   ```bash
   ollama run gemma4:12b
   ```

### Building the Project
Clone or navigate to the project directory and build it using Cabal:
```bash
cabal build
```

### Running the CLI
Run the application using:
```bash
cabal run haskai-cli
```

---

## Technical Details
- **Languages & Libraries**:
  - `haskeline`: For line editing and history.
  - `ansi-terminal`: For colorized console text.
  - `aeson`: JSON serialization for memory files and Ollama API payloads.
  - `http-client`: Handling raw HTTP streaming and connection pooling.
- **Architecture**:
  - `Main.hs`: CLI loop and user command routing.
  - `Ollama.hs`: API wrappers (parsing SSE streams, listing models).
  - `Memory.hs`: Persistence logic for the `.config/memory.json` file.
  - `Types.hs`: Shareable types (AppState, Message, Config).
