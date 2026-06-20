// invoke is resolved lazily after Tauri injects window.__TAURI__
function invoke(cmd, args) {
  return window.__TAURI__.core.invoke(cmd, args);
}

// ── DOM refs ──────────────────────────────────────────────────────────────
const messagesEl  = document.getElementById('messages');
const inputEl     = document.getElementById('user-input');
const sendBtn     = document.getElementById('send-btn');
const statusEl    = document.getElementById('status');
const acEl        = document.getElementById('autocomplete');
const chipSession = document.getElementById('chip-session');
const chipModel   = document.getElementById('chip-model');

// ── All slash commands (for autocomplete) ────────────────────────────────
const COMMANDS = [
  '/help',
  '/memories',
  '/remember ',
  '/forget ',
  '/session list',
  '/session new ',
  '/session load ',
  '/session rename ',
  '/session delete ',
  '/session fork ',
  '/models',
  '/exit',
  '/quit',
];

// ── State ────────────────────────────────────────────────────────────────
let busy = false;
let acSelected = -1;

// ── Status helpers ────────────────────────────────────────────────────────
function setStatus(state) {
  statusEl.className = state;
  statusEl.textContent = { idle: '● 就緒', busy: '◌ 處理中…', error: '✕ 錯誤' }[state] ?? state;
}

// ── Message rendering ─────────────────────────────────────────────────────
function appendMsg(cls, text, dataItems, dataType) {
  const div = document.createElement('div');
  div.className = `msg ${cls}`;
  div.textContent = text;
  if (dataItems) div.appendChild(renderDataList(dataItems, dataType ?? cls));
  messagesEl.appendChild(div);
  messagesEl.scrollTop = messagesEl.scrollHeight;
  return div;
}

function renderDataList(items, type) {
  const list = document.createElement('div');
  list.className = 'data-list';

  if (type === 'sessions' && Array.isArray(items)) {
    items.forEach(s => {
      const row = document.createElement('div');
      row.className = `data-item${s.active ? ' active-row' : ''}`;
      row.innerHTML =
        `<span class="idx">[${s.index}]</span>` +
        `<span>${escHtml(s.name)}</span>` +
        `<span class="badge">${s.msgCount} msg</span>`;
      list.appendChild(row);
    });
  } else if (type === 'memories' && Array.isArray(items)) {
    items.forEach((m, i) => {
      const row = document.createElement('div');
      row.className = 'data-item';
      row.innerHTML = `<span class="idx">[${i + 1}]</span><span>${escHtml(m)}</span>`;
      list.appendChild(row);
    });
  } else if (type === 'models' && Array.isArray(items)) {
    items.forEach(m => {
      const row = document.createElement('div');
      row.className = 'data-item';
      row.innerHTML = `<span>${escHtml(m)}</span>`;
      list.appendChild(row);
    });
  }

  return list;
}

function escHtml(s) {
  return String(s)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

// ── Handle server response ────────────────────────────────────────────────
function handleResponse(resp, thinkingEl) {
  thinkingEl?.remove();

  const type = resp.type;
  const text = resp.text ?? '';
  const data = resp.data ?? null;

  switch (type) {
    case 'chat':
      appendMsg('ai', text);
      break;
    case 'info':
      appendMsg('info', text);
      break;
    case 'error':
      appendMsg('error', text);
      setStatus('error');
      setTimeout(() => setStatus('idle'), 3000);
      return;
    case 'memories':
      appendMsg('info', '📋 Memories' + (data?.length ? ` (${data.length})` : ''), data, 'memories');
      break;
    case 'sessions':
      appendMsg('info', '📂 Sessions' + (data?.length ? ` (${data.length})` : ''), data, 'sessions');
      if (data) {
        const active = data.find(s => s.active);
        if (active) chipSession.textContent = active.name;
      }
      break;
    case 'models':
      appendMsg('info', '🤖 Models' + (data?.length ? ` (${data.length})` : ''), data, 'models');
      break;
    case 'exit':
      appendMsg('system', text);
      break;
    default:
      appendMsg('info', text);
  }
}

// ── Send input ────────────────────────────────────────────────────────────
async function sendInput(text) {
  if (busy) return;
  busy = true;
  sendBtn.disabled = true;
  setStatus('busy');
  hideAutocomplete();

  if (!text.startsWith('/')) {
    appendMsg('user', text);
  } else {
    appendMsg('system', text);
  }

  const thinking = appendMsg('thinking', '…');

  try {
    const resp = await invoke('send_input', { input: text });
    handleResponse(resp, thinking);
  } catch (err) {
    thinking.remove();
    appendMsg('error', `IPC error: ${err}`);
    setStatus('error');
    setTimeout(() => setStatus('idle'), 3000);
  } finally {
    busy = false;
    sendBtn.disabled = false;
    if (statusEl.className !== 'error') setStatus('idle');
    inputEl.focus();
  }
}

// ── Submit ────────────────────────────────────────────────────────────────
function submit() {
  const text = inputEl.value.trim();
  if (!text || busy) return;
  inputEl.value = '';
  autoResize();
  sendInput(text);
}

// ── Textarea auto-resize ──────────────────────────────────────────────────
function autoResize() {
  inputEl.style.height = 'auto';
  inputEl.style.height = Math.min(inputEl.scrollHeight, 120) + 'px';
}

// ── Autocomplete ──────────────────────────────────────────────────────────
function showAutocomplete(matches) {
  acEl.innerHTML = '';
  if (!matches.length) { hideAutocomplete(); return; }

  matches.forEach((cmd, i) => {
    const el = document.createElement('div');
    el.className = 'ac-item';
    el.textContent = cmd;
    el.addEventListener('mousedown', e => {
      e.preventDefault();
      inputEl.value = cmd;
      inputEl.focus();
      hideAutocomplete();
    });
    acEl.appendChild(el);
  });

  acEl.style.display = 'block';
  acSelected = -1;
}

function hideAutocomplete() {
  acEl.style.display = 'none';
  acSelected = -1;
}

function updateAutocomplete(val) {
  if (!val.startsWith('/')) { hideAutocomplete(); return; }
  const matches = COMMANDS.filter(c => c.startsWith(val) && c !== val);
  showAutocomplete(matches);
}

function navigateAc(dir) {
  const items = acEl.querySelectorAll('.ac-item');
  if (!items.length) return false;
  items[acSelected]?.classList.remove('selected');
  acSelected = (acSelected + dir + items.length) % items.length;
  items[acSelected].classList.add('selected');
  return true;
}

// ── Event listeners ───────────────────────────────────────────────────────
inputEl.addEventListener('input', () => {
  autoResize();
  updateAutocomplete(inputEl.value);
});

inputEl.addEventListener('keydown', e => {
  if (e.key === 'Tab' && acEl.style.display !== 'none') {
    e.preventDefault();
    const selected = acEl.querySelector('.ac-item.selected') ?? acEl.querySelector('.ac-item');
    if (selected) { inputEl.value = selected.textContent; hideAutocomplete(); }
    return;
  }
  if (e.key === 'ArrowDown' && acEl.style.display !== 'none') {
    e.preventDefault(); navigateAc(1); return;
  }
  if (e.key === 'ArrowUp' && acEl.style.display !== 'none') {
    e.preventDefault(); navigateAc(-1); return;
  }
  if (e.key === 'Escape') { hideAutocomplete(); return; }
  if (e.key === 'Enter' && !e.shiftKey) {
    e.preventDefault();
    if (acEl.style.display !== 'none' && acSelected >= 0) {
      const sel = acEl.querySelectorAll('.ac-item')[acSelected];
      if (sel) { inputEl.value = sel.textContent; hideAutocomplete(); return; }
    }
    submit();
  }
});

sendBtn.addEventListener('click', submit);

document.addEventListener('click', e => {
  if (!e.target.closest('#input-area')) hideAutocomplete();
});

// ── Init ──────────────────────────────────────────────────────────────────
window.addEventListener('DOMContentLoaded', () => {
  appendMsg('system', 'Haskai 已啟動 — 輸入訊息或 /help 查看命令');
  setStatus('idle');
  inputEl.focus();
});
