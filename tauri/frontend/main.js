const { invoke } = window.__TAURI__.core;

const messagesEl = document.getElementById('messages');
const inputEl    = document.getElementById('user-input');
const formEl     = document.getElementById('input-form');
const sendBtn    = document.getElementById('send-btn');
const statusEl   = document.getElementById('status');

function setStatus(state, label) {
  statusEl.className = `status ${state}`;
  statusEl.textContent = { idle: '● 就緒', busy: '◌ 思考中…', error: '✕ 錯誤' }[state] ?? label;
}

function appendMessage(role, text) {
  const div = document.createElement('div');
  div.className = `message ${role}`;
  div.textContent = text;
  messagesEl.appendChild(div);
  messagesEl.scrollTop = messagesEl.scrollHeight;
  return div;
}

async function sendMessage(text) {
  appendMessage('user', text);
  setStatus('busy');
  sendBtn.disabled = true;

  const thinking = appendMessage('thinking', '…');

  try {
    const response = await invoke('chat', { message: text });
    thinking.remove();
    appendMessage('ai', response);
    setStatus('idle');
  } catch (err) {
    thinking.remove();
    appendMessage('system', `錯誤：${err}`);
    setStatus('error');
  } finally {
    sendBtn.disabled = false;
    inputEl.focus();
  }
}

formEl.addEventListener('submit', (e) => {
  e.preventDefault();
  const text = inputEl.value.trim();
  if (!text) return;
  inputEl.value = '';
  sendMessage(text);
});

inputEl.addEventListener('keydown', (e) => {
  if (e.key === 'Enter' && !e.shiftKey) {
    e.preventDefault();
    formEl.requestSubmit();
  }
});

// Initial greeting
window.addEventListener('DOMContentLoaded', () => {
  appendMessage('system', 'Haskai 已啟動 — 使用本地 Ollama 模型');
});
