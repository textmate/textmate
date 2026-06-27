// Error handler (matches WKWebView.js pattern from About window)
window.addEventListener("error", function(event) {
	webkit.messageHandlers.textmate.postMessage({
		command: "log",
		payload: {
			level: "error",
			message: event.message,
			filename: event.filename,
			lineno: event.lineno,
			colno: event.colno
		}
	});
});

// Shell command tracking for async commands
let _shellCommands = new Map();
let _nextShellId = 1;

class TMShellCommand {
	constructor(id) {
		this.id = id;
		this.outputString = "";
		this.errorString = "";
		this.status = 0;
		this.onreadoutput = null;
		this.onreaderror = null;
		this._exitHandler = null;
	}

	cancel() {
		webkit.messageHandlers.textmate.postMessage({
			command: "shellCancel",
			payload: { id: this.id }
		});
	}

	write(str) {
		webkit.messageHandlers.textmate.postMessage({
			command: "shellWrite",
			payload: { id: this.id, data: str }
		});
	}

	close() {
		webkit.messageHandlers.textmate.postMessage({
			command: "shellClose",
			payload: { id: this.id }
		});
	}

	// Called from native via evaluateJavaScript
	_onOutput(str) {
		this.outputString = str;
		if(this.onreadoutput)
			this.onreadoutput(str);
	}

	_onError(str) {
		this.errorString = str;
		if(this.onreaderror)
			this.onreaderror(str);
	}

	_onExit(status) {
		this.status = status;
		if(this._exitHandler)
			this._exitHandler(this);
		_shellCommands.delete(this.id);
	}
}

let TextMate = {
	// Properties (bidirectional with native)
	_busy: false,
	_progress: 0,

	get busy() { return this._busy; },
	set busy(val) {
		this._busy = val;
		webkit.messageHandlers.textmate.postMessage({
			command: "setBusy",
			payload: { value: val }
		});
	},

	get progress() { return this._progress; },
	set progress(val) {
		this._progress = val;
		webkit.messageHandlers.textmate.postMessage({
			command: "setProgress",
			payload: { value: val }
		});
	},

	get isBusy() { return this._busy; },
	set isBusy(val) { this.busy = val; },

	// TextMate.system(command, handler)
	// If handler is null/undefined, executes synchronously via XMLHttpRequest
	// to the tm-system:// scheme handler, preserving backward compatibility
	// with the Git bundle and other callers that depend on synchronous execution.
	system(command, handler) {
		if(handler !== undefined && handler !== null) {
			// Async path: use message handler
			let id = _nextShellId++;
			let cmd = new TMShellCommand(id);
			cmd._exitHandler = handler;
			_shellCommands.set(id, cmd);

			webkit.messageHandlers.textmate.postMessage({
				command: "system",
				payload: {
					id: id,
					command: command,
					async: true
				}
			});

			return cmd;
		}
		else {
			// Synchronous path: use XMLHttpRequest to tm-system:// scheme handler
			// This blocks the JS thread until the command completes, matching legacy behavior.
			let xhr = new XMLHttpRequest();
			xhr.open("POST", "tm-system://localhost/run", false); // synchronous
			xhr.send(command);

			let result = { outputString: "", errorString: "", status: -1 };
			try {
				let parsed = JSON.parse(xhr.responseText);
				result.outputString = parsed.outputString || "";
				result.errorString = parsed.errorString || "";
				result.status = parsed.status !== undefined ? parsed.status : -1;
			} catch(e) {
				result.errorString = "Failed to parse system() response: " + e.message;
			}

			// Provide the same interface as async commands for consistency
			result.cancel = function() {};
			result.write = function() {};
			result.close = function() {};
			result.onreadoutput = null;
			result.onreaderror = null;

			return result;
		}
	},

	log(msg) {
		webkit.messageHandlers.textmate.postMessage({
			command: "log",
			payload: { message: "" + msg }
		});
	},

	open(path, options) {
		webkit.messageHandlers.textmate.postMessage({
			command: "open",
			payload: {
				path: path,
				options: (options !== undefined && options !== null) ? "" + options : null
			}
		});
	}
};

// Called from native to deliver shell command events (async commands only)
function _tmShellOutput(id, str) {
	let cmd = _shellCommands.get(id);
	if(cmd) cmd._onOutput(str);
}
function _tmShellError(id, str) {
	let cmd = _shellCommands.get(id);
	if(cmd) cmd._onError(str);
}
function _tmShellExit(id, status) {
	let cmd = _shellCommands.get(id);
	if(cmd) cmd._onExit(status);
}

// =====================
// = Scroll Sync       =
// =====================

let _tmScrollSyncEnabled = false;
let _tmScrollSyncIgnoreNext = false;

function _tmScrollTo(ratio) {
	_tmScrollSyncIgnoreNext = true;
	let maxScroll = document.documentElement.scrollHeight - window.innerHeight;
	window.scrollTo(0, maxScroll * ratio);
}

function _tmSetScrollSyncEnabled(enabled) {
	_tmScrollSyncEnabled = enabled;
}

// ========================
// = Active Line Highlight =
// ========================

document.addEventListener("DOMContentLoaded", function() {
	let style = document.createElement("style");
	style.textContent = "._tm-active-line { background-color: rgba(255, 200, 0, 0.30) !important; border-radius: 3px; transition: background-color 0.15s ease; outline: 2px solid rgba(255, 180, 0, 0.25); outline-offset: 2px; }";
	document.head.appendChild(style);
});

let _tmHighlightEnabled = false;
let _tmHighlightedEl = null;
const _tmBlockSelectors = "p, h1, h2, h3, h4, h5, h6, li, blockquote, pre, td, th, dt, dd, figcaption";

function _tmHighlightLine(lineNumber, lineCount) {
	if(!_tmHighlightEnabled) return;
	// Only select top-level block elements so nested elements (e.g. <li> inside <ul>)
	// don't fragment the mapping. Walk backward from the target index just like the
	// Ruby preview command does when placing scroll_to_here.
	// Target the content div used by the TM html output template; fall back to body
	let container = document.getElementById("tm_webpreview_content") || document.body;
	let blocks = Array.from(container.querySelectorAll("p, h1, h2, h3, h4, h5, h6, ul, ol, blockquote, pre, table, hr, figure"));
	if(blocks.length === 0) return;

	// Map source line → block element index.
	// lineNumber is 0-based; use floor so the highlight leads rather than lags.
	let ratio = lineCount > 1 ? lineNumber / lineCount : 0;
	let idx = Math.floor(ratio * blocks.length);
	idx = Math.max(0, Math.min(idx, blocks.length - 1));
	let best = blocks[idx];

	if(best !== _tmHighlightedEl) {
		if(_tmHighlightedEl) _tmHighlightedEl.classList.remove("_tm-active-line");
		best.classList.add("_tm-active-line");
		_tmHighlightedEl = best;
	}
}

function _tmSetHighlightEnabled(enabled) {
	_tmHighlightEnabled = enabled;
	if(!enabled && _tmHighlightedEl) {
		_tmHighlightedEl.classList.remove("_tm-active-line");
		_tmHighlightedEl = null;
	}
}

window.addEventListener("scroll", function() {
	if(!_tmScrollSyncEnabled) return;
	if(_tmScrollSyncIgnoreNext) {
		_tmScrollSyncIgnoreNext = false;
		return;
	}
	let maxScroll = document.documentElement.scrollHeight - window.innerHeight;
	let ratio = maxScroll > 0 ? window.scrollY / maxScroll : 0;
	webkit.messageHandlers.textmate.postMessage({
		command: "scrollSync",
		payload: { ratio: ratio }
	});
}, { passive: true });
