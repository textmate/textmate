"""Optional real-Claude compatibility test; all API traffic goes to a local 401 stub.

Usage: python3 test_claude_cli.py /path/to/textmate-mcp /path/to/claude
No login or model inference is performed. Claude configuration is isolated in /tmp.
"""
import http.server
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import threading
import time
import signal

sys.dont_write_bytecode = True
helper = Path(sys.argv[1]).resolve()
claude = str(Path(sys.argv[2]).resolve())
sys.argv = ['test_mcp.py', str(helper)]
spec = importlib.util.spec_from_file_location('tm_mcp_test', Path(__file__).with_name('test_mcp.py'))
tests = importlib.util.module_from_spec(spec)
spec.loader.exec_module(tests)


class RejectInference(http.server.BaseHTTPRequestHandler):
    def reply(self):
        length = int(self.headers.get('Content-Length', 0))
        self.rfile.read(length)
        self.send_response(401)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps({'type': 'error', 'error': {'type': 'authentication_error',
            'message': 'Local integration test stops before inference.'}}).encode())
    do_POST = do_GET = reply
    def log_message(self, *arguments):
        pass


with tempfile.TemporaryDirectory(prefix='tm-claude-real-', dir='/tmp') as directory:
    app = Path(directory) / 'app "quoted"'
    app.mkdir()
    launcher = app / 'tm-claude'
    shutil.copy2(helper.with_name('tm-claude'), launcher)
    probe = app / 'textmate-mcp'
    output = Path(directory) / 'probe.json'
    probe.write_text('#!' + sys.executable + '\nimport subprocess, json, os\nfrom pathlib import Path\n' +
        'result = subprocess.run([' + repr(str(helper)) + ', "--check"], capture_output=True, text=True)\n' +
        'Path(' + repr(str(output)) + ').write_text(json.dumps({"code": result.returncode, "stderr": result.stderr}))\n' +
        'os.execv(' + repr(str(helper)) + ', [' + repr(str(helper)) + '])\n')
    probe.chmod(0o700)
    api = http.server.ThreadingHTTPServer(('127.0.0.1', 0), RejectInference)
    thread = threading.Thread(target=api.serve_forever)
    thread.start()
    try:
        with tests.context_server(lambda message, _: {'windowId': 'real-claude-window', 'documentName': 'real-claude-test'}) as environment:
            environment.update(TM_CLAUDE_COMMAND=claude, CLAUDE_CONFIG_DIR=str(Path(directory) / 'config'),
                CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC='1', DISABLE_TELEMETRY='1', DISABLE_ERROR_REPORTING='1',
                ANTHROPIC_API_KEY='local-integration-test', ANTHROPIC_BASE_URL=f'http://127.0.0.1:{api.server_port}')
            # End the isolated process group as soon as MCP startup is verified;
            # CLI authentication error UIs can otherwise remain open indefinitely.
            debug = Path(directory) / 'claude-debug.log'
            with subprocess.Popen([str(launcher), '-p', 'local integration test', '--strict-mcp-config',
                    '--no-session-persistence', '--debug-file', str(debug)], env=environment, cwd=directory,
                    text=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, start_new_session=True) as process:
                try:
                    deadline = time.monotonic() + 20
                    while time.monotonic() < deadline and not output.exists() and process.poll() is None:
                        time.sleep(0.05)
                    if not output.exists():
                        details = debug.read_text()[-8000:] if debug.exists() else 'No debug output'
                        raise AssertionError('Claude did not start the MCP helper: ' + details)
                    details = json.loads(output.read_text())
                    assert details['code'] == 0, details
                    print('Official Claude CLI: launcher, inline MCP config, environment expansion, and editor connection passed. No model inference performed.')
                finally:
                    try: os.killpg(process.pid, signal.SIGTERM)
                    except ProcessLookupError: pass
                    try: process.wait(timeout=5)
                    except subprocess.TimeoutExpired:
                        os.killpg(process.pid, signal.SIGKILL)
                        process.wait()
    finally:
        api.shutdown()
        api.server_close()
        thread.join()
