"""Optional real-OpenCode MCP startup check. No login or model inference.

Usage: python3 test_opencode_cli.py /path/to/textmate-mcp /path/to/opencode
"""
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
helper = Path(sys.argv[1]).resolve()
opencode = str(Path(sys.argv[2]).resolve())
sys.argv = ['test_mcp.py', str(helper)]
spec = importlib.util.spec_from_file_location('tm_mcp_test', Path(__file__).with_name('test_mcp.py'))
tests = importlib.util.module_from_spec(spec)
spec.loader.exec_module(tests)

with tempfile.TemporaryDirectory(prefix='tm-opencode-real-', dir='/tmp') as directory:
    app = Path(directory) / 'app "quoted"'
    app.mkdir()
    launcher = app / 'tm-opencode'
    shutil.copy2(helper.with_name('tm-opencode'), launcher)
    probe = app / 'textmate-mcp'
    output = Path(directory) / 'probe.json'
    probe.write_text('#!' + sys.executable + '\nimport subprocess, json, os\nfrom pathlib import Path\n' +
        'result = subprocess.run([' + repr(str(helper)) + ', "--check"], capture_output=True, text=True)\n' +
        'Path(' + repr(str(output)) + ').write_text(json.dumps({"code": result.returncode, "stderr": result.stderr}))\n' +
        'os.execv(' + repr(str(helper)) + ', [' + repr(str(helper)) + '])\n')
    probe.chmod(0o700)
    with tests.context_server(lambda message, _: {'windowId': 'real-opencode-window', 'documentName': 'real-opencode-test'}) as environment:
        # Isolate OpenCode's settings/data; never install or modify the user's CLI.
        for key in list(environment):
            if key.startswith('OPENCODE_'): environment.pop(key)
        environment.update(TM_OPENCODE_COMMAND=opencode,
            XDG_CONFIG_HOME=str(Path(directory) / 'config'), XDG_DATA_HOME=str(Path(directory) / 'data'),
            XDG_STATE_HOME=str(Path(directory) / 'state'), XDG_CACHE_HOME=str(Path(directory) / 'cache'),
            OPENCODE_DISABLE_AUTOUPDATE='true', OPENCODE_DISABLE_MODELS_FETCH='true',
            OPENCODE_DISABLE_DEFAULT_PLUGINS='true',
            OPENCODE_CONFIG_CONTENT=json.dumps({'autoupdate': False, 'mcp': {
                'other': {'type': 'local', 'command': ['unused-test-server'], 'enabled': False}}}))
        result = subprocess.run([str(launcher), 'mcp', 'list'], env=environment, cwd=directory,
                                text=True, capture_output=True, timeout=45)
        if not output.exists():
            raise AssertionError('OpenCode did not start the MCP helper: ' + result.stdout + result.stderr)
        details = json.loads(output.read_text())
        assert details['code'] == 0, details
        assert result.returncode == 0, result.stdout + result.stderr
        assert 'textmate' in result.stdout and 'connected' in result.stdout.lower(), result.stdout
        print('Official OpenCode CLI: launcher, inline MCP configuration, environment expansion, and editor connection passed. No model inference performed.')
