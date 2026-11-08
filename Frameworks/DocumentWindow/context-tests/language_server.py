"""Deterministic LSP fixture for the editor MCP language tools; no network."""
import json
import sys
import time

# Pass --position-encoding=utf-8 to answer as a server that advertises UTF-16 but
# encodes positions differently; the default keeps every other fixture test intact.
position_encoding = "utf-16"
for argument in sys.argv[1:]:
    if argument.startswith("--position-encoding="):
        position_encoding = argument.split("=", 1)[1]


def send(message):
    data = json.dumps(message).encode()
    sys.stdout.buffer.write(f"Content-Length: {len(data)}\r\n\r\n".encode() + data)
    sys.stdout.buffer.flush()


def reply(identifier, result):
    send({"jsonrpc": "2.0", "id": identifier, "result": result})


def span(line=0, column=0):
    return {"start": {"line": line, "character": column},
            "end": {"line": line, "character": column + 1}}


text = ""
while True:
    headers = {}
    while True:
        line = sys.stdin.buffer.readline()
        if not line:
            sys.exit(0)
        if line in (b"\r\n", b"\n"):
            break
        key, value = line.decode().split(":", 1)
        headers[key.lower()] = value.strip()
    message = json.loads(sys.stdin.buffer.read(int(headers["content-length"])))
    method = message.get("method")
    params = message.get("params", {})
    identifier = message.get("id")
    if method == "initialize":
        reply(identifier, {"capabilities": {"positionEncoding": position_encoding, "textDocumentSync": 1,
            "definitionProvider": True, "referencesProvider": True, "hoverProvider": True,
            "documentSymbolProvider": True, "workspaceSymbolProvider": True}})
    elif method == "textDocument/didOpen":
        text = params["textDocument"]["text"]
    elif method == "textDocument/didChange":
        text = params["contentChanges"][-1]["text"]
    elif method == "textDocument/definition":
        reply(identifier, [{"targetUri": params["textDocument"]["uri"],
            "targetRange": span(), "targetSelectionRange": span(0, 3)}])
    elif method == "textDocument/references":
        size = 206 if params["context"]["includeDeclaration"] else 205
        reply(identifier, [{"uri": params["textDocument"]["uri"], "range": span(i)} for i in range(size)])
    elif method == "textDocument/hover":
        reply(identifier, {"contents": {"kind": "plaintext", "value": json.dumps({"text": text, "position": params["position"]})}})
    elif method == "textDocument/documentSymbol":
        reply(identifier, [{"name": "Outer", "kind": 5, "range": span(), "selectionRange": span(),
            "children": [{"name": "inner", "kind": 6, "range": span(0, 3), "selectionRange": span(0, 3)}]}])
    elif method == "workspace/symbol":
        if params["query"] == "timeout":
            continue
        if params["query"] == "error":
            send({"jsonrpc": "2.0", "id": identifier, "error": {"code": -32603, "message": "fixture error"}})
        elif params["query"] == "slow":
            time.sleep(0.3)
            reply(identifier, [])
        else:
            reply(identifier, [{"name": params["query"], "kind": 12,
                "location": {"uri": "file:///tmp/workspace-symbol.txt", "range": span(4)}}])
    elif method == "shutdown":
        reply(identifier, None)
    elif method == "exit":
        break
