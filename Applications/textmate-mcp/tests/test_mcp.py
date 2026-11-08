"""Exercise the shipped stdio executable and its private socket boundary."""
import json
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import threading
import unittest
import contextlib
import time
import shutil
try:
    import tomllib            # Python 3.11+; Xcode's python3 is 3.9
except ModuleNotFoundError:
    tomllib = None
import queue

EXECUTABLE = str(Path(sys.argv.pop(1)).resolve())
VALIDATOR = str(Path(sys.argv.pop(1)).resolve()) if len(sys.argv) > 1 else None


# Methods the helper uses for its own transport (--check and change-token
# polling). validate_arguments accepts them; they are not MCP tools.
INTERNAL_METHODS = {"get_connection_status", "get_editor_change_token"}


def request(identifier, method, **params):
    return {"jsonrpc": "2.0", "id": identifier, "method": method, "params": params}


READY = {"jsonrpc": "2.0", "method": "notifications/initialized"}

INITIALIZE = request(1, "initialize", protocolVersion="2025-11-25", capabilities={},
                     clientInfo={"name": "test", "version": "1"})


@contextlib.contextmanager
def context_server(handler):
    with tempfile.TemporaryDirectory(prefix="tm-context-", dir="/tmp") as directory:
        server = socket.socket(socket.AF_UNIX)
        server.bind(directory + "/s")
        server.listen(8)
        server.settimeout(0.1)
        stopping = threading.Event()
        errors = []
        def serve():
            while not stopping.is_set():
                try:
                    connection, _ = server.accept()
                except socket.timeout:
                    continue
                except OSError:
                    break
                try:
                    with connection:
                        connection.settimeout(5)
                        message = json.loads(connection.makefile("rb").readline())
                        value = handler(message, connection)
                        if value is not None:
                            connection.sendall(json.dumps(value).encode() + b"\n")
                except (BrokenPipeError, ConnectionResetError):
                    pass
                except Exception as error:
                    errors.append(error)
        thread = threading.Thread(target=serve)
        thread.start()
        try:
            yield dict(os.environ, TM_CONTEXT_SOCKET=directory + "/s", TM_CONTEXT_SESSION="test-session", TM_CONTEXT_TOKEN="secret-test-token")
        finally:
            stopping.set()
            server.close()
            thread.join(timeout=6)
            if errors: raise errors[0]
            if thread.is_alive(): raise RuntimeError("test context server did not stop")


class MCPTests(unittest.TestCase):
    def run_server(self, messages, environment=None):
        process = subprocess.run([EXECUTABLE], input="".join(json.dumps(m) + "\n" for m in messages),
                                 text=True, capture_output=True, env=environment, timeout=10, check=True)
        return [json.loads(line) for line in process.stdout.splitlines()]

    def test_language_tools_forward_and_validate(self):
        names = ["get_definition", "find_references", "get_document_symbols", "search_workspace_symbols", "get_hover"]
        received = []
        def serve(message, _):
            received.append(message)
            return {"items": [], "resultId": "result", "truncated": False}
        calls = [{"documentId": "doc", "line": 2, "column": 4, "revision": "r"},
                 {"line": 3, "includeDeclaration": True}, {}, {"query": "Example"}, {}]
        with context_server(serve) as environment:
            replies = self.run_server([INITIALIZE, READY] + [
                request(i+2, "tools/call", name=name, arguments=args)
                for i, (name, args) in enumerate(zip(names, calls))], environment)
        self.assertEqual({m["method"]: m["arguments"] for m in received}, dict(zip(names, calls)))
        self.assertTrue(all(not r["result"]["isError"] for r in replies[1:]))
        invalid = [
            ("get_definition", {"line": 0}), ("get_definition", {"line": True}),
            ("find_references", {"column": 2}), ("find_references", {"includeDeclaration": 1}),
            ("get_document_symbols", {"line": 1}), ("search_workspace_symbols", {}),
            ("search_workspace_symbols", {"query": ""}), ("search_workspace_symbols", {"query": "x"*257}),
            ("get_hover", {"limit": 101}), ("get_hover", {"offset": 1}),
            ("get_hover", {"resultId": ""}), ("get_definition", {"path": "/tmp/file"})]
        replies = self.run_server([INITIALIZE, READY] + [request(i+2, "tools/call", name=name, arguments=args)
            for i, (name, args) in enumerate(invalid)])
        self.assertTrue(all(r["error"]["code"] == -32602 for r in replies[1:]))

    def test_document_edits_forwarding_and_validation(self):
        received = []
        expected = {"documentId": "buffer", "revision": "r1", "summary": "Update code",
                    "edits": [{"offset": 2, "length": 0, "text": "😀"}, {"offset": 10, "length": 4, "text": ""}]}
        def serve(message, _):
            received.append(message)
            return {"proposalId": "proposal", "status": "pending_review", "applied": False}
        with context_server(serve) as environment:
            replies = self.run_server([INITIALIZE, READY,
                request(2, "tools/call", name="propose_document_edits", arguments=expected),
                request(3, "tools/call", name="get_edit_proposal_status", arguments={"proposalId": "proposal"})], environment)
        calls = {message["method"]: message["arguments"] for message in received}
        self.assertEqual(calls["propose_document_edits"], expected)
        self.assertEqual(calls["get_edit_proposal_status"], {"proposalId": "proposal"})
        proposal = next(reply for reply in replies if reply["id"] == 2)
        self.assertFalse(json.loads(proposal["result"]["content"][0]["text"])["applied"])
        bad_edits = [[], {}, [{"offset": True, "length": 0, "text": "x"}],
                     [{"offset": -1, "length": 0, "text": "x"}], [{"offset": 0, "length": 1.5, "text": "x"}],
                     [{"offset": 0, "length": 131073, "text": ""}], [{"offset": 0, "length": 0, "text": "x"*32769}],
                     [{"offset": 0, "length": 0, "text": 1}], [{"offset": 0, "length": 0, "text": "x", "path": "/tmp/file"}],
                     [{"offset": 0, "length": 0, "text": "x"}]*129]
        invalid = [dict(expected, edits=edits) for edits in bad_edits] + [dict(expected, revision=""), dict(expected, documentId=""), dict(expected, summary=""), dict(expected, path="/tmp/file")]
        replies = self.run_server([INITIALIZE, READY] + [request(i+2, "tools/call", name="propose_document_edits", arguments=args)
            for i, args in enumerate(invalid)])
        self.assertTrue(all(reply["error"]["code"] == -32602 for reply in replies[1:]))

    def advertised_tools(self):
        replies = self.run_server([INITIALIZE, READY, request(2, "tools/list")])
        return next(reply for reply in replies if reply["id"] == 2)["result"]["tools"]

    def test_allow_list_admits_every_advertised_tool(self):
        names = sorted(tool["name"] for tool in self.advertised_tools())
        environment = {key: value for key, value in os.environ.items() if not key.startswith("TM_CONTEXT_")}
        replies = self.run_server([INITIALIZE, READY] + [
            request(2 + index, "tools/call", name=name) for index, name in enumerate(names)], environment)
        by_id = {reply["id"]: reply for reply in replies}
        for index, name in enumerate(names):
            # Both the tools/call allow-list and validate_arguments reject an
            # unhandled name with this message; any other outcome, including an
            # ordinary argument error, proves the name reached both copies.
            self.assertNotEqual(by_id[2 + index].get("error", {}).get("message"), "Unknown tool", name)

    @unittest.skipUnless(VALIDATOR, "the textmate-mcp-tool-surface helper was not supplied")
    def test_tool_surface_matches_registry(self):
        advertised = [tool["name"] for tool in self.advertised_tools()]
        registry = subprocess.run([VALIDATOR, "--list"], text=True, capture_output=True, timeout=10, check=True).stdout.split()
        self.assertTrue(registry, "the helper reported an empty tool registry")
        self.assertEqual(advertised, registry,
                         "tools/list disagrees with the compiled tool registry in EditorContextProtocol.h")

        candidates = sorted(set(registry) | INTERNAL_METHODS | {"not_a_tool"})
        process = subprocess.run([VALIDATOR, *candidates], text=True, capture_output=True, timeout=10, check=True)
        verdicts = dict(line.split("\t") for line in process.stdout.splitlines())
        self.assertEqual(set(verdicts), set(candidates), "the helper did not report on every candidate")
        accepted = {name for name, verdict in verdicts.items() if verdict == "accepted"}
        self.assertEqual(accepted, set(registry) | INTERNAL_METHODS,
                         "validate_arguments' compiled branches disagree with the tool registry")

    def test_identifier_limits_match_the_advertised_schemas(self):
        tools = {tool["name"]: tool["inputSchema"]["properties"] for tool in self.advertised_tools()}
        long_id, long_path = "x" * 257, "/" + "x" * 4096
        invalid = [("read_document", {"documentId": long_id}), ("get_diagnostics", {"documentId": long_id}),
                   ("get_diagnostics", {"reportId": long_id}), ("navigate_editor", {"documentId": long_id}),
                   ("navigate_editor", {"documentId": "d", "revision": long_id}), ("navigate_editor", {"path": long_path}),
                   ("get_editor_changes", {"cursor": long_id})]
        replies = self.run_server([INITIALIZE, READY] + [request(i+2, "tools/call", name=name, arguments=args)
            for i, (name, args) in enumerate(invalid)])
        self.assertTrue(all(reply["error"]["code"] == -32602 for reply in replies[1:]))
        for name, key, limit in [("read_document", "documentId", 256), ("get_diagnostics", "reportId", 256),
                                 ("navigate_editor", "path", 4096), ("navigate_editor", "revision", 256)]:
            self.assertEqual(tools[name][key]["maxLength"], limit, f"{name}.{key}")
            self.assertEqual(tools[name][key]["minLength"], 1, f"{name}.{key}")

    def test_stalled_host_does_not_tear_a_stdout_frame(self):
        # The pipe holds far less than the payload, so the write blocks for longer than
        # the three-second socket timeout; the frame must still arrive whole.
        with context_server(lambda message, _: {"text": "x" * 500000}) as environment:
            process = subprocess.Popen([EXECUTABLE], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, env=environment)
            try:
                for message in (INITIALIZE, READY, request(2, "tools/call", name="get_editor_context", arguments={})):
                    process.stdin.write(json.dumps(message) + "\n")
                process.stdin.flush()
                time.sleep(4)
                self.assertEqual(json.loads(process.stdout.readline())["id"], 1)
                reply = json.loads(process.stdout.readline())
                self.assertEqual(reply["id"], 2)
                self.assertEqual(len(json.loads(reply["result"]["content"][0]["text"])["text"]), 500000)
            finally:
                process.stdin.close()
                process.wait(timeout=10)
                process.stdout.close()

    def test_initialization_tools_notifications_and_pipelining(self):
        replies = self.run_server([
            INITIALIZE,
            {"jsonrpc": "2.0", "method": "notifications/initialized"},
            request(2, "tools/list"), request(3, "ping"), request(4, "unknown"),
            request(5, "tools/call", name="not_a_tool"),
            request(6, "tools/call", name="get_editor_context", arguments=[]),
        ])
        self.assertEqual([r["id"] for r in replies], [1, 2, 3, 4, 5, 6])
        self.assertEqual(replies[0]["result"]["protocolVersion"], "2025-11-25")
        tools = replies[1]["result"]["tools"]
        self.assertEqual([t["name"] for t in tools],
                         ["get_editor_context", "list_open_documents", "read_document", "get_diagnostics", "get_definition", "find_references", "get_document_symbols", "search_workspace_symbols", "get_hover", "navigate_editor", "propose_document_edits", "get_edit_proposal_status", "get_editor_changes"])
        self.assertTrue(all(t["annotations"]["readOnlyHint"] for t in tools if t["name"] not in ("navigate_editor", "propose_document_edits")))
        self.assertFalse(next(t for t in tools if t["name"] == "navigate_editor")["annotations"]["readOnlyHint"])
        self.assertEqual(replies[2]["result"], {})
        self.assertEqual(replies[3]["error"]["code"], -32601)
        self.assertEqual(replies[4]["error"]["code"], -32602)
        self.assertEqual(replies[5]["error"]["code"], -32602)

    def test_initialization_required_and_version_fallback(self):
        replies = self.run_server([request(0, "tools/list"),
                                  request(1, "initialize", protocolVersion="future", capabilities={}, clientInfo={"name": "test", "version": "1"})])
        self.assertEqual(replies[0]["error"]["code"], -32000)
        self.assertEqual(replies[1]["result"]["protocolVersion"], "2025-11-25")

    def test_diagnostic_forwarding_and_argument_validation(self):
        received = []
        expected = {"documentId": "open-document", "offset": 2, "limit": 3, "reportId": "report-1"}
        def report(message, _):
            received.append(message)
            return {"status": "reported", "diagnostics": [{"severity": "warning", "message": "Example 😀"}], "reportId": "report-1"}
        with context_server(report) as environment:
            replies = self.run_server([INITIALIZE, READY, request(2, "tools/call", name="get_diagnostics", arguments=expected),
                                       request(3, "tools/call", name="get_diagnostics")], environment)
        self.assertEqual({m["method"] for m in received}, {"get_diagnostics"})
        self.assertCountEqual([m["arguments"] for m in received], [expected, {}])
        by_id = {reply["id"]: reply for reply in replies}
        value = json.loads(by_id[2]["result"]["content"][0]["text"])
        self.assertEqual(value["diagnostics"][0]["message"], "Example 😀")
        self.assertFalse(by_id[2]["result"]["isError"])
        invalid = [{"documentId": ""}, {"documentId": 1}, {"reportId": ""}, {"reportId": False},
                   {"offset": True}, {"offset": -1}, {"offset": 0.5}, {"limit": 0}, {"limit": 101},
                   {"limit": True}, {"severity": "error"}]
        replies = self.run_server([INITIALIZE, READY] + [
            request(index + 2, "tools/call", name="get_diagnostics", arguments=arguments)
            for index, arguments in enumerate(invalid)])
        self.assertEqual(len(replies), 1 + len(invalid))
        self.assertTrue(all(reply["error"]["code"] == -32602 for reply in replies[1:]))

    def test_navigation_forwarding_and_argument_validation(self):
        expected = {"path": "/tmp/a file 😀.txt", "line": 2, "column": 2, "endLine": 3, "endColumn": 1, "focus": False, "revision": "opaque"}
        received = []
        def navigate(message, _):
            received.append(message)
            return {"status": "navigated", "opened": True, "focused": False}
        with context_server(navigate) as environment:
            replies = self.run_server([INITIALIZE, READY, request(2, "tools/call", name="navigate_editor", arguments=expected)], environment)
        self.assertEqual(received[0]["method"], "navigate_editor")
        self.assertEqual(received[0]["arguments"], expected)
        self.assertEqual(json.loads(replies[1]["result"]["content"][0]["text"])["status"], "navigated")
        invalid = [{}, {"documentId": "x", "path": "/tmp/x"}, {"path": "relative.txt"}, {"path": "file:///tmp/x"},
                   {"path": "/tmp/x\0hidden"}, {"documentId": ""}, {"documentId": "x", "focus": "true"},
                   {"documentId": "x", "focus": 1}, {"documentId": "x", "line": True},
                   {"documentId": "x", "line": 0}, {"documentId": "x", "line": 1.5},
                   {"documentId": "x", "column": 2}, {"documentId": "x", "line": 2, "endLine": 2},
                   {"documentId": "x", "line": 2, "endLine": 1, "endColumn": 1},
                   {"documentId": "x", "line": 2, "column": 4, "endLine": 2, "endColumn": 2},
                   {"documentId": "x", "revision": ""}, {"documentId": "x", "unexpected": True}]
        replies = self.run_server([INITIALIZE, READY] + [
            request(index+2, "tools/call", name="navigate_editor", arguments=arguments)
            for index, arguments in enumerate(invalid)])
        self.assertEqual(len(replies), 1+len(invalid))
        self.assertTrue(all(reply["error"]["code"] == -32602 for reply in replies[1:]))

    def test_resource_discovery_reads_and_change_tool(self):
        received = []
        def respond(message, _):
            received.append(message)
            return {"changeCursor": "window/1", "text": "unsaved 😀"}
        uri = "textmate://editor/context"
        with context_server(respond) as environment:
            replies = self.run_server([INITIALIZE, READY, request(2, "resources/list"), request(3, "resources/templates/list"),
                request(4, "resources/read", uri=uri), request(5, "tools/call", name="get_editor_changes", arguments={"cursor": "window/0"}),
                request(6, "resources/read", uri="file:///arbitrary"), request(7, "resources/subscribe", uri="bad"),
                request(8, "tools/call", name="get_editor_changes", arguments={"cursor": ""}),
                request(9, "tools/call", name="get_editor_changes", arguments={"cursor": 1}),
                request(10, "tools/call", name="get_editor_changes", arguments={"cursor": "x"*257}),
                request(11, "tools/call", name="get_editor_changes", arguments={"extra": True})], environment)
        by_id = {r["id"]: r for r in replies}
        self.assertTrue(by_id[1]["result"]["capabilities"]["resources"]["subscribe"])
        self.assertEqual(by_id[2]["result"]["resources"][0]["uri"], uri)
        self.assertEqual(by_id[3]["result"]["resourceTemplates"], [])
        self.assertEqual(json.loads(by_id[4]["result"]["contents"][0]["text"])["text"], "unsaved 😀")
        self.assertCountEqual([(m["method"], m["arguments"]) for m in received], [("get_editor_context", {}), ("get_editor_changes", {"cursor": "window/0"})])
        for identifier in [6, 7]: self.assertEqual(by_id[identifier]["error"]["code"], -32002)
        for identifier in [8, 9, 10, 11]: self.assertEqual(by_id[identifier]["error"]["code"], -32602)

    def test_resource_subscription_updates_unsubscribe_and_idle(self):
        token = ["window/0"]
        queried = []
        def respond(message, _):
            queried.append(message["method"])
            return {"cursor": token[0], "windowId": "w"}
        uri = "textmate://editor/context"
        with context_server(respond) as environment:
            process = subprocess.Popen([EXECUTABLE], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, env=environment)
            messages = queue.Queue()
            def read_output():
                for line in process.stdout: messages.put(json.loads(line))
            reader = threading.Thread(target=read_output)
            reader.start()
            def send(message):
                process.stdin.write(json.dumps(message)+"\n")
                process.stdin.flush()
            def receive(): return messages.get(timeout=5)
            try:
                send(INITIALIZE); self.assertEqual(receive()["id"], 1); send(READY)
                time.sleep(0.6)
                self.assertEqual(queried, [])  # No subscriber, no editor polling.
                send(request(2, "resources/subscribe", uri=uri))
                self.assertEqual(receive()["id"], 2)
                self.assertEqual(receive()["method"], "notifications/resources/updated")
                with self.assertRaises(queue.Empty): messages.get(timeout=0.7)
                token[0] = "window/1"
                self.assertEqual(receive()["params"], {"uri": uri})
                token[0] = "destination/0"  # Session transfer invalidates context.
                self.assertEqual(receive()["method"], "notifications/resources/updated")
                send(request(3, "resources/unsubscribe", uri=uri))
                # Ignore any update already delivered before the acknowledgement.
                while receive().get("id") != 3: pass
                calls = len(queried)
                token[0] = "destination/1"
                with self.assertRaises(queue.Empty): messages.get(timeout=0.7)
                self.assertLessEqual(len(queried), calls+1)  # At most an in-flight token query.
                send(request(4, "ping")); self.assertEqual(receive()["id"], 4)
                self.assertTrue(all(method == "get_editor_change_token" for method in queried))
            finally:
                process.stdin.close()
                process.wait(timeout=5)
                reader.join(timeout=5)
                process.stdout.close(); process.stderr.close()
            self.assertEqual(process.returncode, 0)

    def test_malformed_and_oversized_input_recovers(self):
        data = '{broken\n' + 'x' * (1024 * 1024 + 1) + '\n' + json.dumps(INITIALIZE) + '\n'
        process = subprocess.run([EXECUTABLE], input=data, text=True, capture_output=True, timeout=10, check=True)
        replies = [json.loads(line) for line in process.stdout.splitlines()]
        self.assertEqual([r["error"]["code"] for r in replies[:2]], [-32700, -32700])
        self.assertEqual(replies[2]["id"], 1)

    def test_missing_connection_is_tool_error(self):
        environment = {k: v for k, v in os.environ.items() if not k.startswith("TM_CONTEXT_")}
        replies = self.run_server([INITIALIZE, READY, request(2, "tools/call", name="get_editor_context")], environment)
        self.assertTrue(replies[1]["result"]["isError"])

    def test_socket_forwarding_unicode_and_explicit_configuration(self):
        with tempfile.TemporaryDirectory(prefix="tm-mcp-", dir="/tmp") as directory:
            path = directory + "/context.sock"
            server = socket.socket(socket.AF_UNIX)
            server.bind(path)
            server.listen(1)
            server.settimeout(5)
            received = []
            failures = []

            def serve():
                try:
                    connection, _ = server.accept()
                    with connection:
                        received.append(json.loads(connection.makefile("rb").readline()))
                        body = json.dumps({"text": "unsaved 😀\n" + "a" * 20000}).encode() + b"\n"
                        # Deliberately fragment the response across reads.
                        connection.sendall(body[:7])
                        connection.sendall(body[7:])
                except Exception as error:
                    failures.append(error)

            thread = threading.Thread(target=serve)
            thread.start()
            environment = dict(os.environ, TM_CONTEXT_SOCKET=path, TM_CONTEXT_SESSION="session", TM_CONTEXT_TOKEN="token")
            replies = self.run_server([INITIALIZE, READY, request(2, "tools/call", name="read_document",
                                                          arguments={"documentId": "doc", "offset": 3})], environment)
            thread.join(timeout=6)
            server.close()
            self.assertFalse(failures)
            self.assertFalse(thread.is_alive())
            self.assertEqual(received, [{"session": "session", "token": "token", "method": "read_document",
                                         "arguments": {"documentId": "doc", "offset": 3}}])
            self.assertFalse(replies[1]["result"]["isError"])
            self.assertTrue(json.loads(replies[1]["result"]["content"][0]["text"])["text"].startswith("unsaved 😀"))
            config = json.loads(subprocess.check_output([EXECUTABLE, "--print-config"], env=environment))
            entry = config["mcpServers"]["textmate"]
            self.assertEqual(entry["command"], EXECUTABLE)
            self.assertEqual(entry["env"]["TM_CONTEXT_SESSION"], "session")


    def test_lifecycle_and_request_validation(self):
        replies = self.run_server([
            request(0, "initialize"), INITIALIZE,
            request(2, "tools/list"), READY,
            request(3, "initialize", **INITIALIZE["params"]),
            request(4, "tools/call", name="get_editor_context", arguments={"extra": True}),
            request(5, "tools/call", name="read_document", arguments={"documentId": "x", "offset": True}),
            request(6, "tools/call", name="read_document", arguments={"documentId": "x", "length": 32769}),
            request(7, "tools/list", cursor="invalid"),
            request(True, "ping"), request(None, "ping"), request(1.5, "ping"),
        ])
        by_id = {json.dumps(reply["id"]): reply for reply in replies}
        self.assertEqual(by_id["0"]["error"]["code"], -32602)
        self.assertEqual(by_id["2"]["error"]["code"], -32000)
        self.assertEqual(by_id["3"]["error"]["code"], -32600)
        for identifier in range(4, 8):
            self.assertEqual(by_id[str(identifier)]["error"]["code"], -32602)
        invalid_ids = [reply for reply in replies if reply["id"] is None]
        self.assertEqual(len(invalid_ids), 3)
        self.assertTrue(all(reply["error"]["code"] == -32600 for reply in invalid_ids))

    def test_connection_diagnostics(self):
        missing = {k: v for k, v in os.environ.items() if not k.startswith("TM_CONTEXT_")}
        process = subprocess.run([EXECUTABLE, "--check"], env=missing, text=True, capture_output=True)
        self.assertNotEqual(process.returncode, 0)
        self.assertIn("missing_environment", process.stderr)
        missing.update(TM_CONTEXT_SOCKET="/tmp/no-textmate-context-here/s", TM_CONTEXT_SESSION="closed", TM_CONTEXT_TOKEN="secret-test-token")
        process = subprocess.run([EXECUTABLE, "--check"], env=missing, text=True, capture_output=True)
        self.assertIn("service_unavailable", process.stderr)
        with context_server(lambda message, _: {"error": "The terminal has closed", "code": "session_unavailable"}) as environment:
            process = subprocess.run([EXECUTABLE, "--check"], env=environment, text=True, capture_output=True)
            self.assertIn("session_unavailable", process.stderr)
            self.assertNotIn(environment["TM_CONTEXT_TOKEN"], process.stderr)
        with context_server(lambda message, _: {"windowId": "correct-window", "documentName": "example.txt"}) as environment:
            process = subprocess.run([EXECUTABLE, "--check"], env=environment, text=True, capture_output=True, check=True)
            self.assertIn("correct-window", process.stdout)
            self.assertIn("example.txt", process.stdout)

    def test_overall_deadline_with_a_drip_response(self):
        def drip(message, connection):
            for _ in range(25):
                connection.sendall(b" ")
                time.sleep(0.2)
        with context_server(drip) as environment:
            start = time.monotonic()
            process = subprocess.run([EXECUTABLE, "--check"], env=environment, text=True, capture_output=True, timeout=5)
            self.assertIn("timeout", process.stderr)
            self.assertLess(time.monotonic() - start, 4)

    def test_cancellation_does_not_block_ping(self):
        started = threading.Event()
        def delayed(message, _):
            started.set()
            time.sleep(0.6)
            return {"text": "cancelled result"}
        with context_server(delayed) as environment:
            with subprocess.Popen([EXECUTABLE], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                  text=True, env=environment) as process:
                def send(message):
                    process.stdin.write(json.dumps(message) + "\n")
                    process.stdin.flush()
                send(INITIALIZE)
                self.assertEqual(json.loads(process.stdout.readline())["id"], 1)
                send(READY)
                send(request(2, "tools/call", name="get_editor_context"))
                self.assertTrue(started.wait(2))
                send({"jsonrpc": "2.0", "method": "notifications/cancelled", "params": {"requestId": 2}})
                send(request(3, "ping"))
                self.assertEqual(json.loads(process.stdout.readline())["id"], 3)
                process.stdin.close()
                self.assertEqual(process.stdout.read(), "")
                self.assertEqual(process.wait(timeout=5), 0)

    @unittest.skipUnless(tomllib, "parsing the generated Codex TOML config requires tomllib (Python 3.11+)")
    def test_launcher_preserves_arguments_directory_and_session(self):
        launcher = str(Path(EXECUTABLE).with_name("tm-codex"))
        with tempfile.TemporaryDirectory(prefix="tm-launch-", dir="/tmp") as directory:
            app = Path(directory) / 'app "quoted" 😀'
            app.mkdir()
            shutil.copy2(EXECUTABLE, app / "textmate-mcp")
            shutil.copy2(launcher, app / "tm-codex")
            stub = Path(directory) / "codex-stub"
            stub.write_text("#!" + sys.executable + "\nimport json, os, sys\nprint(json.dumps({'args': sys.argv[1:], 'cwd': os.getcwd(), 'session': os.environ['TM_CONTEXT_SESSION']}))\nsys.exit(23)\n")
            stub.chmod(0o700)
            with context_server(lambda message, _: {"windowId": "w", "documentName": "test"}) as environment:
                environment["TM_CODEX_COMMAND"] = str(stub)
                process = subprocess.run([str(app / "tm-codex"), "-m", "example-model", "--", "prompt with spaces"],
                                         cwd=directory, env=environment, text=True, capture_output=True, timeout=5)
                self.assertEqual(process.returncode, 23, process.stderr)
                actual = json.loads(process.stdout)
                self.assertEqual(actual["cwd"], os.path.realpath(directory))
                self.assertEqual(actual["session"], "test-session")
                self.assertEqual(actual["args"][3:], ["-m", "example-model", "--", "prompt with spaces"])
                self.assertEqual(actual["args"][-2:], ["--", "prompt with spaces"])
                self.assertIn("--no-daemon", actual["args"])
                config = tomllib.loads(actual["args"][actual["args"].index("-c")+1])["mcp_servers"]["textmate"]
                self.assertEqual(config["command"], str((app / "textmate-mcp").resolve()))
                self.assertEqual(config["env"], {})
                self.assertEqual(config["env_vars"], ["TM_CONTEXT_SOCKET", "TM_CONTEXT_SESSION", "TM_CONTEXT_TOKEN"])
                self.assertNotIn(environment["TM_CONTEXT_TOKEN"], " ".join(actual["args"]))


    @unittest.skipUnless(shutil.which("codex"), "Codex CLI is not installed")
    def test_launcher_configuration_with_installed_codex(self):
        launcher = str(Path(EXECUTABLE).with_name("tm-codex"))
        with context_server(lambda message, _: {"windowId": "w", "documentName": "test"}) as environment:
            environment["TM_CODEX_COMMAND"] = shutil.which("codex")
            process = subprocess.run([launcher, "mcp", "get", "textmate", "--json"], env=environment,
                                     text=True, capture_output=True, timeout=10)
            self.assertEqual(process.returncode, 0, process.stderr)
            config = json.loads(process.stdout)
            self.assertEqual(config["name"], "textmate")
            self.assertEqual(config["transport"]["command"], EXECUTABLE)
            self.assertNotIn(environment["TM_CONTEXT_TOKEN"], process.stdout)


    def test_pi_launcher_loads_the_extension_and_passes_arguments_through(self):
        launcher = str(Path(EXECUTABLE).with_name("tm-pi"))
        extension = Path(EXECUTABLE).with_name("textmate-pi-extension.js")
        with tempfile.TemporaryDirectory(prefix="tm-pi-", dir="/tmp") as directory:
            app = Path(directory) / 'app "quoted" 😀'
            app.mkdir()
            shutil.copy2(EXECUTABLE, app / "textmate-mcp")
            shutil.copy2(launcher, app / "tm-pi")
            stub = Path(directory) / "pi-stub"
            stub.write_text("#!" + sys.executable + "\nimport json, os, sys\nprint(json.dumps({'args': sys.argv[1:], 'server': os.environ.get('TM_MCP_SERVER')}))\n")
            stub.chmod(0o700)
            with context_server(lambda message, _: {"windowId": "w", "documentName": "test"}) as environment:
                environment["TM_PI_COMMAND"] = str(stub)
                def run(*arguments):
                    return subprocess.run([str(app / "tm-pi"), *arguments], cwd=directory, env=environment,
                                          text=True, capture_output=True, timeout=5)
                # Without the extension beside it the launcher must refuse, not start pi unconfigured.
                missing = run("prompt")
                self.assertEqual(missing.returncode, 1)
                self.assertIn("extension is missing", missing.stderr)

                shutil.copy2(extension, app / "textmate-pi-extension.js")
                process = run("--model", "example", "--", "prompt with spaces")
                self.assertEqual(process.returncode, 0, process.stderr)
                actual = json.loads(process.stdout)
                self.assertEqual(actual["args"], ["--extension", str((app / "textmate-pi-extension.js").resolve()),
                                                  "--model", "example", "--", "prompt with spaces"])
                self.assertEqual(actual["server"], str((app / "textmate-mcp").resolve()))
                self.assertNotIn(environment["TM_CONTEXT_TOKEN"], process.stdout)

                # Subcommands do not load extensions, and an option before one would read as a prompt.
                for arguments in (["mcp", "list"], ["install", "npm:example"], ["list"]):
                    self.assertEqual(json.loads(run(*arguments).stdout)["args"], arguments)

    @unittest.skipUnless(shutil.which("node"), "node is required to evaluate the pi extension")
    def test_pi_extension_registers_the_server_without_embedding_credentials(self):
        import base64
        source = Path(EXECUTABLE).with_name("textmate-pi-extension.js").read_text(encoding="utf-8")
        encoded = base64.b64encode(source.encode()).decode()
        script = ("const registered = [];"
                  f"const extension = (await import('data:text/javascript;base64,{encoded}')).default;"
                  "extension({ registerMcpServer: (name, config) => registered.push({ name, config }) });"
                  "console.log(JSON.stringify(registered));")
        def evaluate(server):
            environment = dict(os.environ, TM_CONTEXT_TOKEN="secret-test-token")
            environment.pop("TM_MCP_SERVER", None)
            if server: environment["TM_MCP_SERVER"] = server
            process = subprocess.run(["node", "--input-type=module", "-e", script], env=environment,
                                     text=True, capture_output=True, timeout=10)
            self.assertEqual(process.returncode, 0, process.stderr)
            self.assertNotIn("secret-test-token", process.stdout)
            return json.loads(process.stdout)
        registered = evaluate("/app/textmate-mcp")
        self.assertEqual([entry["name"] for entry in registered], ["textmate"])
        config = registered[0]["config"]
        self.assertEqual(config["command"], "/app/textmate-mcp")
        self.assertEqual(config["env"], {key: "${%s}" % key for key in ("TM_CONTEXT_SOCKET", "TM_CONTEXT_SESSION", "TM_CONTEXT_TOKEN")})
        self.assertEqual(config["exposure"], "direct")
        self.assertEqual(evaluate(None), [], "without a helper path nothing may be registered")

    def test_claude_launcher_arguments_connection_and_credentials(self):
        launcher = str(Path(EXECUTABLE).with_name("tm-claude"))
        with tempfile.TemporaryDirectory(prefix="tm-claude-", dir="/tmp") as directory:
            app = Path(directory) / 'app "quoted" 😀'
            app.mkdir()
            shutil.copy2(EXECUTABLE, app / "textmate-mcp")
            shutil.copy2(launcher, app / "tm-claude")
            stub = Path(directory) / "claude-stub"
            stub.write_text("#!" + sys.executable + "\n" +
                "import json, os, subprocess, sys\n" +
                "arguments = sys.argv[1:]\n" +
                "config = json.loads(arguments[arguments.index('--mcp-config')+1])['mcpServers']['textmate']\n" +
                "environment = dict(os.environ)\n" +
                "for key, value in config['env'].items(): environment[key] = os.path.expandvars(value)\n" +
                "probe = subprocess.run([config['command'], '--check'], env=environment, text=True, capture_output=True)\n" +
                "print(json.dumps({'args': arguments, 'cwd': os.getcwd(), 'probe': probe.returncode}))\n" +
                "sys.exit(23)\n")
            stub.chmod(0o700)
            for user_arguments in [["Explain the selection"], ["--model", "sonnet", "--resume", "conversation-id", "--", "prompt with spaces"]]:
                with self.subTest(arguments=user_arguments):
                    received = []
                    def context(message, _):
                        received.append(message)
                        return {"windowId": "claude-window", "documentName": "selected.txt"}
                    with context_server(context) as environment:
                        environment["TM_CLAUDE_COMMAND"] = str(stub)
                        process = subprocess.run([str(app / "tm-claude"), *user_arguments], cwd=directory,
                                                 env=environment, text=True, capture_output=True, timeout=5)
                        self.assertEqual(process.returncode, 23, process.stderr)
                        actual = json.loads(process.stdout)
                        self.assertEqual(actual["cwd"], os.path.realpath(directory))
                        self.assertEqual(actual["probe"], 0)
                        arguments = actual["args"]
                        index = arguments.index("--mcp-config")
                        config = json.loads(arguments[index+1])["mcpServers"]["textmate"]
                        self.assertEqual(arguments[:index] + arguments[index+2:], user_arguments)
                        if "--" in arguments: self.assertLess(index+1, arguments.index("--"))
                        else: self.assertEqual(index+2, len(arguments))
                        self.assertEqual(config["command"], str((app / "textmate-mcp").resolve()))
                        self.assertEqual(config["type"], "stdio")
                        self.assertEqual(config["env"]["TM_CONTEXT_TOKEN"], "${TM_CONTEXT_TOKEN}")
                        self.assertNotIn("--strict-mcp-config", arguments)
                        self.assertNotIn(environment["TM_CONTEXT_TOKEN"], " ".join(arguments))
                        self.assertEqual(len(received), 2)
                        self.assertTrue(all(m["token"] == environment["TM_CONTEXT_TOKEN"] for m in received))

    def test_claude_launcher_help_check_and_missing_installation(self):
        launcher = str(Path(EXECUTABLE).with_name("tm-claude"))
        help_result = subprocess.run([launcher, "--help"], text=True, capture_output=True, check=True)
        self.assertIn("TM_CLAUDE_COMMAND", help_result.stdout)
        with context_server(lambda message, _: {"windowId": "claude-window", "documentName": "test.txt"}) as environment:
            environment["TM_CLAUDE_COMMAND"] = "/tmp/nonexistent-claude-installation"
            process = subprocess.run([launcher, "--check"], env=environment, text=True, capture_output=True, check=True)
            self.assertIn("claude-window", process.stdout)
            process = subprocess.run([launcher], env=environment, text=True, capture_output=True)
            self.assertNotEqual(process.returncode, 0)
            self.assertIn("Claude Code CLI was not found", process.stderr)
            self.assertIn("TM_CLAUDE_COMMAND", process.stderr)


    def test_opencode_launcher_preserves_inline_configuration_and_arguments(self):
        launcher = str(Path(EXECUTABLE).with_name("tm-opencode"))
        retained = {"model": "existing/model", "instructions": ["https://example.test/path/*literal*/"],
                    "mcp": {"other": {"type": "local", "command": ["other-server"], "enabled": False},
                            "textmate": {"type": "remote", "url": "https://old-server.test"}}}
        commented = '// inline settings\n' + json.dumps(retained, indent=2).replace('\n}', ',\n}')
        with tempfile.TemporaryDirectory(prefix="tm-opencode-", dir="/tmp") as directory:
            app = Path(directory) / 'app "quoted" 😀'
            app.mkdir()
            shutil.copy2(EXECUTABLE, app / "textmate-mcp")
            shutil.copy2(launcher, app / "tm-opencode")
            stub = Path(directory) / "opencode-stub"
            stub.write_text("#!" + sys.executable + "\n" +
                "import json, os, subprocess, sys\n" +
                "config = json.loads(os.environ['OPENCODE_CONFIG_CONTENT'])\n" +
                "server = config['mcp']['textmate']\n" +
                "environment = dict(os.environ)\n" +
                "for key, value in server['environment'].items(): environment[key] = os.environ[value[5:-1]]\n" +
                "probe = subprocess.run(server['command'] + ['--check'], env=environment, text=True, capture_output=True)\n" +
                "print(json.dumps({'args': sys.argv[1:], 'cwd': os.getcwd(), 'config': config, 'probe': probe.returncode}))\n" +
                "sys.exit(23)\n")
            stub.chmod(0o700)
            arguments = ["run", "--model", "provider/model", "--", "prompt with spaces"]
            for inline in [None, json.dumps(retained), commented]:
                with self.subTest(inline=inline), context_server(lambda message, _: {"windowId": "opencode-window", "documentName": "test"}) as environment:
                    environment["TM_OPENCODE_COMMAND"] = str(stub)
                    environment.pop("OPENCODE_CONFIG_CONTENT", None)
                    if inline is not None: environment["OPENCODE_CONFIG_CONTENT"] = inline
                    process = subprocess.run([str(app / "tm-opencode"), *arguments], cwd=directory,
                                             env=environment, text=True, capture_output=True, timeout=5)
                    self.assertEqual(process.returncode, 23, process.stderr)
                    actual = json.loads(process.stdout)
                    self.assertEqual(actual["args"], arguments)
                    self.assertEqual(actual["cwd"], os.path.realpath(directory))
                    self.assertEqual(actual["probe"], 0)
                    server = actual["config"]["mcp"]["textmate"]
                    self.assertEqual(server["command"], [str((app / "textmate-mcp").resolve())])
                    self.assertEqual(server["type"], "local")
                    self.assertTrue(server["enabled"])
                    self.assertNotIn("url", server)
                    self.assertEqual(server["environment"]["TM_CONTEXT_TOKEN"], "{env:TM_CONTEXT_TOKEN}")
                    self.assertNotIn(environment["TM_CONTEXT_TOKEN"], json.dumps(actual))
                    if inline is not None:
                        self.assertEqual(actual["config"]["model"], retained["model"])
                        self.assertEqual(actual["config"]["instructions"], retained["instructions"])
                        self.assertEqual(actual["config"]["mcp"]["other"], retained["mcp"]["other"])

    def test_opencode_launcher_help_check_and_missing_installation(self):
        launcher = str(Path(EXECUTABLE).with_name("tm-opencode"))
        result = subprocess.run([launcher, "--help"], text=True, capture_output=True, check=True)
        self.assertIn("TM_OPENCODE_COMMAND", result.stdout)
        with context_server(lambda message, _: {"windowId": "opencode-window", "documentName": "test"}) as environment:
            environment["TM_OPENCODE_COMMAND"] = "/tmp/nonexistent-opencode-installation"
            result = subprocess.run([launcher, "--check"], env=environment, text=True, capture_output=True, check=True)
            self.assertIn("opencode-window", result.stdout)
            result = subprocess.run([launcher], env=environment, text=True, capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("OpenCode CLI was not found", result.stderr)
            self.assertIn("TM_OPENCODE_COMMAND", result.stderr)

    def test_opencode_launcher_rejects_invalid_existing_configuration(self):
        launcher = str(Path(EXECUTABLE).with_name("tm-opencode"))
        with context_server(lambda message, _: {"windowId": "opencode-window", "documentName": "test"}) as environment:
            environment["TM_OPENCODE_COMMAND"] = sys.executable
            for inline in ['{broken', '[]', '{"mcp": []}', '/*unterminated']:
                with self.subTest(inline=inline):
                    environment["OPENCODE_CONFIG_CONTENT"] = inline
                    result = subprocess.run([launcher], env=environment, text=True, capture_output=True, timeout=5)
                    self.assertNotEqual(result.returncode, 0)
                    self.assertIn("OPENCODE_CONFIG_CONTENT must contain", result.stderr)
                    self.assertIn("Existing configuration was not replaced", result.stderr)


if __name__ == "__main__":
    unittest.main()
