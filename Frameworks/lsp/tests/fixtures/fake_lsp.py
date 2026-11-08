#!/usr/bin/env python3
"""Deterministic fake LSP server for the headless pull-diagnostics test.

Usage: fake_lsp.py <scenario> <result_file>

Speaks Content-Length framed JSON-RPC over stdio.  As soon as a scenario
reaches a conclusion it writes a single line verdict to <result_file>:

    SCENARIO <scenario> PASS|FAIL <detail>

The verdict is written once (first write wins) so a later, less specific
observation can never overwrite an earlier failure.  The process keeps
answering requests until the client sends `exit`, is torn down, or stdin
closes; a best-effort verdict is written on any of those paths.

Scenario timings are deliberately generous: the client under test debounces
pulls by 250 ms and the scenarios hold responses until the client acts.
"""

import json
import os
import select
import signal
import sys
import time

SCENARIO = sys.argv[1] if len(sys.argv) > 1 else "unknown"
RESULT_FILE = sys.argv[2] if len(sys.argv) > 2 else "unknown.result"
_stem, _ext = os.path.splitext(RESULT_FILE)
READY_FILE = _stem + ".ready" + _ext
WIRE_FILE = _stem + ".wire" + _ext

# Overall wall-clock budget for a single server process before declaring a
# timeout.  The slowest scenario concludes ~6 s after its trigger.
OVERALL_TIMEOUT = 25.0


def _wire(direction, text):
	try:
		with open(WIRE_FILE, "a") as f:
			f.write("%08.3f %s %s\n" % (time.time(), direction, text))
	except OSError:
		pass


class FakeLSP:
	def __init__(self, scenario, result_file):
		self.scenario = scenario
		self.result_file = result_file
		self.buf = b""
		self.start_time = time.time()
		self.verdict_written = False
		self.verdict = None

		self.stdin_fd = sys.stdin.fileno()
		self.stdout = sys.stdout.buffer

		# protocol bookkeeping
		self.initialize_id = None
		self.uri = None
		self.first_diag_id = None
		self.diag_request_count = 0

		# f0
		self.f0_register_id = None
		self.f0_deadline = None

		# f3
		self.f3_cancel_seen = False
		self.f3_second_seen = False
		self.f3_deadline = None

		# f6
		self.burst_active = False
		self.burst_start = None
		self.burst_count = 0
		self.burst_last_req = None
		self.burst_eval_deadline = None

		# f7
		self.f7_didchange_time = None
		self.f7_deadline = None

		# f8
		self.f8_didclose_time = None
		self.f8_deadline = None
		self.f8_diag_after_close = False

	def now(self):
		return time.time()

	# ---------------------------------------------------------------- output
	def send(self, obj):
		body = json.dumps(obj, separators=(",", ":")).encode("utf-8")
		self.stdout.write(b"Content-Length: %d\r\n\r\n" % len(body))
		self.stdout.write(body)
		self.stdout.flush()
		_wire("SEND", json.dumps(obj))

	def log(self, message):
		sys.stderr.write("fake[%s] %s\n" % (self.scenario, message))
		sys.stderr.flush()

	def set_verdict(self, status, detail):
		if self.verdict_written:
			return
		detail = " ".join(str(detail).split())
		self.verdict = (status, detail)
		self.verdict_written = True
		line = "SCENARIO %s %s %s\n" % (self.scenario, status, detail)
		try:
			with open(self.result_file, "w") as f:
				f.write(line)
				f.flush()
				os.fsync(f.fileno())
		except OSError as e:
			self.log("cannot write result file: %s" % e)
		self.log("VERDICT %s %s" % (status, detail))

	def finish(self):
		if not self.verdict_written:
			self.set_verdict("FAIL", "server exited before a verdict (timeout / no trigger)")

	# ---------------------------------------------------------------- framing
	def try_parse(self):
		while True:
			idx = self.buf.find(b"\r\n\r\n")
			if idx < 0:
				return
			header = self.buf[:idx].decode("ascii", "replace")
			content_length = None
			for line in header.split("\r\n"):
				if line.lower().startswith("content-length:"):
					try:
						content_length = int(line.split(":", 1)[1].strip())
					except ValueError:
						content_length = None
			if content_length is None:
				# Unparseable header; drop it and keep going.
				self.buf = self.buf[idx + 4:]
				continue
			start = idx + 4
			if len(self.buf) < start + content_length:
				return
			body = self.buf[start:start + content_length]
			self.buf = self.buf[start + content_length:]
			try:
				msg = json.loads(body.decode("utf-8"))
			except ValueError:
				self.log("failed to parse JSON body")
				continue
			self.handle(msg)

	# ---------------------------------------------------------------- dispatch
	def handle(self, msg):
		_wire("RECV", json.dumps(msg))
		if "method" in msg:
			if "id" in msg:
				self.handle_request(msg)
			else:
				self.handle_notification(msg)
			return
		if "id" in msg:
			self.handle_response(msg)

	def handle_request(self, msg):
		method = msg["method"]
		if method == "initialize":
			self.initialize_id = msg["id"]
			self.send({
				"jsonrpc": "2.0",
				"id": msg["id"],
				"result": {
					"capabilities": self.capabilities(),
					"serverInfo": {"name": "fake-lsp", "version": "0.1"},
				},
			})
		elif method == "textDocument/diagnostic":
			self.on_diagnostic(msg)
		elif method == "shutdown":
			self.send({"jsonrpc": "2.0", "id": msg["id"], "result": None})
		else:
			# Never leave a client request hanging.
			self.send({"jsonrpc": "2.0", "id": msg["id"], "result": None})

	def handle_notification(self, msg):
		method = msg["method"]
		params = msg.get("params") or {}

		if method == "initialized":
			self.touch_ready()
			if self.scenario == "f0_register":
				self.f0_register_id = 9001
				self.f0_deadline = self.now() + 5.0
				self.send({
					"jsonrpc": "2.0",
					"id": self.f0_register_id,
					"method": "client/registerCapability",
					"params": {
						"registrations": [{
							"id": "fake-reg",
							"method": "fake/dynamicCapability",
							"registerOptions": {},
						}],
					},
				})
		elif method == "workspace/didChangeConfiguration":
			pass
		elif method == "textDocument/didOpen":
			self.uri = params.get("textDocument", {}).get("uri")
		elif method == "textDocument/didChange":
			self.on_did_change()
		elif method == "textDocument/didSave":
			pass
		elif method == "textDocument/didClose":
			self.on_did_close(params)
		elif method == "$/cancelRequest":
			self.on_cancel(params.get("id"))
		elif method == "exit":
			self.finish()
			sys.exit(0)

	def handle_response(self, msg):
		if self.scenario != "f0_register" or self.f0_register_id is None:
			return
		if msg.get("id") != self.f0_register_id:
			return
		if "error" in msg:
			self.set_verdict("FAIL", "client returned error: %s" % msg["error"])
		elif "result" in msg and msg["result"] is None:
			self.set_verdict("PASS", "client/registerCapability response had result:null")
		else:
			self.set_verdict("FAIL", "client/registerCapability response result=%r (missing or non-null)"
				% msg.get("result", "<missing>"))

	# ---------------------------------------------------------------- scenarios
	def capabilities(self):
		caps = {
			"textDocumentSync": {"openClose": True, "change": 1},
			# publishDiagnostics is a client-side declaration, but mirroring it
			# here keeps the fake realistic for mixed push/pull servers.
			"textDocument": {"publishDiagnostics": {"dynamicRegistration": False}},
		}
		if self.scenario != "f7_no_provider":
			caps["diagnosticProvider"] = {
				"identifier": "fake",
				"interFileDependencies": False,
			}
		return caps

	def touch_ready(self):
		try:
			with open(READY_FILE, "w") as f:
				f.write("ready\n")
		except OSError:
			pass

	def on_did_change(self):
		now = self.now()
		if self.scenario == "f6_coalesce" and not self.burst_active:
			self.burst_active = True
			self.burst_start = now
			self.burst_count = 0
			self.burst_last_req = None
			self.burst_eval_deadline = None
		if self.scenario == "f7_no_provider" and self.f7_didchange_time is None:
			self.f7_didchange_time = now
			self.f7_deadline = now + 5.0

	def on_did_close(self, params):
		if self.scenario != "f8_close":
			return
		uri = params.get("textDocument", {}).get("uri")
		if uri is not None and self.uri is not None and uri != self.uri:
			return
		self.f8_didclose_time = self.now()
		self.f8_deadline = self.f8_didclose_time + 6.0

	def on_cancel(self, cancel_id):
		if self.scenario != "f3_supersede" or self.first_diag_id is None:
			return
		if cancel_id != self.first_diag_id:
			return
		self.f3_cancel_seen = True
		if not self.f3_second_seen:
			self.set_verdict("PASS", "$/cancelRequest for R1 id=%s arrived before any second request" % cancel_id)

	def on_diagnostic(self, msg):
		req_id = msg["id"]
		params = msg.get("params") or {}
		self.diag_request_count += 1
		if self.first_diag_id is None:
			self.first_diag_id = req_id

		if self.scenario == "f1_full_empty":
			self.send_full_empty(req_id)
			if self.verdict is None:
				self.set_verdict("PASS", "diagnostic request id=%s arrived; responded full/empty" % req_id)
		elif self.scenario == "f2_unchanged":
			self.send({"jsonrpc": "2.0", "id": req_id, "result": {"kind": "unchanged", "resultId": "r1"}})
			# Only conclude once the post-edit pull has been answered: a client
			# that stopped pulling would otherwise look like one honouring
			# `unchanged`.
			if self.diag_request_count >= 2:
				self.set_verdict("PASS", "responded unchanged to %d diagnostic requests" % self.diag_request_count)
		elif self.scenario == "f3_supersede":
			if req_id == self.first_diag_id:
				# Hold R1 deliberately: the client must cancel it when the next
				# burst supersedes it.
				self.f3_deadline = self.now() + 12.0
			else:
				self.f3_second_seen = True
				self.send_full_empty(req_id)
				self.set_verdict("FAIL", "second diagnostic request id=%s arrived before $/cancelRequest for R1 id=%s"
					% (req_id, self.first_diag_id))
		elif self.scenario == "f4_servercancel":
			if self.verdict is None:
				self.send({
					"jsonrpc": "2.0",
					"id": req_id,
					"error": {"code": -32802, "message": "ServerCancelled"},
				})
				self.set_verdict("PASS", "sent JSON-RPC error -32802 (ServerCancelled) for id=%s" % req_id)
			else:
				self.send_full_empty(req_id)
		elif self.scenario == "f6_coalesce":
			if self.burst_active:
				self.burst_count += 1
				self.burst_last_req = self.now()
				self.burst_eval_deadline = self.burst_last_req + 1.5
			self.send_full_empty(req_id)
		elif self.scenario == "f7_no_provider":
			# The client must not pull at all; any request is a failure.
			self.set_verdict("FAIL", "unexpected textDocument/diagnostic id=%s despite no diagnosticProvider" % req_id)
			self.send_full_empty(req_id)
		elif self.scenario == "f8_close":
			self.send_full_empty(req_id)
			if self.f8_didclose_time is not None:
				self.f8_diag_after_close = True
				self.set_verdict("FAIL", "diagnostic request id=%s arrived after didClose" % req_id)
		else:
			self.send_full_empty(req_id)

	def send_full_empty(self, req_id):
		self.send({"jsonrpc": "2.0", "id": req_id, "result": {"kind": "full", "items": []}})

	# ---------------------------------------------------------------- timers
	def poll(self):
		now = self.now()

		if self.scenario == "f3_supersede" and self.f3_deadline and now > self.f3_deadline:
			if not self.verdict_written:
				if self.f3_cancel_seen:
					self.set_verdict("PASS", "$/cancelRequest for R1 received")
				else:
					self.set_verdict("FAIL", "no $/cancelRequest for R1 id=%s within timeout" % self.first_diag_id)

		if self.scenario == "f0_register" and self.f0_deadline and now > self.f0_deadline:
			if not self.verdict_written:
				self.set_verdict("FAIL", "no response to client/registerCapability within timeout")

		if self.scenario == "f6_coalesce" and self.burst_active and not self.verdict_written:
			if self.burst_last_req is not None and self.burst_eval_deadline and now > self.burst_eval_deadline:
				if self.burst_count == 1:
					self.set_verdict("PASS", "1 diagnostic request in edit burst")
				else:
					self.set_verdict("FAIL", "%d diagnostic requests in edit burst" % self.burst_count)
			elif self.burst_last_req is None and self.burst_start and now - self.burst_start > 3.0:
				self.set_verdict("FAIL", "0 diagnostic requests in edit burst")

		if self.scenario == "f7_no_provider" and self.f7_deadline and now > self.f7_deadline:
			if not self.verdict_written:
				self.set_verdict("PASS", "no textDocument/diagnostic within 5s after didChange")

		if self.scenario == "f8_close" and self.f8_deadline and now > self.f8_deadline:
			if not self.verdict_written:
				if self.f8_diag_after_close:
					self.set_verdict("FAIL", "diagnostic request after didClose")
				else:
					self.set_verdict("PASS", "didClose received and no textDocument/diagnostic afterwards")

		if not self.verdict_written and now - self.start_time > OVERALL_TIMEOUT:
			self.set_verdict("FAIL", "overall timeout with no scenario trigger")

	def run(self):
		_wire("INIT", "scenario=%s" % self.scenario)
		while True:
			self.poll()
			try:
				ready, _, _ = select.select([self.stdin_fd], [], [], 0.05)
			except (InterruptedError, OSError):
				ready = []
			if ready:
				try:
					data = os.read(self.stdin_fd, 65536)
				except OSError:
					data = b""
				if not data:
					break
				self.buf += data
				self.try_parse()
		self.finish()


def _on_sigterm(signum, frame):
	raise SystemExit(0)


def main():
	signal.signal(signal.SIGTERM, _on_sigterm)
	server = FakeLSP(SCENARIO, RESULT_FILE)
	try:
		server.run()
	except SystemExit:
		server.finish()
		raise


if __name__ == "__main__":
	main()
