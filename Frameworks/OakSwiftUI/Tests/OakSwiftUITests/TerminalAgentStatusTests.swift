import AppKit
import SwiftTerm
import Testing
@testable import OakSwiftUI

@MainActor
private func reportedStatus(_ reports: String) -> [TerminalProgramStatus] {
    let terminal = LocalProcessTerminalView(frame: NSRect(x: 0, y: 0, width: 320, height: 160))
    for report in reports.split(separator: "\n") {
        terminal.feed(text: "\u{1b}]7501;\(report)\u{7}")
    }
    return terminal.programStatusRecords
}

@MainActor
@Test func agentStatusPrioritizesBlockedWorkersAndAcknowledgesOnlyResults() {
    var status = TerminalAgentStatus()
    status.update(reportedStatus("state=done:app=claude-code\nstate=working:id=worker\nstate=blocked:kind=question:id=worker/question"))
    #expect(status.value == 2)
    #expect(status.description.contains("Needs an answer"))
    status.acknowledgeResults()
    #expect(status.value == 2)
    status.update(reportedStatus("state=done:app=claude-code\nstate=working:id=worker"))
    #expect(status.value == 1)
    status.update(reportedStatus("state=done:app=claude-code"))
    #expect(status.value == 0)
    status.update(reportedStatus("state=idle:app=claude-code"))
    status.update(reportedStatus("state=done:app=claude-code"))
    #expect(status.value == 3)
    status.update([])
    #expect(status.value == 0)
}

@MainActor
@Test func agentStatusReportsReasonsAndSanitizesMessages() {
    var status = TerminalAgentStatus()
    for (kind, label) in [("permission", "Needs approval"), ("question", "Needs an answer"), ("auth", "Needs sign-in")] {
        status.update(reportedStatus("state=blocked:kind=\(kind)"))
        #expect(status.description == label)
    }
    let message = Data("Hello\u{202E}world\u{200B}".utf8).base64EncodedString()
    status.update(reportedStatus("state=working:progress=42:msg=\(message)"))
    #expect(status.description == "Working — 42% — Helloworld")
    status.update(reportedStatus("state=error"))
    #expect(status.value == 4)
    status.acknowledgeResults()
    #expect(status.value == 0)
}

@MainActor
@Test func agentStatusArrivesThroughTerminalDelegateAndSurvivesTransfer() async throws {
    _ = NSApplication.shared
    let source = OakTerminalPanelController()
    let destination = OakTerminalPanelController()
    source.startSession(inDirectory: NSTemporaryDirectory())
    defer { source.stopSession(); destination.stopSession() }
    let terminal = try #require(source.terminal)
    let id = try #require(source.sessionIdentifiers.first)
    let title = source.sessionTitles
    terminal.feed(text: "\u{1b}]7501;state=blocked:kind=permission:app=codex\u{7}")
    for _ in 0..<100 {
        if source.sessionStatusValues == [2] { break }
        try await Task.sleep(for: .milliseconds(10))
    }
    #expect(source.sessionStatusValues == [2])
    source.selectSession(at: 0)
    #expect(source.sessionStatusValues == [2])
    source.transferSession(identifier: id, to: destination, at: 0)
    #expect(source.sessionStatusValues.isEmpty)
    #expect(destination.sessionStatusValues == [2])
    #expect(destination.sessionTitles == title)
    terminal.feed(text: "\u{1b}]7501;state=done:app=codex\u{7}")
    for _ in 0..<100 {
        if destination.sessionStatusValues == [3] { break }
        try await Task.sleep(for: .milliseconds(10))
    }
    #expect(destination.sessionStatusValues == [3])
    destination.selectSession(at: 0)
    #expect(destination.sessionStatusValues == [0])
    // Repeated delivery must not resurrect an acknowledged result.
    destination.programStatusChanged(source: terminal, records: terminal.programStatusRecords)
    #expect(destination.sessionStatusValues == [0])
    terminal.feed(text: "\u{1b}]7501;state=working\u{7}")
    destination.programStatusChanged(source: terminal, records: terminal.programStatusRecords)
    #expect(destination.sessionStatusValues == [1])
    terminal.programStatusProcessExited()
    destination.programStatusChanged(source: terminal, records: terminal.programStatusRecords)
    #expect(destination.sessionStatusValues == [0])
    destination.stopSession()
    destination.programStatusChanged(source: terminal, records: reportedStatus("state=error"))
    #expect(destination.sessionStatusValues.isEmpty)
}
