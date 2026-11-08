import Testing
import AppKit
import SwiftTerm
import Darwin
@testable import OakSwiftUI

@MainActor
@Test func clearShortcutRequiresTerminalFocus() async throws {
    _ = NSApplication.shared
    let controller = makeTerminalController()
    controller.startSession()
    defer { controller.stopSession() }
    let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 400, height: 300), styleMask: .titled, backing: .buffered, defer: false)
    window.isReleasedWhenClosed = false
    window.contentView?.addSubview(controller.view)
    controller.view.frame = window.contentView!.bounds
    let terminal = try #require(controller.terminal)

    func commandK() throws -> Bool {
        let event = try #require(NSEvent.keyEvent(with: .keyDown, location: .zero, modifierFlags: [.command], timestamp: 0, windowNumber: window.windowNumber, context: nil, characters: "k", charactersIgnoringModifiers: "k", isARepeat: false, keyCode: 40))
        return controller.view.performKeyEquivalent(with: event)
    }

    window.makeFirstResponder(terminal)
    #expect(try commandK())
    let otherView = NSView(frame: .zero)
    window.contentView?.addSubview(otherView)
    window.makeFirstResponder(otherView)
    #expect(try !commandK())
    controller.stopSession()
    #expect(try !commandK())
}

@MainActor
@Test func terminalSessionLifecycleAndResize() async throws {
    _ = NSApplication.shared
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent("TextMate terminal test \(UUID().uuidString)")
    try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    let controller = makeTerminalController()
    controller.applyTheme(OakTerminalTheme(background: .black, foreground: .white, caret: .green, selection: .blue))
    let delegate = TerminalTestDelegate()
    delegate.workingDirectory = folder.path
    controller.delegate = delegate
    defer { withExtendedLifetime(delegate) {} }
    controller.startSession()
    defer { controller.stopSession() }
    let terminal = try #require(controller.terminal)
    #expect(terminal.nativeBackgroundColor == .black)
    #expect(terminal.nativeForegroundColor == .white)
    #expect(terminal.caretColor == .green)
    #expect(terminal.selectedTextBackgroundColor == .blue)
    controller.applyTheme(OakTerminalTheme(background: .white, foreground: .black, caret: .red, selection: .yellow))
    #expect(terminal.nativeBackgroundColor == .white)
    #expect(terminal.nativeForegroundColor == .black)
    #expect(terminal.caretColor == .red)
    #expect(terminal.selectedTextBackgroundColor == .yellow)
    let pid = terminal.process.shellPid
    #expect(pid > 0)

    func waitForFile(_ name: String) async throws -> String {
        let file = folder.appendingPathComponent(name)
        for _ in 0..<100 {
            if let text = try? String(contentsOf: file, encoding: .utf8), !text.isEmpty { return text }
            try await Task.sleep(for: .milliseconds(100))
        }
        Issue.record("Shell did not create \(name)")
        return ""
    }
    terminal.send(txt: "pwd > cwd.txt\n")
    let cwd = try await waitForFile("cwd.txt")
    #expect(URL(fileURLWithPath: cwd.trimmingCharacters(in: .whitespacesAndNewlines)).resolvingSymlinksInPath() == folder.resolvingSymlinksInPath())
    controller.startSession()
    #expect(terminal.process.shellPid == pid)

    for size in [NSSize(width: 700, height: 300), NSSize(width: 0, height: 0), NSSize(width: 400, height: 180)] {
        controller.view.frame.size = size
        controller.view.layoutSubtreeIfNeeded()
        #expect(terminal.terminalDimensions.cols > 0)
        #expect(terminal.terminalDimensions.rows > 0)
    }
    terminal.send(txt: "stty size > size.txt\n")
    let size = try await waitForFile("size.txt")
    #expect(size.split(whereSeparator: \.isWhitespace).compactMap { Int($0) } == [terminal.terminalDimensions.rows, terminal.terminalDimensions.cols])

    for _ in 0..<50 {
        if !controller.hasRunningWork { break }
        try await Task.sleep(for: .milliseconds(100))
    }
    #expect(!controller.hasRunningWork)
    let host = NSView(frame: NSRect(x: 0, y: 0, width: 400, height: 180))
    host.addSubview(controller.view)
    controller.view.removeFromSuperview()
    host.addSubview(controller.view)
    #expect(terminal.process.shellPid == pid)

    terminal.send(txt: "exit\n")
    for _ in 0..<100 {
        if controller.sessionCount == 0 { break }
        try await Task.sleep(for: .milliseconds(50))
    }
    #expect(controller.sessionCount == 0)
    #expect(controller.terminal == nil)
    #expect(!controller.hasRunningWork)
    controller.newTerminal()
    let restartedTerminal = try #require(controller.terminal)
    #expect(restartedTerminal.nativeBackgroundColor == .white)
    #expect(restartedTerminal.nativeForegroundColor == .black)
    #expect(restartedTerminal.caretColor == .red)
    #expect(restartedTerminal.selectedTextBackgroundColor == .yellow)
    let restartedPID = restartedTerminal.process.shellPid
    #expect(restartedPID > 0 && restartedPID != pid)
    restartedTerminal.send(txt: "sleep 30 & echo ready > background.txt\n")
    _ = try await waitForFile("background.txt")
    #expect(!controller.hasRunningWork)
    restartedTerminal.send(txt: "echo ready > work.txt; sleep 30\n")
    _ = try await waitForFile("work.txt")
    for _ in 0..<100 {
        if controller.hasRunningWork { break }
        try await Task.sleep(for: .milliseconds(50))
    }
    #expect(controller.hasRunningWork)
    controller.stopSession()
    for _ in 0..<100 {
        if kill(restartedPID, 0) != 0 { break }
        try await Task.sleep(for: .milliseconds(50))
    }
    #expect(kill(restartedPID, 0) != 0)
    #expect(controller.terminal == nil)
}

@MainActor
@Test func terminalTabsKeepIndependentSessions() async throws {
    _ = NSApplication.shared
    let controller = makeTerminalController()
    controller.startSession()
    defer { controller.stopSession() }
    let first = try #require(controller.terminal)
    let firstPID = first.process.shellPid
    controller.newTerminal()
    let second = try #require(controller.terminal)
    #expect(first !== second)
    #expect(second.process.shellPid != firstPID)
    #expect(controller.sessionCount == 2)
    controller.selectSession(id: 1)
    #expect(controller.terminal === first)
    controller.selectSession(id: 2)
    #expect(controller.terminal === second)
    controller.applyTheme(OakTerminalTheme(background: .black, foreground: .green, caret: .white, selection: .blue))
    #expect(first.nativeForegroundColor == .green)
    #expect(second.nativeForegroundColor == .green)

    // Exit in an inactive tab must close only that tab, leaving the active shell untouched.
    first.send(txt: "exit\n")
    for _ in 0..<100 {
        if controller.sessionCount == 1 { break }
        try await Task.sleep(for: .milliseconds(50))
    }
    #expect(controller.sessionCount == 1)
    #expect(controller.terminal === second)
    #expect(second.process.running)

    for _ in 0..<100 {
        if !controller.hasRunningWork { break }
        try await Task.sleep(for: .milliseconds(50))
    }
    #expect(!controller.hasRunningWork)
    controller.closeSessions(withIdentifiers: controller.sessionIdentifiers)
    #expect(controller.sessionCount == 0)
    #expect(controller.terminal == nil)
    controller.newTerminal()
    #expect(controller.sessionCount == 1)
    #expect(controller.terminal?.process.shellPid != second.process.shellPid)
}

@MainActor
@Test func terminalTabShortcutsRequirePaneFocus() throws {
    _ = NSApplication.shared
    let controller = makeTerminalController()
    controller.startSession()
    defer { controller.stopSession() }
    let first = try #require(controller.terminal)
    controller.newTerminal()
    let second = try #require(controller.terminal)
    let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 500, height: 300), styleMask: .titled, backing: .buffered, defer: false)
    window.isReleasedWhenClosed = false
    window.contentView?.addSubview(controller.view)
    controller.view.frame = window.contentView!.bounds
    window.makeFirstResponder(second)

    func arrow(right: Bool, extra: NSEvent.ModifierFlags = []) throws -> Bool {
        let event = try #require(NSEvent.keyEvent(with: .keyDown, location: .zero,
            modifierFlags: NSEvent.ModifierFlags([.command, .option, .function, .numericPad]).union(extra), timestamp: 0,
            windowNumber: window.windowNumber, context: nil,
            characters: right ? "\u{f703}" : "\u{f702}", charactersIgnoringModifiers: right ? "\u{f703}" : "\u{f702}",
            isARepeat: false, keyCode: right ? 124 : 123))
        return controller.view.performKeyEquivalent(with: event)
    }
    #expect(try arrow(right: false))
    #expect(controller.terminal === first)
    #expect(window.firstResponder === first)
    #expect(try arrow(right: false)) // Wrap from the first to the last tab.
    #expect(controller.terminal === second)
    #expect(try arrow(right: true))
    #expect(controller.terminal === first)
    #expect(try !arrow(right: true, extra: .shift))
    #expect(controller.terminal === first)

    let editor = NSTextView(frame: .zero)
    window.contentView?.addSubview(editor)
    window.makeFirstResponder(editor)
    #expect(try !arrow(right: true))
    #expect(controller.terminal === first)
}

@MainActor
@Test func newTerminalUsesCurrentProjectDirectory() async throws {
    _ = NSApplication.shared
    let root = FileManager.default.temporaryDirectory.appendingPathComponent("TextMate projects \(UUID().uuidString)")
    let firstFolder = root.appendingPathComponent("first project")
    let secondFolder = root.appendingPathComponent("second 'project'")
    for folder in [firstFolder, secondFolder] {
        try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
    }
    defer { try? FileManager.default.removeItem(at: root) }
    let delegate = TerminalTestDelegate()
    delegate.workingDirectory = firstFolder.path
    let controller = makeTerminalController()
    controller.delegate = delegate
    defer { withExtendedLifetime(delegate) {} }
    controller.startSession()
    defer { controller.stopSession() }
    let first = try #require(controller.terminal)
    delegate.workingDirectory = secondFolder.path
    controller.newTerminal()
    let second = try #require(controller.terminal)
    first.send(txt: "pwd > project-cwd.txt\n")
    second.send(txt: "pwd > project-cwd.txt\n")
    for folder in [firstFolder, secondFolder] {
        let output = folder.appendingPathComponent("project-cwd.txt")
        for _ in 0..<100 {
            if let text = try? String(contentsOf: output, encoding: .utf8), !text.isEmpty { break }
            try await Task.sleep(for: .milliseconds(50))
        }
        let path = try String(contentsOf: output, encoding: .utf8).trimmingCharacters(in: .whitespacesAndNewlines)
        #expect(URL(fileURLWithPath: path).resolvingSymlinksInPath() == folder.resolvingSymlinksInPath())
    }
}

@MainActor
@Test func nativeTabOrderAndTransferPreserveShells() throws {
    _ = NSApplication.shared
    let source = makeTerminalController()
    let target = makeTerminalController()
    source.startSession()
    target.startSession()
    defer { source.stopSession(); target.stopSession() }
    let first = try #require(source.terminal)
    let id = try #require(source.sessionIdentifiers.first)
    let pid = first.process.shellPid
    source.newTerminal()
    let second = try #require(source.terminal)
    source.moveSession(from: 0, to: 2)
    #expect(source.sessionIdentifiers.last == id)
    #expect(source.terminal === second)
    source.selectSession(at: 1)
    #expect(source.terminal === first)
    source.transferSession(identifier: id, to: target, at: 0)
    #expect(source.sessionCount == 1)
    #expect(source.terminal === second)
    #expect(target.sessionCount == 2)
    #expect(target.terminal === first)
    #expect(first.process.shellPid == pid)
    #expect(first.processDelegate === target)
    #expect(target.sessionIdentifiers.first == id)
}

@MainActor
@Test func terminalFontFollowsEditorAcrossSessions() throws {
    _ = NSApplication.shared
    let controller = makeTerminalController()
    let destination = makeTerminalController()
    defer { controller.stopSession(); destination.stopSession() }
    let initialFont = try #require(NSFont(name: "Menlo", size: 15))
    let updatedFont = try #require(NSFont(name: "Monaco", size: 18))
    controller.applyFont(initialFont)
    controller.startSession()
    let first = try #require(controller.terminal)
    let pid = first.process.shellPid
    #expect(first.font == initialFont)
    controller.newTerminal()
    let second = try #require(controller.terminal)
    controller.applyFont(updatedFont)
    #expect(first.font == updatedFont)
    #expect(second.font == updatedFont)
    #expect(first.process.shellPid == pid)
    controller.newTerminal()
    #expect(controller.terminal?.font == updatedFont)
    destination.applyFont(initialFont)
    controller.transferSession(identifier: controller.sessionIdentifiers[0], to: destination, at: 0)
    #expect(destination.terminal === first)
    #expect(first.font == initialFont)
    #expect(first.process.shellPid == pid)
}

// MARK: - OSC 7501 completion dot

// `#expect` cannot call a mutating member inline, so the transitions go through this wrapper.
@discardableResult
private func record(_ attention: inout TerminalAttention, _ states: [TerminalProgramStatusState], kind: TerminalProgramStatusKind? = nil, selected: Bool = false) -> Bool {
    attention.update(states: states, blockedKind: kind, isSelected: selected)
}

@MainActor
@Test func terminalAttentionIgnoresFinishedStateWhileSelected() {
    var attention = TerminalAttention()
    #expect(!record(&attention, [.working], selected: true))
    #expect(!record(&attention, [.done], selected: true))
    #expect(!attention.needsAttention)
    #expect(!record(&attention, [.blocked], selected: true))
    #expect(!attention.needsAttention)
}

@MainActor
@Test func terminalAttentionLightsBackgroundCompletionUntilSelected() {
    var attention = TerminalAttention()
    #expect(!record(&attention, [.working]))
    #expect(record(&attention, [.done]))
    #expect(attention.needsAttention)

    // Selecting the session clears the dot; snapshots of the same state stay silent.
    attention.clear()
    #expect(!attention.needsAttention)
    #expect(!record(&attention, [.done]))
    #expect(!attention.needsAttention)
}

@MainActor
@Test func terminalAttentionKeepsDotThroughLaterWorkingStates() {
    var attention = TerminalAttention()
    #expect(!record(&attention, [.working]))
    #expect(record(&attention, [.done]))
    // Further output in a background tab must not take the unseen completion away.
    #expect(!record(&attention, [.working]))
    #expect(attention.needsAttention)
    // ... nor re-light it (no repaint) when it finishes again.
    #expect(!record(&attention, [.done]))
    #expect(attention.needsAttention)
}

@MainActor
@Test func terminalAttentionFoldsConcurrentRecords() {
    // Several 7501 records can live at once; blocked outranks done outranks the rest.
    #expect(TerminalAttention.aggregate([.done, .working, .blocked]) == .blocked)
    #expect(TerminalAttention.aggregate([.idle, .done]) == .done)
    #expect(TerminalAttention.aggregate([.working, .idle]) == .idle)
    #expect(TerminalAttention.aggregate([]) == nil)

    var attention = TerminalAttention()
    #expect(!record(&attention, [.working]))
    #expect(record(&attention, [.done, .working]))
    #expect(attention.needsAttention)
}

@MainActor
@Test func terminalAttentionToleratesRecordsDisappearingAtExit() {
    var attention = TerminalAttention()
    #expect(!record(&attention, [.working]))
    // Process exit removes working/blocked records; an empty store must not raise the dot.
    #expect(!record(&attention, []))
    #expect(!attention.needsAttention)
    #expect(record(&attention, [.done]))
    #expect(attention.needsAttention)
    #expect(!record(&attention, [.done]))
    #expect(attention.needsAttention)
}

@MainActor
@Test func terminalAttentionKeepsTheKindThatRaisedIt() {
    var permission = TerminalAttention()
    #expect(!record(&permission, [.working]))
    #expect(record(&permission, [.blocked], kind: .permission))
    #expect(permission.needsAttention)
    #expect(permission.raisedState == .blocked)
    #expect(permission.blockedKind == .permission)
    permission.clear()
    #expect(!permission.needsAttention)
    // The kind belongs to the transition, so clearing the dot drops it too.
    #expect(permission.blockedKind == nil)

    // A blocked record without a kind is still blocked; the window falls back to the generic wording.
    var bare = TerminalAttention()
    #expect(record(&bare, [.blocked]))
    #expect(bare.raisedState == .blocked)
    #expect(bare.blockedKind == nil)

    // A done transition must not inherit the previous kind, since a tracker outlives the prompt.
    var reusable = TerminalAttention()
    #expect(record(&reusable, [.blocked], kind: .question))
    reusable.clear()
    #expect(record(&reusable, [.done]))
    #expect(reusable.raisedState == .done)
    #expect(reusable.blockedKind == nil)
}

@MainActor
@Test func terminalAttentionSubtitleFoldsTheWholeWindow() {
    func pending(_ state: TerminalProgramStatusState, kind: TerminalProgramStatusKind? = nil) -> TerminalAttention {
        var attention = TerminalAttention()
        _ = record(&attention, [.working])
        _ = record(&attention, [state], kind: kind)
        return attention
    }
    let quiet = TerminalAttention()

    #expect(TerminalAttention.subtitle(for: []) == nil)
    #expect(TerminalAttention.subtitle(for: [quiet, quiet]) == nil)
    #expect(TerminalAttention.subtitle(for: [pending(.done)]) == "terminal finished")
    #expect(TerminalAttention.subtitle(for: [pending(.blocked)]) == "terminal needs your input")
    #expect(TerminalAttention.subtitle(for: [pending(.blocked, kind: .question)]) == "terminal needs your input")
    #expect(TerminalAttention.subtitle(for: [pending(.blocked, kind: .auth)]) == "terminal needs your input")
    #expect(TerminalAttention.subtitle(for: [pending(.blocked, kind: .permission)]) == "terminal needs permission")
    // A blocked tab outranks a finished one, and permission outranks the other kinds that raise the dot.
    #expect(TerminalAttention.subtitle(for: [pending(.done), quiet, pending(.blocked)]) == "terminal needs your input")
    #expect(TerminalAttention.subtitle(for: [pending(.done), pending(.blocked, kind: .auth)]) == "terminal needs your input")
    #expect(TerminalAttention.subtitle(for: [pending(.blocked, kind: .permission), pending(.blocked, kind: .question)]) == "terminal needs permission")
    #expect(TerminalAttention.subtitle(for: [pending(.done), quiet]) == "terminal finished")
}

@MainActor
@Test func terminalAttentionToolTipNamesTheReasonForTheDot() {
    func lit(_ state: TerminalProgramStatusState, kind: TerminalProgramStatusKind? = nil) -> TerminalAttention {
        var attention = TerminalAttention()
        _ = record(&attention, [.working])
        _ = record(&attention, [state], kind: kind)
        return attention
    }

    #expect(lit(.done).toolTip == "Terminal: Finished")
    #expect(lit(.blocked, kind: .permission).toolTip == "Terminal: Needs permission")
    #expect(lit(.blocked, kind: .question).toolTip == "Terminal: Waiting for input")
    #expect(lit(.blocked, kind: .auth).toolTip == "Terminal: Waiting for input")
    #expect(lit(.blocked).toolTip == "Terminal: Waiting for input")

    // No dot means no tooltip; AppKit shows nothing for an empty one.
    #expect(TerminalAttention().toolTip == "")
    var cleared = lit(.done)
    cleared.clear()
    #expect(cleared.toolTip == "")
}

@MainActor
@Test func sessionToolTipsTrackEachSessionsDot() async throws {
    _ = NSApplication.shared
    let controller = makeTerminalController()
    #expect(controller.sessionToolTips == [])
    controller.startSession()
    defer { controller.stopSession() }
    controller.newTerminal()
    #expect(controller.sessionToolTips == ["", ""])

    controller.simulateSessionAttention(at: 0, state: "done", kind: nil)
    #expect(controller.sessionToolTips == ["Terminal: Finished", ""])
    controller.simulateSessionAttention(at: 1, state: "blocked", kind: "permission")
    #expect(controller.sessionToolTips == ["Terminal: Finished", "Terminal: Needs permission"])

    // Selecting a tab clears its dot, so its tooltip has to go with it.
    controller.selectSession(at: 0)
    #expect(controller.sessionToolTips == ["", "Terminal: Needs permission"])
    controller.simulateSessionAttention(at: 1, state: nil, kind: nil)
    #expect(controller.sessionToolTips == ["", ""])
}

@MainActor
@Test func osc7501LightsDotOnBackgroundTabUntilSelected() async throws {
    _ = NSApplication.shared
    let controller = makeTerminalController()
    controller.startSession()
    defer { controller.stopSession() }
    let first = try #require(controller.terminal)

    // A completion reported while the tab is selected is already being looked at.
    first.feed(text: oscStatus("state=done"))
    let stored = try await waitForRecords(first, .done)
    #expect(stored)
    // The status event is a separate main-queue hop; give it its turn before asserting nothing happened.
    try await Task.sleep(for: .milliseconds(200))
    #expect(controller.sessionNeedsAttention == [false])

    controller.newTerminal()
    let second = try #require(controller.terminal)
    #expect(second !== first)
    #expect(controller.sessionNeedsAttention == [false, false])

    first.feed(text: oscStatus("state=working"))
    // Let the working snapshot reach the delegate; back-to-back reports are coalesced and the
    // intermediate state would otherwise be lost, hiding the transition.
    try await Task.sleep(for: .milliseconds(300))
    first.feed(text: oscStatus("state=done"))
    let lit = try await waitForAttention(controller, [true, false])
    #expect(lit)

    // Selecting the tab shows the output and clears the dot.
    controller.selectSession(id: 1)
    #expect(controller.terminal === first)
    #expect(controller.sessionNeedsAttention == [false, false])

    // Switching away must not re-light it without a new done/blocked transition.
    controller.selectSession(id: 2)
    try await Task.sleep(for: .milliseconds(300))
    #expect(controller.sessionNeedsAttention == [false, false])

    first.feed(text: oscStatus("state=working"))
    try await Task.sleep(for: .milliseconds(300))
    first.feed(text: oscStatus("state=blocked:kind=permission"))
    let blocked = try await waitForAttention(controller, [true, false])
    #expect(blocked)
    // The window subtitle carries the same fact for a collapsed panel or a background window.
    #expect(controller.attentionSubtitle == "terminal needs permission")
    controller.selectSession(id: 1)
    #expect(controller.attentionSubtitle == nil)
}

@MainActor
@Test func transferringSessionMovesAttentionAndSelectionClearsIt() async throws {
    _ = NSApplication.shared
    let source = makeTerminalController()
    let target = makeTerminalController()
    source.startSession()
    target.startSession()
    defer { source.stopSession(); target.stopSession() }

    let first = try #require(source.terminal)
    let identifier = try #require(source.sessionIdentifiers.first)
    source.newTerminal()
    first.feed(text: oscStatus("state=working"))
    first.feed(text: oscStatus("state=done"))
    let lit = try await waitForAttention(source, [true, false])
    #expect(lit)
    #expect(source.attentionSubtitle == "terminal finished")

    source.transferSession(identifier: identifier, to: target, at: 0)
    #expect(source.sessionCount == 1)
    #expect(target.sessionCount == 2)
    #expect(target.terminal === first)
    // The attention the source window was showing left with the tab.
    #expect(source.attentionSubtitle == nil)
    // The moved session lands selected in the destination, so the usual selection rule clears
    // its dot; the tracker travels with it, so the same snapshot cannot re-light it either.
    #expect(target.sessionNeedsAttention == [false, false])
    first.feed(text: oscStatus("state=done"))
    try await Task.sleep(for: .milliseconds(300))
    #expect(target.sessionNeedsAttention == [false, false])
}

private func oscStatus(_ fields: String) -> String { "\u{1b}]7501;\(fields)\u{07}" }

@MainActor
private func waitForAttention(_ controller: OakTerminalPanelController, _ expected: [Bool]) async throws -> Bool {
    for _ in 0..<150 {
        if controller.sessionNeedsAttention == expected { return true }
        try await Task.sleep(for: .milliseconds(20))
    }
    return false
}

@MainActor
private func waitForRecords(_ terminal: LocalProcessTerminalView, _ state: TerminalProgramStatusState) async throws -> Bool {
    for _ in 0..<150 {
        if terminal.programStatusRecords.contains(where: { $0.state == state }) { return true }
        try await Task.sleep(for: .milliseconds(20))
    }
    return false
}

@MainActor
private final class TerminalToggleTarget: NSObject {
    var panelVisible = true
    @objc func toggleTerminalPanel(_ sender: Any?) { panelVisible.toggle() }
}

@MainActor
@Test func terminalPanelToggleShortcutReachesMenuFromTerminalFocus() throws {
    _ = NSApplication.shared
    let controller = makeTerminalController()
    controller.startSession()
    defer { controller.stopSession() }
    let terminal = try #require(controller.terminal)
    let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 500, height: 300), styleMask: .titled, backing: .buffered, defer: false)
    window.isReleasedWhenClosed = false
    window.contentView = controller.view
    defer { window.contentView = nil }
    window.makeFirstResponder(terminal)

    let target = TerminalToggleTarget()
    let menu = NSMenu()
    let item = menu.addItem(withTitle: "Hide Terminal Panel", action: #selector(TerminalToggleTarget.toggleTerminalPanel(_:)), keyEquivalent: "`")
    item.keyEquivalentModifierMask = [.control]
    item.target = target
    let event = try #require(NSEvent.keyEvent(with: .keyDown, location: .zero, modifierFlags: [.control], timestamp: 0, windowNumber: window.windowNumber, context: nil, characters: "\u{00}", charactersIgnoringModifiers: "`", isARepeat: false, keyCode: 50))
    #expect(!controller.handleTerminalKeyEvent(event, source: terminal))
    #expect(!controller.view.performKeyEquivalent(with: event))
    #expect(menu.performKeyEquivalent(with: event))
    #expect(!target.panelVisible)
}

@MainActor
@Test func terminalControlKeysStayWithFocusedSession() async throws {
    _ = NSApplication.shared
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    let controller = makeTerminalController()
    let delegate = TerminalTestDelegate()
    delegate.workingDirectory = folder.path
    controller.delegate = delegate
    defer { withExtendedLifetime(delegate) {} }
    controller.startSession()
    defer { controller.stopSession() }
    let terminal = try #require(controller.terminal)
    let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 500, height: 300), styleMask: .titled, backing: .buffered, defer: false)
    window.isReleasedWhenClosed = false
    window.contentView = controller.view
    defer { window.contentView = nil }
    let event = try #require(NSEvent.keyEvent(with: .keyDown, location: .zero, modifierFlags: [.control], timestamp: 0, windowNumber: window.windowNumber, context: nil, characters: "\u{0b}", charactersIgnoringModifiers: "k", isARepeat: false, keyCode: 40))
    window.makeFirstResponder(controller.view)
    #expect(!controller.handleTerminalKeyEvent(event, source: terminal))
    window.makeFirstResponder(terminal)
    terminal.send(txt: "stty -icanon -echo; printf ready > ready; dd bs=1 count=1 of=key 2>/dev/null; stty sane\n")
    for _ in 0..<100 {
        if FileManager.default.fileExists(atPath: folder.appendingPathComponent("ready").path) { break }
        try await Task.sleep(for: .milliseconds(50))
    }
    #expect(FileManager.default.fileExists(atPath: folder.appendingPathComponent("ready").path))
    #expect(controller.handleTerminalKeyEvent(event, source: terminal))
    for _ in 0..<100 {
        if let bytes = try? Data(contentsOf: folder.appendingPathComponent("key")), bytes == Data([11]) { return }
        try await Task.sleep(for: .milliseconds(50))
    }
    Issue.record("Ctrl+K did not reach the focused terminal process")
}

@MainActor
@Test func contextRegistrationFollowsTerminalLifetimeAndTransfers() throws {
    _ = NSApplication.shared
    let source = OakTerminalPanelController()
    let destination = OakTerminalPanelController()
    var owners: [UUID: String] = [:]
    var closed: [UUID] = []
    source.contextEnvironmentProvider = { owners[$0] = "source"; return [:] }
    destination.contextEnvironmentProvider = { owners[$0] = "destination"; return [:] }
    source.contextSessionClosed = { closed.append($0); owners.removeValue(forKey: $0) }
    destination.contextSessionClosed = { closed.append($0); owners.removeValue(forKey: $0) }
    source.startSession()
    let identifier = try #require(source.sessionIdentifiers.first)
    #expect(owners[identifier] == "source")
    source.transferSession(identifier: identifier, to: destination, at: 0)
    #expect(owners[identifier] == "destination")
    source.stopSession()
    #expect(closed.isEmpty)
    destination.stopSession()
    #expect(closed == [identifier])
    #expect(owners.isEmpty)
}

/// A plain sh without rc files, so results do not depend on the user's login shell, prompt plugins or dotfiles.
@MainActor
func makeTerminalController() -> OakTerminalPanelController {
    let controller = OakTerminalPanelController()
    controller.shellPath = "/bin/sh"
    controller.shellArguments = []
    return controller
}

@MainActor
final class TerminalTestDelegate: NSObject, OakTerminalPanelControllerDelegate {
    var opened: (URL, Int, Int)?
    var workingDirectory: String?
    var lastSessionClosed = 0

    func terminalPanel(_ panel: OakTerminalPanelController, openFile url: URL, line: Int, column: Int) { opened = (url, line, column) }
    func terminalPanelWorkingDirectory(_ panel: OakTerminalPanelController) -> String? { workingDirectory }
    func terminalPanelDidCloseLastSession(_ panel: OakTerminalPanelController) { lastSessionClosed += 1 }
}

@Test func fishLoginRestoresBundleDirectoryAfterStartupFiles() throws {
    let fish = OakTerminalPanelController.shellLaunchArguments(shell: "/run/current-system/sw/bin/fish", requested: nil)
    #expect(fish.prefix(2) == ["-l", "--init-command"])
    #expect(fish.count == 3 && fish[2].contains("TM_MCP_SERVER"))
    #expect(OakTerminalPanelController.shellLaunchArguments(shell: "/bin/zsh", requested: nil) == ["-l"])
    #expect(OakTerminalPanelController.shellLaunchArguments(shell: "/usr/bin/fish", requested: ["-i"]) == ["-i"])

    // The command must behave in the real shell: nix-darwin overwrites PATH, so a login fish has to put the directory back.
    guard FileManager.default.isExecutableFile(atPath: "/opt/homebrew/bin/fish") || FileManager.default.isExecutableFile(atPath: "/run/current-system/sw/bin/fish") else { return }
    let shell = FileManager.default.isExecutableFile(atPath: "/opt/homebrew/bin/fish") ? "/opt/homebrew/bin/fish" : "/run/current-system/sw/bin/fish"
    let process = Process()
    process.executableURL = URL(fileURLWithPath: shell)
    process.arguments = fish + ["-c", "echo $PATH[-1]"]
    process.environment = ["HOME": NSTemporaryDirectory(), "PATH": "/usr/bin:/bin", "TM_MCP_SERVER": "/Applications/Example Dir/textmate-mcp"]
    let pipe = Pipe()
    process.standardOutput = pipe
    try process.run()
    let output = pipe.fileHandleForReading.readDataToEndOfFile()
    process.waitUntilExit()
    #expect(String(decoding: output, as: UTF8.self).trimmingCharacters(in: .whitespacesAndNewlines) == "/Applications/Example Dir")
}
