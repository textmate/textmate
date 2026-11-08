import AppKit
import Darwin
import Testing
@testable import OakSwiftUI

@Test func terminalFileReferenceParsing() throws {
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    for name in ["main.swift", "file with spaces.swift", "日本語.swift", ".hidden"] {
        try "test".write(to: folder.appendingPathComponent(name), atomically: true, encoding: .utf8)
    }
    let reference = try #require(TerminalFileReference.resolve("main.swift:42:7:", directory: folder.path))
    #expect(reference.url.path == folder.appendingPathComponent("main.swift").path)
    #expect(reference.line == 42)
    #expect(reference.column == 7)
    #expect(TerminalFileReference.resolve(folder.appendingPathComponent("file with spaces.swift").absoluteString, directory: "/") != nil)
    #expect(TerminalFileReference.resolve("file://remote.invalid" + folder.appendingPathComponent("main.swift").path, directory: folder.path) == nil)
    #expect(TerminalFileReference.resolve("missing.swift:9", directory: folder.path) == nil)
    #expect(TerminalFileReference.resolve(folder.path, directory: folder.path) == nil)
    #expect(TerminalFileReference.resolve("https://example.com/main.swift", directory: folder.path) == nil)
    let matches = TerminalFileReference.matches(in: "error: \"file with spaces.swift\":12:3 and 日本語.swift:8, .hidden", directory: folder.path)
    #expect(matches.count == 3)
    #expect(matches.first?.1.line == 12)
    #expect(matches.first?.1.column == 3)
}

@MainActor
@Test func terminalFileClicksAndDirectoryHistory() async throws {
    _ = NSApplication.shared
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    let other = folder.appendingPathComponent("other")
    try FileManager.default.createDirectory(at: other, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    for parent in [folder, other] {
        try "test".write(to: parent.appendingPathComponent("main.swift"), atomically: true, encoding: .utf8)
    }
    let owner = makeTerminalController()
    let terminal = FileLinkTerminalView(frame: NSRect(x: 0, y: 0, width: 600, height: 300))
    terminal.owner = owner
    terminal.launchDirectory = folder.path
    let window = NSWindow(contentRect: terminal.frame, styleMask: .titled, backing: .buffered, defer: false)
    window.isReleasedWhenClosed = false
    window.contentView = terminal
    defer { window.contentView = nil }
    terminal.feed(text: "main.swift:42:7\r\n")
    for _ in 0..<100 {
        if terminal.terminalContentSnapshot(region: .viewport)?.rows.first?.text.contains("main.swift") == true { break }
        try await Task.sleep(for: .milliseconds(10))
    }
    terminal.noteDirectoryIfChanged()
    terminal.reportedDirectory = other.path
    terminal.noteDirectoryIfChanged()
    let size = terminal.getWindowSize()
    let scale = window.backingScaleFactor
    let cellWidth = CGFloat(size.ws_xpixel) / CGFloat(size.ws_col) / scale
    let cellHeight = CGFloat(size.ws_ypixel) / CGFloat(size.ws_row) / scale
    let point = NSPoint(x: cellWidth * 2.5, y: terminal.bounds.maxY - cellHeight / 2)
    let hit = try #require(terminal.fileHit(at: point))
    #expect(hit.reference.url.path == folder.appendingPathComponent("main.swift").path)
    #expect(hit.reference.line == 42)
    #expect(hit.reference.column == 7)
    let delegate = TerminalTestDelegate()
    owner.delegate = delegate
    func event(_ type: NSEvent.EventType, modifiers: NSEvent.ModifierFlags) throws -> NSEvent {
        try #require(NSEvent.mouseEvent(with: type, location: terminal.convert(point, to: nil), modifierFlags: modifiers, timestamp: 0, windowNumber: window.windowNumber, context: nil, eventNumber: 0, clickCount: 1, pressure: 1))
    }
    terminal.mouseDown(with: try event(.leftMouseDown, modifiers: []))
    terminal.mouseUp(with: try event(.leftMouseUp, modifiers: []))
    #expect(delegate.opened == nil)
    terminal.mouseDown(with: try event(.leftMouseDown, modifiers: [.command]))
    terminal.mouseUp(with: try event(.leftMouseUp, modifiers: [.command]))
    #expect(delegate.opened?.0 == hit.reference.url)
    #expect(delegate.opened?.1 == 42)
    #expect(delegate.opened?.2 == 7)
    terminal.requestOpenLink(source: terminal, link: other.appendingPathComponent("main.swift").absoluteString, params: [:])
    #expect(delegate.opened?.0.path == other.appendingPathComponent("main.swift").path)
    delegate.opened = nil
    terminal.feed(text: "\u{1b}[2J\u{1b}[H\u{1b}]8;;\(other.appendingPathComponent("main.swift").absoluteString)\u{1b}\\main.swift\u{1b}]8;;\u{1b}\\")
    try await Task.sleep(for: .milliseconds(100))
    terminal.mouseDown(with: try event(.leftMouseDown, modifiers: [.command]))
    terminal.mouseUp(with: try event(.leftMouseUp, modifiers: [.command]))
    #expect(delegate.opened?.0.path == other.appendingPathComponent("main.swift").path)
}

@MainActor
@Test func terminalTracksShellDirectoryAfterCD() async throws {
    _ = NSApplication.shared
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    let controller = makeTerminalController()
    controller.startSession()
    defer { controller.stopSession() }
    let terminal = try #require(controller.terminal as? FileLinkTerminalView)
    terminal.send(txt: "cd '\(folder.path)'\n")
    for _ in 0..<100 {
        if URL(fileURLWithPath: terminal.currentDirectory).resolvingSymlinksInPath() == folder.resolvingSymlinksInPath() { return }
        try await Task.sleep(for: .milliseconds(50))
    }
    Issue.record("Terminal did not follow the shell directory after cd")
}

@MainActor
@Test func terminalFileLinksAcrossWrappedRows() async throws {
    _ = NSApplication.shared
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    let name = String(repeating: "a", count: 90) + ".swift"
    try "test".write(to: folder.appendingPathComponent(name), atomically: true, encoding: .utf8)
    let terminal = FileLinkTerminalView(frame: NSRect(x: 0, y: 0, width: 320, height: 300))
    terminal.launchDirectory = folder.path
    terminal.feed(text: name + ":12:3\r\n")
    for _ in 0..<100 {
        if terminal.terminalContentSnapshot(region: .viewport)?.rows.contains(where: { $0.text.contains("swift") }) == true { break }
        try await Task.sleep(for: .milliseconds(10))
    }
    let size = terminal.getWindowSize()
    let scale = NSScreen.main?.backingScaleFactor ?? 1
    let height = CGFloat(size.ws_ypixel) / CGFloat(size.ws_row) / scale
    let hit = try #require(terminal.fileHit(at: NSPoint(x: 5, y: terminal.bounds.maxY - height * 1.5)))
    #expect(hit.reference.url.lastPathComponent == name)
    #expect(hit.reference.line == 12)
    #expect(hit.reference.column == 3)
    #expect(Set(hit.rects.map(\.minY)).count > 1)
}

@Test func localHostMatchesShortAndQualifiedNames() {
    var buffer = [CChar](repeating: 0, count: 256)
    gethostname(&buffer, buffer.count - 1)
    let short = String(cString: buffer).split(separator: ".").first.map(String.init) ?? ""
    #expect(LocalHost.isLocal(nil))
    #expect(LocalHost.isLocal(""))
    #expect(LocalHost.isLocal("LocalHost"))
    #expect(LocalHost.isLocal(short))
    #expect(LocalHost.isLocal("\(short.uppercased()).example.invalid"))
    #expect(!LocalHost.isLocal("remote.invalid"))
}

@MainActor
@Test func terminalOptsOutOfEditorKeyEquivalents() {
    let terminal = FileLinkTerminalView(frame: .zero)
    #expect(terminal.responds(to: NSSelectorFromString("oak_ownsKeyEquivalents")))
}

@MainActor
@Test func directoryMarkersRecordOnlyChanges() async throws {
    let terminal = FileLinkTerminalView(frame: NSRect(x: 0, y: 0, width: 600, height: 300))
    terminal.launchDirectory = "/a"
    let columns = terminal.terminalDimensions.cols
    terminal.feed(text: "one\r\ntwo\r\n")
    terminal.noteDirectoryIfChanged()
    terminal.noteDirectoryIfChanged()
    #expect(terminal.recordedDirectory(forRow: 0, columns: columns) == "/a")

    terminal.reportedDirectory = "/b"
    terminal.feed(text: "three\r\n")
    terminal.noteDirectoryIfChanged()
    terminal.feed(text: "four\r\n")
    terminal.noteDirectoryIfChanged() // unchanged: must not add a marker that would move the boundary
    #expect(terminal.recordedDirectory(forRow: 1, columns: columns) == "/a")
    #expect(terminal.recordedDirectory(forRow: 2, columns: columns) == "/a")
    #expect(terminal.recordedDirectory(forRow: 3, columns: columns) == "/b")
    #expect(terminal.recordedDirectory(forRow: 4, columns: columns) == "/b")
    #expect(terminal.recordedDirectory(forRow: 0, columns: columns + 1) == nil) // resized since recording
}

/// A terminal that has printed more lines than it holds and then changed directory, with a `main.swift` in both directories.
@MainActor
private func makeTerminalAfterDirectoryChange(launch: URL, other: URL, window: NSWindow) async throws -> FileLinkTerminalView {
    for parent in [launch, other] {
        try "test".write(to: parent.appendingPathComponent("main.swift"), atomically: true, encoding: .utf8)
    }
    let terminal = FileLinkTerminalView(frame: NSRect(x: 0, y: 0, width: 600, height: 300))
    terminal.launchDirectory = launch.path
    window.contentView = terminal
    terminal.feed(text: (1...200).map { "line \($0)\r\n" }.joined())
    for _ in 0..<100 {
        if terminal.terminalContentSnapshot(region: .viewport)?.rows.contains(where: { $0.text.contains("line 200") }) == true { break }
        try await Task.sleep(for: .milliseconds(10))
    }
    terminal.noteDirectoryIfChanged()
    terminal.reportedDirectory = other.path
    terminal.noteDirectoryIfChanged()
    return terminal
}

/// Looks for a link on the terminal's first visible row, after waiting for `main.swift` to be drawn there.
@MainActor
private func firstRowHit(in terminal: FileLinkTerminalView) async throws -> (reference: TerminalFileReference, rects: [NSRect])? {
    for _ in 0..<100 {
        if terminal.terminalContentSnapshot(region: .viewport)?.rows.first?.text.contains("main.swift") == true { break }
        try await Task.sleep(for: .milliseconds(10))
    }
    let size = terminal.getWindowSize()
    let scale = terminal.window?.backingScaleFactor ?? 1
    let cellWidth = CGFloat(size.ws_xpixel) / CGFloat(size.ws_col) / scale
    let cellHeight = CGFloat(size.ws_ypixel) / CGFloat(size.ws_row) / scale
    return terminal.fileHit(at: NSPoint(x: cellWidth * 2.5, y: terminal.bounds.maxY - cellHeight / 2))
}

@MainActor
@Test func clearingScrollbackDropsStaleDirectoryMarkers() async throws {
    _ = NSApplication.shared
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    let other = folder.appendingPathComponent("other")
    try FileManager.default.createDirectory(at: other, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 600, height: 300), styleMask: .titled, backing: .buffered, defer: false)
    window.isReleasedWhenClosed = false
    defer { window.contentView = nil }
    let terminal = try await makeTerminalAfterDirectoryChange(launch: folder, other: other, window: window)

    // `clear` erases the screen and the scrollback, which renumbers every remaining row from zero.
    terminal.feed(text: "\u{1b}[2J\u{1b}[3J\u{1b}[Hmain.swift\r\n")
    let hit = try #require(try await firstRowHit(in: terminal))
    #expect(hit.reference.url.path == other.appendingPathComponent("main.swift").path)
}

@MainActor
@Test func eraseInPlaceKeepsDirectoryMarkers() async throws {
    _ = NSApplication.shared
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    let other = folder.appendingPathComponent("other")
    try FileManager.default.createDirectory(at: other, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 600, height: 300), styleMask: .titled, backing: .buffered, defer: false)
    window.isReleasedWhenClosed = false
    defer { window.contentView = nil }
    let terminal = try await makeTerminalAfterDirectoryChange(launch: folder, other: other, window: window)

    // Ctrl-L moves the cursor to the top without renumbering rows, so the top row still predates the directory change.
    terminal.feed(text: "\u{1b}[H\u{1b}[2Jmain.swift\r\n")
    let hit = try #require(try await firstRowHit(in: terminal))
    #expect(hit.reference.url.path == folder.appendingPathComponent("main.swift").path)
}

private func sameDirectory(_ a: String?, _ b: String) -> Bool {
    guard let a else { return false }
    return URL(fileURLWithPath: a).resolvingSymlinksInPath() == URL(fileURLWithPath: b).resolvingSymlinksInPath()
}

private func childPid(of parent: pid_t) async throws -> pid_t? {
    for _ in 0..<100 {
        var children = [pid_t](repeating: 0, count: 8)
        let count = proc_listchildpids(parent, &children, Int32(children.count * MemoryLayout<pid_t>.size))
        if count > 0, children[0] > 0 { return children[0] }
        try await Task.sleep(for: .milliseconds(50))
    }
    return nil
}

@MainActor
@Test func shellDirectoryFollowsNestedShellWhileItRunsACommand() async throws {
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    // This test process stands in for the login shell: it is the root of the ancestry that is walked.
    let nested = Process()
    nested.executableURL = URL(fileURLWithPath: "/bin/bash")
    nested.arguments = ["--norc", "--noprofile", "-c", "cd '\(folder.path)' && /bin/sleep 10; true"]
    try nested.run()
    defer { nested.terminate() }
    let command = try #require(try await childPid(of: nested.processIdentifier))
    let login = getpid()

    #expect(FileLinkTerminalView.isShell(pid: nested.processIdentifier))
    #expect(!FileLinkTerminalView.isShell(pid: command))
    #expect(sameDirectory(FileLinkTerminalView.shellDirectory(foreground: command, login: login), folder.path))
    #expect(sameDirectory(FileLinkTerminalView.shellDirectory(foreground: nested.processIdentifier, login: login), folder.path))
}

@MainActor
@Test func shellDirectoryFallsBackToLoginShell() async throws {
    let login = getpid()
    let loginDirectory = try #require(FileLinkTerminalView.processDirectory(pid: login))
    let command = Process()
    command.executableURL = URL(fileURLWithPath: "/bin/sleep")
    command.arguments = ["10"]
    try command.run()
    defer { command.terminate() }
    // A foreground program started directly by the login shell, no shell between them.
    #expect(sameDirectory(FileLinkTerminalView.shellDirectory(foreground: command.processIdentifier, login: login), loginDirectory))
    #expect(sameDirectory(FileLinkTerminalView.shellDirectory(foreground: login, login: login), loginDirectory))
    // An unreadable foreground group (a pipeline leader that already exited).
    #expect(sameDirectory(FileLinkTerminalView.shellDirectory(foreground: 0x7ffffff0, login: login), loginDirectory))
    #expect(FileLinkTerminalView.shellDirectory(foreground: 1, login: 0) == nil)
}

@MainActor
@Test func terminalTracksNestedShellDirectory() async throws {
    _ = NSApplication.shared
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    let controller = makeTerminalController()
    controller.startSession()
    defer { controller.stopSession() }
    let terminal = try #require(controller.terminal as? FileLinkTerminalView)
    let loginDirectory = terminal.currentDirectory

    terminal.send(txt: "bash --norc --noprofile\n")
    terminal.send(txt: "cd '\(folder.path)'\n")
    var followed = false
    for _ in 0..<100 {
        if sameDirectory(terminal.currentDirectory, folder.path) { followed = true; break }
        try await Task.sleep(for: .milliseconds(50))
    }
    #expect(followed)
    #expect(!sameDirectory(loginDirectory, folder.path))

    // The nested shell's directory must survive a foreground command, which has its own process group.
    terminal.send(txt: "sleep 3\n")
    for _ in 0..<100 {
        if tcgetpgrp(terminal.process.childfd) != terminal.process.shellPid && controller.hasRunningWork { break }
        try await Task.sleep(for: .milliseconds(50))
    }
    #expect(controller.hasRunningWork)
    #expect(sameDirectory(terminal.currentDirectory, folder.path))
}

@Test func terminalFileReferenceMatchOnlyResolvesTokenUnderOffset() throws {
    let folder = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: folder) }
    for name in ["main.swift", "other.swift"] {
        try "test".write(to: folder.appendingPathComponent(name), atomically: true, encoding: .utf8)
    }
    let text = "see main.swift and missing.swift, other.swift:3."
    func offset(of needle: String, plus extra: Int = 0) -> Int { (text as NSString).range(of: needle).location + extra }

    let main = try #require(TerminalFileReference.match(in: text, at: offset(of: "main.swift", plus: 2), directory: folder.path))
    #expect(main.1.url.lastPathComponent == "main.swift")
    #expect((text as NSString).substring(with: main.0) == "main.swift")
    #expect(TerminalFileReference.match(in: text, at: offset(of: "missing", plus: 2), directory: folder.path) == nil)
    #expect(TerminalFileReference.match(in: text, at: 1, directory: folder.path) == nil)
    let other = try #require(TerminalFileReference.match(in: text, at: offset(of: "other.swift", plus: 3), directory: folder.path))
    #expect(other.1.line == 3)
    // The trailing sentence period is not part of the reference.
    #expect(TerminalFileReference.match(in: text, at: text.count - 1, directory: folder.path) == nil)
}
