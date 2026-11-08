import AppKit
import Darwin
import os
import SwiftTerm

// Keep the terminal's grid valid even when the outer panel is temporarily compressed to zero.
@MainActor
private final class TerminalContainer: NSView {
    weak var terminal: NSView?
    var tabBar: NSView?
    override var acceptsFirstResponder: Bool { true }
    let emptyLabel = NSTextField(labelWithString: "No terminal sessions. Click + to open one.")
    weak var controller: OakTerminalPanelController?
    override func layout() {
        super.layout()
        // The container sets the bar's frame, so its height comes from what the bar asks for.
        let barHeight = min(max(0, tabBar?.intrinsicContentSize.height ?? 0), bounds.height)
        tabBar?.frame = NSRect(x: 0, y: 0, width: bounds.width, height: barHeight)
        emptyLabel.frame = NSRect(x: 8, y: barHeight + max(0, (bounds.height - barHeight) / 2 - 10), width: max(0, bounds.width - 16), height: 20)
        let padding: CGFloat = 6
        terminal?.frame = NSRect(x: padding, y: barHeight + padding, width: max(32, bounds.width - 2 * padding), height: max(32, bounds.height - barHeight - 2 * padding))
    }
    override func resizeSubviews(withOldSize oldSize: NSSize) {
        needsLayout = true
        layoutSubtreeIfNeeded()
    }
    override func performKeyEquivalent(with event: NSEvent) -> Bool {
        // Leave the editor's tab shortcuts alone unless this pane owns the focus.
        guard event.type == .keyDown,
              let window,
              let responder = window.firstResponder as? NSView,
              responder.isDescendant(of: self),
              let controller else {
            return super.performKeyEquivalent(with: event)
        }
        // Arrow events also carry function/numeric-pad flags; those are not modifiers.
        let modifiers = event.modifierFlags.intersection([.command, .option, .control, .shift])
        if modifiers == [.command, .option] && (event.keyCode == 123 || event.keyCode == 124) {
            controller.selectAdjacentSession(forward: event.keyCode == 124)
            return true
        }
        if modifiers == [.command] && event.charactersIgnoringModifiers == "k" && controller.terminal != nil {
            controller.clearTerminal()
            return true
        }
        return super.performKeyEquivalent(with: event)
    }
}

@MainActor
@objc public final class OakTerminalPanelController: NSViewController, LocalProcessTerminalViewDelegate {
    private struct Session {
        let id: Int
        let uuid: UUID
        let terminal: LocalProcessTerminalView
        var title: String
        var attention = TerminalAttention()
        var agentStatus = TerminalAgentStatus()
    }
    private var sessions: [Session] = []
    private var selectedID: Int?
    private var nextID = 1
    private static func defaultTitle(forID id: Int) -> String { "Terminal \(id)" }
    var terminal: LocalProcessTerminalView? { sessions.first { $0.id == selectedID }?.terminal }
    var sessionCount: Int { sessions.count }
    @objc public var sessionIdentifiers: [UUID] { sessions.map(\.uuid) }
    /// Unseen OSC 7501 completions, parallel to `sessionIdentifiers`; drives the tab bar dot.
    @objc public var sessionNeedsAttention: [Bool] { sessions.map(\.attention.needsAttention) }
    /// Per-tab hover tooltip for that dot, parallel to `sessionIdentifiers`; "" means no tooltip.
    @objc public var sessionToolTips: [String] { sessions.map(\.attention.toolTip) }
    /// Worst unseen state across this panel's sessions, as window subtitle text; nil when nothing needs the user.
    @objc public var attentionSubtitle: String? { TerminalAttention.subtitle(for: sessions.map(\.attention)) }
    @objc public var sessionTitles: [String] { sessions.map(\.title) }
    @objc public var sessionStatusValues: [NSNumber] { sessions.map { NSNumber(value: $0.agentStatus.value) } }
    @objc public var sessionStatusDescriptions: [String] { sessions.map { $0.agentStatus.description } }
    @objc public var selectedSessionIndex: Int { sessions.firstIndex { $0.id == selectedID } ?? NSNotFound }
    @objc public var tabsDidChange: (() -> Void)?
    @objc public weak var delegate: OakTerminalPanelControllerDelegate?

    @objc public func installTabBar(_ tabBar: NSView) {
        guard let container = view as? TerminalContainer else { return }
        container.tabBar?.removeFromSuperview()
        container.tabBar = tabBar
        container.addSubview(tabBar)
        container.needsLayout = true
        rebuildTabs()
    }

    @objc public func selectSession(at index: Int) {
        guard sessions.indices.contains(index) else { return }
        acknowledgeResults(at: index)
        selectSession(id: sessions[index].id)
        focusTerminal()
    }

    @objc public func moveSession(from source: Int, to destination: Int) {
        guard sessions.indices.contains(source), (0...sessions.count).contains(destination) else { return }
        let session = sessions.remove(at: source)
        sessions.insert(session, at: destination > source ? destination - 1 : destination)
        rebuildTabs()
    }

    @objc public func transferSession(identifier: UUID, to destination: OakTerminalPanelController, at insertionIndex: Int) {
        guard destination !== self,
              let index = sessions.firstIndex(where: { $0.uuid == identifier }) else { return }
        let session = sessions.remove(at: index)
        session.terminal.removeFromSuperview()
        if selectedID == session.id {
            selectedID = sessions.isEmpty ? nil : sessions[min(index, sessions.count - 1)].id
        }
        showSelectedSession()
        let newID = destination.nextID
        destination.nextID += 1
        destination.started = true
        session.terminal.processDelegate = destination
        (session.terminal as? FileLinkTerminalView)?.owner = destination
        destination.sessions.insert(Session(id: newID, uuid: session.uuid, terminal: session.terminal, title: session.title, attention: session.attention, agentStatus: session.agentStatus), at: min(max(0, insertionIndex), destination.sessions.count))
        _ = destination.contextEnvironmentProvider?(session.uuid)
        destination.applyFont(destination.terminalFont)
        destination.updateTerminalColors()
        destination.selectSession(id: newID)
        destination.focusTerminal()
    }

    @objc public func closeSessions(withIdentifiers identifiers: [UUID]) {
        let closing = sessions.filter { identifiers.contains($0.uuid) }
        confirmClose(terminals: closing.map(\.terminal), in: nil) { [weak self] canClose in
            guard canClose, let self else { return }
            // The confirmation sheet is asynchronous; a session may have exited while it was up.
            for session in closing { self.removeSession(id: session.id) }
        }
    }
    func requestOpenFile(_ url: URL, line: Int, column: Int) {
        delegate?.terminalPanel(self, openFile: url, line: line, column: column)
    }
    /// Replaces the account's login shell; used by tests so they do not depend on the user's rc files.
    @objc public var shellPath: String?
    @objc public var shellArguments: [String]?
    @objc public var contextEnvironmentProvider: ((UUID) -> [String: String])?
    @objc public var contextSessionClosed: ((UUID) -> Void)?
    private var terminalFont = NSFont.monospacedSystemFont(ofSize: 13, weight: .regular)

    @objc public func applyFont(_ font: NSFont) {
        terminalFont = font
        for session in sessions where session.terminal.font != font {
            session.terminal.font = font
        }
    }

    private var started = false
    private var theme: OakTerminalTheme?

    @objc public func applyTheme(_ theme: OakTerminalTheme) {
        self.theme = theme
        updateTerminalColors()
    }

    private func updateTerminalColors() {
        guard let colors = theme else { return }
        if isViewLoaded {
            view.layer?.backgroundColor = colors.background.cgColor
            (view as? TerminalContainer)?.emptyLabel.textColor = colors.foreground
        }
        let palette = terminalPalette(background: colors.background, foreground: colors.foreground, syntaxColors: colors.syntaxColors)
        for session in sessions {
            let terminal = session.terminal
            terminal.installColors(palette)
            terminal.nativeBackgroundColor = colors.background
            terminal.nativeForegroundColor = colors.foreground
            terminal.caretColor = colors.caret
            terminal.caretTextColor = colors.background
            terminal.selectedTextBackgroundColor = colors.selection
            terminal.selectedTextForegroundColor = colors.foreground
            terminal.needsDisplay = true
        }
    }

    public override func loadView() {
        let container = TerminalContainer(frame: NSRect(x: 0, y: 0, width: 320, height: 220))
        container.wantsLayer = true
        container.layer?.masksToBounds = true
        container.controller = self
        view = container
        container.emptyLabel.alignment = .center
        container.emptyLabel.lineBreakMode = .byTruncatingTail
        container.addSubview(container.emptyLabel)
        NotificationCenter.default.addObserver(self, selector: #selector(stopSession), name: NSApplication.willTerminateNotification, object: nil)
    }

    @objc public func startSession() {
        guard !started else { return }
        started = true
        launchShell()
    }

    @objc public func newTerminal() {
        launchShell()
        focusTerminal()
    }

    /// Arguments for the login shell. nix-darwin's fish setup overwrites PATH from its own list, which drops the
    /// bundle directory appended to the environment, so fish re-appends it from TM_MCP_SERVER once its startup files have run.
    nonisolated static func shellLaunchArguments(shell: String, requested: [String]?) -> [String] {
        if let requested { return requested }
        guard URL(fileURLWithPath: shell).lastPathComponent == "fish" else { return ["-l"] }
        let restore = "if set -q TM_MCP_SERVER; set -l d (path dirname $TM_MCP_SERVER); contains -- $d $PATH; or set -gxa PATH $d; end"
        return ["-l", "--init-command", restore]
    }

    private static func existingDirectory(_ path: String?) -> String {
        var isDirectory: ObjCBool = false
        guard let path, FileManager.default.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue else { return NSHomeDirectory() }
        return path
    }

    private func launchShell() {
        _ = view
        let launchDirectory = Self.existingDirectory(delegate?.terminalPanelWorkingDirectory?(self))
        let terminal = FileLinkTerminalView(frame: NSRect(x: 0, y: 0, width: max(32, view.bounds.width), height: max(32, view.bounds.height)))
        terminal.owner = self
        terminal.launchDirectory = launchDirectory
        // SwiftTerm calls the handler on its IO thread once per output batch; one main-queue hop is enough for a burst.
        let hopPending = OSAllocatedUnfairLock(initialState: false)
        terminal.setProcessOutputHandler { [weak terminal] in
            guard !hopPending.withLock({ pending in defer { pending = true }; return pending }) else { return }
            DispatchQueue.main.async {
                // Cleared before scheduling, so a batch that lands during the capture queues another hop.
                hopPending.withLock { $0 = false }
                terminal?.scheduleOutputCapture()
            }
        }
        terminal.font = terminalFont
        terminal.processDelegate = self
        terminal.setAccessibilityLabel("Terminal")
        let id = nextID
        nextID += 1
        let sessionUUID = UUID()
        sessions.append(Session(id: id, uuid: sessionUUID, terminal: terminal, title: Self.defaultTitle(forID: id)))
        updateTerminalColors()
        selectSession(id: id)

        let configuredShell = shellPath ?? getpwuid(getuid()).flatMap { $0.pointee.pw_shell }.map { String(cString: $0) } ?? "/bin/zsh"
        let shell = FileManager.default.isExecutableFile(atPath: configuredShell) ? configuredShell : "/bin/zsh"
        var environment = delegate?.terminalPanel?(self, environmentForDirectory: launchDirectory) ?? ProcessInfo.processInfo.environment
        // A Dock or Finder launch has no locale; without one the shell runs in the C locale and mangles non-ASCII text.
        if environment["LC_ALL"] == nil && environment["LC_CTYPE"] == nil && environment["LANG"] == nil {
            environment["LC_CTYPE"] = "en_US.UTF-8"
        }
        environment["TERM"] = "xterm-256color"
        environment["COLORTERM"] = "truecolor"
        environment["TERM_PROGRAM"] = "TextMate"
        environment["SHELL"] = shell
        environment["PWD"] = launchDirectory
        for key in Array(environment.keys) where key.hasPrefix("TM_CONTEXT_") {
            environment.removeValue(forKey: key)
        }
        environment.removeValue(forKey: "TM_MCP_SERVER")
        environment.merge(contextEnvironmentProvider?(sessionUUID) ?? [:]) { _, new in new }
        if let executable = Bundle.main.executableURL?.deletingLastPathComponent().appendingPathComponent("textmate-mcp").path,
           FileManager.default.isExecutableFile(atPath: executable) {
            environment["TM_MCP_SERVER"] = executable
            let directory = URL(fileURLWithPath: executable).deletingLastPathComponent().path
            // Appended: the directory also holds mate and tm_query, which must not shadow tools the user installed.
            environment["PATH"] = (environment["PATH"] ?? "/usr/bin:/bin:/usr/sbin:/sbin") + ":" + directory
        }
        // Do not propagate the debug app's injected runtime into shell commands.
        for key in Array(environment.keys) where key.hasPrefix("DYLD_") || key == "ASAN_OPTIONS" {
            environment.removeValue(forKey: key)
        }
        terminal.startProcess(executable: shell, args: Self.shellLaunchArguments(shell: shell, requested: shellArguments), environment: environment.map { "\($0.key)=\($0.value)" }, currentDirectory: launchDirectory)
    }

    // Run before AppKit's key-equivalent search: the editor and global bundle menu
    // can otherwise claim a terminal shortcut before the focused view sees it.
    func handleTerminalKeyEvent(_ event: NSEvent, source: LocalProcessTerminalView) -> Bool {
        guard event.type == .keyDown, let window = source.window,
              event.window === window, window.firstResponder === source,
              terminal === source else { return false }
        if let index = sessions.firstIndex(where: { $0.terminal === source }) { acknowledgeResults(at: index) }
        let modifiers = event.modifierFlags.intersection([.command, .option, .control, .shift])
        // The app menu owns Ctrl+` so the panel can also be hidden from terminal focus.
        if modifiers == [.control] && event.charactersIgnoringModifiers == "`" { return false }
        if modifiers.contains(.control) && !modifiers.contains(.command) {
            source.keyDown(with: event)
            return true
        }
        if modifiers == [.command] && event.charactersIgnoringModifiers == "k" {
            clearTerminal()
            return true
        }
        if modifiers == [.command, .option] && (event.keyCode == 123 || event.keyCode == 124) {
            selectAdjacentSession(forward: event.keyCode == 124)
            return true
        }
        return false
    }

    @objc public func focusTerminal() {
        view.window?.makeFirstResponder(terminal ?? view)
    }

    /// Clears the display and scrollback, then has the shell redraw its prompt.
    @objc public func clearTerminal() {
        guard let terminal else { return }
        (terminal as? FileLinkTerminalView)?.clearFileLinkHistory()
        terminal.feed(text: "\u{1b}[2J\u{1b}[3J")
        // Only the shell can reprint its prompt; Ctrl+L asks it to redraw at the top.
        terminal.send(txt: "\u{0c}")
    }

    @objc public var hasRunningWork: Bool { sessions.contains { hasRunningWork(in: $0.terminal) } }

    private func hasRunningWork(in terminal: LocalProcessTerminalView) -> Bool {
        guard let process = terminal.process, process.running, process.shellPid > 0 else { return false }
        // Only a foreground job counts. Background children are not distinguishable from prompt helpers
        // (gitstatusd, direnv, async prompt workers), which would make every close or quit ask.
        let foreground = tcgetpgrp(process.childfd)
        return foreground > 0 && foreground != process.shellPid
    }

    /// Asks, as a sheet on the given window, before ending sessions that run a foreground program; calls back immediately when nothing is running.
    @objc public func confirmClose(in window: NSWindow?, completion: @escaping (Bool) -> Void) {
        confirmClose(terminals: sessions.map(\.terminal), in: window, completion: completion)
    }

    private func confirmClose(terminals: [LocalProcessTerminalView], in window: NSWindow?, completion: @escaping (Bool) -> Void) {
        guard terminals.contains(where: { hasRunningWork(in: $0) }) else { return completion(true) }
        let alert = NSAlert()
        alert.messageText = "Close terminal sessions?"
        alert.informativeText = "Programs are still running in the terminal sessions being closed. Closing them will end those sessions."
        alert.addButton(withTitle: "Cancel")
        alert.addButton(withTitle: "Close")
        if let window = window ?? view.window, window.isVisible {
            alert.beginSheetModal(for: window) { completion($0 == .alertSecondButtonReturn) }
        } else {
            completion(alert.runModal() == .alertSecondButtonReturn)
        }
    }

    @objc public func stopSession() {
        for session in sessions {
            contextSessionClosed?(session.uuid)
            stop(terminal: session.terminal)
        }
        sessions.removeAll()
        selectedID = nil
        if isViewLoaded { showSelectedSession() }
    }

    private func stop(terminal: LocalProcessTerminalView) {
        terminal.processDelegate = nil
        if let process = terminal.process, process.running, process.shellPid > 0 {
            let foreground = tcgetpgrp(process.childfd)
            if foreground > 0 && foreground != getpgrp() { kill(-foreground, SIGHUP) }
            kill(process.shellPid, SIGHUP)
            terminal.terminate()
        }
        terminal.removeFromSuperview()
    }

    func selectAdjacentSession(forward: Bool) {
        guard let index = sessions.firstIndex(where: { $0.id == selectedID }), !sessions.isEmpty else { return }
        let next = (index + (forward ? 1 : sessions.count - 1)) % sessions.count
        acknowledgeResults(at: next)
        selectSession(id: sessions[next].id)
        focusTerminal()
    }

    func selectSession(id: Int) {
        guard let index = sessions.firstIndex(where: { $0.id == id }) else { return }
        selectedID = id
        let hadAttention = sessions[index].attention.needsAttention
        sessions[index].attention.clear()
        showSelectedSession()
        // OakTabBarView only re-reads the dot in reloadData, so clearing it needs a repaint here.
        if hadAttention {
            rebuildTabs()
        }
    }

    private func showSelectedSession() {
        guard let container = view as? TerminalContainer else { return }
        let hadFocus = (view.window?.firstResponder as? NSView)?.isDescendant(of: view) == true
        container.terminal?.removeFromSuperview()
        container.terminal = terminal
        if let terminal { container.addSubview(terminal, positioned: .below, relativeTo: container.tabBar) }
        container.emptyLabel.isHidden = terminal != nil
        rebuildTabs()
        container.needsLayout = true
        container.layoutSubtreeIfNeeded()
        if hadFocus { focusTerminal() }
    }

    private func rebuildTabs() { tabsDidChange?() }

    /// Test hook: raises or clears one session's unseen flag without a program reporting status, so the
    /// surrounding plumbing can be tested without spawning a child that speaks OSC 7501.
    @objc public func simulateSessionAttention(at index: Int, state: String?, kind: String?) {
        guard sessions.indices.contains(index) else { return }
        let raised = state.flatMap(TerminalProgramStatusState.init(rawValue:))
        sessions[index].attention.force(raised, blockedKind: kind.flatMap(TerminalProgramStatusKind.init(rawValue:)))
        rebuildTabs()
    }

    private func removeSession(id: Int) {
        guard let index = sessions.firstIndex(where: { $0.id == id }) else { return }
        let wasSelected = selectedID == id
        let hadFocus = (view.window?.firstResponder as? NSView)?.isDescendant(of: view) == true
        contextSessionClosed?(sessions[index].uuid)
        stop(terminal: sessions[index].terminal)
        sessions.remove(at: index)
        if wasSelected { selectedID = sessions.isEmpty ? nil : sessions[min(index, sessions.count - 1)].id }
        showSelectedSession()
        if wasSelected && hadFocus { focusTerminal() }
        if sessions.isEmpty { delegate?.terminalPanelDidCloseLastSession?(self) }
    }

    private func acknowledgeResults(at index: Int) {
        let previous = sessions[index].agentStatus.value
        sessions[index].agentStatus.acknowledgeResults()
        if previous != sessions[index].agentStatus.value { rebuildTabs() }
    }

    public func programStatusChanged(source: TerminalView, records: [TerminalProgramStatus]) {
        guard let index = sessions.firstIndex(where: { $0.terminal === source }) else { return }
        sessions[index].agentStatus.update(records)
        _ = sessions[index].attention.update(records: records, isSelected: sessions[index].id == selectedID)
        rebuildTabs()
    }

    public func sizeChanged(source: LocalProcessTerminalView, newCols: Int, newRows: Int) {}
    public func setTerminalTitle(source: LocalProcessTerminalView, title: String) {
        guard let index = sessions.firstIndex(where: { $0.terminal === source }) else { return }
        let title = title.components(separatedBy: .controlCharacters).joined().trimmingCharacters(in: .whitespacesAndNewlines)
        sessions[index].title = title.isEmpty ? Self.defaultTitle(forID: sessions[index].id) : String(title.prefix(120))
        rebuildTabs()
    }
    public func hostCurrentDirectoryUpdate(source: TerminalView, directory: String?) {
        guard let terminal = source as? FileLinkTerminalView, let directory else { return }
        if let url = URL(string: directory), url.isFileURL,
           LocalHost.isLocal(url.host) {
            terminal.reportedDirectory = url.path
        } else if directory.hasPrefix("/") {
            terminal.reportedDirectory = directory
        }
    }
    public func processTerminated(source: TerminalView, exitCode: Int32?) {
        guard let session = sessions.first(where: { $0.terminal === source }) else { return }
        removeSession(id: session.id)
    }
    public func processFailedToStart(source: TerminalView, error: LocalProcessError) {
        // Keep the failed tab so SwiftTerm's launch error remains visible; it can be closed with ×.
        guard let index = sessions.firstIndex(where: { $0.terminal === source }) else { return }
        sessions[index].title = "Shell failed to start"
        rebuildTabs()
    }
}
