import AppKit
import Darwin
import SwiftTerm

enum LocalHost {
    // ProcessInfo.hostName does a reverse-DNS lookup that can block for seconds, and this runs on every OSC 7 report.
    private static let shortNames: Set<String> = {
        var buffer = [CChar](repeating: 0, count: 256)
        gethostname(&buffer, buffer.count - 1)
        return [shortName(String(cString: buffer))]
    }()

    private static func shortName(_ host: String) -> String {
        host.lowercased().split(separator: ".", maxSplits: 1).first.map(String.init) ?? ""
    }

    static func isLocal(_ host: String?) -> Bool {
        guard let host, !host.isEmpty else { return true }
        return host.lowercased() == "localhost" || shortNames.contains(shortName(host))
    }
}

struct TerminalFileReference: Equatable {
    // Quoted paths may contain spaces; unquoted paths terminate at shell/output delimiters.
    private static let tokenRegex = try! NSRegularExpression(pattern: #"(?:"[^"\r\n]+"|'[^'\r\n]+')(?::[1-9][0-9]*(?::[1-9][0-9]*)?)?|[^\s<>\[\](){}"'`,;]+"#)

    let url: URL
    let line: Int?
    let column: Int?

    static func resolve(_ text: String, directory: String) -> TerminalFileReference? {
        var path = text
        var line: Int?
        var column: Int?
        if let match = path.range(of: #":([1-9][0-9]*)(?::([1-9][0-9]*))?:?$"#, options: .regularExpression) {
            let numbers = path[match].split(separator: ":").compactMap { Int($0) }
            line = numbers.first
            column = numbers.count > 1 ? numbers[1] : nil
            path.removeSubrange(match)
        }
        path = path.trimmingCharacters(in: CharacterSet(charactersIn: "\"'"))
        let url: URL
        if path.hasPrefix("file:") {
            guard let parsed = URL(string: path), parsed.isFileURL,
                  LocalHost.isLocal(parsed.host) else { return nil }
            url = parsed
        } else {
            guard !path.contains("://"), !path.isEmpty else { return nil }
            let expanded = (path as NSString).expandingTildeInPath
            url = expanded.hasPrefix("/") ? URL(fileURLWithPath: expanded) : URL(fileURLWithPath: directory).appendingPathComponent(expanded)
        }
        let normalized = url.standardizedFileURL
        var isDirectory: ObjCBool = false
        guard FileManager.default.fileExists(atPath: normalized.path, isDirectory: &isDirectory), !isDirectory.boolValue else { return nil }
        return TerminalFileReference(url: normalized, line: line, column: column)
    }

    private static func resolve(token range: NSRange, in text: String, directory: String) -> (NSRange, TerminalFileReference)? {
        let token = (text as NSString).substring(with: range)
        if let reference = resolve(token, directory: directory) { return (range, reference) }
        // Sentence punctuation is not part of a compiler's path.
        var trimmed = token
        while trimmed.last == "." || trimmed.last == ":" { trimmed.removeLast() }
        guard let reference = resolve(trimmed, directory: directory) else { return nil }
        return (NSRange(location: range.location, length: (trimmed as NSString).length), reference)
    }

    static func matches(in text: String, directory: String) -> [(NSRange, TerminalFileReference)] {
        tokenRegex.matches(in: text, range: NSRange(text.startIndex..., in: text)).compactMap { resolve(token: $0.range, in: text, directory: directory) }
    }

    /// Only the token under `offset` is resolved; a hover must not stat every other token on the line.
    static func match(in text: String, at offset: Int, directory: String) -> (NSRange, TerminalFileReference)? {
        guard let token = tokenRegex.matches(in: text, range: NSRange(text.startIndex..., in: text)).first(where: { NSLocationInRange(offset, $0.range) }),
              let match = resolve(token: token.range, in: text, directory: directory),
              NSLocationInRange(offset, match.0) else { return nil }
        return match
    }
}

@MainActor
final class FileLinkTerminalView: LocalProcessTerminalView {
    weak var owner: OakTerminalPanelController?
    var launchDirectory = NSHomeDirectory()
    var reportedDirectory: String?
    // Where the shell changed directory, by scroll-invariant row. Relative paths in older output resolve against
    // the directory that was current when they were printed, which only changes at these points.
    private var directoryMarkers: [(row: Int, directory: String)] = []
    private var markerColumns = 0
    // End of the primary screen's row numbering; it only shrinks when ESC[3J or a reset renumbers the rows.
    private var rowExtent = Int.min
    private var directoryCheckPending = false
    private var lastDirectoryCheck: TimeInterval = 0

    func scheduleOutputCapture() {
        hoverCache = nil
        let now = ProcessInfo.processInfo.systemUptime
        if now - lastDirectoryCheck >= 0.05 {
            lastDirectoryCheck = now
            noteDirectoryIfChanged()
        } else if !directoryCheckPending {
            directoryCheckPending = true
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) { [weak self] in
                guard let self else { return }
                self.directoryCheckPending = false
                self.lastDirectoryCheck = ProcessInfo.processInfo.systemUptime
                self.noteDirectoryIfChanged()
            }
        }
    }
    private var eventMonitor: Any?
    private var underlineLayers: [CALayer] = []
    private var pressedLink: TerminalFileReference?
    private var pressedPoint = NSPoint.zero
    private var handledNativeLink = false
    private var hoverCache: (cell: [Int], time: TimeInterval, hit: (reference: TerminalFileReference, rects: [NSRect])?)?

    func clearFileLinkHistory() { directoryMarkers.removeAll(); hoverCache = nil; clearHighlight() }

    /// Looked up by selector in OakTextView.performKeyEquivalent: so editor bundle bindings skip the document while the terminal has focus.
    @objc func oak_ownsKeyEquivalents() -> Bool { true }

    static func processDirectory(pid: pid_t) -> String? {
        guard pid > 0 else { return nil }
        var info = proc_vnodepathinfo()
        let size = MemoryLayout<proc_vnodepathinfo>.size
        guard proc_pidinfo(pid, PROC_PIDVNODEPATHINFO, 0, &info, Int32(size)) == size else { return nil }
        return withUnsafeBytes(of: &info.pvi_cdir.vip_path) { bytes in
            let path = String(decoding: bytes.prefix { $0 != 0 }, as: UTF8.self)
            return path.isEmpty ? nil : path
        }
    }

    // Shells are recognised by executable name, since a nested shell is just another process in the tree.
    private static let shellNames: Set<String> = {
        let listed = (try? String(contentsOfFile: "/etc/shells", encoding: .utf8)) ?? ""
        let names = listed.split(whereSeparator: \.isNewline)
            .filter { !$0.hasPrefix("#") }
            .map { ($0.trimmingCharacters(in: .whitespaces) as NSString).lastPathComponent }
        return Set(names)
    }()

    static func isShell(pid: pid_t) -> Bool {
        var buffer = [UInt8](repeating: 0, count: Int(MAXPATHLEN) * 4)
        guard proc_pidpath(pid, &buffer, UInt32(buffer.count)) > 0 else { return false }
        return shellNames.contains((String(decoding: buffer.prefix { $0 != 0 }, as: UTF8.self) as NSString).lastPathComponent)
    }

    private static func parentPid(of pid: pid_t) -> pid_t? {
        var info = proc_bsdinfo()
        let size = MemoryLayout<proc_bsdinfo>.size
        guard proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, &info, Int32(size)) == size, info.pbi_ppid > 1 else { return nil }
        return pid_t(info.pbi_ppid)
    }

    /// The directory of the innermost shell on the foreground job's ancestry, so a `cd` in a nested shell (`bash`, `su`, `nix-shell`) is followed.
    /// Walking up from the foreground group rather than looking only at its leader keeps a nested shell current while it runs a command.
    static func shellDirectory(foreground: pid_t, login: pid_t) -> String? {
        guard login > 0 else { return nil }
        var pid = foreground
        // The leader of a pipeline may have exited, or the process may belong to another user; the login shell is the fallback.
        var depth = 0
        while pid > 1 && pid != login && depth < 32 {
            if Self.isShell(pid: pid) { return Self.processDirectory(pid: pid) ?? Self.processDirectory(pid: login) }
            guard let parent = Self.parentPid(of: pid) else { break }
            pid = parent
            depth += 1
        }
        return Self.processDirectory(pid: login)
    }

    var currentDirectory: String {
        Self.shellDirectory(foreground: tcgetpgrp(process.childfd), login: process.shellPid) ?? reportedDirectory ?? launchDirectory
    }

    /// Cheap on every call: reading the shell directory is one syscall, and the screen is only touched when it changed.
    func noteDirectoryIfChanged() {
        if directoryMarkers.isEmpty {
            directoryMarkers = [(Int.min, launchDirectory)]
            markerColumns = terminalDimensions.cols
        }
        let directory = currentDirectory
        guard directory != directoryMarkers.last?.directory,
              let snapshot = terminalContentSnapshot(region: .history(maximumScrollbackRows: -1)),
              !snapshot.inputState.isAlternateBuffer else { return }
        // Reflow changed which rows hold which output, and a renumbering moved them, so older markers no longer apply.
        let renumbered = rowsWereRenumbered(snapshot)
        if markerColumns != snapshot.inputState.dimensions.cols || renumbered {
            directoryMarkers = [(Int.min, directory)]
            markerColumns = snapshot.inputState.dimensions.cols
            return
        }
        directoryMarkers.append((snapshot.cursorRow, directory))
        if directoryMarkers.count > 500 { directoryMarkers.removeFirst(250) }
    }

    /// ESC[3J with scrollback and a terminal reset renumber the rows, which strands the recorded marker rows.
    /// Ctrl-L erases in place and keeps the numbering, so the cursor position cannot be the signal.
    private func rowsWereRenumbered(_ snapshot: TerminalContentSnapshot) -> Bool {
        guard !snapshot.inputState.isAlternateBuffer else { return false }
        let extent = snapshot.liveTopRow + snapshot.inputState.dimensions.rows
        defer { rowExtent = extent }
        return extent < rowExtent
    }

    /// The directory current when `row` was printed, or nil when that is unknown (no change recorded, or the terminal was resized since).
    func recordedDirectory(forRow row: Int, columns: Int) -> String? {
        guard columns == markerColumns else { return nil }
        return directoryMarkers.last { $0.row <= row }?.directory
    }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if let eventMonitor { NSEvent.removeMonitor(eventMonitor); self.eventMonitor = nil }
        clearHighlight()
        guard window != nil else { return }
        eventMonitor = NSEvent.addLocalMonitorForEvents(matching: [.keyDown, .mouseMoved, .flagsChanged, .scrollWheel, .leftMouseDragged]) { [weak self] event in
            if event.type == .keyDown {
                if let self, self.owner?.handleTerminalKeyEvent(event, source: self) == true { return nil }
            } else {
                self?.updateHighlight(event)
            }
            return event
        }
    }

    private func clearHighlight() {
        if !underlineLayers.isEmpty { NSCursor.iBeam.set() }
        for layer in underlineLayers { layer.removeFromSuperlayer() }
        underlineLayers.removeAll()
    }

    private func updateHighlight(_ event: NSEvent) {
        clearHighlight()
        guard event.window === window, event.modifierFlags.contains(.command),
              event.type != .scrollWheel, event.type != .leftMouseDragged,
              let window, let hit = cachedFileHit(at: convert(window.mouseLocationOutsideOfEventStream, from: nil)) else { return }
        NSCursor.pointingHand.set()
        for rect in hit.rects {
            let underline = CALayer()
            underline.backgroundColor = nativeForegroundColor.cgColor
            underline.frame = NSRect(x: rect.minX, y: rect.minY + 1, width: rect.width, height: 1)
            layer?.addSublayer(underline)
            underlineLayers.append(underline)
        }
    }

    // Mouse moves arrive per pixel; the hit (regex plus a stat per token) only changes per cell or when output arrives.
    private func cachedFileHit(at point: NSPoint) -> (reference: TerminalFileReference, rects: [NSRect])? {
        let cell = cellSize()
        guard cell.width > 0, cell.height > 0 else { return fileHit(at: point) }
        let key = [Int((bounds.maxY - point.y) / cell.height), Int((point.x - bounds.minX) / cell.width)]
        let now = ProcessInfo.processInfo.systemUptime
        if let cache = hoverCache, cache.cell == key, now - cache.time < 0.5 { return cache.hit }
        let hit = fileHit(at: point)
        hoverCache = (key, now, hit)
        return hit
    }

    private func cellSize() -> CGSize {
        let size = getWindowSize()
        let scale = window?.backingScaleFactor ?? NSScreen.main?.backingScaleFactor ?? 1
        return CGSize(width: CGFloat(size.ws_xpixel) / CGFloat(max(1, size.ws_col)) / scale,
                      height: CGFloat(size.ws_ypixel) / CGFloat(max(1, size.ws_row)) / scale)
    }

    // Snapshot cells preserve wide Unicode glyphs and soft wraps without reading mutable terminal state.
    func fileHit(at point: NSPoint) -> (reference: TerminalFileReference, rects: [NSRect])? {
        guard bounds.contains(point), let snapshot = terminalContentSnapshot(region: .viewport), !snapshot.inputState.isAlternateBuffer else { return nil }
        let cell = cellSize()
        let (cellWidth, cellHeight) = (cell.width, cell.height)
        guard cellWidth > 0, cellHeight > 0 else { return nil }
        let rowIndex = Int((bounds.maxY - point.y) / cellHeight)
        let column = Int((point.x - bounds.minX) / cellWidth)
        guard snapshot.rows.indices.contains(rowIndex), column < snapshot.inputState.dimensions.cols else { return nil }
        var first = rowIndex
        while first > 0 && snapshot.rows[first].isWrapped { first -= 1 }
        var last = rowIndex
        while last + 1 < snapshot.rows.count && snapshot.rows[last + 1].isWrapped { last += 1 }
        var text = ""
        var cells: [(range: NSRange, rect: NSRect)] = []
        var clickedOffset: Int?
        var length = 0
        for r in first...last {
            for (c, cell) in snapshot.rows[r].cells.enumerated() where cell.width > 0 {
                let value = cell.text.isEmpty || cell.text == "\0" ? " " : cell.text
                let offset = length
                let range = NSRange(location: offset, length: (value as NSString).length)
                if r == rowIndex && (c..<c + cell.width).contains(column) { clickedOffset = offset }
                text += value
                length += range.length
                cells.append((range, NSRect(x: CGFloat(c) * cellWidth, y: bounds.maxY - CGFloat(r + 1) * cellHeight, width: CGFloat(cell.width) * cellWidth, height: cellHeight)))
            }
        }
        guard let clickedOffset else { return nil }
        if rowsWereRenumbered(snapshot) && !directoryMarkers.isEmpty {
            directoryMarkers = [(Int.min, currentDirectory)]
            markerColumns = snapshot.inputState.dimensions.cols
        }
        let row = snapshot.rows[first]
        let directory = recordedDirectory(forRow: row.absoluteRow, columns: snapshot.inputState.dimensions.cols) ?? currentDirectory
        guard let match = TerminalFileReference.match(in: text, at: clickedOffset, directory: directory) else { return nil }
        return (match.1, cells.filter { NSIntersectionRange($0.range, match.0).length > 0 }.map(\.rect))
    }

    override func mouseDown(with event: NSEvent) {
        pressedLink = nil
        if event.modifierFlags.contains(.command), let hit = fileHit(at: convert(event.locationInWindow, from: nil)) {
            pressedLink = hit.reference
            pressedPoint = event.locationInWindow
        }
        super.mouseDown(with: event)
    }

    override func mouseUp(with event: NSEvent) {
        handledNativeLink = false
        // Explicit OSC 8 hyperlinks take precedence over a filename in their caption.
        super.mouseUp(with: event)
        defer { pressedLink = nil; clearHighlight() }
        if !handledNativeLink, let pressedLink,
           event.modifierFlags.contains(.command),
           hypot(event.locationInWindow.x - pressedPoint.x, event.locationInWindow.y - pressedPoint.y) < 4,
           fileHit(at: convert(event.locationInWindow, from: nil))?.reference == pressedLink {
            openFile(pressedLink)
        }
    }

    private func openFile(_ reference: TerminalFileReference) {
        owner?.requestOpenFile(reference.url, line: reference.line ?? 0, column: reference.column ?? 0)
    }

    override func requestOpenLink(source: TerminalView, link: String, params: [String: String]) {
        handledNativeLink = true
        if let file = TerminalFileReference.resolve(link, directory: currentDirectory) {
            openFile(file)
        } else if let scheme = URL(string: link)?.scheme?.lowercased(), ["http", "https", "mailto"].contains(scheme) {
            super.requestOpenLink(source: source, link: link, params: params)
        }
    }
}
