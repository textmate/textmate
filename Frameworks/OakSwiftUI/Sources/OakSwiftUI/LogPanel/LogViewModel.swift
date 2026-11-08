import SwiftUI
import Combine

public struct LogEntry: Identifiable, Equatable {
    public let id = UUID()
    public let date = Date()
    public let message: String
    public let level: Int // LSP MessageType: 1=Error, 2=Warning, 3=Info, 4=Log
    public let source: String
}

@MainActor
public class LogViewModel: ObservableObject {
    @Published public var entries: [LogEntry] = []
    private let maxEntries = 1000

    nonisolated init() {}

    public func add(message: String, level: Int, source: String = "LSP") {
        let entry = LogEntry(message: message, level: level, source: source)
        self.entries.append(entry)
        if self.entries.count > self.maxEntries {
            self.entries.removeFirst(self.entries.count - self.maxEntries)
        }
    }

    public func clear() {
        self.entries.removeAll()
    }
}
