import Foundation
import SwiftTerm

/// Presentation values shared with OakTabStatus in OakTabBarView.h.
struct TerminalAgentStatus {
    private(set) var records: [TerminalProgramStatus] = []
    private var acknowledged: [String: TerminalProgramStatus] = [:]

    mutating func update(_ records: [TerminalProgramStatus]) {
        acknowledged = acknowledged.filter { id, record in records.contains { $0.id == id && $0 == record } }
        self.records = records
    }

    mutating func acknowledgeResults() {
        for record in records where record.state == .done || record.state == .error {
            acknowledged[record.id] = record
        }
    }

    // A blocked worker still needs attention when its parent has finished.
    var visibleRecord: TerminalProgramStatus? {
        records.filter { acknowledged[$0.id] != $0 && $0.state != .idle }
            .max { priority($0) < priority($1) }
    }

    private func priority(_ record: TerminalProgramStatus) -> Int {
        switch record.state {
        case .blocked: 4
        case .working: 3
        case .error: 2
        case .done: 1
        case .idle: 0
        }
    }

    var value: Int {
        switch visibleRecord?.state {
        case .working: 1
        case .blocked: 2
        case .done: 3
        case .error: 4
        default: 0
        }
    }

    var description: String {
        guard let record = visibleRecord else { return "" }
        let state: String
        switch record.state {
        case .working: state = "Working"
        case .blocked:
            switch record.kind {
            case .permission: state = "Needs approval"
            case .question: state = "Needs an answer"
            case .auth: state = "Needs sign-in"
            default: state = "Needs attention"
            }
        case .done: state = "Result ready"
        case .error: state = "Failed"
        case .idle: state = "Idle"
        }
        var parts = [state]
        if let app = record.effectiveApp { parts.insert(app, at: 0) }
        if let progress = record.progress { parts.append("\(progress)%") }
        if let message = record.message, !message.isEmpty { parts.append(message) }
        // Reports are untrusted plain text, including when placed in accessibility labels.
        let scalars = parts.joined(separator: " — ").unicodeScalars.filter {
            !CharacterSet.controlCharacters.contains($0) && $0.properties.generalCategory != .format
        }
        return String(String.UnicodeScalarView(scalars.prefix(512)))
    }
}
