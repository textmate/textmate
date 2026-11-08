import SwiftTerm

/// Tracks the unseen-completion dot for one terminal session.
///
/// SwiftTerm hands over the whole OSC 7501 store on every change, so the dot comes from the
/// folded state *changing* into a finished one; the presence of a `.done` record is not itself
/// an event. A terminal can hold several records at once (nested or anonymous ids), and
/// `.blocked` outranks `.done` because a program waiting on the user is the more urgent fact.
/// No records means the store was emptied (process exit), which counts as a change away from
/// finished rather than an error.
struct TerminalAttention: Equatable {
    /// The state that raised the dot; stays set until the session is looked at or the process exits.
    private(set) var raisedState: TerminalProgramStatusState?
    /// The action a blocked program asked for; nil for `.done` and for a blocked record without a kind.
    private(set) var blockedKind: TerminalProgramStatusKind?
    private var lastState: TerminalProgramStatusState?

    var needsAttention: Bool { raisedState != nil }

    /// Hover text for the tab, parallel to the dot: empty when nothing is lit, since an empty tooltip is no tooltip.
    ///
    /// Capitalised `Subject: state` wording, matching the Copilot indicator's tooltip.
    var toolTip: String {
        switch raisedState {
        case .done: return "Terminal: Finished"
        case .blocked: return blockedKind == .permission ? "Terminal: Needs permission" : "Terminal: Waiting for input"
        default: return ""
        }
    }

    /// Returns true when the dot changed, i.e. when the tab bar needs a repaint.
    @discardableResult
    mutating func update(records: [TerminalProgramStatus], isSelected: Bool) -> Bool {
        update(states: records.map(\.state), blockedKind: Self.kind(of: records), isSelected: isSelected)
    }

    @discardableResult
    mutating func update(states: [TerminalProgramStatusState], isSelected: Bool) -> Bool {
        update(states: states, blockedKind: nil, isSelected: isSelected)
    }

    @discardableResult
    mutating func update(states: [TerminalProgramStatusState], blockedKind: TerminalProgramStatusKind?, isSelected: Bool) -> Bool {
        let state = Self.aggregate(states)
        defer { lastState = state }
        // Selection clears the dot but leaves `lastState` at the finished value, so a repeated
        // snapshot of the same state cannot re-light it after the user switches away again.
        guard !isSelected, state != lastState, state == .done || state == .blocked, !needsAttention else { return false }
        raisedState = state
        // Only a blocked transition carries a kind; a done transition must not inherit a stale one.
        self.blockedKind = state == .blocked ? blockedKind : nil
        return true
    }

    mutating func clear() {
        raisedState = nil
        blockedKind = nil
    }

    static func aggregate(_ states: [TerminalProgramStatusState]) -> TerminalProgramStatusState? {
        if states.contains(.blocked) { return .blocked }
        if states.contains(.done) { return .done }
        return states.last
    }

    /// The most specific kind among concurrent blocked records, since a permission prompt outranks
    /// the generic question wording the window shows.
    static func kind(of records: [TerminalProgramStatus]) -> TerminalProgramStatusKind? {
        let kinds = records.filter { $0.state == .blocked }.compactMap(\.kind)
        return kinds.first(where: { $0 == .permission }) ?? kinds.first
    }

    /// Text for the window subtitle, folded over every session in one window (nil when nothing needs the user).
    ///
    /// A blocked session outranks a finished one, and a blocked session asking for permission outranks
    /// the question/auth wording, so two tabs needing different things cannot disagree once the user
    /// selects one of them.
    static func subtitle(for attentions: [TerminalAttention]) -> String? {
        let pending = attentions.filter(\.needsAttention)
        if pending.contains(where: { $0.blockedKind == .permission }) { return "terminal needs permission" }
        if pending.contains(where: { $0.raisedState == .blocked }) { return "terminal needs your input" }
        if pending.contains(where: { $0.raisedState == .done }) { return "terminal finished" }
        return nil
    }

    /// Test hook: raises the dot for one state, or clears it when `state` is nil, without an OSC 7501 report.
    mutating func force(_ state: TerminalProgramStatusState?, blockedKind: TerminalProgramStatusKind? = nil) {
        raisedState = state
        self.blockedKind = state == .blocked ? blockedKind : nil
    }
}
