import AppKit
import Combine

public enum PaletteMode: String, CaseIterable, Sendable {
	case recentProjects
	case commands
	case symbols
	case bundleEditor
	case goToLine
	case findInProject
	case settings

	public var prefix: Character? {
		switch self {
		case .recentProjects: return nil
		case .commands:       return ">"
		case .symbols:        return "@"
		case .bundleEditor:   return "#"
		case .goToLine:       return ":"
		case .findInProject:  return "/"
		case .settings:       return "~"
		}
	}

	public var label: String {
		switch self {
		case .recentProjects: return "Recent Projects"
		case .commands:       return "Commands"
		case .symbols:        return "Symbols"
		case .bundleEditor:   return "Bundles"
		case .goToLine:       return "Go to Line"
		case .findInProject:  return "Find"
		case .settings:       return "Settings"
		}
	}

	public var placeholder: String {
		switch self {
		case .recentProjects: return "Search recent projects..."
		case .commands:       return "Search commands..."
		case .symbols:        return "Search symbols..."
		case .bundleEditor:   return "Search bundles..."
		case .goToLine:       return "Type a line number..."
		case .findInProject:  return "Search in project..."
		case .settings:       return "Search settings..."
		}
	}

	public var intValue: Int {
		switch self {
		case .recentProjects: return 0
		case .commands:       return 1
		case .symbols:        return 2
		case .bundleEditor:   return 3
		case .goToLine:       return 4
		case .findInProject:  return 5
		case .settings:       return 6
		}
	}

	public init?(prefix: Character) {
		guard let mode = PaletteMode.allCases.first(where: { $0.prefix == prefix }) else {
			return nil
		}
		self = mode
	}
}

public struct RankedItem: Identifiable, Sendable {
	public let item: OakCommandPaletteItem
	public let matchedIndices: [Int]
	public let score: Double
	public var id: UUID { item.id }
}

public struct FrecencyEntry {
	public var count: Int
	public var lastUsed: TimeInterval

	public init(count: Int, lastUsed: TimeInterval) {
		self.count = count
		self.lastUsed = lastUsed
	}
}

@MainActor
public class CommandPaletteViewModel: ObservableObject {
	@Published public var filterText: String = "" {
		didSet { processFilterChange() }
	}
	@Published public private(set) var activeMode: PaletteMode = .recentProjects
	@Published public private(set) var filteredItems: [RankedItem] = []
	@Published public var selectedIndex: Int = 0

	public var onItemSelected: ((OakCommandPaletteItem) -> Void)?
	public var onDismiss: (() -> Void)?
	public var onModeSwitch: ((PaletteMode) -> [OakCommandPaletteItem])?
	public var onSearchDocument: ((String) -> [OakCommandPaletteItem])?

	private var itemsByMode: [PaletteMode: [OakCommandPaletteItem]] = [:]
	public private(set) var frecencyData: [String: FrecencyEntry] = [:]

	public init() {}

	public var queryText: String {
		guard let prefix = activeMode.prefix, filterText.first == prefix else {
			return filterText
		}
		return String(filterText.dropFirst())
	}

	public func setItems(_ items: [OakCommandPaletteItem], forMode mode: PaletteMode) {
		itemsByMode[mode] = items
		if mode == activeMode {
			applyFilter()
		}
	}

	public func selectNext() {
		if selectedIndex < filteredItems.count - 1 {
			selectedIndex += 1
		}
	}

	public func selectPrevious() {
		if selectedIndex > 0 {
			selectedIndex -= 1
		}
	}

	public func acceptSelection() {
		guard selectedIndex >= 0, selectedIndex < filteredItems.count else { return }
		let item = filteredItems[selectedIndex].item
		guard item.enabled else { return }
		onItemSelected?(item)
	}

	public func requestDismiss() {
		onDismiss?()
	}

	public func updateFrecency(for identifier: String, at timestamp: TimeInterval? = nil) {
		let ts = timestamp ?? Date().timeIntervalSinceReferenceDate
		var entry = frecencyData[identifier] ?? FrecencyEntry(count: 0, lastUsed: ts)
		entry.count += 1
		entry.lastUsed = ts
		frecencyData[identifier] = entry
	}

	public func frecencyBoost(for identifier: String) -> Double {
		guard let entry = frecencyData[identifier] else { return 0 }
		let now = Date().timeIntervalSinceReferenceDate
		let hoursSinceLastUse = (now - entry.lastUsed) / 3600.0
		return Double(min(entry.count, 20)) * exp(-hoursSinceLastUse / 168.0)
	}

	public func loadFrecency(_ data: [String: FrecencyEntry]) {
		frecencyData = data
	}

	public var selectedItem: OakCommandPaletteItem? {
		guard selectedIndex >= 0, selectedIndex < filteredItems.count else { return nil }
		return filteredItems[selectedIndex].item
	}

	private func processFilterChange() {
		let newMode: PaletteMode
		if let first = filterText.first, let mode = PaletteMode(prefix: first) {
			newMode = mode
		} else {
			newMode = .recentProjects
		}

		if newMode != activeMode {
			activeMode = newMode
			// goToLine generates synthetic items — no delegate needed
			if newMode != .goToLine {
				// Settings always re-fetch (titles change based on toggle state)
				let shouldRefresh = newMode == .settings || itemsByMode[newMode] == nil
				if shouldRefresh, let items = onModeSwitch?(newMode) {
					itemsByMode[newMode] = items.isEmpty ? nil : items
				}
			}
		}

		// Find mode searches the current document — re-query on every keystroke
		if activeMode == .findInProject {
			let q = queryText
			if !q.isEmpty, let items = onSearchDocument?(q), !items.isEmpty {
				itemsByMode[.findInProject] = items
			} else {
				itemsByMode[.findInProject] = nil
			}
		}

		applyFilter()
		selectedIndex = 0
	}

	private func applyFilter() {
		let query = queryText

		// GoToLine generates a synthetic item from the query
		if activeMode == .goToLine {
			filteredItems = goToLineItems(query: query)
			return
		}

		guard let items = itemsByMode[activeMode] else {
			filteredItems = []
			return
		}

		if query.isEmpty {
			filteredItems = items.map { RankedItem(item: $0, matchedIndices: [], score: 0) }
			return
		}

		filteredItems = items
			.compactMap { item -> RankedItem? in
				guard let result = FuzzyMatcher.score(item.title, query: query) else { return nil }
				let frecency = frecencyBoost(for: item.actionIdentifier)
				let combinedScore = Double(result.score) * (1.0 + frecency)
				return RankedItem(item: item, matchedIndices: result.matchedIndices, score: combinedScore)
			}
			.sorted { $0.score > $1.score }
	}

	private func goToLineItems(query: String) -> [RankedItem] {
		let lineStr = query.isEmpty ? "..." : query
		let title = "Go to line \(lineStr)"
		let item = OakCommandPaletteItem(
			title: title, subtitle: "", keyEquivalent: "",
			category: .goToLine, actionIdentifier: "line:\(query)")
		return [RankedItem(item: item, matchedIndices: [], score: 0)]
	}

	// findInProject is no longer synthetic — items are provided by the delegate
}
