import AppKit
import Combine

public enum DocPanelPosition {
	case right, below, above
}

@MainActor
public class CompletionViewModel: ObservableObject {
	@Published public private(set) var filteredItems: [OakCompletionItem] = []
	@Published public var selectedIndex: Int = 0
	@Published public private(set) var resolvedDocumentation: NSAttributedString?
	@Published public var docPanelPosition: DocPanelPosition = .right

	private var allItems: [OakCompletionItem] = []
	private var currentFilter: String = ""
	private var resolveTimer: Timer?

	public var onResolveNeeded: ((OakCompletionItem) -> Void)?

	public init() {}

	public func setItems(_ items: [OakCompletionItem]) {
		allItems = items
		applyFilter()
	}

	public func updateFilter(_ text: String) {
		currentFilter = text
		applyFilter()
		selectedIndex = 0
		scheduleResolve()
	}

	public func selectNext() {
		if selectedIndex < filteredItems.count - 1 {
			selectedIndex += 1
			scheduleResolve()
		}
	}

	public func selectPrevious() {
		if selectedIndex > 0 {
			selectedIndex -= 1
			scheduleResolve()
		}
	}

	public var selectedItem: OakCompletionItem? {
		guard selectedIndex >= 0, selectedIndex < filteredItems.count else { return nil }
		return filteredItems[selectedIndex]
	}

	public func resolveCompleted(for item: OakCompletionItem, documentation: NSAttributedString?) {
		item.documentation = documentation
		item.isResolved = true
		if item === selectedItem {
			resolvedDocumentation = documentation
		}
	}

	public func scheduleResolve() {
		resolveTimer?.invalidate()

		guard let item = selectedItem else {
			resolvedDocumentation = nil
			return
		}

		if item.isResolved {
			resolvedDocumentation = item.documentation
			return
		}

		resolveTimer = Timer.scheduledTimer(withTimeInterval: 0.15, repeats: false) { [weak self] _ in
			Task { @MainActor in
				guard let self, let current = self.selectedItem, current === item else { return }
				self.onResolveNeeded?(item)
			}
		}
	}

	public func cancelResolve() {
		resolveTimer?.invalidate()
		resolveTimer = nil
	}

	private func applyFilter() {
		filteredItems = FuzzyMatcher.filter(allItems, query: currentFilter, keyPath: \.label)
	}
}
