import AppKit
import SwiftUI
import Combine

@MainActor @objc public class OakCompletionPopup: NSObject {
	@objc public weak var delegate: OakCompletionPopupDelegate?
	@objc public var supportsResolve: Bool = false

	private var window: NSWindow?
	private var viewModel: CompletionViewModel?
	private let theme: OakThemeEnvironment
	private static let docPanelWidth: CGFloat = 262
	private static let verticalDocHeight: CGFloat = 200
	private var cancellables = Set<AnyCancellable>()
	private var listHeight: CGFloat = 0

	@objc public init(theme: OakThemeEnvironment) {
		self.theme = theme
		super.init()
	}

	@objc public func show(in parentView: NSView, at point: NSPoint, items: [OakCompletionItem]) {
		if let w = window {
			w.parent?.removeChildWindow(w)
			w.orderOut(nil)
			window = nil
			viewModel = nil
			cancellables.removeAll()
		}

		let vm = CompletionViewModel()
		vm.setItems(items)
		vm.onResolveNeeded = { [weak self] item in
			guard let self else { return }
			self.delegate?.completionPopup?(self, resolveItem: item)
		}
		self.viewModel = vm

		let rowHeight = max(theme.fontSize * 1.8, 22)
		let hasMultiline = items.contains { $0.multiline }
		let maxVisible = hasMultiline ? 6 : 12
		let computedListHeight = computeListHeight(items: items, rowHeight: rowHeight, maxVisible: maxVisible)
		self.listHeight = computedListHeight

		let detailFont = NSFont.systemFont(ofSize: max(theme.fontSize - 2, 9))
		let maxLabelWidth = items.prefix(50).map { ($0.label as NSString).size(withAttributes: [.font: theme.font]).width }.max() ?? 200
		let maxDetailWidth = items.prefix(50).map { ($0.detail as NSString).size(withAttributes: [.font: detailFont]).width }.max() ?? 0
		let listWidth = min(max(maxLabelWidth + maxDetailWidth + 60, 280), 650)

		let screenPoint = parentView.window?.convertPoint(toScreen:
			parentView.convert(point, to: nil)) ?? point
		let screen = NSScreen.main?.visibleFrame ?? NSRect(x: 0, y: 0, width: 1440, height: 900)

		var width = listWidth
		if supportsResolve {
			let horizontalWidth = listWidth + Self.docPanelWidth
			if horizontalWidth <= screen.maxX - screenPoint.x {
				vm.docPanelPosition = .right
				width = horizontalWidth
			} else {
				// Not enough horizontal space — stack doc panel vertically
				let spaceBelow = screenPoint.y - screen.minY
				let neededVertical = computedListHeight + Self.verticalDocHeight
				vm.docPanelPosition = spaceBelow >= neededVertical ? .below : .above
			}
		}

		var totalHeight = computedListHeight
		if supportsResolve && vm.docPanelPosition != .right {
			totalHeight += Self.verticalDocHeight
		}

		var origin = NSPoint(x: screenPoint.x, y: screenPoint.y - totalHeight)
		if origin.y < screen.minY {
			origin.y = screenPoint.y + 20
		}

		let listView = CompletionListView(viewModel: vm, showDocPanel: supportsResolve)
			.environmentObject(theme)
		let hostingView = NSHostingView(rootView: listView)

		let panel = NSPanel(
			contentRect: NSRect(origin: origin, size: NSSize(width: width, height: totalHeight)),
			styleMask: [.borderless, .nonactivatingPanel],
			backing: .buffered,
			defer: false
		)
		panel.level = .floating
		panel.isOpaque = false
		panel.backgroundColor = .clear
		panel.hasShadow = true
		panel.contentView = hostingView

		panel.orderFront(nil)
		parentView.window?.addChildWindow(panel, ordered: .above)
		self.window = panel

		if supportsResolve {
			vm.$resolvedDocumentation
				.receive(on: DispatchQueue.main)
				.sink { [weak self] _ in self?.resizeForDocs() }
				.store(in: &cancellables)
		}

		vm.scheduleResolve()
	}

	@objc public func updateFilter(_ text: String) {
		viewModel?.updateFilter(text)
		resizePanelToFit()
	}

	@objc public func resolveCompleted(for item: OakCompletionItem, documentation: NSAttributedString?, insertText: String?) {
		if let newInsert = insertText, !newInsert.isEmpty {
			item.updateInsertText(newInsert)
		}
		viewModel?.resolveCompleted(for: item, documentation: documentation)
	}

	private func computeListHeight(items: [OakCompletionItem], rowHeight: CGFloat, maxVisible: Int) -> CGFloat {
		let multilineRowHeight = rowHeight * 3
		var total: CGFloat = 8 // vertical padding
		for item in items.prefix(maxVisible) {
			total += item.multiline ? multilineRowHeight : rowHeight
		}
		return total
	}

	private func resizePanelToFit() {
		guard let w = window, let vm = viewModel else { return }
		let rowHeight = max(theme.fontSize * 1.8, 22)
		let hasMultiline = vm.filteredItems.contains { $0.multiline }
		let maxVisible = hasMultiline ? 6 : 12
		let newListHeight = computeListHeight(items: Array(vm.filteredItems), rowHeight: rowHeight, maxVisible: maxVisible)
		self.listHeight = newListHeight

		let targetHeight: CGFloat
		if supportsResolve && vm.docPanelPosition != .right {
			targetHeight = newListHeight + Self.verticalDocHeight
		} else if supportsResolve {
			targetHeight = heightForHorizontalDocs(listHeight: newListHeight)
		} else {
			targetHeight = newListHeight
		}

		var frame = w.frame
		let delta = targetHeight - frame.height
		frame.origin.y -= delta
		frame.size.height = targetHeight
		w.setFrame(frame, display: true, animate: false)
	}

	private func resizeForDocs() {
		guard let w = window, let vm = viewModel else { return }

		let targetHeight: CGFloat
		if vm.docPanelPosition != .right {
			targetHeight = listHeight + Self.verticalDocHeight
		} else {
			targetHeight = heightForHorizontalDocs(listHeight: listHeight)
		}

		var frame = w.frame
		let delta = targetHeight - frame.height
		guard abs(delta) > 1 else { return }
		frame.origin.y -= delta
		frame.size.height = targetHeight
		w.setFrame(frame, display: true, animate: false)
	}

	private func heightForHorizontalDocs(listHeight: CGFloat) -> CGFloat {
		guard let vm = viewModel, let docs = vm.resolvedDocumentation, docs.length > 0 else {
			return listHeight
		}

		let maxScreenHeight = (NSScreen.main?.visibleFrame.height ?? 800) * 0.4
		let docWidth = Self.docPanelWidth - 20
		let boundingRect = docs.boundingRect(
			with: NSSize(width: docWidth, height: .greatestFiniteMagnitude),
			options: [.usesLineFragmentOrigin, .usesFontLeading]
		)
		let docHeight = ceil(boundingRect.height) + 30

		return min(max(listHeight, docHeight), maxScreenHeight)
	}

	@objc public func handleKeyEvent(_ event: NSEvent) -> Bool {
		guard let vm = viewModel else { return false }

		switch event.keyCode {
		case 125: // down arrow
			vm.selectNext()
			return true
		case 126: // up arrow
			vm.selectPrevious()
			return true
		case 36: // return
			if let item = vm.selectedItem {
				delegate?.completionPopup(self, didSelectItem: item)
				dismiss()
			}
			return true
		case 48: // tab
			if let item = vm.selectedItem {
				delegate?.completionPopup(self, didSelectItem: item)
				dismiss()
			}
			return true
		case 53: // escape
			dismiss()
			return true
		default:
			return false
		}
	}

	@objc public func dismiss() {
		if let w = window {
			viewModel?.cancelResolve()
			cancellables.removeAll()
			w.parent?.removeChildWindow(w)
			w.orderOut(nil)
			window = nil
			viewModel = nil
			delegate?.completionPopupDidDismiss(self)
		}
	}

	@objc public var isVisible: Bool {
		window?.isVisible ?? false
	}
}
