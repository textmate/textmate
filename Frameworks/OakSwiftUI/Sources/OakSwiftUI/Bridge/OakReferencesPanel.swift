import AppKit
import SwiftUI

@MainActor @objc public class OakReferencesPanel: NSObject, NSWindowDelegate {
	@objc public weak var delegate: OakReferencesPanelDelegate?

	private var panel: NSPanel?
	private var viewModel: ReferencesViewModel?

	@objc public init(theme: OakThemeEnvironment) {
		self.theme = theme
	}

	private let theme: OakThemeEnvironment

	@objc public func show(in parentView: NSView, items: [OakReferenceItem], symbol: String) {
		close()

		guard let parentWindow = parentView.window else { return }

		let vm = ReferencesViewModel(items: items)
		self.viewModel = vm

		let view = ReferencesListView(viewModel: vm, theme: theme) { [weak self] item in
			guard let self else { return }
			self.delegate?.referencesPanel(self, didSelectItem: item)
		}

		let hostingView = NSHostingView(rootView: view)

		// ~22pt per reference row + ~28pt per file group header, capped at 40% of screen
		let groupCount = Set(items.map { $0.filePath }).count
		let contentHeight = CGFloat(items.count) * 22 + CGFloat(groupCount) * 28 + 40
		let maxHeight = (NSScreen.main?.visibleFrame.height ?? 800) * 0.4
		let height = min(max(contentHeight, 150), maxHeight)

		let p = NSPanel(
			contentRect: NSRect(x: 0, y: 0, width: 500, height: height),
			styleMask: [.titled, .closable, .resizable, .utilityWindow],
			backing: .buffered,
			defer: false
		)
		p.title = "\(items.count) References to '\(symbol)'"
		p.contentView = hostingView
		p.contentMinSize = NSSize(width: 300, height: 150)
		p.delegate = self
		p.center()

		parentWindow.addChildWindow(p, ordered: .above)
		p.makeKeyAndOrderFront(nil)
		self.panel = p
	}

	@objc public func close() {
		if let p = panel {
			p.parent?.removeChildWindow(p)
			p.orderOut(nil)
			panel = nil
			viewModel = nil
		}
	}

	@objc public var isVisible: Bool {
		panel?.isVisible ?? false
	}

	nonisolated public func windowWillClose(_ notification: Notification) {
		MainActor.assumeIsolated {
			panel = nil
			viewModel = nil
			delegate?.referencesPanelDidClose(self)
		}
	}
}
