import AppKit
import SwiftUI

@MainActor @objc public class OakRenamePreviewPanel: NSObject, NSWindowDelegate {
	@objc public weak var delegate: OakRenamePreviewPanelDelegate?

	private var panel: NSPanel?
	private var viewModel: RenamePreviewViewModel?
	private let theme: OakThemeEnvironment

	@objc public init(theme: OakThemeEnvironment) {
		self.theme = theme
	}

	@objc public func show(items: [OakRenameItem], oldName: String, newName: String, parentWindow: NSWindow) {
		close()

		let vm = RenamePreviewViewModel(items: items)
		self.viewModel = vm

		let view = RenamePreviewListView(
			viewModel: vm,
			theme: theme,
			onConfirm: { [weak self] in
				guard let self else { return }
				self.delegate?.renamePreviewPanelDidConfirm(self)
				self.close()
			},
			onCancel: { [weak self] in
				guard let self else { return }
				self.delegate?.renamePreviewPanelDidCancel(self)
				self.close()
			}
		)

		let hostingView = NSHostingView(rootView: view)

		let contentHeight = CGFloat(items.count) * 48 + CGFloat(vm.totalFiles) * 28 + 100
		let maxHeight = (NSScreen.main?.visibleFrame.height ?? 800) * 0.5
		let height = min(max(contentHeight, 200), maxHeight)

		let p = NSPanel(
			contentRect: NSRect(x: 0, y: 0, width: 500, height: height),
			styleMask: [.titled, .closable, .resizable, .utilityWindow],
			backing: .buffered,
			defer: false
		)
		p.title = "Rename '\(oldName)' → '\(newName)'"
		p.contentView = hostingView
		p.contentMinSize = NSSize(width: 350, height: 200)
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
			delegate?.renamePreviewPanelDidCancel(self)
		}
	}
}
