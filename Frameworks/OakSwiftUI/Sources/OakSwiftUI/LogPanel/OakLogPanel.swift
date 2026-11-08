import AppKit
import SwiftUI

private let kFrameKey = "OakLSPLogWindow.frame"

@MainActor
@objc public class OakLogPanel: NSObject {
	@objc public static let shared = OakLogPanel()

	private let model = LogViewModel()
	private var windowController: NSWindowController?
	private var frameObservers: [Any] = []

	private override init() {}

	@objc public func log(message: String, level: Int, source: String) {
		self.model.add(message: message, level: level, source: source)
	}

	@objc public func show() {
		self.showPanel()
	}

	@objc public func toggle() {
		if let window = self.windowController?.window, window.isVisible {
			window.close()
		} else {
			self.showPanel()
		}
	}

	private func removeFrameObservers() {
		frameObservers.forEach { NotificationCenter.default.removeObserver($0) }
		frameObservers.removeAll()
	}

	private func showPanel() {
		if windowController == nil || windowController?.window == nil {
			let hostingView = NSHostingView(rootView: LogView(model: model))

			let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 800, height: 600),
			                      styleMask: [.titled, .closable, .miniaturizable, .resizable],
			                      backing: .buffered, defer: false)
			window.title = "LSP Log"
			window.isReleasedWhenClosed = false
			window.contentView = hostingView
			window.contentMinSize = NSSize(width: 400, height: 300)

			if let frameString = UserDefaults.standard.string(forKey: kFrameKey) {
				window.setFrame(NSRectFromString(frameString), display: false)
			} else {
				window.center()
			}

			removeFrameObservers()
			for name in [NSWindow.didResizeNotification, NSWindow.didMoveNotification] {
				let token = NotificationCenter.default.addObserver(
					forName: name, object: window, queue: .main
				) { _ in
					// queue: .main guarantees main thread; re-fetch window from self
					// to avoid capturing MainActor-isolated refs in @Sendable closure
					DispatchQueue.main.async { [weak self] in
						guard let w = self?.windowController?.window, w.isVisible else { return }
						UserDefaults.standard.set(NSStringFromRect(w.frame), forKey: kFrameKey)
					}
				}
				frameObservers.append(token)
			}
			let closeToken = NotificationCenter.default.addObserver(
				forName: NSWindow.willCloseNotification, object: window, queue: .main
			) { _ in
				DispatchQueue.main.async { [weak self] in
					self?.removeFrameObservers()
				}
			}
			frameObservers.append(closeToken)

			windowController = NSWindowController(window: window)
		}

		windowController?.showWindow(nil)
		windowController?.window?.makeKeyAndOrderFront(nil)
	}
}
