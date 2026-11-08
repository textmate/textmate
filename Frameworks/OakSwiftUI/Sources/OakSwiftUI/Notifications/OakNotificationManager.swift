import AppKit
import SwiftUI
import Combine

@MainActor
@objc public class OakNotificationManager: NSObject {
	@objc public static let shared = OakNotificationManager()

	private let model = ToastViewModel()
	private var windowController: NSWindowController?
	private var cancellables = Set<AnyCancellable>()

	private override init() {}

	@objc public func show(message: String, type: Int) {
		var controlAndWhitespace = CharacterSet.whitespacesAndNewlines
		controlAndWhitespace.formUnion(.controlCharacters)
		controlAndWhitespace.insert(charactersIn: "\u{200B}\u{200C}\u{200D}\u{FEFF}")
		let trimmed = message.trimmingCharacters(in: controlAndWhitespace)
		guard !trimmed.isEmpty else { return }

		let toastType: ToastType
		switch type {
		case 1: toastType = .error
		case 2: toastType = .warning
		case 3: toastType = .info
		case 4: toastType = .success
		default: toastType = .info
		}
		self.ensureWindow()
		self.model.show(message: trimmed, type: toastType)
	}

	@objc public func showInteractive(message: String, type: Int, actions: [String], callback: @escaping (String?) -> Void) {
		var controlAndWhitespace = CharacterSet.whitespacesAndNewlines
		controlAndWhitespace.formUnion(.controlCharacters)
		controlAndWhitespace.insert(charactersIn: "\u{200B}\u{200C}\u{200D}\u{FEFF}")
		let trimmed = message.trimmingCharacters(in: controlAndWhitespace)
		guard !trimmed.isEmpty else {
			callback(nil)
			return
		}

		let toastType: ToastType
		switch type {
		case 1: toastType = .error
		case 2: toastType = .warning
		case 3: toastType = .info
		case 4: toastType = .success
		default: toastType = .info
		}
		self.ensureWindow()
		self.model.showInteractive(message: trimmed, type: toastType, actions: actions, onAction: callback)
	}

	private func ensureWindow() {
		if windowController != nil { return }

		let view = NotificationView(model: model)
		let hostingView = NSHostingView(rootView: view)
		hostingView.autoresizingMask = [.width, .height]

		let window = NSPanel(contentRect: .zero,
		                     styleMask: [.borderless, .nonactivatingPanel],
		                     backing: .buffered, defer: false)
		window.isOpaque = false
		window.backgroundColor = .clear
		window.level = .floating
		window.hidesOnDeactivate = true
		window.collectionBehavior = [.fullScreenAuxiliary]
		window.ignoresMouseEvents = true
		window.hasShadow = false
		window.alphaValue = 0

		window.contentView = hostingView
		windowController = NSWindowController(window: window)

		model.$currentToast
			.receive(on: DispatchQueue.main)
			.sink { [weak self] toast in
				guard let self, let window = self.windowController?.window else { return }
				if toast != nil {
					self.repositionWindow(window)
					window.ignoresMouseEvents = false
					// Defer ordering front so SwiftUI renders the toast content first
					DispatchQueue.main.async { [weak self] in
						guard let window = self?.windowController?.window else { return }
						window.alphaValue = 1
						window.orderFrontRegardless()
					}
				} else {
					window.alphaValue = 0
					window.orderOut(nil)
					window.ignoresMouseEvents = true
				}
			}
			.store(in: &cancellables)
	}

	private func repositionWindow(_ window: NSWindow) {
		guard let screen = NSScreen.main else { return }
		let screenRect = screen.visibleFrame
		let frame = NSRect(x: screenRect.origin.x,
		                   y: screenRect.origin.y,
		                   width: screenRect.width,
		                   height: 100)
		window.setFrame(frame, display: false)
	}
}
