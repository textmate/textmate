import SwiftUI
import Combine

public enum ToastType {
	case info
	case warning
	case error
	case success
}

public struct Toast: Identifiable, Equatable {
	public let id = UUID()
	public let message: String
	public let type: ToastType
	public let duration: TimeInterval
	public let actions: [String]?
	public let onAction: ((String?) -> Void)?

	public static func == (lhs: Toast, rhs: Toast) -> Bool {
		lhs.id == rhs.id
	}
}

@MainActor
public class ToastViewModel: ObservableObject {
	@Published public var currentToast: Toast?
	private var dismissTask: Task<Void, Never>?
	private var pendingInteractive: [Toast] = []

	nonisolated init() {}

	public func show(message: String, type: ToastType, duration: TimeInterval = 3.0) {
		if currentToast?.actions != nil {
			// Don't overwrite interactive toast — it needs a user response
			return
		}
		self.currentToast = Toast(message: message, type: type, duration: duration, actions: nil, onAction: nil)
		self.dismissTask?.cancel()
		self.dismissTask = Task {
			try? await Task.sleep(nanoseconds: UInt64(duration * 1_000_000_000))
			if !Task.isCancelled {
				self.currentToast = nil
				self.showNextQueued()
			}
		}
	}

	public func showInteractive(message: String, type: ToastType, actions: [String], onAction: @escaping (String?) -> Void) {
		let toast = Toast(message: message, type: type, duration: 0, actions: actions, onAction: onAction)
		if currentToast?.actions != nil {
			pendingInteractive.append(toast)
		} else {
			self.dismissTask?.cancel()
			self.currentToast = toast
		}
	}

	public func showNextQueued() {
		if !pendingInteractive.isEmpty {
			self.currentToast = pendingInteractive.removeFirst()
		}
	}

	public func dismissCurrent() {
		if let toast = currentToast {
			toast.onAction?(nil)
		}
		currentToast = nil
		showNextQueued()
	}
}
