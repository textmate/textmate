import SwiftUI

struct NotificationView: View {
	@ObservedObject var model: ToastViewModel

	var body: some View {
		VStack {
			Spacer()
			if let toast = model.currentToast {
				HStack(spacing: 12) {
					Image(systemName: iconName(for: toast.type))
						.font(.system(size: 18))
						.foregroundStyle(color(for: toast.type))
					Text(toast.message)
						.font(.system(size: 14, weight: .medium))
						.foregroundStyle(.white)
						.lineLimit(toast.actions != nil ? 4 : 2)
					if let actions = toast.actions, !actions.isEmpty {
						ForEach(actions, id: \.self) { action in
							Button(action) {
								toast.onAction?(action)
								withAnimation { model.currentToast = nil }
								model.showNextQueued()
							}
							.buttonStyle(.bordered)
							.tint(color(for: toast.type))
						}
					}
				}
				.id(toast.id)
				.padding(.horizontal, 20)
				.padding(.vertical, 14)
				.frame(maxWidth: toast.actions != nil ? 700 : 500)
				.fixedSize()
				.background(.ultraThinMaterial, in: RoundedRectangle(cornerRadius: 12))
				.environment(\.colorScheme, .dark)
				.shadow(color: .black.opacity(0.3), radius: 10, y: 3)
				.transition(.asymmetric(insertion: .move(edge: .bottom).combined(with: .opacity),
				                        removal: .opacity))
				.onTapGesture {
					if toast.actions != nil {
						withAnimation { model.dismissCurrent() }
					} else {
						withAnimation { model.currentToast = nil }
					}
				}
			}
		}
		.frame(maxWidth: .infinity)
		.animation(.easeInOut(duration: 0.3), value: model.currentToast)
		.padding(.bottom, 8)
	}

	func iconName(for type: ToastType) -> String {
		switch type {
		case .info: return "info.circle.fill"
		case .warning: return "exclamationmark.triangle.fill"
		case .error: return "xmark.circle.fill"
		case .success: return "checkmark.circle.fill"
		}
	}

	func color(for type: ToastType) -> Color {
		switch type {
		case .info: return .blue
		case .warning: return .orange
		case .error: return .red
		case .success: return .green
		}
	}
}
