import SwiftUI

struct DocDetailView: View {
	let documentation: NSAttributedString
	var isVerticalLayout: Bool = false
	@EnvironmentObject var theme: OakThemeEnvironment

	var body: some View {
		ScrollView {
			AttributedTextView(attributedString: documentation, fontSize: max(theme.fontSize - 1, 10))
				.frame(maxWidth: .infinity, alignment: .leading)
				.padding(10)
		}
		.modifier(DocPanelFrameModifier(isVerticalLayout: isVerticalLayout))
	}
}

private struct DocPanelFrameModifier: ViewModifier {
	let isVerticalLayout: Bool

	func body(content: Content) -> some View {
		if isVerticalLayout {
			content
				.frame(maxWidth: .infinity)
				.frame(maxHeight: 200)
		} else {
			content
				.frame(width: 260)
		}
	}
}
