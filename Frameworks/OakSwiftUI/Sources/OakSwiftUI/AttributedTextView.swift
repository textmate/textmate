import SwiftUI

struct AttributedTextView: NSViewRepresentable {
	let attributedString: NSAttributedString
	let fontSize: CGFloat
	var maxLayoutWidth: CGFloat = 240

	func makeNSView(context: Context) -> NSTextField {
		let field = NSTextField(frame: .zero)
		field.isEditable = false
		field.isSelectable = true
		field.isBordered = false
		field.drawsBackground = false
		field.lineBreakMode = .byWordWrapping
		field.preferredMaxLayoutWidth = maxLayoutWidth
		field.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
		return field
	}

	func updateNSView(_ field: NSTextField, context: Context) {
		field.attributedStringValue = attributedString
		field.preferredMaxLayoutWidth = maxLayoutWidth
	}
}
