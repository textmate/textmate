import SwiftUI

struct CompletionRowView: View {
	let item: OakCompletionItem
	let isSelected: Bool
	@EnvironmentObject var theme: OakThemeEnvironment

	private var kindColor: Color {
		switch item.kind {
		case 2, 3, 4:  return .blue     // Method, Function, Constructor
		case 5, 6, 10: return .cyan     // Field, Variable, Property
		case 7, 8, 22: return .purple   // Class, Interface, Struct
		case 13, 20:   return .orange   // Enum, EnumMember
		case 14:       return .pink     // Keyword
		case 21:       return .teal     // Constant
		case 9:        return .indigo   // Module
		default:       return .gray
		}
	}

	var body: some View {
		HStack(spacing: 6) {
			Image(systemName: item.kindSymbolName)
				.font(.system(size: max(theme.fontSize - 2, 9)))
				.foregroundStyle(isSelected ? Color(nsColor: .alternateSelectedControlTextColor) : kindColor)
				.frame(width: 18, alignment: .center)

			Text(item.label)
				.font(.system(size: theme.fontSize, design: .monospaced))
				.foregroundStyle(isSelected ? Color(nsColor: .alternateSelectedControlTextColor) : .primary)
				.lineLimit(item.multiline ? 3 : 1)
				.fixedSize(horizontal: false, vertical: item.multiline)

			Spacer(minLength: 4)

			if !item.detail.isEmpty {
				Text(item.detail)
					.font(.system(size: max(theme.fontSize - 2, 9)))
					.foregroundStyle(isSelected ? Color(nsColor: .alternateSelectedControlTextColor).opacity(0.7) : .secondary)
					.lineLimit(1)
			}
		}
		.padding(.horizontal, 8)
		.padding(.vertical, 3)
		.background(isSelected ? Color.accentColor : Color.clear)
		.cornerRadius(4)
	}
}
