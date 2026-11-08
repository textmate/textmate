import SwiftUI
import UniformTypeIdentifiers

struct ReferencesListView: View {
	@ObservedObject var viewModel: ReferencesViewModel
	@ObservedObject var theme: OakThemeEnvironment
	var onSelect: (OakReferenceItem) -> Void

	var body: some View {
		List {
			ForEach(viewModel.groups) { group in
				Section {
					ForEach(Array(group.items.enumerated()), id: \.offset) { _, item in
						Button {
							onSelect(item)
						} label: {
							ReferenceRowView(item: item, theme: theme)
						}
						.buttonStyle(.plain)
					}
				} header: {
					HStack(spacing: 4) {
						Image(nsImage: NSWorkspace.shared.icon(for: UTType(filenameExtension: (group.displayPath as NSString).pathExtension) ?? .plainText))
							.resizable()
							.frame(width: 16, height: 16)
						Text(group.displayPath)
							.fontWeight(.semibold)
						Text("(\(group.items.count))")
							.foregroundStyle(.secondary)
					}
				}
			}
		}
		.listStyle(.sidebar)
		.font(.system(size: theme.fontSize, design: .monospaced))
	}
}

struct ReferenceRowView: View {
	let item: OakReferenceItem
	@ObservedObject var theme: OakThemeEnvironment

	var body: some View {
		HStack(spacing: 8) {
			Text("\(item.line + 1)")
				.foregroundStyle(.secondary)
				.frame(minWidth: 30, alignment: .trailing)
			Text(item.content)
				.lineLimit(1)
				.truncationMode(.tail)
		}
		.frame(maxWidth: .infinity, alignment: .leading)
		.font(.system(size: theme.fontSize, design: .monospaced))
	}
}
