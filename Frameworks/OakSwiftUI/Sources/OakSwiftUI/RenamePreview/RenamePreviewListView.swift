import SwiftUI
import UniformTypeIdentifiers

struct RenamePreviewListView: View {
	@ObservedObject var viewModel: RenamePreviewViewModel
	@ObservedObject var theme: OakThemeEnvironment
	var onConfirm: () -> Void
	var onCancel: () -> Void

	var body: some View {
		VStack(spacing: 0) {
			Text("\(viewModel.totalEdits) changes in \(viewModel.totalFiles) file\(viewModel.totalFiles == 1 ? "" : "s")")
				.font(.system(size: theme.fontSize))
				.foregroundStyle(.secondary)
				.frame(maxWidth: .infinity, alignment: .leading)
				.padding(.horizontal, 12)
				.padding(.vertical, 8)

			List {
				ForEach(viewModel.groups) { group in
					Section {
						ForEach(Array(group.items.enumerated()), id: \.offset) { _, item in
							RenamePreviewRowView(item: item, theme: theme)
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

			HStack {
				Spacer()
				Button("Cancel") { onCancel() }
					.keyboardShortcut(.cancelAction)
				Button("Apply") { onConfirm() }
					.keyboardShortcut(.defaultAction)
			}
			.padding(12)
		}
	}
}

struct RenamePreviewRowView: View {
	let item: OakRenameItem
	@ObservedObject var theme: OakThemeEnvironment

	var body: some View {
		VStack(alignment: .leading, spacing: 2) {
			HStack(spacing: 8) {
				Text("\(item.line + 1)")
					.foregroundStyle(.secondary)
					.frame(minWidth: 30, alignment: .trailing)
				Text(item.oldText)
					.strikethrough()
					.foregroundStyle(.red.opacity(0.8))
					.lineLimit(1)
					.truncationMode(.tail)
			}
			HStack(spacing: 8) {
				Text("")
					.frame(minWidth: 30, alignment: .trailing)
				Text(item.newText)
					.foregroundStyle(.green.opacity(0.8))
					.lineLimit(1)
					.truncationMode(.tail)
			}
		}
		.frame(maxWidth: .infinity, alignment: .leading)
		.font(.system(size: theme.fontSize, design: .monospaced))
	}
}
