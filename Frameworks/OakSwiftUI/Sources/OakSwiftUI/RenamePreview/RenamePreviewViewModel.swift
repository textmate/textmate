import Foundation

struct RenameFileGroup: Identifiable {
	let id: String
	let displayPath: String
	let items: [OakRenameItem]
}

@MainActor
final class RenamePreviewViewModel: ObservableObject {
	@Published var groups: [RenameFileGroup] = []
	let totalEdits: Int
	let totalFiles: Int

	init(items: [OakRenameItem]) {
		var groupMap: [String: [OakRenameItem]] = [:]
		var order: [String] = []

		for item in items {
			if groupMap[item.filePath] == nil {
				order.append(item.filePath)
			}
			groupMap[item.filePath, default: []].append(item)
		}

		self.groups = order.compactMap { path in
			guard let items = groupMap[path] else { return nil }
			return RenameFileGroup(
				id: path,
				displayPath: items.first?.displayPath ?? path,
				items: items.sorted { $0.line < $1.line }
			)
		}
		self.totalEdits = items.count
		self.totalFiles = order.count
	}
}
