import Foundation

struct ReferenceGroup: Identifiable {
	let id: String
	let displayPath: String
	let items: [OakReferenceItem]
}

@MainActor
final class ReferencesViewModel: ObservableObject {
	@Published var groups: [ReferenceGroup] = []

	init(items: [OakReferenceItem]) {
		var groupMap: [String: [OakReferenceItem]] = [:]
		var order: [String] = []

		for item in items {
			if groupMap[item.filePath] == nil {
				order.append(item.filePath)
			}
			groupMap[item.filePath, default: []].append(item)
		}

		self.groups = order.compactMap { path in
			guard let items = groupMap[path] else { return nil }
			return ReferenceGroup(
				id: path,
				displayPath: items.first?.displayPath ?? path,
				items: items.sorted { $0.line < $1.line }
			)
		}
	}
}
