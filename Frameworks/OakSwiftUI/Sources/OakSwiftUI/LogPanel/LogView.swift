import SwiftUI

struct LogView: View {
	@ObservedObject var model: LogViewModel
	@State private var searchText = ""
	@State private var autoScroll = true
	@State private var selection = Set<UUID>()

	var filteredEntries: [LogEntry] {
		if searchText.isEmpty {
			return model.entries
		} else {
			return model.entries.filter { $0.message.localizedCaseInsensitiveContains(searchText) }
		}
	}

	var body: some View {
		VStack(spacing: 0) {
			ScrollViewReader { proxy in
				List(filteredEntries, selection: $selection) { entry in
					LogRow(entry: entry)
						.id(entry.id)
						.listRowSeparator(.hidden)
				}
				.listStyle(PlainListStyle())
				.onChange(of: model.entries.count) { _, _ in
					if autoScroll {
						withAnimation {
							proxy.scrollTo(filteredEntries.last?.id, anchor: .bottom)
						}
					}
				}
				.onChange(of: searchText) { _, _ in
					if autoScroll, let last = filteredEntries.last {
						proxy.scrollTo(last.id, anchor: .bottom)
					}
				}
			}
			.copyable(entries: filteredEntries, selection: selection)

			HStack(spacing: 12) {
				TextField("Filter…", text: $searchText)
					.textFieldStyle(RoundedBorderTextFieldStyle())
					.overlay(alignment: .trailing) {
						if !searchText.isEmpty {
							Button {
								searchText = ""
							} label: {
								Image(systemName: "xmark.circle.fill")
									.foregroundStyle(.secondary)
							}
							.buttonStyle(.plain)
							.padding(.trailing, 4)
						}
					}

				Toggle("Auto-scroll", isOn: $autoScroll)
					.toggleStyle(.switch)
					.controlSize(.small)

				Button("Clear log") {
					model.clear()
					selection.removeAll()
				}
				.controlSize(.small)
			}
			.padding(.horizontal, 12)
			.padding(.vertical, 8)
			.background(Color(NSColor.windowBackgroundColor))
		}
		.frame(minWidth: 400, minHeight: 300)
	}
}

// Cmd+C support for selected log entries
private struct CopyableModifier: ViewModifier {
	let entries: [LogEntry]
	let selection: Set<UUID>

	private static let timeFormatter: DateFormatter = {
		let f = DateFormatter()
		f.dateFormat = "HH:mm:ss.SSS"
		return f
	}()

	func body(content: Content) -> some View {
		content.onCopyCommand {
			let selected = entries.filter { selection.contains($0.id) }
			guard !selected.isEmpty else { return [] }
			let text = selected.map { entry in
				"\(CopyableModifier.timeFormatter.string(from: entry.date)) [\(entry.source)] \(entry.message)"
			}.joined(separator: "\n")
			return [NSItemProvider(object: text as NSString)]
		}
	}
}

private extension View {
	func copyable(entries: [LogEntry], selection: Set<UUID>) -> some View {
		modifier(CopyableModifier(entries: entries, selection: selection))
	}
}

struct LogRow: View {
	let entry: LogEntry

	private static let timeFormatter: DateFormatter = {
		let f = DateFormatter()
		f.dateFormat = "HH:mm:ss.SSS"
		return f
	}()

	private var arrow: String {
		switch entry.source {
		case "request": return "→"
		case "notify":  return "→"
		case "response": return "←"
		case "event":   return "←"
		case "server":  return "◇"
		case "error":   return "✗"
		default:        return "·"
		}
	}

	private var arrowColor: Color {
		switch entry.source {
		case "request":  return .blue
		case "notify":   return .cyan
		case "response": return .green
		case "event":    return .purple
		case "server":   return .secondary
		case "error":    return .red
		default:         return .secondary
		}
	}

	private var messageColor: Color {
		switch entry.source {
		case "error": return .red
		case "server": return .secondary
		default: return .primary
		}
	}

	var body: some View {
		HStack(alignment: .firstTextBaseline, spacing: 6) {
			Text(arrow)
				.font(.system(size: 12, weight: .bold, design: .monospaced))
				.foregroundColor(arrowColor)
				.frame(width: 14, alignment: .center)

			Text(LogRow.timeFormatter.string(from: entry.date))
				.font(.system(size: 11, design: .monospaced))
				.foregroundColor(.secondary)

			Text(entry.message)
				.font(.system(size: 12, design: .monospaced))
				.foregroundColor(messageColor)
				.fixedSize(horizontal: false, vertical: true)
		}
		.padding(.vertical, 1)
	}
}
