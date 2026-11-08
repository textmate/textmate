import SwiftUI

struct CommandPaletteView: View {
	@ObservedObject var viewModel: CommandPaletteViewModel
	@EnvironmentObject var theme: OakThemeEnvironment
	@FocusState private var isSearchFieldFocused: Bool

	var body: some View {
		VStack(spacing: 0) {
			searchField
			Divider()
			resultsList
		}
		.background(Color(nsColor: theme.backgroundColor).opacity(0.85))
		.clipShape(RoundedRectangle(cornerRadius: 6))
		.onKeyPress(.downArrow) { viewModel.selectNext(); return .handled }
		.onKeyPress(.upArrow) { viewModel.selectPrevious(); return .handled }
		.onKeyPress(.return) { viewModel.acceptSelection(); return .handled }
		.onKeyPress(.escape) { viewModel.requestDismiss(); return .handled }
		.onAppear { isSearchFieldFocused = true }
	}

	private var searchField: some View {
		HStack(spacing: 6) {
			if viewModel.activeMode != .recentProjects {
				modePill
			}
			TextField(viewModel.activeMode.placeholder, text: $viewModel.filterText)
				.textFieldStyle(.plain)
				.font(.system(size: theme.fontSize, design: .monospaced))
				.focused($isSearchFieldFocused)
		}
		.padding(.horizontal, 12)
		.padding(.vertical, 8)
	}

	private var modePill: some View {
		Text(viewModel.activeMode.label)
			.font(.system(size: max(theme.fontSize - 2, 9), weight: .medium))
			.padding(.horizontal, 6)
			.padding(.vertical, 2)
			.background(Color.accentColor.opacity(0.2))
			.foregroundStyle(Color.accentColor)
			.clipShape(RoundedRectangle(cornerRadius: 4))
	}

	private var resultsList: some View {
		ScrollViewReader { proxy in
			ScrollView(.vertical) {
				LazyVStack(spacing: 0) {
					ForEach(Array(viewModel.filteredItems.enumerated()), id: \.element.id) { index, ranked in
						CommandPaletteRowView(
							item: ranked.item,
							matchedIndices: ranked.matchedIndices,
							isSelected: index == viewModel.selectedIndex
						)
						.id(ranked.id)
						.contentShape(Rectangle())
						.onTapGesture {
							if ranked.item.enabled {
								viewModel.selectedIndex = index
								viewModel.acceptSelection()
							}
						}
					}
				}
				.padding(.vertical, 4)
				.padding(.horizontal, 4)
			}
			.frame(maxHeight: max(theme.fontSize * 2.4, 32) * 10 + 8)
			.onChange(of: viewModel.selectedIndex) { _, newValue in
				guard newValue < viewModel.filteredItems.count else { return }
				withAnimation(.easeOut(duration: 0.1)) {
					proxy.scrollTo(viewModel.filteredItems[newValue].id, anchor: .center)
				}
			}
		}
	}
}
