import SwiftUI

struct TooltipContentView: View {
	let content: OakTooltipContent
	@EnvironmentObject var theme: OakThemeEnvironment
	@State private var selectedTab: Int = 0

	private var sections: [OakTooltipSection] {
		content.sections
	}

	private var showTabBar: Bool {
		sections.count > 1
	}

	var body: some View {
		if sections.isEmpty {
			Text("No information available")
				.font(.system(size: 11))
				.foregroundStyle(.secondary)
				.padding(12)
		} else {
			VStack(spacing: 0) {
				contentArea
				if showTabBar {
					Divider()
					tabBar
				}
			}
			.frame(maxWidth: 600)
			.background(Color.clear)
		}
	}

	// MARK: - Content Area

	@ViewBuilder
	private var contentArea: some View {
		ScrollView(.vertical) {
			sectionContent(for: selectedTab)
				.padding(.horizontal, 12)
				.padding(.vertical, 10)
				.frame(maxWidth: .infinity, alignment: .leading)
				.id(selectedTab)
		}
		.scrollContentBackground(.hidden)
		.frame(maxHeight: 350)
		.animation(.easeInOut(duration: 0.1), value: selectedTab)
	}

	@ViewBuilder
	private func sectionContent(for index: Int) -> some View {
		let safeIndex = min(max(index, 0), max(sections.count - 1, 0))
		if safeIndex < sections.count {
			let section = sections[safeIndex]
			AttributedTextView(
				attributedString: section.content,
				fontSize: theme.fontSize,
				maxLayoutWidth: 576
			)
			.fixedSize(horizontal: false, vertical: true)
			.transition(.opacity)
		}
	}

	// MARK: - Tab Bar

	private var tabBar: some View {
		TabBarView(
			sections: sections,
			selectedTab: $selectedTab
		)
		.frame(height: 32)
		.padding(.horizontal, 8)
	}
}

// MARK: - Tab Bar

private struct TabBarView: View {
	let sections: [OakTooltipSection]
	@Binding var selectedTab: Int
	@State private var contentOverflows: Bool = false
	@State private var contentWidth: CGFloat = 0
	@State private var containerWidth: CGFloat = 0

	var body: some View {
		HStack(spacing: 0) {
			if contentOverflows {
				chevronButton(direction: .leading)
			}

			ScrollViewReader { proxy in
				ScrollView(.horizontal, showsIndicators: false) {
					HStack(spacing: 4) {
						ForEach(Array(sections.enumerated()), id: \.offset) { index, section in
							tabButton(label: section.label, index: index)
								.id(index)
						}
					}
					.padding(.horizontal, 4)
					.background(
						GeometryReader { inner in
							Color.clear.preference(
								key: ContentWidthKey.self,
								value: inner.size.width
							)
						}
					)
				}
				.onChange(of: selectedTab) { _, newTab in
					withAnimation(.easeInOut(duration: 0.15)) {
						proxy.scrollTo(newTab, anchor: .center)
					}
				}
			}
			.background(
				GeometryReader { outer in
					Color.clear.preference(
						key: ContainerWidthKey.self,
						value: outer.size.width
					)
				}
			)
			.onPreferenceChange(ContentWidthKey.self) { width in
				contentWidth = width
				contentOverflows = contentWidth > containerWidth + 1
			}
			.onPreferenceChange(ContainerWidthKey.self) { width in
				containerWidth = width
				contentOverflows = contentWidth > containerWidth + 1
			}

			if contentOverflows {
				chevronButton(direction: .trailing)
			}
		}
	}

	private enum ChevronDirection {
		case leading, trailing
	}

	private func chevronButton(direction: ChevronDirection) -> some View {
		Button {
			// Scroll to reveal more tabs without changing selection
			let scrollTarget: Int
			switch direction {
			case .leading:
				scrollTarget = max(selectedTab - 1, 0)
			case .trailing:
				scrollTarget = min(selectedTab + 1, sections.count - 1)
			}
			// Use ScrollViewReader to scroll — this triggers via selectedTab's onChange
			// which calls proxy.scrollTo. We temporarily shift selection to scroll.
			// Better approach: just scroll the visible tab into view
			selectedTab = scrollTarget
		} label: {
			Image(systemName: direction == .leading ? "chevron.left" : "chevron.right")
				.font(.system(size: 9, weight: .semibold))
				.foregroundStyle(.secondary)
				.frame(width: 16, height: 24)
				.contentShape(Rectangle())
		}
		.buttonStyle(.plain)
	}

	private func tabButton(label: String, index: Int) -> some View {
		Button {
			selectedTab = index
		} label: {
			Text(label)
				.font(.system(size: 11))
				.foregroundStyle(index == selectedTab ? .primary : .secondary)
				.padding(.horizontal, 10)
				.padding(.vertical, 4)
				.background {
					if index == selectedTab {
						Capsule()
							.fill(Color.primary.opacity(0.15))
					}
				}
		}
		.buttonStyle(.plain)
	}
}

// MARK: - Preference Keys

private struct ContentWidthKey: PreferenceKey {
	nonisolated(unsafe) static var defaultValue: CGFloat = 0
	static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {
		value = max(value, nextValue())
	}
}

private struct ContainerWidthKey: PreferenceKey {
	nonisolated(unsafe) static var defaultValue: CGFloat = 0
	static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {
		value = max(value, nextValue())
	}
}
