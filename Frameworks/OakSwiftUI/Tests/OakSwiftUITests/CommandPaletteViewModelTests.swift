import Foundation
import Testing
@testable import OakSwiftUI

@MainActor
@Suite("CommandPaletteViewModel")
struct CommandPaletteViewModelTests {
	@Test func defaultModeIsRecentProjects() {
		let vm = CommandPaletteViewModel()
		#expect(vm.activeMode == .recentProjects)
	}

	@Test func typingGreaterThanSwitchesToCommandsMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = ">format"
		#expect(vm.activeMode == .commands)
		#expect(vm.queryText == "format")
	}

	@Test func typingAtSwitchesToSymbolsMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = "@viewDidLoad"
		#expect(vm.activeMode == .symbols)
		#expect(vm.queryText == "viewDidLoad")
	}

	@Test func typingColonSwitchesToGoToLineMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = ":42"
		#expect(vm.activeMode == .goToLine)
		#expect(vm.queryText == "42")
	}

	@Test func typingTildeSwitchesToSettingsMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = "~wrap"
		#expect(vm.activeMode == .settings)
		#expect(vm.queryText == "wrap")
	}

	@Test func typingSlashSwitchesToFindMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = "/auth"
		#expect(vm.activeMode == .findInProject)
		#expect(vm.queryText == "auth")
	}

	@Test func typingHashSwitchesToBundleEditorMode() {
		let vm = CommandPaletteViewModel()
		vm.filterText = "#ruby"
		#expect(vm.activeMode == .bundleEditor)
		#expect(vm.queryText == "ruby")
	}

	@Test func emptyFilterDefaultsToRecentProjects() {
		let vm = CommandPaletteViewModel()
		vm.filterText = ">test"
		vm.filterText = ""
		#expect(vm.activeMode == .recentProjects)
	}

	@Test func fuzzyFilteringWorks() {
		let vm = CommandPaletteViewModel()
		let items = [
			OakCommandPaletteItem(title: "Format File", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "menu:formatFile:"),
			OakCommandPaletteItem(title: "Find in Project", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "menu:findInProject:"),
			OakCommandPaletteItem(title: "Open Recent", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "menu:openRecent:"),
		]
		vm.setItems(items, forMode: .commands)
		vm.filterText = ">fmt"
		#expect(vm.filteredItems.count == 1)
		#expect(vm.filteredItems.first?.item.title == "Format File")
	}

	@Test func frecencyBoostIncreasesScoreForRecentItems() {
		let vm = CommandPaletteViewModel()
		let now = Date().timeIntervalSinceReferenceDate

		vm.updateFrecency(for: "menu:formatFile:", at: now)
		vm.updateFrecency(for: "menu:formatFile:", at: now)
		vm.updateFrecency(for: "menu:formatFile:", at: now)

		let boost = vm.frecencyBoost(for: "menu:formatFile:")
		#expect(boost > 0)
	}

	@Test func frecencyBoostDecaysOverTime() {
		let vm = CommandPaletteViewModel()
		let now = Date().timeIntervalSinceReferenceDate
		let oneWeekAgo = now - 168 * 3600

		vm.updateFrecency(for: "old", at: oneWeekAgo)
		vm.updateFrecency(for: "recent", at: now)

		let oldBoost = vm.frecencyBoost(for: "old")
		let recentBoost = vm.frecencyBoost(for: "recent")
		#expect(recentBoost > oldBoost)
	}

	@Test func selectionNavigationWorks() {
		let vm = CommandPaletteViewModel()
		let items = [
			OakCommandPaletteItem(title: "Aa", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "a"),
			OakCommandPaletteItem(title: "Bb", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "b"),
			OakCommandPaletteItem(title: "Cc", subtitle: "", keyEquivalent: "", category: .menuAction, actionIdentifier: "c"),
		]
		vm.setItems(items, forMode: .commands)
		vm.filterText = ">"
		#expect(vm.selectedIndex == 0)
		vm.selectNext()
		#expect(vm.selectedIndex == 1)
		vm.selectNext()
		#expect(vm.selectedIndex == 2)
		vm.selectNext()
		#expect(vm.selectedIndex == 2)
		vm.selectPrevious()
		#expect(vm.selectedIndex == 1)
	}
}
