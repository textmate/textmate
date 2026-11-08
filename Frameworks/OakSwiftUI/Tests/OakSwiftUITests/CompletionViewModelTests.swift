import Testing
import AppKit
@testable import OakSwiftUI

@Test @MainActor func initialState() {
    let vm = CompletionViewModel()
    #expect(vm.filteredItems.isEmpty)
    #expect(vm.selectedIndex == 0)
}

@Test @MainActor func setItemsPopulatesFiltered() {
    let vm = CompletionViewModel()
    vm.setItems([
        OakCompletionItem(label: "foo", insertText: nil, detail: "", kind: 1),
        OakCompletionItem(label: "bar", insertText: nil, detail: "", kind: 1),
    ])
    #expect(vm.filteredItems.count == 2)
}

@Test @MainActor func filterReducesItems() {
    let vm = CompletionViewModel()
    vm.setItems([
        OakCompletionItem(label: "fooBar", insertText: nil, detail: "", kind: 1),
        OakCompletionItem(label: "bazQux", insertText: nil, detail: "", kind: 1),
    ])
    vm.updateFilter("foo")
    #expect(vm.filteredItems.count == 1)
    #expect(vm.filteredItems.first?.label == "fooBar")
}

@Test @MainActor func selectNext() {
    let vm = CompletionViewModel()
    vm.setItems([
        OakCompletionItem(label: "a", insertText: nil, detail: "", kind: 1),
        OakCompletionItem(label: "b", insertText: nil, detail: "", kind: 1),
        OakCompletionItem(label: "c", insertText: nil, detail: "", kind: 1),
    ])
    #expect(vm.selectedIndex == 0)
    vm.selectNext()
    #expect(vm.selectedIndex == 1)
    vm.selectNext()
    #expect(vm.selectedIndex == 2)
    vm.selectNext()
    #expect(vm.selectedIndex == 2)
}

@Test @MainActor func selectPrevious() {
    let vm = CompletionViewModel()
    vm.setItems([
        OakCompletionItem(label: "a", insertText: nil, detail: "", kind: 1),
        OakCompletionItem(label: "b", insertText: nil, detail: "", kind: 1),
    ])
    vm.selectNext()
    #expect(vm.selectedIndex == 1)
    vm.selectPrevious()
    #expect(vm.selectedIndex == 0)
    vm.selectPrevious()
    #expect(vm.selectedIndex == 0)
}

@Test @MainActor func filterResetsSelection() {
    let vm = CompletionViewModel()
    vm.setItems([
        OakCompletionItem(label: "alpha", insertText: nil, detail: "", kind: 1),
        OakCompletionItem(label: "beta", insertText: nil, detail: "", kind: 1),
    ])
    vm.selectNext()
    vm.updateFilter("a")
    #expect(vm.selectedIndex == 0)
}
