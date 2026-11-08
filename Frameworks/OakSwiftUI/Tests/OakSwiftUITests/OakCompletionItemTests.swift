import Testing
import AppKit
@testable import OakSwiftUI

@Test func itemProperties() {
    let item = OakCompletionItem(
        label: "myFunction",
        insertText: "myFunction()",
        detail: "func myFunction() -> Int",
        kind: 3
    )
    #expect(item.label == "myFunction")
    #expect(item.insertText == "myFunction()")
    #expect(item.detail == "func myFunction() -> Int")
    #expect(item.kind == 3)
    #expect(item.icon == nil)
}

@Test func itemDefaultInsertText() {
    let item = OakCompletionItem(label: "value", insertText: nil, detail: "", kind: 6)
    #expect(item.effectiveInsertText == "value")
}
