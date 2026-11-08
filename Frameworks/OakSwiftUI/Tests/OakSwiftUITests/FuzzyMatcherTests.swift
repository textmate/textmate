import Testing
@testable import OakSwiftUI

@Test func exactMatch() {
    let result = FuzzyMatcher.score("hello", query: "hello")
    #expect(result != nil)
    #expect(result!.score > 0)
}

@Test func prefixMatch() {
    let result = FuzzyMatcher.score("hello", query: "hel")
    #expect(result != nil)
    #expect(result!.score > 0)
}

@Test func subsequenceMatch() {
    let result = FuzzyMatcher.score("NSTableView", query: "ntv")
    #expect(result != nil)
}

@Test func noMatch() {
    let result = FuzzyMatcher.score("hello", query: "xyz")
    #expect(result == nil)
}

@Test func caseInsensitive() {
    let result = FuzzyMatcher.score("NSTableView", query: "nstable")
    #expect(result != nil)
}

@Test func emptyQuery() {
    let result = FuzzyMatcher.score("anything", query: "")
    #expect(result != nil)
}

@Test func filterAndSort() {
    let items = ["NSView", "NSTableView", "UIView", "NSTextField"]
    let filtered = FuzzyMatcher.filter(items, query: "nsv", keyPath: \.self)
    #expect(filtered.count == 2)
    #expect(filtered.first == "NSView")
}
