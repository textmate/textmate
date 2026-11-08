import Testing
import AppKit
@testable import OakSwiftUI

@Test @MainActor func defaultThemeValues() {
    let theme = OakThemeEnvironment()
    #expect(theme.fontName == "Menlo")
    #expect(theme.fontSize == 12)
}

@Test @MainActor func applyThemeFromDictionary() {
    let theme = OakThemeEnvironment()
    let dict: NSDictionary = [
        "fontName": "Monaco",
        "fontSize": NSNumber(value: 14),
        "backgroundColor": NSColor.black,
        "foregroundColor": NSColor.white,
    ]
    theme.applyTheme(dict)
    #expect(theme.fontName == "Monaco")
    #expect(theme.fontSize == 14)
    #expect(theme.backgroundColor == NSColor.black)
    #expect(theme.foregroundColor == NSColor.white)
}

@Test @MainActor func applyThemeIgnoresUnknownKeys() {
    let theme = OakThemeEnvironment()
    let dict: NSDictionary = [
        "unknownKey": "value",
        "fontName": "Courier",
    ]
    theme.applyTheme(dict)
    #expect(theme.fontName == "Courier")
}
