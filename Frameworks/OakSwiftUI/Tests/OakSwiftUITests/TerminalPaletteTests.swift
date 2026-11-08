import Testing
import AppKit
@testable import OakSwiftUI

@Test func terminalPalettePreservesSyntaxAccents() {
    let accent = NSColor(srgbRed: 0.3, green: 0.8, blue: 0.5, alpha: 1)
    let palette = terminalPalette(background: .black, foreground: .white, syntaxColors: [accent])
    #expect(palette.count == 16)
    #expect(abs(Int(palette[1].red) - Int(0.3 * 65535)) <= 1)
    #expect(abs(Int(palette[1].green) - Int(0.8 * 65535)) <= 1)
    #expect(palette[1] != palette[9])
}

@Test func terminalPaletteAddsReadableVariationToPlainThemes() {
    for (background, foreground) in [(NSColor.black, NSColor.white), (.white, .black)] {
        let palette = terminalPalette(background: background, foreground: foreground,
                                      syntaxColors: Array(repeating: foreground, count: 6))
        #expect(palette.count == 16)
        let accents = Array(palette[1...6])
        for index in accents.indices {
            #expect(!accents.prefix(index).contains(accents[index]))
            let color = accents[index]
            func linear(_ component: UInt16) -> Double {
                let value = Double(component) / 65535
                return value <= 0.04045 ? value / 12.92 : pow((value + 0.055) / 1.055, 2.4)
            }
            let luminance = 0.2126 * linear(color.red) + 0.7152 * linear(color.green) + 0.0722 * linear(color.blue)
            let contrast = background == .black ? (luminance + 0.05) / 0.05 : 1.05 / (luminance + 0.05)
            #expect(contrast >= 2.99)
        }
    }
}
