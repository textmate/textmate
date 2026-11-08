import AppKit

@objc public final class OakTerminalTheme: NSObject {
    @objc public let background: NSColor
    @objc public let foreground: NSColor
    @objc public let caret: NSColor
    @objc public let selection: NSColor
    @objc public let syntaxColors: [NSColor]

    @objc public convenience init(background: NSColor, foreground: NSColor, caret: NSColor, selection: NSColor) {
        self.init(background: background, foreground: foreground, caret: caret, selection: selection, syntaxColors: [])
    }

    @objc public init(background: NSColor, foreground: NSColor, caret: NSColor, selection: NSColor, syntaxColors: [NSColor]) {
        self.background = background
        self.foreground = foreground
        self.caret = caret
        self.selection = selection
        self.syntaxColors = syntaxColors
    }
}
