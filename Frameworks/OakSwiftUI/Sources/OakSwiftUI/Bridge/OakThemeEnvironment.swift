import AppKit
import Combine

@MainActor @objc public class OakThemeEnvironment: NSObject, ObservableObject {
    @Published @objc public var fontName: String = "Menlo"
    @Published @objc public var fontSize: CGFloat = 12
    @Published @objc public var backgroundColor: NSColor = .textBackgroundColor
    @Published @objc public var foregroundColor: NSColor = .textColor
    @Published @objc public var selectionColor: NSColor = .selectedTextBackgroundColor
    @Published @objc public var keywordColor: NSColor = .systemBlue
    @Published @objc public var commentColor: NSColor = .systemGreen
    @Published @objc public var stringColor: NSColor = .systemRed

    @objc public var font: NSFont {
        NSFont(name: fontName, size: fontSize) ?? .monospacedSystemFont(ofSize: fontSize, weight: .regular)
    }

    @objc public func applyTheme(_ dict: NSDictionary) {
        if let v = dict["fontName"] as? String { fontName = v }
        if let v = dict["fontSize"] as? NSNumber { fontSize = CGFloat(v.doubleValue) }
        if let v = dict["backgroundColor"] as? NSColor { backgroundColor = v }
        if let v = dict["foregroundColor"] as? NSColor { foregroundColor = v }
        if let v = dict["selectionColor"] as? NSColor { selectionColor = v }
        if let v = dict["keywordColor"] as? NSColor { keywordColor = v }
        if let v = dict["commentColor"] as? NSColor { commentColor = v }
        if let v = dict["stringColor"] as? NSColor { stringColor = v }
    }
}
