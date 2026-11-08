import AppKit

@objc public enum OakCommandPaletteCategory: Int, Sendable {
	case menuAction = 0
	case bundleCommand
	case recentProject
	case symbol
	case bundleEditor
	case goToLine
	case findInProject
	case setting
}

@MainActor @objc public class OakCommandPaletteItem: NSObject, Identifiable {
	public let id = UUID()
	@objc public let title: String
	@objc public let subtitle: String
	@objc public let keyEquivalent: String
	@objc public let category: OakCommandPaletteCategory
	@objc public let actionIdentifier: String
	@objc public var icon: NSImage?
	@objc public var enabled: Bool = true
	@objc public weak var sourceMenuItem: NSMenuItem?

	@objc public init(title: String, subtitle: String, keyEquivalent: String,
	                  category: OakCommandPaletteCategory, actionIdentifier: String) {
		self.title = title
		self.subtitle = subtitle
		self.keyEquivalent = keyEquivalent
		self.category = category
		self.actionIdentifier = actionIdentifier
		super.init()
	}

	public var categorySymbolName: String {
		switch category {
		case .menuAction:     return "terminal.fill"
		case .bundleCommand:  return "terminal.fill"
		case .recentProject:  return "folder.fill"
		case .symbol:         return "number"
		case .bundleEditor:   return "puzzlepiece.fill"
		case .goToLine:       return "text.cursor"
		case .findInProject:  return "magnifyingglass"
		case .setting:        return "gearshape"
		@unknown default:     return "questionmark.square"
		}
	}
}
