import AppKit

@objc public class OakCompletionItem: NSObject, Identifiable, @unchecked Sendable {
	public let id = UUID()
	@objc public let label: String
	@objc public private(set) var insertText: String?
	@objc public let detail: String
	@objc public let kind: Int
	@objc public var icon: NSImage?
	@objc public var isSnippet: Bool = false
	@objc public var documentation: NSAttributedString?
	@objc public var originalItem: NSDictionary?
	@objc public var isResolved: Bool = false
	@objc public var multiline: Bool = false

	@objc public var effectiveInsertText: String {
		insertText ?? label
	}

	/// SF Symbol name for the LSP CompletionItemKind
	public var kindSymbolName: String {
		switch kind {
		case 1:  return "doc.text"           // Text
		case 2:  return "m.square"           // Method
		case 3:  return "f.square"           // Function
		case 4:  return "wrench"             // Constructor
		case 5:  return "character"          // Field
		case 6:  return "v.square"           // Variable
		case 7:  return "c.square"           // Class
		case 8:  return "i.square"           // Interface
		case 9:  return "shippingbox"        // Module
		case 10: return "p.square"           // Property
		case 11: return "gearshape"          // Unit
		case 12: return "number"             // Value
		case 13: return "e.square"           // Enum
		case 14: return "key"               // Keyword
		case 15: return "text.snippet"       // Snippet
		case 16: return "paintpalette"       // Color
		case 17: return "doc"               // File
		case 18: return "arrow.right.circle" // Reference
		case 19: return "folder"            // Folder
		case 20: return "e.square.fill"      // EnumMember
		case 21: return "k.square"           // Constant
		case 22: return "s.square"           // Struct
		case 23: return "bolt"              // Event
		case 24: return "o.square"           // Operator
		case 25: return "t.square"           // TypeParameter
		default: return "questionmark.square"
		}
	}

	/// Short label for the LSP CompletionItemKind
	public var kindLabel: String {
		switch kind {
		case 1:  return "text"
		case 2:  return "method"
		case 3:  return "func"
		case 4:  return "init"
		case 5:  return "field"
		case 6:  return "var"
		case 7:  return "class"
		case 8:  return "iface"
		case 9:  return "module"
		case 10: return "prop"
		case 11: return "unit"
		case 12: return "value"
		case 13: return "enum"
		case 14: return "keyword"
		case 15: return "snippet"
		case 16: return "color"
		case 17: return "file"
		case 18: return "ref"
		case 19: return "folder"
		case 20: return "member"
		case 21: return "const"
		case 22: return "struct"
		case 23: return "event"
		case 24: return "op"
		case 25: return "type"
		default: return ""
		}
	}

	@objc public func updateInsertText(_ text: String) {
		insertText = text
	}

	@objc public init(label: String, insertText: String?, detail: String, kind: Int) {
		self.label = label
		self.insertText = insertText
		self.detail = detail
		self.kind = kind
		super.init()
	}
}
