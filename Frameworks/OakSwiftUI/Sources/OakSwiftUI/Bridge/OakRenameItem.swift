import Foundation

@MainActor @objc public class OakRenameItem: NSObject {
	@objc public let filePath: String
	@objc public let displayPath: String
	@objc public let line: Int
	@objc public let oldText: String
	@objc public let newText: String

	@objc public init(filePath: String, displayPath: String, line: Int, oldText: String, newText: String) {
		self.filePath = filePath
		self.displayPath = displayPath
		self.line = line
		self.oldText = oldText
		self.newText = newText
	}
}
