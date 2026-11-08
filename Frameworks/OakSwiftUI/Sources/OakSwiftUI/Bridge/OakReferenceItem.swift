import Foundation

@MainActor @objc public class OakReferenceItem: NSObject {
	@objc public let filePath: String
	@objc public let displayPath: String
	@objc public let line: Int
	@objc public let column: Int
	@objc public let content: String

	@objc public init(filePath: String, displayPath: String, line: Int, column: Int, content: String) {
		self.filePath = filePath
		self.displayPath = displayPath
		self.line = line
		self.column = column
		self.content = content
	}
}
