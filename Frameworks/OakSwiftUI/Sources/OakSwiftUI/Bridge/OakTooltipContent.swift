import AppKit

@objc public class OakTooltipSection: NSObject {
	@objc public var label: String
	private var _content: NSAttributedString?
	private var _contentProvider: (() -> NSAttributedString)?

	@objc public var isEager: Bool { _content != nil }

	@objc public var content: NSAttributedString {
		if let c = _content { return c }
		let c = _contentProvider?() ?? NSAttributedString()
		_content = c
		_contentProvider = nil
		return c
	}

	@objc public init(label: String, content: NSAttributedString) {
		self.label = label
		self._content = content
		super.init()
	}

	@objc public init(label: String, contentProvider: @escaping () -> NSAttributedString) {
		self.label = label
		self._contentProvider = contentProvider
		super.init()
	}
}

@objc public class OakTooltipContent: NSObject {
	@objc public var sections: [OakTooltipSection]

	@objc public init(sections: [OakTooltipSection]) {
		self.sections = sections
		super.init()
	}
}
