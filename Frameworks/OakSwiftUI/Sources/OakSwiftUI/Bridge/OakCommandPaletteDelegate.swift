import AppKit

@MainActor @objc public protocol OakCommandPaletteDelegate: AnyObject {
	func commandPaletteDidSelectItem(_ item: OakCommandPaletteItem)
	func commandPaletteDidDismiss()
	func commandPaletteRequestItems(forMode mode: Int) -> [OakCommandPaletteItem]
	func commandPaletteSearchDocument(_ query: String) -> [OakCommandPaletteItem]
}
