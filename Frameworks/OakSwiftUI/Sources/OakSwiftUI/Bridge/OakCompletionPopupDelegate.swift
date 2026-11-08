import AppKit

@objc public protocol OakCompletionPopupDelegate: AnyObject {
	func completionPopup(_ popup: OakCompletionPopup, didSelectItem item: OakCompletionItem)
	func completionPopupDidDismiss(_ popup: OakCompletionPopup)
	@objc optional func completionPopup(_ popup: OakCompletionPopup, resolveItem item: OakCompletionItem)
}
