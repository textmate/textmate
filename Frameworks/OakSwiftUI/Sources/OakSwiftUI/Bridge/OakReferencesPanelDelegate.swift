import Foundation

@objc public protocol OakReferencesPanelDelegate: AnyObject {
	func referencesPanel(_ panel: OakReferencesPanel, didSelectItem item: OakReferenceItem)
	func referencesPanelDidClose(_ panel: OakReferencesPanel)
}
