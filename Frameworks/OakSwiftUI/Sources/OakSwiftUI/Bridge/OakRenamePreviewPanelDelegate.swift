import Foundation

@objc public protocol OakRenamePreviewPanelDelegate: AnyObject {
	func renamePreviewPanelDidConfirm(_ panel: OakRenamePreviewPanel)
	func renamePreviewPanelDidCancel(_ panel: OakRenamePreviewPanel)
}
