import Foundation

@objc public protocol OakRenameFieldDelegate: AnyObject {
    func renameField(_ field: OakRenameField, didConfirmWithName newName: String)
    func renameFieldDidDismiss(_ field: OakRenameField)
}
