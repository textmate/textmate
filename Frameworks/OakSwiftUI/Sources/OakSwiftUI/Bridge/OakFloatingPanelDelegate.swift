import Foundation

@objc public protocol OakFloatingPanelDelegate: AnyObject {
    func floatingPanelDidClose(_ panel: OakFloatingPanel)
}
