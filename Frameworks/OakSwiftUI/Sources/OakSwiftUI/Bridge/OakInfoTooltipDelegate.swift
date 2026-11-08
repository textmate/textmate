import Foundation

@objc public protocol OakInfoTooltipDelegate: AnyObject {
    func infoTooltipDidDismiss(_ tooltip: OakInfoTooltip)
}
