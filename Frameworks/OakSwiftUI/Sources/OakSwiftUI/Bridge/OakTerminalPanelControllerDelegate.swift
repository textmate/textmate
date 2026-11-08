import Foundation

@MainActor @objc public protocol OakTerminalPanelControllerDelegate: AnyObject {
    /// A file reference in the terminal output was Command-clicked. `line` and `column` are 0 when absent.
    func terminalPanel(_ panel: OakTerminalPanelController, openFile url: URL, line: Int, column: Int)
    /// Directory a new shell starts in; nil, or a path that is not a directory, starts in the home directory.
    @objc optional func terminalPanelWorkingDirectory(_ panel: OakTerminalPanelController) -> String?
    /// Base environment for a new shell started in `directory`; without it the shell inherits the app's own environment.
    @objc optional func terminalPanel(_ panel: OakTerminalPanelController, environmentForDirectory directory: String) -> [String: String]
    /// The last session ended by being closed or exiting; not sent when sessions are transferred away or the panel is stopped.
    @objc optional func terminalPanelDidCloseLastSession(_ panel: OakTerminalPanelController)
}
