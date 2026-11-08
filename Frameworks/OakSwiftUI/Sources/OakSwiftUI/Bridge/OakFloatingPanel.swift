import AppKit

@MainActor @objc public class OakFloatingPanel: NSObject, NSWindowDelegate {
    @objc public weak var delegate: OakFloatingPanelDelegate?

    private var panel: NSPanel?

    @objc public func show(content: NSView, title: String, parentWindow: NSWindow) {
        close()

        let p = NSPanel(
            contentRect: NSRect(x: 0, y: 0, width: 400, height: 300),
            styleMask: [.titled, .closable, .resizable, .utilityWindow],
            backing: .buffered,
            defer: false
        )
        p.title = title
        p.contentView = content
        p.delegate = self
        p.center()

        parentWindow.addChildWindow(p, ordered: .above)
        p.makeKeyAndOrderFront(nil)
        self.panel = p
    }

    @objc public func close() {
        if let p = panel {
            p.parent?.removeChildWindow(p)
            p.orderOut(nil)
            panel = nil
        }
    }

    @objc public var isVisible: Bool {
        panel?.isVisible ?? false
    }

    nonisolated public func windowWillClose(_ notification: Notification) {
        MainActor.assumeIsolated {
            panel = nil
            delegate?.floatingPanelDidClose(self)
        }
    }
}
