import AppKit

private class KeyablePanel: NSPanel {
    override var canBecomeKey: Bool { true }

    // Dismiss on focus loss
    override func resignKey() {
        super.resignKey()
        if let renameField = (delegate as? OakRenameField) {
            renameField.dismiss()
            renameField.delegate?.renameFieldDidDismiss(renameField)
        }
    }
}

@MainActor @objc public class OakRenameField: NSObject, NSTextFieldDelegate, NSWindowDelegate {
    @objc public weak var delegate: OakRenameFieldDelegate?

    private let theme: OakThemeEnvironment
    private var panel: KeyablePanel?
    private var textField: NSTextField?

    @objc public init(theme: OakThemeEnvironment) {
        self.theme = theme
    }

    @objc public func show(in parentView: NSView, at screenPoint: NSPoint, placeholder: String) {
        dismiss()

        guard let parentWindow = parentView.window else { return }

        let font = NSFont.monospacedSystemFont(ofSize: theme.fontSize, weight: .regular)
        let textWidth = (placeholder as NSString).size(withAttributes: [.font: font]).width

        // Layout constants
        let hPad: CGFloat = 12
        let vPad: CGFloat = 8
        let buttonSize: CGFloat = 20
        let buttonGap: CGFloat = 4
        let buttonsWidth = buttonSize * 2 + buttonGap + 8

        let fieldWidth = max(textWidth + 24, 200)
        let fieldHeight = ceil(font.ascender - font.descender + font.leading) + 4
        let panelWidth = hPad + fieldWidth + buttonsWidth + hPad
        let panelHeight = fieldHeight + vPad * 2

        // Text field
        let field = NSTextField(string: placeholder)
        field.font = font
        field.isEditable = true
        field.isSelectable = true
        field.isBordered = false
        field.drawsBackground = false
        field.focusRingType = .none
        field.cell?.isScrollable = true
        field.cell?.wraps = false
        field.delegate = self
        field.textColor = .labelColor
        field.frame = NSRect(x: hPad, y: vPad, width: fieldWidth, height: fieldHeight)

        // Confirm button (checkmark)
        let confirmBtn = NSButton(frame: NSRect(
            x: hPad + fieldWidth + 8,
            y: (panelHeight - buttonSize) / 2,
            width: buttonSize, height: buttonSize
        ))
        confirmBtn.bezelStyle = .inline
        confirmBtn.isBordered = false
        confirmBtn.image = NSImage(systemSymbolName: "checkmark.circle.fill", accessibilityDescription: "Confirm")
        confirmBtn.contentTintColor = .controlAccentColor
        confirmBtn.target = self
        confirmBtn.action = #selector(confirmAction)

        // Cancel button (X)
        let cancelBtn = NSButton(frame: NSRect(
            x: hPad + fieldWidth + 8 + buttonSize + buttonGap,
            y: (panelHeight - buttonSize) / 2,
            width: buttonSize, height: buttonSize
        ))
        cancelBtn.bezelStyle = .inline
        cancelBtn.isBordered = false
        cancelBtn.image = NSImage(systemSymbolName: "xmark.circle", accessibilityDescription: "Cancel")
        cancelBtn.contentTintColor = .secondaryLabelColor
        cancelBtn.target = self
        cancelBtn.action = #selector(cancelAction)

        // Vibrancy background with proper rounded mask
        let effect = NSVisualEffectView(frame: NSRect(x: 0, y: 0, width: panelWidth, height: panelHeight))
        effect.material = .hudWindow
        effect.blendingMode = .behindWindow
        effect.state = .active
        effect.maskImage = Self.roundedMask(size: NSSize(width: panelWidth, height: panelHeight), radius: 8)
        effect.addSubview(field)
        effect.addSubview(confirmBtn)
        effect.addSubview(cancelBtn)

        let p = KeyablePanel(
            contentRect: NSRect(origin: .zero, size: NSSize(width: panelWidth, height: panelHeight)),
            styleMask: [.borderless, .nonactivatingPanel],
            backing: .buffered,
            defer: false
        )
        p.contentView = effect
        p.isOpaque = false
        p.backgroundColor = .clear
        p.hasShadow = true
        p.level = .floating
        p.isReleasedWhenClosed = false
        p.delegate = self

        p.setFrameTopLeftPoint(screenPoint)

        parentWindow.addChildWindow(p, ordered: .above)
        p.makeKeyAndOrderFront(nil)
        p.makeFirstResponder(field)
        if let editor = field.currentEditor() as? NSTextView {
            editor.drawsBackground = false
            editor.backgroundColor = .clear
            editor.insertionPointColor = .labelColor
        }
        field.currentEditor()?.selectAll(nil)

        self.panel = p
        self.textField = field
    }

    // MARK: - Actions

    @objc private func confirmAction() {
        let newName = textField?.stringValue.trimmingCharacters(in: .whitespaces) ?? ""
        if !newName.isEmpty {
            delegate?.renameField(self, didConfirmWithName: newName)
        }
        dismiss()
    }

    @objc private func cancelAction() {
        dismiss()
        delegate?.renameFieldDidDismiss(self)
    }

    // MARK: - NSTextFieldDelegate

    public func controlTextDidBeginEditing(_ obj: Notification) {
        // The field editor draws an opaque background over vibrancy — clear it
        if let editor = panel?.fieldEditor(false, for: textField) as? NSTextView {
            editor.drawsBackground = false
            editor.backgroundColor = .clear
        }
    }

    public func control(_ control: NSControl, textView: NSTextView, doCommandBy commandSelector: Selector) -> Bool {
        if commandSelector == #selector(NSResponder.insertNewline(_:)) {
            confirmAction()
            return true
        }
        if commandSelector == #selector(NSResponder.cancelOperation(_:)) {
            cancelAction()
            return true
        }
        return false
    }

    // MARK: - Dismiss

    @objc public func dismiss() {
        if let p = panel {
            p.delegate = nil
            p.parent?.makeKeyAndOrderFront(nil)
            p.parent?.removeChildWindow(p)
            p.orderOut(nil)
            panel = nil
            textField = nil
        }
    }

    @objc public var isVisible: Bool {
        panel?.isVisible ?? false
    }

    private static func roundedMask(size: NSSize, radius: CGFloat) -> NSImage {
        NSImage(size: size, flipped: false) { rect in
            let path = NSBezierPath(roundedRect: rect, xRadius: radius, yRadius: radius)
            NSColor.black.setFill()
            path.fill()
            return true
        }
    }
}
