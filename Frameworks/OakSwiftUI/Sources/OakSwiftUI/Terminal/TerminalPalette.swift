import AppKit
import SwiftTerm

/// Builds the sixteen ANSI entries from the editor's base colours and six syntax roles.
func terminalPalette(background: NSColor, foreground: NSColor, syntaxColors: [NSColor]) -> [SwiftTerm.Color] {
    let background = background.usingColorSpace(.sRGB) ?? .black
    let foreground = foreground.usingColorSpace(.sRGB) ?? .white

    func blend(_ color: NSColor, toward target: NSColor, amount: CGFloat) -> NSColor {
        NSColor(srgbRed: color.redComponent * (1 - amount) + target.redComponent * amount,
                green: color.greenComponent * (1 - amount) + target.greenComponent * amount,
                blue: color.blueComponent * (1 - amount) + target.blueComponent * amount, alpha: 1)
    }
    func luminance(_ color: NSColor) -> CGFloat {
        func linear(_ value: CGFloat) -> CGFloat {
            value <= 0.04045 ? value / 12.92 : pow((value + 0.055) / 1.055, 2.4)
        }
        return 0.2126 * linear(color.redComponent) + 0.7152 * linear(color.greenComponent) + 0.0722 * linear(color.blueComponent)
    }
    let backgroundLuminance = luminance(background)
    func readable(_ color: NSColor) -> NSColor {
        // Preserve syntax colours where possible, nudging low-contrast ones toward the text colour.
        for step in 0...10 {
            let adjusted = blend(color, toward: foreground, amount: CGFloat(step) / 10)
            let light = luminance(adjusted)
            if (max(light, backgroundLuminance) + 0.05) / (min(light, backgroundLuminance) + 0.05) >= 3 {
                return adjusted
            }
        }
        return foreground
    }

    let dark = backgroundLuminance < 0.5
    let hues: [CGFloat] = [0, 1.0 / 3, 1.0 / 6, 0.6, 0.83, 0.5]
    let accents = hues.enumerated().map { index, hue -> NSColor in
        let candidate = index < syntaxColors.count ? syntaxColors[index].usingColorSpace(.sRGB) : nil
        let color: NSColor
        if let candidate, candidate.saturationComponent > 0.08,
           abs(candidate.redComponent - foreground.redComponent)
            + abs(candidate.greenComponent - foreground.greenComponent)
            + abs(candidate.blueComponent - foreground.blueComponent) > 0.1 {
            color = candidate
        } else {
            // Unstyled roles still get some variation, tinted toward the theme's foreground.
            let fallback = NSColor(calibratedHue: hue, saturation: 0.55, brightness: dark ? 0.8 : 0.55, alpha: 1)
                .usingColorSpace(.sRGB)!
            color = blend(fallback, toward: foreground, amount: 0.15)
        }
        return readable(color)
    }
    let normal = [blend(background, toward: foreground, amount: 0.12)] + accents + [foreground]
    let bright = [blend(background, toward: foreground, amount: 0.45)]
        + accents.map { blend($0, toward: foreground, amount: 0.25) } + [foreground]
    return (normal + bright).map { color in
        func component(_ value: CGFloat) -> UInt16 {
            UInt16((min(1, max(0, value)) * 65535).rounded())
        }
        return SwiftTerm.Color(red: component(color.redComponent), green: component(color.greenComponent), blue: component(color.blueComponent))
    }
}
