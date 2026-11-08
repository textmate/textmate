// Run from the repository root: swift Applications/ThemeEditor/resources/build-icon.swift
// Keep TextMate's original pixels; only composite the separate pencil overlay.
import AppKit

let files = FileManager.default
let root = URL(fileURLWithPath: files.currentDirectoryPath)
let resources = root.appendingPathComponent("Applications/ThemeEditor/resources")
let temporary = files.temporaryDirectory.appendingPathComponent("ThemeEditorIcon-\(UUID().uuidString)")
try files.createDirectory(at: temporary, withIntermediateDirectories: true)
defer { try? files.removeItem(at: temporary) }

func iconutil(_ arguments: [String]) throws {
	let process = Process()
	process.executableURL = URL(fileURLWithPath: "/usr/bin/iconutil")
	process.arguments = arguments
	try process.run()
	process.waitUntilExit()
	precondition(process.terminationStatus == 0, "iconutil failed")
}

let originals = temporary.appendingPathComponent("Original.iconset")
let output = temporary.appendingPathComponent("ThemeEditor.iconset")
try iconutil(["-c", "iconset", root.appendingPathComponent("Applications/TextMate/icons/TextMate.icns").path, "-o", originals.path])
try files.createDirectory(at: output, withIntermediateDirectories: true)
let pencil = NSImage(contentsOf: resources.appendingPathComponent("Pencil.png"))!
var checkedPixels = 0

for file in try files.contentsOfDirectory(at: originals, includingPropertiesForKeys: nil).sorted(by: { $0.path < $1.path }) {
	guard file.pathExtension == "png" else { continue }
	let original = NSBitmapImageRep(data: try Data(contentsOf: file))!
	precondition(original.bitsPerSample == 8 && original.samplesPerPixel == 4)
	precondition(original.bitmapFormat.contains(.alphaNonpremultiplied) && !original.bitmapFormat.contains(.alphaFirst))
	let result = original.copy() as! NSBitmapImageRep
	let size = original.pixelsWide
	// Draw only the pencil into a separate premultiplied RGBA bitmap.
	let overlay = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: size, pixelsHigh: size,
		bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
		colorSpaceName: original.colorSpaceName, bitmapFormat: [], bytesPerRow: size * 4, bitsPerPixel: 32)!
	memset(overlay.bitmapData!, 0, overlay.bytesPerRow * size)
	NSGraphicsContext.saveGraphicsState()
	NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: overlay)
	NSGraphicsContext.current!.imageInterpolation = .high
	pencil.draw(in: NSRect(x: Double(size) * 0.55, y: Double(size) * 0.55,
		width: Double(size) * 0.44, height: Double(size) * 0.44),
		from: .zero, operation: .sourceOver, fraction: 1)
	NSGraphicsContext.restoreGraphicsState()

	var base = [UInt](repeating: 0, count: 4)
	var top = [UInt](repeating: 0, count: 4)
	var combined = [UInt](repeating: 0, count: 4)
	for y in 0..<size {
		for x in 0..<size {
			overlay.getPixel(&top, atX: x, y: y)
			guard top[3] > 0 else { continue }
			original.getPixel(&base, atX: x, y: y)
			let alpha = Double(top[3]) / 255
			let remaining = Double(base[3]) / 255 * (1 - alpha)
			let outputAlpha = alpha + remaining
			for channel in 0..<3 {
				combined[channel] = UInt(min(255, max(0, ((Double(top[channel]) + Double(base[channel]) * remaining) / outputAlpha).rounded())))
			}
			combined[3] = UInt((outputAlpha * 255).rounded())
			result.setPixel(&combined, atX: x, y: y)
		}
	}
	let encoded = result.representation(using: .png, properties: [:])!
	try encoded.write(to: output.appendingPathComponent(file.lastPathComponent))
	let decoded = NSBitmapImageRep(data: encoded)!
	for y in 0..<size {
		for x in 0..<size {
			overlay.getPixel(&top, atX: x, y: y)
			if top[3] == 0 {
				original.getPixel(&base, atX: x, y: y)
				decoded.getPixel(&combined, atX: x, y: y)
				precondition(base == combined, "Original pixels changed outside pencil overlay")
				checkedPixels += 1
			}
		}
	}
	if file.lastPathComponent == "icon_512x512@2x.png" {
		try encoded.write(to: resources.appendingPathComponent("ThemeEditor.png"))
	}
}
try iconutil(["-c", "icns", output.path, "-o", resources.appendingPathComponent("ThemeEditor.icns").path])
print("Verified \(checkedPixels) original pixels unchanged outside the pencil across every native TextMate icon size.")
