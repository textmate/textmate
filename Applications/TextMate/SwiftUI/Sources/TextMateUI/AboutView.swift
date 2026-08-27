import AppKit
import SwiftUI

public struct TextMateAboutView: View {
    private let version = Bundle.main.object(forInfoDictionaryKey: "CFBundleShortVersionString") as? String ?? "3"
    private let build = Bundle.main.object(forInfoDictionaryKey: "CFBundleVersion") as? String ?? ""
    private let copyright = Bundle.main.object(forInfoDictionaryKey: "NSHumanReadableCopyright") as? String ?? ""

    public init() {}

    public var body: some View {
        ScrollView {
            VStack(spacing: 24) {
                Image(nsImage: NSApplication.shared.applicationIconImage)
                    .resizable()
                    .scaledToFit()
                    .frame(width: 112, height: 112)
                    .accessibilityHidden(true)

                VStack(spacing: 6) {
                    Text("TextMate")
                        .font(.system(size: 34, weight: .bold, design: .rounded))
                    Text("Version \(version)\(build.isEmpty ? "" : " (\(build))")")
                        .font(.headline)
                        .foregroundStyle(.secondary)
                }

                Label("Free and open-source — no registration or license key required", systemImage: "checkmark.seal.fill")
                    .foregroundStyle(.green)
                    .font(.callout.weight(.medium))
                    .padding(.horizontal, 16)
                    .padding(.vertical, 10)
                    .background(.green.opacity(0.10), in: Capsule())

                Text("A powerful and customizable text editor for macOS, built by its contributors and distributed under the GNU General Public License v3.")
                    .multilineTextAlignment(.center)
                    .foregroundStyle(.secondary)
                    .frame(maxWidth: 520)

                HStack(spacing: 12) {
                    Link(destination: URL(string: "https://macromates.com/documentation/manual/")!) {
                        Label("Manual", systemImage: "book.closed")
                    }
                    Link(destination: URL(string: "https://github.com/textmate/textmate")!) {
                        Label("Source Code", systemImage: "chevron.left.forwardslash.chevron.right")
                    }
                    Link(destination: URL(string: "https://github.com/textmate/textmate/issues")!) {
                        Label("Report an Issue", systemImage: "exclamationmark.bubble")
                    }
                }
                .buttonStyle(.bordered)

                Divider().frame(maxWidth: 560)
                Text(copyright)
                    .font(.footnote)
                    .foregroundStyle(.tertiary)
                    .multilineTextAlignment(.center)
            }
            .frame(maxWidth: .infinity)
            .padding(40)
        }
        .background(.background)
        .frame(minWidth: 560, minHeight: 440)
    }
}

@MainActor
private final class AboutHost {
    lazy var view = NSHostingView(rootView: TextMateAboutView())
}

@_cdecl("TMAboutCreate")
@MainActor
public func createAboutView() -> UnsafeMutableRawPointer {
    Unmanaged.passRetained(AboutHost()).toOpaque()
}

@_cdecl("TMAboutView")
@MainActor
public func aboutView(_ pointer: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer {
    let host = Unmanaged<AboutHost>.fromOpaque(pointer).takeUnretainedValue()
    return Unmanaged.passUnretained(host.view).toOpaque()
}

@_cdecl("TMAboutRelease")
@MainActor
public func releaseAboutView(_ pointer: UnsafeMutableRawPointer) {
    Unmanaged<AboutHost>.fromOpaque(pointer).release()
}
