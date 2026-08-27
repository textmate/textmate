import SwiftUI

public struct CommandOutputView: View {
    private let model: CommandOutputModel

    public init(model: CommandOutputModel) {
        self.model = model
    }

    public var body: some View {
        VStack(spacing: 0) {
            ScrollView {
                Text(model.text)
                    .font(.system(.body, design: .monospaced))
                    .textSelection(.enabled)
                    .frame(maxWidth: .infinity, alignment: .topLeading)
                    .padding()
            }

            Divider()

            HStack {
                if model.isRunning {
                    ProgressView()
                        .controlSize(.small)
                    Text("Running")
                        .foregroundStyle(.secondary)
                } else {
                    Text("Finished")
                        .foregroundStyle(.secondary)
                }

                Spacer()

                Button("Clear") {
                    model.clear()
                }
                .disabled(model.text.isEmpty)
                .accessibilityHint("Removes all command output")
            }
            .padding(8)
        }
        .frame(minWidth: 320, minHeight: 180)
    }
}

@MainActor
private final class CommandOutputHost {
    let model = CommandOutputModel()
    lazy var view = NSHostingView(rootView: CommandOutputView(model: model))
}

@_cdecl("TMCommandOutputCreate")
@MainActor
public func createCommandOutput() -> UnsafeMutableRawPointer {
    Unmanaged.passRetained(CommandOutputHost()).toOpaque()
}

@_cdecl("TMCommandOutputView")
@MainActor
public func commandOutputView(_ pointer: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer {
    let host = Unmanaged<CommandOutputHost>.fromOpaque(pointer).takeUnretainedValue()
    return Unmanaged.passUnretained(host.view).toOpaque()
}

@_cdecl("TMCommandOutputBegin")
@MainActor
public func beginCommandOutput(_ pointer: UnsafeMutableRawPointer, _ title: UnsafePointer<CChar>?) {
    let host = Unmanaged<CommandOutputHost>.fromOpaque(pointer).takeUnretainedValue()
    host.model.begin(title: title.map { String(cString: $0) } ?? "Command Output")
}

@_cdecl("TMCommandOutputAppend")
@MainActor
public func appendCommandOutput(_ pointer: UnsafeMutableRawPointer, _ bytes: UnsafeRawPointer?, _ length: Int) {
    guard let bytes, length > 0 else { return }
    let host = Unmanaged<CommandOutputHost>.fromOpaque(pointer).takeUnretainedValue()
    host.model.append(bytes: UnsafeRawBufferPointer(start: bytes, count: length))
}

@_cdecl("TMCommandOutputFinish")
@MainActor
public func finishCommandOutput(_ pointer: UnsafeMutableRawPointer) {
    Unmanaged<CommandOutputHost>.fromOpaque(pointer).takeUnretainedValue().model.finish()
}

@_cdecl("TMCommandOutputRelease")
@MainActor
public func releaseCommandOutput(_ pointer: UnsafeMutableRawPointer) {
    Unmanaged<CommandOutputHost>.fromOpaque(pointer).release()
}
