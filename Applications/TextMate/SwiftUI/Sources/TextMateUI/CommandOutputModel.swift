import Foundation
import Observation

@MainActor
@Observable
public final class CommandOutputModel {
    public private(set) var title = "Command Output"
    public private(set) var output = Data()
    public private(set) var isRunning = false

    public var text: String {
        String(decoding: output, as: UTF8.self)
    }

    public init() {}

    public func begin(title: String = "Command Output") {
        self.title = title
        output.removeAll(keepingCapacity: true)
        isRunning = true
    }

    public func append(_ output: String) {
        self.output.append(contentsOf: output.utf8)
    }

    public func append(bytes: UnsafeRawBufferPointer) {
        output.append(contentsOf: bytes)
    }

    public func finish() {
        isRunning = false
    }

    public func clear() {
        output.removeAll(keepingCapacity: true)
    }
}
