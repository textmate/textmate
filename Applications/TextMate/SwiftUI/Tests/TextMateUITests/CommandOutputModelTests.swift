import XCTest
@testable import TextMateUI

@MainActor
final class CommandOutputModelTests: XCTestCase {
    func testCommandLifecycleAndSplitUTF8Input() {
        let model = CommandOutputModel()

        model.begin(title: "Build")
        model.append("first\n")
        let suffix = Array("second 👋\n".utf8)
        suffix.withUnsafeBytes { bytes in
            model.append(bytes: UnsafeRawBufferPointer(rebasing: bytes[..<8]))
            model.append(bytes: UnsafeRawBufferPointer(rebasing: bytes[8...]))
        }

        XCTAssertTrue(model.isRunning)
        XCTAssertEqual(model.title, "Build")
        XCTAssertEqual(model.text, "first\nsecond 👋\n")

        model.finish()
        XCTAssertFalse(model.isRunning)

        model.clear()
        XCTAssertEqual(model.text, "")
    }
}
