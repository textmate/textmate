import Testing
@testable import OakSwiftUI

@Test func frameworkVersion() {
    #expect(OakSwiftUIFramework.version == "0.1.0")
}
