// swift-tools-version: 6.2
import PackageDescription

let package = Package(
    name: "OakSwiftUI",
    platforms: [.macOS(.v14)],
    products: [
        .library(name: "OakSwiftUI", type: .dynamic, targets: ["OakSwiftUI"])
    ],
    dependencies: [
        .package(url: "https://github.com/migueldeicaza/SwiftTerm.git", exact: "1.99.1")
    ],
    targets: [
        .target(
            name: "OakSwiftUI",
            dependencies: [.product(name: "SwiftTerm", package: "SwiftTerm")],
            path: "Sources/OakSwiftUI"
        ),
        .testTarget(
            name: "OakSwiftUITests",
            dependencies: ["OakSwiftUI"],
            path: "Tests/OakSwiftUITests"
        )
    ]
)
