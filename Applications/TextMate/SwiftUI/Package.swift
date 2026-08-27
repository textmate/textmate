// swift-tools-version: 5.9

import PackageDescription

let package = Package(
    name: "TextMateUI",
    platforms: [.macOS(.v14)],
    products: [
        .library(name: "TextMateUI", targets: ["TextMateUI"]),
    ],
    targets: [
        .target(name: "TextMateUI"),
        .testTarget(name: "TextMateUITests", dependencies: ["TextMateUI"]),
    ]
)
