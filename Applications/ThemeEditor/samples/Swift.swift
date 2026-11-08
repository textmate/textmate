import Foundation

/// A theme preview covering declarations, generics, and interpolation.
/// - Important: This code is a sample, and is never executed.
struct Theme: Codable, Identifiable {
    let id: UUID
    var name: String
    var isDark: Bool = false
    var colors: [String: String]

    var description: String {
        "\(name) — \(isDark ? "dark" : "light")"
    }
}

enum PreviewError: Error {
    case missingTheme(String)
    case invalidColor(value: String)
}

protocol Repository {
    associatedtype Item
    func all() async throws -> [Item]
}

actor ThemeStore: Repository {
    typealias Item = Theme
    private var themes: [UUID: Theme] = [:]

    func insert(_ theme: Theme) {
        themes[theme.id] = theme
    }

    func all() async throws -> [Theme] {
        themes.values.sorted { $0.name < $1.name }
    }

    func find(id: UUID) throws -> Theme {
        guard let theme = themes[id] else {
            throw PreviewError.missingTheme(id.uuidString)
        }
        return theme
    }
}

@MainActor
final class PreviewModel {
    private(set) var status = "Ready"
    let limit: Int = 1_024

    func describe(_ name: String?) -> String {
        let title = name ?? "Untitled"
        let values: [Double] = [3.14159, 6.02e23, -42]
        let escaped = "Tab:\t newline:\n star:\u{2605}"
        let report = """
        Theme: \(title)
        Count: \(values.count)
        Message: \(escaped)
        """
        for (index, value) in values.enumerated() where value > 0 {
            print("\(index): \(value)")
        }
        return report
    }
}
