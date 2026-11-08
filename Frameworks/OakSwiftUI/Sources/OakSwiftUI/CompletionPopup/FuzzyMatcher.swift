import Foundation

public struct FuzzyMatchResult: Sendable {
	public let score: Int
	public let matchedIndices: [Int]
}

public enum FuzzyMatcher {
	public static func score(_ candidate: String, query: String) -> FuzzyMatchResult? {
		if query.isEmpty { return FuzzyMatchResult(score: 0, matchedIndices: []) }

		let candidateLower = candidate.lowercased()
		let queryLower = query.lowercased()
		let candidateChars = Array(candidateLower)
		let queryChars = Array(queryLower)

		var matchedIndices: [Int] = []
		var qi = 0
		var score = 0

		for (ci, ch) in candidateChars.enumerated() {
			guard qi < queryChars.count else { break }
			if ch == queryChars[qi] {
				matchedIndices.append(ci)
				if matchedIndices.count > 1 && matchedIndices[matchedIndices.count - 2] == ci - 1 {
					score += 3
				}
				if ci == 0 || !candidate[candidate.index(candidate.startIndex, offsetBy: ci - 1)].isLetter {
					score += 5
				}
				score += 1
				qi += 1
			}
		}

		guard qi == queryChars.count else { return nil }

		// Strong bonus for prefix match (query matches from the start)
		if matchedIndices.first == 0 && matchedIndices.count > 1 {
			let isContiguousPrefix = matchedIndices.enumerated().prefix(queryChars.count).allSatisfy { $0.offset == $0.element }
			if isContiguousPrefix {
				score += 50
			}
		}

		// Bonus for exact match
		if candidate.count == query.count { score += 10 }

		// Penalty for longer candidates (prefer shorter, more relevant matches)
		score -= candidate.count / 5

		return FuzzyMatchResult(score: score, matchedIndices: matchedIndices)
	}

	public static func filter<T>(_ items: [T], query: String, keyPath: KeyPath<T, String>) -> [T] {
		if query.isEmpty { return items }
		return items
			.compactMap { item -> (T, Int)? in
				guard let result = score(item[keyPath: keyPath], query: query) else { return nil }
				return (item, result.score)
			}
			.sorted { $0.1 > $1.1 }
			.map(\.0)
	}
}
