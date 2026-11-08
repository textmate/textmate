import Testing
import AppKit
@testable import OakSwiftUI

// MARK: - OakTooltipContent

@Test func tooltipContentInitWithSections() {
    let section = OakTooltipSection(label: "Signature", content: NSAttributedString(string: "func foo()"))
    let content = OakTooltipContent(sections: [section])
    #expect(content.sections.count == 1)
    #expect(content.sections[0].label == "Signature")
    #expect(content.sections[0].content.string == "func foo()")
}

@Test func tooltipContentMultipleSections() {
    let sig = OakTooltipSection(label: "Signature", content: NSAttributedString(string: "func bar()"))
    let docs = OakTooltipSection(label: "Documentation", content: NSAttributedString(string: "Does bar things"))
    let content = OakTooltipContent(sections: [sig, docs])
    #expect(content.sections.count == 2)
    #expect(content.sections[0].label == "Signature")
    #expect(content.sections[1].label == "Documentation")
    #expect(content.sections[1].content.string == "Does bar things")
}

@Test func tooltipContentEmptySections() {
    let content = OakTooltipContent(sections: [])
    #expect(content.sections.isEmpty)
}

@Test func tooltipSectionEagerContent() {
    let section = OakTooltipSection(label: "Info", content: NSAttributedString(string: "initial"))
    #expect(section.label == "Info")
    #expect(section.content.string == "initial")
    #expect(section.isEager)
}

@Test func tooltipSectionLazyContent() {
    var callCount = 0
    let section = OakTooltipSection(label: "Docs", contentProvider: {
        callCount += 1
        return NSAttributedString(string: "lazy result")
    })
    #expect(!section.isEager)
    #expect(section.content.string == "lazy result")
    #expect(section.isEager)
    _ = section.content
    #expect(callCount == 1)
}

// MARK: - OakCompletionPopup

@Test @MainActor func completionPopupInitWithTheme() {
    let theme = OakThemeEnvironment()
    let popup = OakCompletionPopup(theme: theme)
    #expect(popup.isVisible == false)
    #expect(popup.delegate == nil)
}

@Test @MainActor func completionPopupDismissWhenNotShown() {
    let theme = OakThemeEnvironment()
    let popup = OakCompletionPopup(theme: theme)
    // Dismissing when no window is open should not crash
    popup.dismiss()
    #expect(popup.isVisible == false)
}

@Test @MainActor func completionPopupUpdateFilterWhenNotShown() {
    let theme = OakThemeEnvironment()
    let popup = OakCompletionPopup(theme: theme)
    // Filtering with no viewModel should not crash
    popup.updateFilter("test")
    #expect(popup.isVisible == false)
}

// MARK: - OakInfoTooltip

@Test @MainActor func infoTooltipInitWithTheme() {
    let theme = OakThemeEnvironment()
    let tooltip = OakInfoTooltip(theme: theme)
    #expect(tooltip.isVisible == false)
    #expect(tooltip.delegate == nil)
}

@Test @MainActor func infoTooltipDismissWhenNotShown() {
    let theme = OakThemeEnvironment()
    let tooltip = OakInfoTooltip(theme: theme)
    // Dismissing when no popover exists should not crash
    tooltip.dismiss()
    #expect(tooltip.isVisible == false)
}

// MARK: - OakFloatingPanel

@Test @MainActor func floatingPanelDefaultState() {
    let panel = OakFloatingPanel()
    #expect(panel.isVisible == false)
    #expect(panel.delegate == nil)
}

@Test @MainActor func floatingPanelCloseWhenNotShown() {
    let panel = OakFloatingPanel()
    // Closing when no panel exists should not crash
    panel.close()
    #expect(panel.isVisible == false)
}
