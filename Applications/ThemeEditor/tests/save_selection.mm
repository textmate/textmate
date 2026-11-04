#import <Cocoa/Cocoa.h>
#import <OakSystem/application.h>
#import <OakTextView/OakDocumentView.h>
#import <document/OakDocument.h>
#import <bundles/item.h>
#import <bundles/locations.h>
#import <bundles/query.h>
#import <test/jail.h>
#import <ns/ns.h>

// Exercise the actual controller, including its AppKit selection notifications.
@interface ThemeEditorController : NSObject
- (void)applicationDidFinishLaunching:(NSNotification*)notification;
- (void)application:(NSApplication*)application openURLs:(NSArray<NSURL*>*)urls;
- (NSAlert*)themeAlert;
- (void)ruleSelected:(id)sender;
- (void)nameChanged:(id)sender;
- (void)appearanceChanged:(id)sender;
- (void)removeRule:(id)sender;
- (void)addRule:(id)sender;
- (void)settingChanged:(NSTextField*)field;
- (void)colorChanged:(NSColorWell*)well;
- (void)sampleLanguageChanged:(id)sender;
- (void)revertTheme:(id)sender;
- (void)newTheme:(id)sender;
- (void)duplicateTheme:(id)sender;
- (void)loadSource;
- (void)duplicateThemeFromMenu:(NSMenuItem*)item;
- (void)deleteThemeFromMenu:(NSMenuItem*)item;
- (void)downloadThemeFromMenu:(NSMenuItem*)item;
- (void)downloadThemeWithPropertyList:(NSDictionary*)theme;
- (BOOL)writeThemeToURL:(NSURL*)url error:(NSError**)error;
- (BOOL)saveCurrentTheme;
- (BOOL)confirmDiscard;
- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender;
@end

// Capture the save panel's input while exercising the actual context-menu action.
@interface ExportTestThemeEditorController : ThemeEditorController
@property (nonatomic, strong) NSDictionary* exportedTheme;
@end
@implementation ExportTestThemeEditorController
- (void)downloadThemeWithPropertyList:(NSDictionary*)theme { self.exportedTheme = theme; }
@end

static void Require (bool condition, char const* message)
{
	if(!condition)
		throw std::runtime_error(message);
}

int main ()
{
	@autoreleasepool
	{
		test::jail_t jail;
		oak::application_t::set_name("Theme Editor Tests");
		oak::application_t::set_support(jail.path("Support"));
		bundles::set_locations({ jail.path("Support") });
		oak::uuid_t bundleUUID;
		bundleUUID.generate();
		auto bundle = std::make_shared<bundles::item_t>(bundleUUID, bundles::item_ptr(), bundles::kItemTypeBundle);
		bundle->set_plist({ { "uuid", to_s(bundleUUID) }, { "name", "Selection Tests" } });
		NSMutableDictionary<NSString*, NSURL*>* themeURLs = [NSMutableDictionary dictionary];
		for(auto const& name : { "Alpha", "Middle", "Zulu" })
		{
			oak::uuid_t uuid;
			uuid.generate();
			themeURLs[to_ns(name)] = [NSURL URLWithString:[@"textmate-theme-editor://theme/" stringByAppendingString:to_ns(to_s(uuid))]];
			auto item = std::make_shared<bundles::item_t>(uuid, bundle, bundles::kItemTypeTheme);
			item->set_plist({
				{ "uuid", to_s(uuid) }, { "name", name }, { "semanticClass", "theme.light.test" },
				{ "settings", plist::array_t{
					plist::dictionary_t{ { "settings", plist::dictionary_t{ { "background", "#FFFFFF" } } } },
					plist::dictionary_t{ { "name", "Comments" }, { "scope", "comment" }, { "settings", plist::dictionary_t{ { "foreground", "#008800" } } } }
				} }
			});
			Require(item->save(), "Could not create temporary theme fixture");
		}
		for(auto const& scope : { "source.js", "source.python" })
		{
			oak::uuid_t uuid;
			uuid.generate();
			auto grammar = std::make_shared<bundles::item_t>(uuid, bundle, bundles::kItemTypeGrammar);
			grammar->set_plist({ { "uuid", to_s(uuid) }, { "name", scope }, { "scopeName", scope }, { "patterns", plist::array_t{} } });
			Require(grammar->save(), "Could not create temporary grammar fixture");
		}
		auto themeURLWithFileType = [&] (NSString* name, NSString* fileType) {
			NSURLComponents* url = [NSURLComponents componentsWithURL:themeURLs[name] resolvingAgainstBaseURL:NO];
			url.queryItems = @[ [NSURLQueryItem queryItemWithName:@"fileType" value:fileType] ];
			return url.URL;
		};

		[NSApplication sharedApplication];
		[NSApp setActivationPolicy:NSApplicationActivationPolicyAccessory];
		ExportTestThemeEditorController* controller = [ExportTestThemeEditorController new];
		NSWindow* window = nil;
		int result = 0;
		try
		{
			NSUserDefaults* defaults = NSUserDefaults.standardUserDefaults;
			id lineNumberPreference = [defaults objectForKey:@"DocumentView Disable Line Numbers"];
			[defaults setBool:YES forKey:@"DocumentView Disable Line Numbers"];
			[defaults setObject:@"Swift.swift" forKey:@"ThemeEditorSampleLanguage"];
			[controller application:NSApp openURLs:@[ themeURLWithFileType(@"Middle", @"source.python") ]];
			[controller applicationDidFinishLaunching:nil];
			Require([[[controller valueForKey:@"draft"] objectForKey:@"name"] isEqualToString:@"Middle"], "Launch did not select the theme requested by TextMate");
			NSPopUpButton* launchLanguages = [controller valueForKey:@"sampleLanguage"];
			Require([launchLanguages.titleOfSelectedItem isEqualToString:@"Python"], "Launch did not select the active file's sample language");
			[controller application:NSApp openURLs:@[ themeURLWithFileType(@"Zulu", @"source.js.jsx") ]];
			Require([[[controller valueForKey:@"draft"] objectForKey:@"name"] isEqualToString:@"Zulu"], "An already-running editor did not select TextMate's theme");
			Require([launchLanguages.titleOfSelectedItem isEqualToString:@"JavaScript"], "An already-running editor did not select the sample for a language subtype");
			[controller application:NSApp openURLs:@[ themeURLWithFileType(@"Zulu", @"source.rust") ]];
			Require([launchLanguages.titleOfSelectedItem isEqualToString:@"JavaScript"], "Unsupported file type changed the sample language");
			[controller application:NSApp openURLs:@[ [NSURL URLWithString:@"textmate-theme-editor://theme/invalid"] ]];
			Require([[[controller valueForKey:@"draft"] objectForKey:@"name"] isEqualToString:@"Zulu"], "Invalid theme request changed the selection");
			NSTextField* launchForeground = ((NSDictionary*)[controller valueForKey:@"fields"])[@"foreground"];
			launchForeground.stringValue = @"#123456";
			[controller settingChanged:launchForeground];
			[controller application:NSApp openURLs:@[ themeURLs[@"Zulu"] ]];
			Require([[controller valueForKey:@"dirty"] boolValue] && [launchForeground.stringValue isEqualToString:@"#123456"], "Reopening the current theme discarded its edits");
			NSTimer* cancelThemeSwitch = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer* timer) {
				if(NSApp.modalWindow)
				{
					[NSApp stopModalWithCode:NSAlertSecondButtonReturn];
					[timer invalidate];
				}
			}];
			[[NSRunLoop currentRunLoop] addTimer:cancelThemeSwitch forMode:NSModalPanelRunLoopMode];
			[controller application:NSApp openURLs:@[ themeURLWithFileType(@"Alpha", @"source.python") ]];
			[cancelThemeSwitch invalidate];
			Require([[[controller valueForKey:@"draft"] objectForKey:@"name"] isEqualToString:@"Zulu"] && [[controller valueForKey:@"dirty"] boolValue], "Canceling a theme request lost the edited theme");
			Require([launchLanguages.titleOfSelectedItem isEqualToString:@"JavaScript"], "Canceling the theme switch changed the sample language");
			[controller loadSource];
			[controller application:NSApp openURLs:@[ themeURLs[@"Alpha"] ]];
			OakDocumentView* previewView = [controller valueForKey:@"preview"];
			NSPopUpButton* languages = [controller valueForKey:@"sampleLanguage"];
			Require([languages.itemTitles isEqual:@[ @"JavaScript", @"Python" ]], "Sample menu includes languages without installed grammars");
			Require([languages.titleOfSelectedItem isEqualToString:@"JavaScript"], "Missing remembered grammar did not select the first supported sample");
			[languages selectItemWithTitle:@"Python"];
			[controller sampleLanguageChanged:languages];
			Require([previewView.document.fileType isEqualToString:@"source.python"], "Filtered sample menu selected the wrong grammar");
			Require([previewView.document.virtualPath isEqualToString:@"Python.py"], "Filtered sample menu selected the wrong sample file");
			[languages selectItemWithTitle:@"JavaScript"];
			[controller sampleLanguageChanged:languages];
			Require(previewView.showsLineNumbers, "Preview did not show line numbers when the editor preference hides them");
			Require([defaults boolForKey:@"DocumentView Disable Line Numbers"], "Preview changed the editor's line-number preference");
			if(lineNumberPreference)
				[defaults setObject:lineNumberPreference forKey:@"DocumentView Disable Line Numbers"];
			else
				[defaults removeObjectForKey:@"DocumentView Disable Line Numbers"];
			NSImage* bundledIcon = [[NSImage alloc] initWithContentsOfURL:[NSBundle.mainBundle URLForResource:@"ThemeEditor" withExtension:@"icns"]];
			Require(bundledIcon != nil, "Missing bundled application icon");
			CGFloat expectedIconPixels = 512 * NSScreen.mainScreen.backingScaleFactor;
			BOOL hasFullResolutionIcon = NO;
			for(NSImageRep* representation in NSApp.applicationIconImage.representations)
				hasFullResolutionIcon |= representation.pixelsWide >= expectedIconPixels && representation.pixelsHigh >= expectedIconPixels;
			Require(hasFullResolutionIcon, "Running app icon was reduced to a blurry thumbnail");
			Require([controller themeAlert].icon == NSApp.applicationIconImage, "Confirmation dialog did not use the Theme Editor icon");
			window = [controller valueForKey:@"window"];
			window.releasedWhenClosed = NO;
			NSSplitView* split = (NSSplitView*)window.contentView;
			CGFloat sidebarWidth = NSWidth(split.subviews[0].frame);
			CGFloat inspectorWidth = NSWidth(split.subviews[1].frame);
			NSSize originalSize = split.bounds.size;
			for(NSValue* sizeValue in @[ [NSValue valueWithSize:NSMakeSize(1700, 900)], [NSValue valueWithSize:NSMakeSize(1000, 650)], [NSValue valueWithSize:originalSize] ])
			{
				[window setContentSize:sizeValue.sizeValue];
				[window layoutIfNeeded];
				[[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.01]];
				NSRect sidebar = split.subviews[0].frame;
				NSRect inspector = split.subviews[1].frame;
				NSRect preview = split.subviews[2].frame;
				NSView* pane = split.subviews[1];
				NSScrollView* form = ((NSView*)[controller valueForKey:@"controls"]).enclosingScrollView;
				Require(pane.subviews.count == 1, "The controls pane still contains an action footer");
				Require(NSEqualRects(form.frame, pane.bounds), "The scrolling controls do not fill the middle column");
				Require(fabs(NSWidth(sidebar) - sidebarWidth) < 1, "Resizing expanded the theme list");
				Require(fabs(NSWidth(inspector) - inspectorWidth) < 1, "Resizing left extra space beside the controls");
				Require(fabs(NSMinX(preview) - NSMaxX(inspector) - split.dividerThickness) < 1, "Resizing left a gap before the preview");
				Require(fabs(NSMaxX(preview) - NSMaxX(split.bounds)) < 1, "Preview did not fill the available width");
				Require(fabs(NSHeight(preview) - NSHeight(split.bounds)) < 1, "Preview did not fill the available height");
			}
			puts("Preview fills the resized window while the theme list and controls retain their widths.");
			for(NSInteger divider = 0; divider < 2; ++divider)
			{
				for(CGFloat position : { CGFloat(-1000), CGFloat(10000) })
				{
					[split setPosition:position ofDividerAtIndex:divider];
					[window layoutIfNeeded];
					Require(fabs(NSWidth(split.subviews[0].frame) - sidebarWidth) < 1, "Moving a divider resized the fixed theme list");
					Require(fabs(NSWidth(split.subviews[1].frame) - inspectorWidth) < 1, "Moving a divider resized the fixed controls");
					Require(NSWidth(split.subviews[2].frame) >= 360, "Dragging the divider squeezed the preview below its minimum width");
				}
			}
			[window setContentSize:NSMakeSize(1700, 900)];
			[split setPosition:900 ofDividerAtIndex:1];
			[split setPosition:550 ofDividerAtIndex:0];
			[window setContentSize:NSMakeSize(1000, 650)];
			[window layoutIfNeeded];
			Require(fabs(NSWidth(split.subviews[0].frame) - sidebarWidth) < 1 && fabs(NSWidth(split.subviews[1].frame) - inspectorWidth) < 1 && NSWidth(split.subviews[2].frame) >= 360, "Shrinking the window changed the fixed column widths");
			[window setContentSize:originalSize];
			[split setPosition:sidebarWidth ofDividerAtIndex:0];
			[split setPosition:sidebarWidth + split.dividerThickness + inspectorWidth ofDividerAtIndex:1];
			NSTableView* table = [controller valueForKey:@"table"];
			NSClipView* themeClip = table.enclosingScrollView.contentView;
			NSRect originalTableFrame = table.frame;
			// Simulate overflowing content and diagonal panning, including negative overscroll.
			[table setFrameSize:NSMakeSize(NSWidth(themeClip.bounds) + 300, NSHeight(themeClip.bounds) + 500)];
			[themeClip scrollToPoint:NSMakePoint(0, 100)];
			NSRect beforeHorizontalGesture = themeClip.bounds;
			for(int delta : { -120, 120 })
			{
				CGEventRef gesture = CGEventCreateScrollWheelEvent(NULL, kCGScrollEventUnitPixel, 2, 0, delta);
				[table.enclosingScrollView scrollWheel:[NSEvent eventWithCGEvent:gesture]];
				CFRelease(gesture);
				Require(NSEqualRects(themeClip.bounds, beforeHorizontalGesture), "Horizontal gestures changed the theme list position");
			}
			for(CGFloat x : { CGFloat(-120), CGFloat(120) })
			{
				NSRect proposed = themeClip.bounds;
				proposed.origin = NSMakePoint(x, 100);
				Require(NSMinX([themeClip constrainBoundsRect:proposed]) == 0, "Theme list permits horizontal gesture scrolling");
				[themeClip scrollToPoint:proposed.origin];
				Require(NSMinX(themeClip.bounds) == 0, "Theme list moved horizontally");
				Require(NSMinY(themeClip.bounds) == 100, "Theme list no longer scrolls vertically");
			}
			table.frame = originalTableFrame;
			[themeClip scrollToPoint:NSZeroPoint];
			Require(!table.floatsGroupRows, "Theme section headings remain pinned while scrolling");
			NSPopUpButton* rules = [controller valueForKey:@"rules"];
			NSButton* removeRuleButton = [controller valueForKey:@"removeRuleButton"];
			NSView* fontFamily = [controller valueForKey:@"fontFamily"];
			NSView* gutterControls = [controller valueForKey:@"gutterControls"];
			[rules selectItemAtIndex:0];
			[controller ruleSelected:nil];
			Require(!removeRuleButton.enabled, "Global editor style can be removed through the minus button");
			Require(fontFamily.superview && !fontFamily.isHiddenOrHasHiddenAncestor, "Font settings should be visible for the global style");
			Require(gutterControls.superview && !gutterControls.isHiddenOrHasHiddenAncestor, "Gutter colors should be visible for the global style");
			NSDictionary* beforeGlobalRemoval = [[controller valueForKey:@"draft"] copy];
			[controller removeRule:nil];
			Require(rules.numberOfItems == 2 && [beforeGlobalRemoval isEqual:[controller valueForKey:@"draft"]] && ![[controller valueForKey:@"dirty"] boolValue], "Removing Global editor style changed the theme");
			[rules selectItemAtIndex:1];
			[controller ruleSelected:nil];
			Require(removeRuleButton.enabled, "A scoped style rule cannot be removed");
			Require(!fontFamily.isHiddenOrHasHiddenAncestor, "Font settings should be visible for scoped styles");
			Require(gutterControls.isHiddenOrHasHiddenAncestor, "Gutter colors should be hidden for scoped styles");
			[rules selectItemAtIndex:0];
			[controller ruleSelected:nil];
			Require(!gutterControls.isHiddenOrHasHiddenAncestor, "Returning to the global style did not restore gutter colors");
			auto selectedName = [&] { return ((NSTableCellView*)[table viewAtColumn:0 row:table.selectedRow makeIfNecessary:YES]).textField; };
			NSSegmentedControl* appearance = [controller valueForKey:@"appearance"];
			Require(appearance.segmentCount == 3 && appearance.trackingMode == NSSegmentSwitchTrackingSelectOne, "Appearance is not a three-button exclusive choice");
			Require([[appearance labelForSegment:0] isEqualToString:@"Light"] && [[appearance labelForSegment:1] isEqualToString:@"Dark"] && [[appearance labelForSegment:2] isEqualToString:@"Unspecified"], "Appearance buttons have incorrect labels");
			auto requireDirty = [&] (BOOL dirty) {
				Require([[controller valueForKey:@"dirty"] boolValue] == dirty, "Theme change state does not match the current edits");
			};
			requireDirty(NO);
			NSTextField* foreground = ((NSDictionary*)[controller valueForKey:@"fields"])[@"foreground"];
			NSString* originalForeground = [foreground.stringValue copy];
			foreground.stringValue = @"invalid color";
			[controller settingChanged:foreground];
			requireDirty(YES);
			foreground.stringValue = originalForeground;
			[controller settingChanged:foreground];
			requireDirty(NO);
			NSDictionary* fields = [controller valueForKey:@"fields"];
			NSColorWell* gutterForeground = ((NSDictionary*)[controller valueForKey:@"wells"])[@"gutter.foreground"];
			NSTextField* gutterForegroundField = fields[@"gutter.foreground"];
			Require(gutterForegroundField.stringValue.length == 0 && gutterForeground.color.alphaComponent == 1, "Inherited gutter foreground opens the picker as transparent");
			gutterForeground.color = [NSColor colorWithCalibratedRed:0.8 green:0.2 blue:0.1 alpha:gutterForeground.color.alphaComponent];
			[controller colorChanged:gutterForeground];
			Require([gutterForegroundField.stringValue hasSuffix:@"FF"] && previewView.showsLineNumbers && CGColorGetAlpha(previewView.textView.theme->gutter_styles().foreground) == 1, "Changing the gutter foreground hid the line numbers");
			gutterForegroundField.stringValue = @"";
			[controller settingChanged:gutterForegroundField];
			Require(gutterForeground.color.alphaComponent == 1, "Clearing the gutter override lost its inherited color");
			[controller loadSource];
			requireDirty(NO);
			auto commandClick = [&] (NSInteger row) {
				NSRect rect = [table rectOfRow:row];
				NSPoint point = [table convertPoint:NSMakePoint(NSMidX(rect), NSMidY(rect)) toView:nil];
				NSEvent* event = [NSEvent mouseEventWithType:NSEventTypeLeftMouseDown location:point modifierFlags:NSEventModifierFlagCommand timestamp:0 windowNumber:window.windowNumber context:nil eventNumber:0 clickCount:1 pressure:1];
				[table mouseDown:event];
			};
			[table selectRowIndexes:[NSIndexSet indexSetWithIndex:2] byExtendingSelection:NO];
			[window layoutIfNeeded];
			NSTextField* displayName = selectedName();
			[window layoutIfNeeded];
			NSRect displayFrame = displayName.frame;
			Require(fabs(NSMidY(displayFrame) - NSMidY(displayName.superview.bounds)) < 1, "Theme name is not centered in its row");
			commandClick(2);
			[window layoutIfNeeded];
			NSTextField* name = selectedName();
			Require(NSEqualRects(name.frame, displayFrame), "Renaming moved the theme name");
			Require(table.selectedRow == 2 && [name.stringValue isEqualToString:@"Middle"], "Command-click did not select the clicked theme");
			Require([name isDescendantOf:table], "Rename field is not in the theme list");
			NSText* nameEditor = name.currentEditor;
			Require(nameEditor && NSEqualRanges(nameEditor.selectedRange, NSMakeRange(0, name.stringValue.length)), "Command-click did not select the name for renaming");
			commandClick(2);
			Require(table.selectedRow == 2 && name.currentEditor != nil, "Command-click deselected the current theme");
			commandClick(0);
			Require(table.selectedRow == 2, "Command-click selected a section heading");
			[window makeFirstResponder:table];
			Require(!name.editable, "Theme label remained editable after leaving rename");
			Require(![[controller valueForKey:@"dirty"] boolValue], "Command-click modified the theme");
			auto cancelRename = [&] {
				NSInteger selectedRow = table.selectedRow;
				commandClick(table.selectedRow);
				NSTextView* editor = (NSTextView*)selectedName().currentEditor;
				[editor insertText:@"Canceled name" replacementRange:editor.selectedRange];
				[editor didChangeText];
				Require([[[controller valueForKey:@"draft"] objectForKey:@"name"] isEqualToString:@"Canceled name"], "Rename fixture did not change the draft");
				[editor doCommandBySelector:@selector(cancelOperation:)];
				Require([selectedName().stringValue isEqualToString:@"Middle"], "Escape did not restore the sidebar name");
				Require([[[controller valueForKey:@"draft"] objectForKey:@"name"] isEqualToString:@"Middle"], "Escape did not restore the draft name");
				Require(!selectedName().editable && selectedName().currentEditor == nil, "Escape left the rename editor open");
				Require(table.selectedRow == selectedRow, "Escape changed the selected theme");
			};
			cancelRename();
			Require(![[controller valueForKey:@"dirty"] boolValue], "Canceling a rename left a clean theme dirty");
			requireDirty(NO);
			appearance.selectedSegment = 1;
			[controller appearanceChanged:appearance];
			requireDirty(YES);
			// The draft's appearance edit must survive cancellation of its rename.
			cancelRename();
			Require([[controller valueForKey:@"dirty"] boolValue], "Canceling a rename cleared earlier edits");
			Require(appearance.selectedSegment == 1, "Canceling a rename changed the draft appearance");
			appearance.selectedSegment = 0;
			[controller appearanceChanged:appearance];
			requireDirty(NO);

			// Light header is row 0; Middle is deliberately not the first theme.
			[table selectRowIndexes:[NSIndexSet indexSetWithIndex:2] byExtendingSelection:NO];
			Require([name.stringValue isEqualToString:@"Middle"], "Fixture selection failed");
			[rules selectItemAtIndex:1];
			[controller ruleSelected:nil];

			auto checkSave = [&] (NSString* expectedName) {
				Require([controller saveCurrentTheme], "Save failed");
				[[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.01]];
				Require([selectedName().stringValue isEqualToString:expectedName], "Save changed the active theme");
				Require(rules.indexOfSelectedItem == 1, "Save reset the active style rule");
				Require(table.selectedRow == [[controller valueForKey:@"selectedRow"] integerValue], "Save lost the sidebar selection");
				Require([selectedName().stringValue isEqualToString:expectedName], "Save selected a different sidebar theme");
				Require(![[controller valueForKey:@"dirty"] boolValue], "Save left the draft dirty");
				requireDirty(NO);
			};
			checkSave(@"Middle");

			commandClick(table.selectedRow);
			NSTextView* editor = (NSTextView*)selectedName().currentEditor;
			Require(editor != nil, "Inline name editor was not active");
			[editor insertText:@"ZZZ Renamed" replacementRange:editor.selectedRange];
			[editor didChangeText];
			Require([[[controller valueForKey:@"draft"] objectForKey:@"name"] isEqualToString:@"ZZZ Renamed"], "Inline rename did not update the draft");
			checkSave(@"ZZZ Renamed");

			appearance.selectedSegment = 1; // Move from Light to Dark.
			[controller appearanceChanged:appearance];
			checkSave(@"ZZZ Renamed");
			Require(appearance.selectedSegment == 1, "Save changed the appearance");
			checkSave(@"ZZZ Renamed"); // A repeated save must also keep selection.
			puts("Save keeps the active theme and rule after rename, regroup, and repeated saves.");
			auto removeRuleResponding = [&] (NSModalResponse response) {
				NSTimer* timer = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer* timer) {
					if(NSApp.modalWindow)
					{
						[NSApp stopModalWithCode:response];
						[timer invalidate];
					}
				}];
				[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSModalPanelRunLoopMode];
				[controller removeRule:nil];
				[timer invalidate];
			};
			auto addRuleResponding = [&] (NSModalResponse response, NSArray<NSString*>* scopes) {
				__block NSUInteger attempt = 0;
				NSTimer* timer = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer* timer) {
					if(!NSApp.modalWindow)
						return;
					[NSApp.modalWindow makeFirstResponder:nil];
					NSMutableArray<NSView*>* pending = [NSMutableArray arrayWithObject:NSApp.modalWindow.contentView];
					while(pending.count)
					{
						NSView* view = pending.lastObject;
						[pending removeLastObject];
						if([view.identifier isEqualToString:@"new-rule-name"])
							((NSTextField*)view).stringValue = @"  Strings  ";
						if([view.identifier isEqualToString:@"new-rule-scope"])
							((NSTextField*)view).stringValue = scopes[attempt];
						[pending addObjectsFromArray:view.subviews];
					}
					[NSApp stopModalWithCode:response];
					if(++attempt == scopes.count)
						[timer invalidate];
				}];
				[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSModalPanelRunLoopMode];
				[controller addRule:nil];
				[timer invalidate];
			};
			addRuleResponding(NSAlertSecondButtonReturn, @[ @"string" ]);
			Require(rules.numberOfItems == 2 && rules.indexOfSelectedItem == 1, "Canceling Add rule changed the rules");
			Require(![[controller valueForKey:@"dirty"] boolValue], "Canceling Add rule modified the theme");
			addRuleResponding(NSAlertFirstButtonReturn, @[ @"  ", @"  string  " ]);
			Require(rules.numberOfItems == 3 && rules.indexOfSelectedItem == 2, "Adding a named rule did not select it");
			Require([rules.titleOfSelectedItem isEqualToString:@"Strings"], "The rule did not use the dialog name");
			Require([[controller valueForKey:@"dirty"] boolValue], "Adding a rule did not update the draft");
			NSArray* addedRules = [controller valueForKey:@"draft"][@"settings"];
			Require([addedRules.lastObject[@"scope"] isEqualToString:@"string"], "Adding a Strings rule did not preserve its scope selector");
			Require([addedRules[1][@"scope"] isEqualToString:@"comment"], "Adding a Strings rule changed the existing comment rule");
			Require(removeRuleButton.enabled && gutterControls.isHiddenOrHasHiddenAncestor, "The added rule was treated as a global style");
			removeRuleResponding(NSAlertSecondButtonReturn);
			[rules selectItemAtIndex:1];
			[controller ruleSelected:nil];
			checkSave(@"ZZZ Renamed");
			removeRuleResponding(NSAlertFirstButtonReturn); // Cancel is the default.
			Require(rules.numberOfItems == 2 && rules.indexOfSelectedItem == 1, "Cancel changed the selected rule");
			Require(![[controller valueForKey:@"dirty"] boolValue], "Cancel modified the theme");
			removeRuleResponding(NSAlertSecondButtonReturn);
			Require(rules.numberOfItems == 1 && rules.indexOfSelectedItem == 0, "Confirmed removal did not remove the selected rule");
			Require([[controller valueForKey:@"dirty"] boolValue], "Confirmed removal did not update the draft");
			[controller removeRule:nil];
			Require(rules.numberOfItems == 1, "Removal deleted the last rule");
			requireDirty(YES);
			NSTimer* discard = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer* timer) {
				if(NSApp.modalWindow)
				{
					[NSApp stopModalWithCode:NSAlertThirdButtonReturn];
					[timer invalidate];
				}
			}];
			[[NSRunLoop currentRunLoop] addTimer:discard forMode:NSModalPanelRunLoopMode];
			[controller revertTheme:nil];
			[discard invalidate];
			Require(rules.numberOfItems == 2, "Revert did not restore the saved rules");
			requireDirty(NO);
			NSInteger previousRow = table.selectedRow;
			auto contextMenu = [&] (NSInteger row) {
				NSRect rect = [table rectOfRow:row];
				NSPoint point = [table convertPoint:NSMakePoint(NSMidX(rect), NSMidY(rect)) toView:nil];
				NSEvent* event = [NSEvent mouseEventWithType:NSEventTypeRightMouseDown location:point modifierFlags:0 timestamp:0 windowNumber:window.windowNumber context:nil eventNumber:0 clickCount:1 pressure:1];
				return [table menuForEvent:event];
			};
			Require(contextMenu(0) == nil, "Section heading has theme actions");
			NSMenu* menu = contextMenu(1); // Alpha is not the selected theme.
			Require(menu.numberOfItems == 3 && [menu.itemArray[0].title isEqualToString:@"Duplicate"] && [menu.itemArray[1].title isEqualToString:@"Delete"] && [menu.itemArray[2].title isEqualToString:@"Download..."], "Theme context menu has incorrect actions");
			Require(menu.itemArray[2].action == NSSelectorFromString(@"downloadThemeFromMenu:") && [menu.itemArray[2].representedObject isEqual:menu.itemArray[0].representedObject], "Download does not target the clicked theme");
			NSMenuItem* download = [NSApp.mainMenu itemWithTitle:@"File"].submenu.itemArray.lastObject;
			Require([download.title isEqualToString:@"Download..."] && download.action == NSSelectorFromString(@"downloadTheme:"), "File menu is missing Download Theme");
			Require(table.selectedRow == previousRow, "Opening the context menu changed selection");
			[controller duplicateThemeFromMenu:menu.itemArray[0]];
			Require([[[controller valueForKey:@"draft"] objectForKey:@"name"] isEqualToString:@"Alpha Copy"], "Context menu duplicated the selected theme instead of the clicked theme");
			Require([[controller valueForKey:@"newTheme"] boolValue], "Duplicate is not a new theme");
			requireDirty(YES);
			NSMutableDictionary* draft = [controller valueForKey:@"draft"];
			// Preserve settings not exposed in the inspector as well as unsaved edits.
			draft[@"customMetadata"] = @{ @"author": @"Theme export test" };
			NSURL* exportURL = [NSURL fileURLWithPath:to_ns(jail.path("Downloaded.tmTheme"))];
			NSError* exportError = nil;
			Require([controller writeThemeToURL:exportURL error:&exportError], "Could not export an unsaved theme");
			NSPropertyListFormat format;
			NSDictionary* exported = [NSPropertyListSerialization propertyListWithData:[NSData dataWithContentsOfURL:exportURL] options:0 format:&format error:&exportError];
			Require(format == NSPropertyListXMLFormat_v1_0 && [exported isEqual:draft], "Export did not preserve the complete theme as XML");
			Require(table.selectedRow == [[controller valueForKey:@"selectedRow"] integerValue] && rules.indexOfSelectedItem == 0, "Export changed theme or rule selection");
			Require([[controller valueForKey:@"newTheme"] boolValue] && [[controller valueForKey:@"dirty"] boolValue], "Export saved or cleared the unsaved draft");
			requireDirty(YES);
			Require(![controller writeThemeToURL:[NSURL fileURLWithPath:to_ns(jail.path("Missing/Downloaded.tmTheme"))] error:&exportError] && exportError != nil, "Export did not report a write failure");
			NSInteger selectionBeforeExport = table.selectedRow;
			__block NSUInteger exportPromptCount = 0;
			NSTimer* unexpectedExportPrompt = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer*) {
				if(NSApp.modalWindow)
				{
					++exportPromptCount;
					[NSApp stopModalWithCode:NSAlertSecondButtonReturn];
				}
			}];
			[[NSRunLoop currentRunLoop] addTimer:unexpectedExportPrompt forMode:NSModalPanelRunLoopMode];
			[controller downloadThemeFromMenu:contextMenu(1).itemArray[2]]; // Export Alpha while Alpha Copy is dirty.
			[unexpectedExportPrompt invalidate];
			Require(exportPromptCount == 0, "Exporting another theme prompted to save or revert the current draft");
			Require([controller.exportedTheme[@"uuid"] isEqual:themeURLs[@"Alpha"].lastPathComponent] && [controller.exportedTheme[@"name"] isEqualToString:@"Alpha"], "Context-menu export did not use the clicked theme's property list");
			auto clickedTheme = bundles::lookup(oak::uuid_t(to_s(themeURLs[@"Alpha"].lastPathComponent)));
			Require([controller.exportedTheme isEqual:ns::to_dictionary(clickedTheme->plist())], "Context-menu export did not preserve the clicked theme's complete property list");
			Require(table.selectedRow == selectionBeforeExport && [controller valueForKey:@"draft"] == draft, "Exporting another theme changed the selected draft");
			Require([draft[@"customMetadata"] isEqual:@{ @"author": @"Theme export test" }] && [[controller valueForKey:@"dirty"] boolValue], "Exporting another theme lost unsaved edits");
			NSMenuItem* deleteItem = contextMenu(table.selectedRow).itemArray[1];
			auto deleteResponding = [&] (NSModalResponse response) {
				NSTimer* timer = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer* timer) {
					if(NSApp.modalWindow)
					{
						[NSApp stopModalWithCode:response];
						[timer invalidate];
					}
				}];
				[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSModalPanelRunLoopMode];
				[controller deleteThemeFromMenu:deleteItem];
				[timer invalidate];
			};
			deleteResponding(NSAlertFirstButtonReturn);
			Require([[[controller valueForKey:@"draft"] objectForKey:@"name"] isEqualToString:@"Alpha Copy"], "Cancel deleted the theme");
			deleteResponding(NSAlertSecondButtonReturn);
			Require(![[[controller valueForKey:@"draft"] objectForKey:@"name"] isEqualToString:@"Alpha Copy"], "Confirmed deletion retained the theme");
			for(NSInteger row = 0; row < table.numberOfRows; ++row)
			{
				NSMenu* remaining = contextMenu(row);
				Require(!remaining || ![remaining.itemArray[0].representedObject isEqual:deleteItem.representedObject], "Deleted theme remains in the list");
			}
			requireDirty(NO);
			[controller application:NSApp openURLs:@[ themeURLs[@"Alpha"] ]];
			Require([selectedName().stringValue isEqualToString:@"Alpha"], "Rename-on-save fixture did not select Alpha");
			commandClick(table.selectedRow);
			NSTextView* renameEditor = (NSTextView*)selectedName().currentEditor;
			[renameEditor insertText:@"Zzz" replacementRange:renameEditor.selectedRange];
			[renameEditor didChangeText];
			[window makeFirstResponder:table];
			requireDirty(YES);
			NSTimer* saveBeforeSelection = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer* timer) {
				if(NSApp.modalWindow)
				{
					[NSApp stopModalWithCode:NSAlertFirstButtonReturn];
					[timer invalidate];
				}
			}];
			[[NSRunLoop currentRunLoop] addTimer:saveBeforeSelection forMode:NSModalPanelRunLoopMode];
			NSEvent* arrowDown = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:window.windowNumber context:nil characters:@"\uF701" charactersIgnoringModifiers:@"\uF701" isARepeat:NO keyCode:125];
			[table keyDown:arrowDown];
			[saveBeforeSelection invalidate];
			NSDate* selectionDeadline = [NSDate dateWithTimeIntervalSinceNow:1];
			while([[controller valueForKey:@"pendingThemeSelection"] boolValue] && selectionDeadline.timeIntervalSinceNow > 0)
				[[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.01]];
			Require([[[controller valueForKey:@"draft"] objectForKey:@"uuid"] isEqualToString:themeURLs[@"Zulu"].lastPathComponent], "Arrow-down selected the wrong theme after rename-on-save reordered the rows");
			Require([selectedName().stringValue isEqualToString:@"Zulu"], "Sidebar selection did not follow the intended theme after rename-on-save");
			Require(table.selectedRow == [[controller valueForKey:@"selectedRow"] integerValue], "Rename-on-save left the controller and sidebar selections out of sync");
			requireDirty(NO);
			auto renameCurrentTheme = [&] (NSString* title) {
				commandClick(table.selectedRow);
				NSTextView* editor = (NSTextView*)selectedName().currentEditor;
				[editor insertText:title replacementRange:editor.selectedRange];
				[editor didChangeText];
				[window makeFirstResponder:table];
			};
			[controller newTheme:nil];
			renameCurrentTheme(@"New A");
			NSString* newAUUID = [controller valueForKey:@"draft"][@"uuid"];
			// Retain a clean unsaved fixture so both drafts predate bundle registration.
			[controller loadSource];
			[controller newTheme:nil];
			renameCurrentTheme(@"New B");
			NSString* newBUUID = [controller valueForKey:@"draft"][@"uuid"];
			Require([controller saveCurrentTheme], "Could not save the second new theme");
			[controller application:NSApp openURLs:@[ [NSURL URLWithString:[@"textmate-theme-editor://theme/" stringByAppendingString:newAUUID]] ]];
			Require([controller saveCurrentTheme], "Could not save the first new theme after the second");
			auto newA = bundles::lookup(oak::uuid_t(to_s(newAUUID)));
			auto newB = bundles::lookup(oak::uuid_t(to_s(newBUUID)));
			Require(newA && newB && newA->bundle() == newB->bundle(), "New themes with the same bundle UUID did not reuse one bundle object");
			NSString* bundleDirectory = to_ns(jail.path("Support/Bundles"));
			NSUInteger editorBundleCount = 0;
			for(NSString* entry in [NSFileManager.defaultManager contentsOfDirectoryAtPath:bundleDirectory error:nil])
			{
				NSString* infoPath = [[bundleDirectory stringByAppendingPathComponent:entry] stringByAppendingPathComponent:@"info.plist"];
				NSDictionary* info = [NSDictionary dictionaryWithContentsOfFile:infoPath];
				if([info[@"uuid"] isEqualToString:@"1C514B9A-BA40-4E6C-A79B-E169CE2AF4D2"])
					++editorBundleCount;
			}
			Require(editorBundleCount == 1, "Saving new themes created multiple bundles with the Theme Editor UUID");
			auto discardNewTheme = [&] (BOOL switchAway) {
				[controller newTheme:nil];
				NSString* discardedUUID = [controller valueForKey:@"draft"][@"uuid"];
				NSInteger rowsBeforeDiscard = table.numberOfRows;
				NSTimer* timer = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer* timer) {
					if(NSApp.modalWindow)
					{
						[NSApp stopModalWithCode:NSAlertThirdButtonReturn];
						[timer invalidate];
					}
				}];
				[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSModalPanelRunLoopMode];
				if(switchAway)
					[controller application:NSApp openURLs:@[ [NSURL URLWithString:[@"textmate-theme-editor://theme/" stringByAppendingString:newBUUID]] ]];
				else
					[controller revertTheme:nil];
				[timer invalidate];
				Require(table.numberOfRows == rowsBeforeDiscard - 1, "Reverting a new theme left an unsaved row in the list");
				for(NSInteger row = 0; row < table.numberOfRows; ++row)
				{
					NSMenu* menu = contextMenu(row);
					Require(!menu || ![menu.itemArray[0].representedObject isEqual:discardedUUID], "The discarded new theme remains selectable");
				}
				Require(![[controller valueForKey:@"newTheme"] boolValue], "Reverting a new theme did not select a saved theme");
				requireDirty(NO);
			};
			discardNewTheme(YES);
			Require([[[controller valueForKey:@"draft"] objectForKey:@"uuid"] isEqual:newBUUID], "Discarding a new theme changed the requested destination");
			discardNewTheme(NO);
			NSMutableDictionary* falseValueDraft = [controller valueForKey:@"draft"];
			falseValueDraft[@"customFlag"] = @NO;
			falseValueDraft[@"customEnabled"] = @YES;
			falseValueDraft[@"customZero"] = @0;
			NSTimer* unexpectedSaveAlert = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer*) {
				if(NSApp.modalWindow)
					[NSApp stopModalWithCode:NSAlertFirstButtonReturn];
			}];
			[[NSRunLoop currentRunLoop] addTimer:unexpectedSaveAlert forMode:NSModalPanelRunLoopMode];
			BOOL firstFalseValueSave = [controller saveCurrentTheme];
			BOOL secondFalseValueSave = firstFalseValueSave && [controller saveCurrentTheme];
			[unexpectedSaveAlert invalidate];
			Require(firstFalseValueSave && secondFalseValueSave, "A top-level false value prevented repeated saves");
			NSDictionary* savedFalseValueDraft = [controller valueForKey:@"draft"];
			Require([savedFalseValueDraft[@"customFlag"] isEqual:@NO] && [savedFalseValueDraft[@"customEnabled"] isEqual:@YES] && [savedFalseValueDraft[@"customZero"] isEqual:@0], "Repeated saves changed unrelated draft values");
			NSString* savedForeground = [foreground.stringValue copy];
			foreground.stringValue = @"#FEDCBA";
			[controller settingChanged:foreground];
			requireDirty(YES);
			__block NSUInteger closePromptCount = 0;
			NSTimer* revertBeforeClosing = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer*) {
				if(NSApp.modalWindow)
				{
					++closePromptCount;
					[NSApp stopModalWithCode:NSAlertThirdButtonReturn];
				}
			}];
			[[NSRunLoop currentRunLoop] addTimer:revertBeforeClosing forMode:NSModalPanelRunLoopMode];
			// Window closing confirms first, then NSApp performs its termination check.
			BOOL closeConfirmed = [controller confirmDiscard];
			NSApplicationTerminateReply termination = [controller applicationShouldTerminate:NSApp];
			[revertBeforeClosing invalidate];
			Require(closeConfirmed && termination == NSTerminateNow, "Reverting changes did not allow window closing and termination");
			Require(closePromptCount == 1, "Reverting while closing prompted again during termination");
			requireDirty(NO);
			Require([foreground.stringValue isEqual:savedForeground], "Revert cleared the dirty state without restoring the saved settings");
			[controller application:NSApp openURLs:@[ themeURLs[@"Zulu"] ]];
			renameCurrentTheme(@"Edited Name");
			requireDirty(YES);
			__block NSUInteger duplicatePromptCount = 0;
			NSTimer* revertBeforeDuplicating = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer*) {
				if(NSApp.modalWindow)
				{
					++duplicatePromptCount;
					[NSApp stopModalWithCode:NSAlertThirdButtonReturn];
				}
			}];
			[[NSRunLoop currentRunLoop] addTimer:revertBeforeDuplicating forMode:NSModalPanelRunLoopMode];
			[controller duplicateTheme:nil];
			[revertBeforeDuplicating invalidate];
			NSDictionary* revertedCopy = [controller valueForKey:@"draft"];
			Require(duplicatePromptCount == 1 && [revertedCopy[@"name"] isEqualToString:@"Zulu Copy"], "Duplicate used the discarded draft name after Revert");
			Require(![revertedCopy[@"uuid"] isEqual:themeURLs[@"Zulu"].lastPathComponent], "Duplicate retained the source theme UUID");
			auto zulu = bundles::lookup(oak::uuid_t(to_s(themeURLs[@"Zulu"].lastPathComponent)));
			Require([revertedCopy[@"settings"] isEqual:ns::to_dictionary(zulu->plist())[@"settings"]], "Duplicate did not copy the reverted source settings");
			requireDirty(YES);
			NSArray<NSDictionary*>* malformedThemes = @[
				@{ @"settings": @[ @"not a rule", @{ @"scope": @"string", @"name": @"Strings", @"settings": @{ @"foreground": @"#123456" } } ] },
				@{ @"settings": @"not an array" },
				@{ @"settings": @{ @"scope": @"comment" } },
				@{ @"settings": @[ @{ @"scope": @"string", @"settings": @"not a dictionary" } ] },
				@{ @"settings": @[ @{ @"scope": @"string", @"settings": @[ @"not a dictionary" ] } ] },
				@{ @"settings": @[ @{ @"scope": @42, @"settings": @{} }, @{ @"scope": @[], @"settings": @{} }, @{ @"scope": @{}, @"settings": @{} } ] },
				@{ @"settings": @[ @{ @"scope": @"string", @"name": @42, @"settings": @{} } ] },
				@{ @"name": @42, @"settings": @[] },
				@{ @"name": @[], @"settings": @[] },
				@{ @"settings": @[ @{ @"settings": @{ @"fontName": @{}, @"fontStyle": @[], @"fontSize": @[] } } ], @"gutterSettings": @[], @"colorSpaceName": @42 },
			];
			NSMutableArray<NSURL*>* malformedURLs = [NSMutableArray array];
			NSMutableArray<NSString*>* malformedFiles = [NSMutableArray array];
			NSMutableArray<NSData*>* malformedOriginals = [NSMutableArray array];
			for(NSDictionary* malformed in malformedThemes)
			{
				oak::uuid_t uuid;
				uuid.generate();
				NSMutableDictionary* value = [@{ @"uuid": to_ns(to_s(uuid)), @"name": @"Malformed fixture", @"customMetadata": @{ @"author": @"Keep me" } } mutableCopy];
				[value addEntriesFromDictionary:malformed];
				auto item = std::make_shared<bundles::item_t>(uuid, bundle, bundles::kItemTypeTheme);
				item->set_plist(plist::convert((__bridge CFPropertyListRef)value));
				Require(item->save(), "Could not save malformed theme fixture");
				NSString* file = to_ns(item->paths().front());
				[malformedFiles addObject:file];
				[malformedOriginals addObject:[NSData dataWithContentsOfFile:file]];
				[malformedURLs addObject:[NSURL URLWithString:[@"textmate-theme-editor://theme/" stringByAppendingString:to_ns(to_s(uuid))]]];
			}
			ThemeEditorController* malformedController = [ThemeEditorController new];
			[malformedController application:NSApp openURLs:@[ malformedURLs.firstObject ]];
			[malformedController applicationDidFinishLaunching:nil];
			for(NSUInteger index = 0; index < malformedURLs.count; ++index)
			{
				[malformedController application:NSApp openURLs:@[ malformedURLs[index] ]];
				NSDictionary* draft = [malformedController valueForKey:@"draft"];
				Require([draft[@"uuid"] isEqual:malformedURLs[index].lastPathComponent], "Malformed theme did not load");
				Require([draft[@"name"] isKindOfClass:NSString.class], "Malformed theme name was not sanitized");
				Require([draft[@"settings"] isKindOfClass:NSArray.class] && [draft[@"settings"] count] > 0, "Malformed settings did not produce an editable global rule");
				Require([draft[@"customMetadata"] isEqual:@{ @"author": @"Keep me" }], "Sanitizing a theme removed unrelated metadata");
				NSPopUpButton* malformedRules = [malformedController valueForKey:@"rules"];
				for(NSInteger row = 0; row < malformedRules.numberOfItems; ++row)
				{
					[malformedRules selectItemAtIndex:row];
					[malformedController ruleSelected:nil];
				}
				if(index == 0)
					Require([draft[@"settings"] count] == 2 && [draft[@"settings"][1][@"settings"][@"foreground"] isEqualToString:@"#123456"], "Sanitizing malformed entries lost a valid scoped rule");
				if(index == 5)
					Require([draft[@"settings"] count] == 1, "Malformed scopes became global rules");
				if(index == 6)
					Require([malformedRules.titleOfSelectedItem isEqualToString:@"string"], "Malformed rule name did not fall back to its scope");
				Require(![[malformedController valueForKey:@"dirty"] boolValue], "Loading a malformed theme marked it edited");
				Require([[NSData dataWithContentsOfFile:malformedFiles[index]] isEqual:malformedOriginals[index]], "Loading a malformed theme modified its file");
			}
			NSWindow* malformedWindow = [malformedController valueForKey:@"window"];
			malformedWindow.releasedWhenClosed = NO;
			malformedWindow.delegate = nil;
			[malformedWindow close];
		}
		catch(std::exception const& error)
		{
			fprintf(stderr, "%s\n", error.what());
			result = 1;
		}
		if(window)
		{
			window.delegate = nil;
			[window close];
		}
		return result;
	}
}
