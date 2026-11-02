#import <Cocoa/Cocoa.h>
#import <OakSystem/application.h>
#import <OakTextView/OakDocumentView.h>
#import <bundles/item.h>
#import <bundles/locations.h>
#import <test/jail.h>
#import <ns/ns.h>

// Exercise the actual controller, including its AppKit selection notifications.
@interface ThemeEditorController : NSObject
- (void)applicationDidFinishLaunching:(NSNotification*)notification;
- (NSAlert*)themeAlert;
- (void)ruleSelected:(id)sender;
- (void)nameChanged:(id)sender;
- (void)appearanceChanged:(id)sender;
- (void)removeRule:(id)sender;
- (void)addRule:(id)sender;
- (void)settingChanged:(NSTextField*)field;
- (void)colorChanged:(NSColorWell*)well;
- (void)revertTheme:(id)sender;
- (void)loadSource;
- (void)duplicateThemeFromMenu:(NSMenuItem*)item;
- (void)deleteThemeFromMenu:(NSMenuItem*)item;
- (BOOL)writeThemeToURL:(NSURL*)url error:(NSError**)error;
- (BOOL)saveCurrentTheme;
@end

static void Require (bool condition, char const* message)
{
	if(!condition)
		throw std::runtime_error(message);
}

static std::vector<uint8_t> IconPixels (NSImage* image)
{
	std::vector<uint8_t> pixels(256 * 256 * 4);
	CGColorSpaceRef space = CGColorSpaceCreateWithName(kCGColorSpaceSRGB);
	CGContextRef context = CGBitmapContextCreate(pixels.data(), 256, 256, 8, 256 * 4, space, kCGImageAlphaPremultipliedLast);
	NSRect rect = NSMakeRect(0, 0, 1024, 1024);
	CGContextDrawImage(context, CGRectMake(0, 0, 256, 256), [image CGImageForProposedRect:&rect context:nil hints:nil]);
	CGContextRelease(context);
	CGColorSpaceRelease(space);
	return pixels;
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
		for(auto const& name : { "Alpha", "Middle", "Zulu" })
		{
			oak::uuid_t uuid;
			uuid.generate();
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

		[NSApplication sharedApplication];
		[NSApp setActivationPolicy:NSApplicationActivationPolicyAccessory];
		ThemeEditorController* controller = [ThemeEditorController new];
		NSWindow* window;
		int result = 0;
		try
		{
			NSUserDefaults* defaults = NSUserDefaults.standardUserDefaults;
			id lineNumberPreference = [defaults objectForKey:@"DocumentView Disable Line Numbers"];
			[defaults setBool:YES forKey:@"DocumentView Disable Line Numbers"];
			[controller applicationDidFinishLaunching:nil];
			OakDocumentView* previewView = [controller valueForKey:@"preview"];
			Require(previewView.showsLineNumbers, "Preview did not show line numbers when the editor preference hides them");
			Require([defaults boolForKey:@"DocumentView Disable Line Numbers"], "Preview changed the editor's line-number preference");
			if(lineNumberPreference)
				[defaults setObject:lineNumberPreference forKey:@"DocumentView Disable Line Numbers"];
			else
				[defaults removeObjectForKey:@"DocumentView Disable Line Numbers"];
			NSImage* bundledIcon = [[NSImage alloc] initWithContentsOfURL:[NSBundle.mainBundle URLForResource:@"ThemeEditor" withExtension:@"icns"]];
			Require(bundledIcon != nil, "Missing bundled application icon");
			BOOL hasFullResolutionIcon = NO;
			for(NSImageRep* representation in NSApp.applicationIconImage.representations)
				hasFullResolutionIcon |= representation.pixelsWide >= 1024 && representation.pixelsHigh >= 1024;
			Require(hasFullResolutionIcon, "Running app icon was reduced to a blurry thumbnail");
			NSImage* systemIcon = [NSWorkspace.sharedWorkspace iconForFile:NSBundle.mainBundle.bundlePath];
			auto expectedIcon = IconPixels(systemIcon);
			auto runningIcon = IconPixels(NSApp.applicationIconImage);
			double iconDifference = 0;
			for(size_t index = 0; index < expectedIcon.size(); ++index)
				iconDifference += abs(int(expectedIcon[index]) - int(runningIcon[index]));
			// AppKit snapshots the assigned image in the display's color space.
			Require(iconDifference / expectedIcon.size() < 3, "Running app did not preserve the system-rendered icon framing");
			auto dialogIcon = IconPixels([controller themeAlert].icon);
			double dialogIconDifference = 0;
			for(size_t index = 0; index < expectedIcon.size(); ++index)
				dialogIconDifference += abs(int(expectedIcon[index]) - int(dialogIcon[index]));
			Require(dialogIconDifference / expectedIcon.size() < 3, "Confirmation dialog did not use the Theme Editor icon");
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
			NSStackView* fontControls = [controller valueForKey:@"fontControls"];
			Require(!fontControls.hidden, "Font settings should always be visible");
			NSStackView* gutterControls = [controller valueForKey:@"gutterControls"];
			Require(!gutterControls.hidden, "Gutter colors should always be visible");
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
			auto addRuleResponding = [&] (NSModalResponse response) {
				NSTimer* timer = [NSTimer timerWithTimeInterval:0.01 repeats:YES block:^(NSTimer* timer) {
					if(!NSApp.modalWindow)
						return;
					NSMutableArray<NSView*>* pending = [NSMutableArray arrayWithObject:NSApp.modalWindow.contentView];
					while(pending.count)
					{
						NSView* view = pending.lastObject;
						[pending removeLastObject];
						if([view.identifier isEqualToString:@"new-rule-name"])
						{
							[NSApp.modalWindow makeFirstResponder:nil];
							((NSTextField*)view).stringValue = @"  Named rule  ";
							[NSApp stopModalWithCode:response];
							[timer invalidate];
							return;
						}
						[pending addObjectsFromArray:view.subviews];
					}
				}];
				[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSModalPanelRunLoopMode];
				[controller addRule:nil];
				[timer invalidate];
			};
			addRuleResponding(NSAlertSecondButtonReturn);
			Require(rules.numberOfItems == 2 && rules.indexOfSelectedItem == 1, "Canceling Add rule changed the rules");
			Require(![[controller valueForKey:@"dirty"] boolValue], "Canceling Add rule modified the theme");
			addRuleResponding(NSAlertFirstButtonReturn);
			Require(rules.numberOfItems == 3 && rules.indexOfSelectedItem == 2, "Adding a named rule did not select it");
			Require([rules.titleOfSelectedItem isEqualToString:@"Named rule"], "The rule did not use the dialog name");
			Require([[controller valueForKey:@"dirty"] boolValue], "Adding a rule did not update the draft");
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
		}
		catch(std::exception const& error)
		{
			fprintf(stderr, "%s\n", error.what());
			result = 1;
		}
		window.delegate = nil;
		[window close];
		return result;
	}
}
