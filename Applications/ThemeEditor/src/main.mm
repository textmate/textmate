#import <Cocoa/Cocoa.h>
#import <QuartzCore/QuartzCore.h>
#import <UniformTypeIdentifiers/UniformTypeIdentifiers.h>
#import "appearance.h"
#import "bundle_index.h"
#import "colors.h"
#import "ase.h"
#import "dtcg.h"
#import "samples.h"
#import "rule_matching.h"
#import <theme/font_size.h>
#import <mach-o/dyld.h>
#import <OakSystem/application.h>
#import <OakTextView/OakDocumentView.h>
#import <Preferences/Keys.h>
#import <bundles/query.h>
#import <document/OakDocument.h>
#import <io/path.h>
#import <ns/ns.h>
#import <settings/settings.h>

static NSMutableDictionary* MutableTheme (plist::dictionary_t const& value)
{
	NSDictionary* dictionary = ns::to_dictionary(value);
	NSMutableDictionary* draft = CFBridgingRelease(CFPropertyListCreateDeepCopy(kCFAllocatorDefault, (__bridge CFPropertyListRef)dictionary, kCFPropertyListMutableContainersAndLeaves));
	// Treat malformed values like the renderer's missing settings. Never turn
	// an invalid scoped rule into a global rule, which would restyle all text.
	if(![draft[@"name"] isKindOfClass:NSString.class])
		draft[@"name"] = @"Untitled Theme";
	if(draft[@"colorSpaceName"] && ![draft[@"colorSpaceName"] isKindOfClass:NSString.class])
		[draft removeObjectForKey:@"colorSpaceName"];
	if(draft[@"gutterSettings"] && ![draft[@"gutterSettings"] isKindOfClass:NSDictionary.class])
		[draft removeObjectForKey:@"gutterSettings"];
	NSMutableArray* rules = [NSMutableArray array];
	if([draft[@"settings"] isKindOfClass:NSArray.class])
	{
		for(id entry in draft[@"settings"])
		{
			if(![entry isKindOfClass:NSDictionary.class])
				continue;
			NSMutableDictionary* rule = entry;
			if(rule[@"scope"] && ![rule[@"scope"] isKindOfClass:NSString.class])
				continue;
			if(rule[@"name"] && ![rule[@"name"] isKindOfClass:NSString.class])
				[rule removeObjectForKey:@"name"];
			if(![rule[@"settings"] isKindOfClass:NSDictionary.class])
				rule[@"settings"] = [NSMutableDictionary dictionary];
			NSMutableDictionary* settings = rule[@"settings"];
			for(NSString* key in @[ @"fontName", @"fontStyle" ])
			{
				if(settings[key] && ![settings[key] isKindOfClass:NSString.class])
					[settings removeObjectForKey:key];
			}
			id size = settings[@"fontSize"];
			if(size && ![size isKindOfClass:NSString.class] && ![size isKindOfClass:NSNumber.class])
				[settings removeObjectForKey:@"fontSize"];
			[rules addObject:rule];
		}
	}
	draft[@"settings"] = rules;
	return draft;
}

@interface ThemeSidebarPanel : NSView
@end
@implementation ThemeSidebarPanel
- (void)drawRect:(NSRect)dirtyRect
{
	[NSColor.controlBackgroundColor setFill];
	NSRectFill(self.bounds);
	[NSColor.separatorColor setStroke];
	NSBezierPath* outline = [NSBezierPath bezierPathWithRoundedRect:NSInsetRect(self.bounds, 0.5, 0.5) xRadius:13.5 yRadius:13.5];
	outline.lineWidth = 1;
	[outline stroke];
}

- (void)viewDidChangeEffectiveAppearance
{
	[super viewDidChangeEffectiveAppearance];
	self.needsDisplay = YES;
}
@end

// Draw section outlines without changing NSBox's content geometry.
@interface ThemeSettingsBox : NSBox
- (void)updateOutline;
@end
@implementation ThemeSettingsBox
- (void)updateOutline
{
	[self.effectiveAppearance performAsCurrentDrawingAppearance:^{
		self.layer.borderColor = NSColor.separatorColor.CGColor;
	}];
}

- (void)viewDidMoveToWindow
{
	[super viewDidMoveToWindow];
	[self updateOutline];
}

- (void)viewDidChangeEffectiveAppearance
{
	[super viewDidChangeEffectiveAppearance];
	[self updateOutline];
}
@end

// A short inspector should stay at the top of its scroll view.
@interface ThemeInspectorStackView : NSStackView
@end
@implementation ThemeInspectorStackView
- (BOOL)isFlipped { return YES; }
@end

@interface ThemeEditorSplitView : NSSplitView
@end
@implementation ThemeEditorSplitView
- (void)drawDividerInRect:(NSRect)rect
{
	// The inset sidebar supplies its own edge instead of a full-height rule.
	if(self.subviews.count && fabs(NSMinX(rect) - NSMaxX(self.subviews.firstObject.frame)) < 1)
		return;
	[super drawDividerInRect:rect];
}
@end

// NSTextField changes its alignment insets when it becomes editable.
@interface ThemeNameField : NSTextField
@end
@implementation ThemeNameField
- (NSEdgeInsets)alignmentRectInsets { return NSEdgeInsetsZero; }
@end

@interface ThemeActionsBar : NSView
@end
@implementation ThemeActionsBar
- (void)drawRect:(NSRect)dirtyRect
{
	[NSColor.controlBackgroundColor setFill];
	[[NSBezierPath bezierPathWithRoundedRect:self.bounds xRadius:10 yRadius:10] fill];
	// Square upper corners meet the group; lower corners follow its outline.
	NSRectFill(NSMakeRect(0, NSHeight(self.bounds) / 2, NSWidth(self.bounds), NSHeight(self.bounds) / 2));
	[NSColor.separatorColor setFill];
	NSRectFill(NSMakeRect(0, NSHeight(self.bounds) - 1 / self.window.backingScaleFactor, NSWidth(self.bounds), 1 / self.window.backingScaleFactor));
}

- (void)viewDidChangeEffectiveAppearance
{
	[super viewDidChangeEffectiveAppearance];
	self.needsDisplay = YES;
}
@end

// A hidden horizontal scroller does not prevent trackpad panning of wider content.
@interface ThemeListClipView : NSClipView
@end
@implementation ThemeListClipView
- (void)scrollToPoint:(NSPoint)point
{
	point.x = 0;
	[super scrollToPoint:point];
}

- (NSRect)constrainBoundsRect:(NSRect)proposedBounds
{
	NSRect bounds = [super constrainBoundsRect:proposedBounds];
	bounds.origin.x = 0;
	return bounds;
}

- (void)setBoundsOrigin:(NSPoint)origin
{
	origin.x = 0;
	[super setBoundsOrigin:origin];
}
@end

@interface ThemeListScrollView : NSScrollView
@end
@implementation ThemeListScrollView
- (void)scrollWheel:(NSEvent*)event
{
	// Remove horizontal input before AppKit's scrolling and momentum animation.
	if(event.scrollingDeltaX == 0)
	{
		[super scrollWheel:event];
		return;
	}
	if(event.scrollingDeltaY == 0)
		return;
	CGEventRef verticalEvent = CGEventCreateCopy(event.CGEvent);
	for(CGEventField field : { kCGScrollWheelEventDeltaAxis2, kCGScrollWheelEventFixedPtDeltaAxis2, kCGScrollWheelEventPointDeltaAxis2 })
		CGEventSetIntegerValueField(verticalEvent, field, 0);
	[super scrollWheel:[NSEvent eventWithCGEvent:verticalEvent]];
	CFRelease(verticalEvent);
}
@end

@protocol ThemeListDelegate <NSTableViewDelegate>
- (void)renameThemeAtRow:(NSInteger)row;
- (NSMenu*)themeMenuForRow:(NSInteger)row;
@end

@interface ThemeListView : NSTableView
@end
@implementation ThemeListView
- (NSMenu*)menuForEvent:(NSEvent*)event
{
	NSInteger row = [self rowAtPoint:[self convertPoint:event.locationInWindow fromView:nil]];
	return [(id<ThemeListDelegate>)self.delegate themeMenuForRow:row];
}

- (void)keyDown:(NSEvent*)event
{
	NSEventModifierFlags modifiers = event.modifierFlags & (NSEventModifierFlagCommand|NSEventModifierFlagControl|NSEventModifierFlagOption|NSEventModifierFlagShift);
	if(modifiers == 0 && ([event.charactersIgnoringModifiers isEqualToString:@"\r"] || [event.charactersIgnoringModifiers isEqualToString:@"\x03"]))
	{
		[(id<ThemeListDelegate>)self.delegate renameThemeAtRow:self.selectedRow];
		return;
	}
	[super keyDown:event];
}

- (void)mouseDown:(NSEvent*)event
{
	if((event.modifierFlags & NSEventModifierFlagDeviceIndependentFlagsMask) == NSEventModifierFlagCommand)
	{
		NSInteger row = [self rowAtPoint:[self convertPoint:event.locationInWindow fromView:nil]];
		if(row >= 0)
			[(id<ThemeListDelegate>)self.delegate renameThemeAtRow:row];
		return;
	}
	[super mouseDown:event];
}
@end

// OakDocumentView creates a placeholder document during initialization too.
@interface ThemePreviewView : OakDocumentView
@end
@implementation ThemePreviewView
- (void)setDocument:(OakDocument*)document
{
	document.disableLanguageServices = YES;
	[super setDocument:document];
}
@end

@interface ThemeEditorController : NSObject <NSApplicationDelegate, NSWindowDelegate, NSTableViewDataSource, ThemeListDelegate, NSTextFieldDelegate, OakTextViewDelegate, NSSplitViewDelegate>
{
	std::vector<bundles::item_ptr> _themes;
	struct theme_row_t
	{
		NSString* title;
		bundles::item_ptr item;
	};
	std::vector<theme_row_t> _themeRows;
	BOOL _rebuildingThemes;
	BOOL _pendingThemeSelection;
	bundles::item_ptr _source;
	bundles::item_ptr _themeEditorBundle;
	oak::uuid_t _previewUUID;
	NSMutableDictionary* _draft;
	plist::dictionary_t _loadedTheme;
	NSMutableDictionary<NSString*, NSData*>* _originalFiles;
	NSWindow* _window;
	NSTableView* _table;
	NSStackView* _controls;
	NSStackView* _gutterControls;
	NSStackView* _paletteControls;
	NSTextField* _paletteHeading;
	NSPopUpButton* _paletteColors;
	NSPopUpButton* _paletteTarget;
	NSTextField* _gutterHeading;
	OakDocumentView* _preview;
	NSPopUpButton* _fontFamily;
	NSPopUpButton* _fontTypeface;
	NSSegmentedControl* _fontStyles;
	NSPopUpButton* _rules;
	NSButton* _removeRuleButton;
	NSSegmentedControl* _appearance;
	NSPopUpButton* _sampleLanguage;
	std::vector<theme_editor::sample_t> _availableSamples;
	NSTextField* _sampleStatus;
	NSMutableDictionary<NSString*, NSString*>* _sampleEdits;
	NSString* _currentSample;
	BOOL _previewShowInvisibles;
	NSFont* _baseFont;
	NSMutableDictionary<NSString*, NSTextField*>* _fields;
	NSMutableDictionary<NSString*, NSColorWell*>* _wells;
	BOOL _dirty;
	NSString* _renameOriginalName;
	BOOL _renameWasDirty;
	BOOL _newTheme;
	BOOL _hasPlaceholderGlobalRule;
	NSInteger _activeRule;
	NSMutableSet<NSString*>* _invalidFields;
	NSInteger _selectedRow;
	NSString* _requestedThemeUUID;
	NSString* _requestedFileType;
}
@end

@implementation ThemeEditorController
- (NSAlert*)themeAlert
{
	NSAlert* alert = [NSAlert new];
	alert.icon = NSApp.applicationIconImage;
	return alert;
}

- (NSButton*)button:(NSString*)title action:(SEL)action
{
	return [NSButton buttonWithTitle:title target:self action:action];
}

- (void)addColorControlForKey:(NSString*)key label:(NSString*)title inStack:(NSStackView*)stack
{
	NSColorWell* well = [[NSColorWell alloc] initWithFrame:NSMakeRect(0, 0, 40, 24)];
	well.identifier = key;
	well.accessibilityLabel = title;
	well.target = self;
	well.action = @selector(colorChanged:);
	[well.widthAnchor constraintEqualToConstant:40].active = YES;
	[well.heightAnchor constraintEqualToConstant:24].active = YES;
	_wells[key] = well;

	NSTextField* label = [NSTextField labelWithString:title];
	label.font = [NSFont systemFontOfSize:NSFont.smallSystemFontSize];
	label.lineBreakMode = NSLineBreakByTruncatingTail;
	label.toolTip = title;
	[label.widthAnchor constraintEqualToConstant:120].active = YES;

	NSTextField* field = [NSTextField textFieldWithString:@""];
	field.identifier = key;
	field.accessibilityLabel = [title stringByAppendingString:@" hex color"];
	field.placeholderString = @"#RRGGBB[AA]";
	field.toolTip = @"Enter #RGB, #RGBA, #RRGGBB, or #RRGGBBAA. Leave blank to inherit the color.";
	field.font = [NSFont monospacedSystemFontOfSize:12 weight:NSFontWeightRegular];
	field.target = self;
	field.action = @selector(settingChanged:);
	field.delegate = self;
	field.continuous = NO;
	[field.widthAnchor constraintEqualToConstant:90].active = YES;
	_fields[key] = field;

	NSView* spacer = [NSView new];
	[spacer setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[spacer setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	NSStackView* row = [NSStackView stackViewWithViews:@[ well, label, spacer, field ]];
	row.alignment = NSLayoutAttributeCenterY;
	row.spacing = 8;
	[row.widthAnchor constraintEqualToConstant:275].active = YES;
	[stack addArrangedSubview:row];
}

// Grouped settings stay visible, with inset separators between related controls.
- (NSStackView*)settingsGroup:(NSString*)title
{
	NSTextField* heading = [NSTextField labelWithString:title];
	heading.font = [NSFont boldSystemFontOfSize:NSFont.systemFontSize];
	[_controls addArrangedSubview:heading];
	if([title isEqualToString:@"Gutter colors"])
		_gutterHeading = heading;
	ThemeSettingsBox* panel = [ThemeSettingsBox new];
	panel.boxType = NSBoxCustom;
	panel.borderWidth = 0;
	panel.wantsLayer = YES;
	panel.layer.borderWidth = 1;
	[panel updateOutline];
	panel.layer.cornerRadius = 10;
	panel.transparent = NO;
	panel.titlePosition = NSNoTitle;
	panel.fillColor = NSColor.controlBackgroundColor;
	panel.cornerRadius = 10;
	panel.contentViewMargins = NSZeroSize;
	panel.translatesAutoresizingMaskIntoConstraints = NO;
	[_controls addArrangedSubview:panel];
	[panel.widthAnchor constraintEqualToAnchor:_controls.widthAnchor constant:-24].active = YES;
	NSStackView* stack = [NSStackView new];
	stack.orientation = NSUserInterfaceLayoutOrientationVertical;
	stack.alignment = NSLayoutAttributeLeading;
	stack.spacing = 8;
	stack.translatesAutoresizingMaskIntoConstraints = NO;
	[panel.contentView addSubview:stack];
	[NSLayoutConstraint activateConstraints:@[
		[stack.leadingAnchor constraintEqualToAnchor:panel.contentView.leadingAnchor constant:10],
		[stack.trailingAnchor constraintEqualToAnchor:panel.contentView.trailingAnchor constant:-10],
		[stack.topAnchor constraintEqualToAnchor:panel.contentView.topAnchor constant:12],
		[stack.bottomAnchor constraintEqualToAnchor:panel.contentView.bottomAnchor constant:-12],
	]];
	[_controls setCustomSpacing:18 afterView:panel];
	return stack;
}

- (void)addSettingsDividerInStack:(NSStackView*)stack
{
	NSBox* divider = [NSBox new];
	divider.boxType = NSBoxSeparator;
	[stack addArrangedSubview:divider];
	[divider.widthAnchor constraintEqualToAnchor:stack.widthAnchor].active = YES;
	[divider.heightAnchor constraintEqualToConstant:1].active = YES;
}

- (void)applicationDidFinishLaunching:(NSNotification*)notification
{
	// Preserve macOS's app-icon framing and padding when launching through the CLI.
	NSImage* appIcon = [[NSWorkspace.sharedWorkspace iconForFile:NSBundle.mainBundle.bundlePath] copy];
	// NSApplication snapshots this image at its nominal size; 32pt produces blur.
	appIcon.size = NSMakeSize(512, 512);
	NSApp.applicationIconImage = appIcon;
	_previewUUID.generate();
	_selectedRow = -1;
	auto index = theme_editor::load_bundle_index();
	bundles::set_index(index.first, index.second);
	_themes = bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeTheme);
	std::sort(_themes.begin(), _themes.end(), [](auto const& lhs, auto const& rhs) { return [to_ns(lhs->name()) localizedCaseInsensitiveCompare:to_ns(rhs->name())] == NSOrderedAscending; });

	[self rebuildThemesSelecting:bundles::item_ptr()];

	_window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 1280, 800) styleMask:NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskMiniaturizable|NSWindowStyleMaskResizable backing:NSBackingStoreBuffered defer:NO];
	_window.title = @"Theme Editor";
	_window.titlebarAppearsTransparent = YES;
	_window.titlebarSeparatorStyle = NSTitlebarSeparatorStyleNone;
	_window.minSize = NSMakeSize(1000, 600);
	_window.delegate = self;
	[_window setFrameAutosaveName:@"ThemeEditorWindow"];
	NSSplitView* split = [[ThemeEditorSplitView alloc] initWithFrame:_window.contentView.bounds];
	split.vertical = YES;
	split.delegate = self;
	split.dividerStyle = NSSplitViewDividerStyleThin;
	split.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
	_window.contentView = split;

	NSView* sidebar = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 230, 800)];
	ThemeSidebarPanel* sidebarPanel = [[ThemeSidebarPanel alloc] initWithFrame:NSInsetRect(sidebar.bounds, 8, 8)];
	sidebarPanel.identifier = @"theme-sidebar-panel";
	sidebarPanel.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
	sidebarPanel.wantsLayer = YES;
	sidebarPanel.layer.cornerRadius = 14;
	sidebarPanel.layer.masksToBounds = YES;
	[sidebar addSubview:sidebarPanel];
	NSScrollView* themeScroll = [[ThemeListScrollView alloc] initWithFrame:NSMakeRect(0, 24, NSWidth(sidebarPanel.bounds), NSHeight(sidebarPanel.bounds) - 24)];
	themeScroll.contentView = [[ThemeListClipView alloc] initWithFrame:themeScroll.contentView.frame];
	themeScroll.drawsBackground = NO;
	themeScroll.contentView.drawsBackground = NO;
	themeScroll.borderType = NSNoBorder;
	themeScroll.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
	themeScroll.hasVerticalScroller = YES;
	themeScroll.hasHorizontalScroller = NO;
	themeScroll.horizontalScrollElasticity = NSScrollElasticityNone;
	themeScroll.autohidesScrollers = YES;
	themeScroll.usesPredominantAxisScrolling = NO;
	_table = [[ThemeListView alloc] initWithFrame:themeScroll.bounds];
	NSTableColumn* column = [[NSTableColumn alloc] initWithIdentifier:@"theme"];
	_table.headerView = nil;
	column.width = themeScroll.contentSize.width;
	[_table addTableColumn:column];
	_table.dataSource = self;
	_table.style = NSTableViewStyleSourceList;
	_table.backgroundColor = NSColor.clearColor;
	_table.rowHeight = 26;

	_table.allowsEmptySelection = NO;
	_table.floatsGroupRows = NO;
	_table.accessibilityLabel = @"Installed TextMate themes";
	themeScroll.documentView = _table;
	[sidebarPanel addSubview:themeScroll];
	NSButton* newTheme = [self button:@"" action:@selector(newTheme:)];
	newTheme.image = [NSImage imageWithSystemSymbolName:@"plus" accessibilityDescription:nil];
	newTheme.accessibilityLabel = @"New theme";
	newTheme.toolTip = @"Create a new theme.";
	newTheme.bordered = NO;
	newTheme.imagePosition = NSImageOnly;
	newTheme.controlSize = NSControlSizeSmall;
	newTheme.contentTintColor = NSColor.secondaryLabelColor;
	newTheme.image = [newTheme.image imageWithSymbolConfiguration:[NSImageSymbolConfiguration configurationWithPointSize:12 weight:NSFontWeightMedium]];
	newTheme.frame = NSMakeRect(0, 0, 24, 24);
	ThemeActionsBar* themeActions = [[ThemeActionsBar alloc] initWithFrame:NSMakeRect(0, 0, NSWidth(sidebarPanel.bounds), 24)];
	themeActions.autoresizingMask = NSViewWidthSizable;
	[themeActions addSubview:newTheme];
	NSBox* themeActionDivider = [[NSBox alloc] initWithFrame:NSMakeRect(24, 4, 1, 16)];
	themeActionDivider.boxType = NSBoxSeparator;
	[themeActions addSubview:themeActionDivider];
	NSButton* deleteTheme = [self button:@"" action:@selector(deleteTheme:)];
	deleteTheme.image = [[NSImage imageWithSystemSymbolName:@"minus" accessibilityDescription:nil] imageWithSymbolConfiguration:[NSImageSymbolConfiguration configurationWithPointSize:12 weight:NSFontWeightMedium]];
	deleteTheme.accessibilityLabel = @"Delete theme";
	deleteTheme.toolTip = @"Delete the selected theme.";
	deleteTheme.bordered = NO;
	deleteTheme.imagePosition = NSImageOnly;
	deleteTheme.controlSize = NSControlSizeSmall;
	deleteTheme.contentTintColor = NSColor.secondaryLabelColor;
	deleteTheme.frame = NSMakeRect(25, 0, 24, 24);
	[themeActions addSubview:deleteTheme];
	[sidebarPanel addSubview:themeActions];
	[split addSubview:sidebar];

	NSView* inspectorPane = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 320, 800)];
	NSScrollView* inspector = [NSScrollView new];
	inspector.translatesAutoresizingMaskIntoConstraints = NO;
	inspector.hasVerticalScroller = YES;
	[inspectorPane addSubview:inspector];
	_controls = [ThemeInspectorStackView new];
	_controls.orientation = NSUserInterfaceLayoutOrientationVertical;
	_controls.alignment = NSLayoutAttributeLeading;
	_controls.spacing = 6;
	_controls.detachesHiddenViews = YES;
	inspector.backgroundColor = NSColor.windowBackgroundColor;
	_controls.edgeInsets = NSEdgeInsetsMake(12, 12, 12, 12);
	inspector.documentView = _controls;
	_controls.translatesAutoresizingMaskIntoConstraints = NO;
	[_controls.widthAnchor constraintEqualToAnchor:inspector.contentView.widthAnchor].active = YES;
	NSStackView* appearanceControls = [self settingsGroup:@"Appearance"];
	_appearance = [NSSegmentedControl segmentedControlWithLabels:@[ @"Light", @"Dark", @"Unspecified" ] trackingMode:NSSegmentSwitchTrackingSelectOne target:self action:@selector(appearanceChanged:)];
	_appearance.segmentDistribution = NSSegmentDistributionFillEqually;
	[_appearance setToolTip:@"Available for either light or dark appearance." forSegment:2];
	_appearance.accessibilityLabel = @"Theme appearance";
	[_appearance.widthAnchor constraintEqualToConstant:275].active = YES;
	[appearanceControls addArrangedSubview:_appearance];
	NSButton* invert = [self button:@"Invert" action:@selector(invertTheme:)];
	invert.toolTip = @"Reverse lightness for all theme colors while preserving hues and opacity. Click again to invert back.";
	[appearanceControls addArrangedSubview:invert];
	_paletteControls = [self settingsGroup:@"Imported palette"];
	_paletteHeading = (NSTextField*)_controls.arrangedSubviews[_controls.arrangedSubviews.count - 2];
	_paletteColors = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	_paletteColors.accessibilityLabel = @"Imported swatch";
	[_paletteColors.widthAnchor constraintEqualToConstant:275].active = YES;
	[_paletteControls addArrangedSubview:_paletteColors];
	_paletteTarget = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	[_paletteTarget addItemsWithTitles:@[ @"Foreground", @"Background", @"Caret", @"Selection", @"Invisibles" ]];
	_paletteTarget.accessibilityLabel = @"Apply swatch to";
	NSButton* applySwatch = [self button:@"Apply" action:@selector(applyPaletteColor:)];
	NSStackView* paletteActions = [NSStackView stackViewWithViews:@[ _paletteTarget, applySwatch ]];
	[_paletteControls addArrangedSubview:paletteActions];
	NSStackView* ruleControls = [self settingsGroup:@"Style rule"];
	_rules = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	_rules.target = self;
	_rules.action = @selector(ruleSelected:);
	NSButton* editRuleButton = [self button:@"" action:@selector(renameRule:)];
	editRuleButton.image = [NSImage imageWithSystemSymbolName:@"pencil" accessibilityDescription:nil];
	editRuleButton.accessibilityLabel = @"Edit style rule";
	editRuleButton.toolTip = @"Edit the selected rule’s name and scope selector";
	[_rules setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[_rules setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[editRuleButton setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
	NSStackView* ruleSelector = [NSStackView stackViewWithViews:@[ _rules, editRuleButton ]];
	ruleSelector.spacing = 8;
	ruleSelector.alignment = NSLayoutAttributeCenterY;
	ruleSelector.distribution = NSStackViewDistributionFill;
	[ruleControls addArrangedSubview:ruleSelector];
	[ruleSelector.widthAnchor constraintEqualToAnchor:ruleControls.widthAnchor].active = YES;
	ruleControls.spacing = 4;
	NSButton* addRuleButton = [self button:@"" action:@selector(addRule:)];
	addRuleButton.image = [NSImage imageWithSystemSymbolName:@"plus" accessibilityDescription:nil];
	addRuleButton.accessibilityLabel = @"Add rule";
	addRuleButton.toolTip = @"Add rule";
	_removeRuleButton = [self button:@"" action:@selector(removeRule:)];
	_removeRuleButton.image = [NSImage imageWithSystemSymbolName:@"minus" accessibilityDescription:nil];
	_removeRuleButton.accessibilityLabel = @"Remove rule";
	_removeRuleButton.toolTip = @"Remove rule";
	for(NSButton* button in @[ editRuleButton, addRuleButton, _removeRuleButton ])
	{
		button.bordered = NO;
		button.imagePosition = NSImageOnly;
		button.controlSize = NSControlSizeSmall;
		button.contentTintColor = NSColor.secondaryLabelColor;
		button.image = [button.image imageWithSymbolConfiguration:[NSImageSymbolConfiguration configurationWithPointSize:12 weight:NSFontWeightMedium]];
		[button.widthAnchor constraintEqualToConstant:24].active = YES;
		[button.heightAnchor constraintEqualToConstant:24].active = YES;
	}
	NSBox* ruleActionDivider = [NSBox new];
	ruleActionDivider.boxType = NSBoxSeparator;
	[ruleActionDivider.widthAnchor constraintEqualToConstant:1].active = YES;
	[ruleActionDivider.heightAnchor constraintEqualToConstant:16].active = YES;
	NSStackView* ruleActions = [NSStackView stackViewWithViews:@[ addRuleButton, ruleActionDivider, _removeRuleButton ]];
	ruleActions.spacing = 0;
	ruleActions.alignment = NSLayoutAttributeCenterY;
	NSView* ruleGroupContent = ruleControls.superview;
	for(NSLayoutConstraint* constraint in ruleGroupContent.constraints)
	{
		if(constraint.firstItem == ruleControls && constraint.firstAttribute == NSLayoutAttributeBottom)
			constraint.active = NO;
	}
	ThemeActionsBar* ruleBar = [ThemeActionsBar new];
	ruleBar.translatesAutoresizingMaskIntoConstraints = NO;
	ruleActions.translatesAutoresizingMaskIntoConstraints = NO;
	[ruleGroupContent addSubview:ruleBar];
	[ruleBar addSubview:ruleActions];
	[NSLayoutConstraint activateConstraints:@[
		[ruleBar.leadingAnchor constraintEqualToAnchor:ruleGroupContent.leadingAnchor],
		[ruleBar.trailingAnchor constraintEqualToAnchor:ruleGroupContent.trailingAnchor],
		[ruleBar.bottomAnchor constraintEqualToAnchor:ruleGroupContent.bottomAnchor],
		[ruleBar.heightAnchor constraintEqualToConstant:24],
		[ruleControls.bottomAnchor constraintEqualToAnchor:ruleBar.topAnchor constant:-8],
		[ruleActions.leadingAnchor constraintEqualToAnchor:ruleBar.leadingAnchor],
		[ruleActions.centerYAnchor constraintEqualToAnchor:ruleBar.centerYAnchor],
	]];
	_fields = [NSMutableDictionary dictionary];
	_invalidFields = [NSMutableSet set];
	_wells = [NSMutableDictionary dictionary];
	for(NSArray<NSString*>* color in @[ @[ @"foreground", @"Foreground" ], @[ @"background", @"Background" ], @[ @"caret", @"Caret" ], @[ @"selection", @"Selection" ], @[ @"invisibles", @"Invisibles" ] ])
	{
		[self addSettingsDividerInStack:ruleControls];
		[self addColorControlForKey:color[0] label:color[1] inStack:ruleControls];
	}
	[self addSettingsDividerInStack:ruleControls];
	[ruleControls setCustomSpacing:14 afterView:ruleControls.arrangedSubviews.lastObject];
	for(NSString* key in @[ @"fontName", @"fontStyle" ])
	{
		NSTextField* field = [NSTextField textFieldWithString:@""];
		field.identifier = key;
		_fields[key] = field;
	}
	_fontFamily = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	[_fontFamily addItemWithTitle:@"Inherited"];
	for(NSString* family in [NSFontManager.sharedFontManager.availableFontFamilies sortedArrayUsingSelector:@selector(localizedStandardCompare:)])
		[_fontFamily addItemWithTitle:family];
	_fontFamily.target = self;
	_fontFamily.action = @selector(fontFamilyChanged:);
	_fontFamily.accessibilityLabel = @"Font family";
	[_fontFamily.widthAnchor constraintEqualToConstant:275].active = YES;
	[ruleControls addArrangedSubview:_fontFamily];
	[ruleControls setCustomSpacing:10 afterView:_fontFamily];
	_fontTypeface = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	_fontTypeface.target = self;
	_fontTypeface.action = @selector(fontTypefaceChanged:);
	_fontTypeface.accessibilityLabel = @"Typeface";
	[_fontTypeface.widthAnchor constraintEqualToConstant:193].active = YES;
	NSTextField* size = [NSTextField textFieldWithString:@""];
	size.identifier = @"fontSize";
	size.placeholderString = @"Auto";
	size.accessibilityLabel = @"Font size in points";
	size.toolTip = @"Font size in points (4–200), percent (120%), or em (1.2em). Clear to inherit.";
	size.delegate = self;
	size.target = self;
	size.action = @selector(settingChanged:);
	[size.widthAnchor constraintEqualToConstant:74].active = YES;
	_fields[@"fontSize"] = size;
	NSStackView* fontRow = [NSStackView stackViewWithViews:@[ _fontTypeface, size ]];
	fontRow.spacing = 8;
	fontRow.alignment = NSLayoutAttributeCenterY;
	[ruleControls addArrangedSubview:fontRow];
	[ruleControls setCustomSpacing:10 afterView:fontRow];
	_fontStyles = [NSSegmentedControl segmentedControlWithLabels:@[ @"B", @"I", @"U", @"S" ] trackingMode:NSSegmentSwitchTrackingSelectAny target:self action:@selector(fontStylesChanged:)];
	_fontStyles.segmentDistribution = NSSegmentDistributionFillEqually;
	NSArray* styleNames = @[ @"Bold", @"Italic", @"Underline", @"Strikethrough" ];
	NSArray* symbols = @[ @"bold", @"italic", @"underline", @"strikethrough" ];
	for(NSInteger i = 0; i < 4; ++i)
	{
		[_fontStyles setImage:[NSImage imageWithSystemSymbolName:symbols[i] accessibilityDescription:styleNames[i]] forSegment:i];
		[_fontStyles setLabel:@"" forSegment:i];
		[_fontStyles setToolTip:styleNames[i] forSegment:i];
	}
	_fontStyles.accessibilityLabel = @"Font formatting";
	[_fontStyles.widthAnchor constraintEqualToConstant:180].active = YES;
	NSButton* resetStyle = [self button:@"Inherited" action:@selector(resetFontStyle:)];
	resetStyle.controlSize = NSControlSizeSmall;
	resetStyle.toolTip = @"Remove the formatting override for this rule.";
	NSStackView* styleRow = [NSStackView stackViewWithViews:@[ _fontStyles, resetStyle ]];
	styleRow.spacing = 8;
	styleRow.alignment = NSLayoutAttributeCenterY;
	[ruleControls addArrangedSubview:styleRow];
	_gutterControls = [self settingsGroup:@"Gutter colors"];
	for(NSArray<NSString*>* color in @[ @[ @"foreground", @"Line numbers" ], @[ @"background", @"Background" ], @[ @"divider", @"Divider" ], @[ @"selectionForeground", @"Selected numbers" ], @[ @"selectionBackground", @"Selected background" ] ])
	{
		if(_gutterControls.arrangedSubviews.count)
			[self addSettingsDividerInStack:_gutterControls];
		[self addColorControlForKey:[@"gutter." stringByAppendingString:color[0]] label:color[1] inStack:_gutterControls];
	}
	[NSLayoutConstraint activateConstraints:@[
		[inspector.leadingAnchor constraintEqualToAnchor:inspectorPane.leadingAnchor],
		[inspector.trailingAnchor constraintEqualToAnchor:inspectorPane.trailingAnchor],
		[inspector.topAnchor constraintEqualToAnchor:inspectorPane.topAnchor],
		[inspector.bottomAnchor constraintEqualToAnchor:inspectorPane.bottomAnchor],
	]];
	[split addSubview:inspectorPane];

	_preview = [[ThemePreviewView alloc] initWithFrame:NSMakeRect(0, 0, 730, 800)];
	_preview.hideStatusBar = YES;
	_preview.showsLineNumbers = YES;
	_preview.accessibilityLabel = @"Live TextMate preview";
	_preview.textView.delegate = self;
	_sampleEdits = [NSMutableDictionary dictionary];
	_sampleLanguage = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	_sampleLanguage.accessibilityLabel = @"Sample language";
	_sampleLanguage.target = self;
	_sampleLanguage.action = @selector(sampleLanguageChanged:);
	[_sampleLanguage.widthAnchor constraintEqualToConstant:180].active = YES;
	for(auto const& sample : theme_editor::samples())
	{
		for(auto const& scope : sample.scopes)
		{
			if(bundles::query(bundles::kFieldGrammarScope, scope, scope::wildcard, bundles::kItemTypeGrammar).empty())
				continue;
			_availableSamples.push_back({ sample.name, sample.file, { scope } });
			break;
		}
	}
	if(_availableSamples.empty())
	{
		_availableSamples.push_back({ "Plain Text", theme_editor::samples().front().file, { "text.plain" } });
		_sampleLanguage.enabled = NO;
	}
	std::sort(_availableSamples.begin(), _availableSamples.end(), [](auto const& lhs, auto const& rhs) {
		return [to_ns(lhs.name) localizedStandardCompare:to_ns(rhs.name)] == NSOrderedAscending;
	});
	for(auto const& sample : _availableSamples)
		[_sampleLanguage addItemWithTitle:to_ns(sample.name)];
	NSString* remembered = [NSUserDefaults.standardUserDefaults stringForKey:@"ThemeEditorSampleLanguage"];
	NSInteger initialSample = 0;
	for(size_t index = 0; index < _availableSamples.size(); ++index)
		if([remembered isEqualToString:to_ns(_availableSamples[index].file)])
			initialSample = index;
	NSInteger requestedSample = [self sampleIndexForFileType:_requestedFileType];
	if(requestedSample >= 0)
		initialSample = requestedSample;
	[_sampleLanguage selectItemAtIndex:initialSample];
	NSButton* invisibles = [NSButton checkboxWithTitle:@"Invisibles" target:self action:@selector(togglePreviewInvisibles:)];
	NSStackView* sampleActions = [NSStackView stackViewWithViews:@[
		_sampleLanguage, [self button:@"Reset Sample" action:@selector(resetSample:)], invisibles
	]];
	sampleActions.spacing = 8;
	_sampleStatus = [NSTextField wrappingLabelWithString:@""];
	_sampleStatus.font = [NSFont systemFontOfSize:NSFont.smallSystemFontSize];
	_sampleStatus.textColor = NSColor.secondaryLabelColor;
	_sampleStatus.accessibilityLabel = @"Sample highlighting status";
	NSStackView* sampleBar = [NSStackView stackViewWithViews:@[ sampleActions, _sampleStatus ]];
	sampleBar.orientation = NSUserInterfaceLayoutOrientationVertical;
	sampleBar.alignment = NSLayoutAttributeLeading;
	sampleBar.spacing = 6;
	sampleBar.edgeInsets = NSEdgeInsetsMake(10, 12, 10, 12);
	[_sampleStatus.widthAnchor constraintEqualToAnchor:sampleBar.widthAnchor constant:-24].active = YES;
	[_preview addAuxiliaryView:sampleBar atEdge:NSMaxYEdge];
	[self displaySampleReset:NO];
	[split addSubview:_preview];
	// Only the preview changes width when the window resizes.
	[split setHoldingPriority:480 forSubviewAtIndex:0];
	[split setHoldingPriority:480 forSubviewAtIndex:1];
	[split setHoldingPriority:NSLayoutPriorityDefaultLow forSubviewAtIndex:2];
	[split adjustSubviews];
	[split setPosition:230 ofDividerAtIndex:0];
	[split setPosition:230 + split.dividerThickness + 320 ofDividerAtIndex:1];
	[self createMenu];
	_table.delegate = self;
	if(!_themes.empty())
	{
		_source = _themes.front();
		for(auto const& theme : _themes)
			if(_requestedThemeUUID && [to_ns(to_s(theme->uuid())) caseInsensitiveCompare:_requestedThemeUUID] == NSOrderedSame)
				_source = theme;
		[self loadSource];
	}
	else
		[self newTheme:nil];
	_requestedThemeUUID = nil;
	_requestedFileType = nil;
	[_window center];
	[_window makeKeyAndOrderFront:nil];
	[_controls layoutSubtreeIfNeeded];
	[_controls scrollPoint:NSZeroPoint];
	[NSApp activateIgnoringOtherApps:YES];
}

- (NSInteger)sampleIndexForFileType:(NSString*)fileType
{
	NSInteger match = -1;
	size_t specificity = 0;
	if(!fileType.length)
		return match;
	for(size_t index = 0; index < _availableSamples.size(); ++index)
	{
		for(auto const& scope : _availableSamples[index].scopes)
		{
			NSString* candidate = to_ns(scope);
			if(scope.size() > specificity && ([fileType isEqualToString:candidate] || [fileType hasPrefix:[candidate stringByAppendingString:@"."]]))
			{
				match = index;
				specificity = scope.size();
			}
		}
	}
	return match;
}

- (void)application:(NSApplication*)application openURLs:(NSArray<NSURL*>*)urls
{
	for(NSURL* url in urls)
	{
		if(![url.scheme isEqualToString:@"textmate-theme-editor"] || ![url.host isEqualToString:@"theme"])
			continue;
		NSUUID* uuid = [[NSUUID alloc] initWithUUIDString:url.lastPathComponent];
		if(!uuid)
			continue;
		NSString* fileType = nil;
		for(NSURLQueryItem* item in [NSURLComponents componentsWithURL:url resolvingAgainstBaseURL:NO].queryItems)
			if([item.name isEqualToString:@"fileType"])
				fileType = item.value;
		if(!_window)
		{
			_requestedThemeUUID = uuid.UUIDString;
			_requestedFileType = fileType;
			continue;
		}
		for(size_t row = 0; row < _themeRows.size(); ++row)
		{
			if(_themeRows[row].item && [to_ns(to_s(_themeRows[row].item->uuid())) caseInsensitiveCompare:uuid.UUIDString] == NSOrderedSame)
			{
				if([self selectThemeAtRow:row])
				{
					[_table scrollRowToVisible:_table.selectedRow];
					NSInteger sample = [self sampleIndexForFileType:fileType];
					if(sample >= 0 && sample != _sampleLanguage.indexOfSelectedItem)
					{
						[_sampleLanguage selectItemAtIndex:sample];
						[self displaySampleReset:NO];
					}
				}
				break;
			}
		}
	}
}

- (CGFloat)splitView:(NSSplitView*)splitView constrainSplitPosition:(CGFloat)position ofSubviewAt:(NSInteger)divider
{
	return NSMinX(splitView.bounds) + (divider == 0 ? 230 : 230 + splitView.dividerThickness + 320);
}

- (NSRect)splitView:(NSSplitView*)splitView effectiveRect:(NSRect)proposedEffectiveRect forDrawnRect:(NSRect)drawnRect ofDividerAtIndex:(NSInteger)divider
{
	// Keep the thin visual separators without draggable divider hit areas.
	return NSZeroRect;
}

- (BOOL)splitView:(NSSplitView*)splitView shouldAdjustSizeOfSubview:(NSView*)view
{
	return view == _preview;
}

- (void)splitView:(NSSplitView*)splitView resizeSubviewsWithOldSize:(NSSize)oldSize
{
	if(splitView.subviews.count != 3)
	{
		[splitView adjustSubviews];
		return;
	}
	// The window minimum size leaves room for both fixed columns and the preview.
	CGFloat available = NSWidth(splitView.bounds) - 2 * splitView.dividerThickness;
	CGFloat widths[] = { 230, 320, available - 230 - 320 };
	CGFloat x = NSMinX(splitView.bounds);
	for(size_t index = 0; index < 3; ++index)
	{
		splitView.subviews[index].frame = NSMakeRect(x, NSMinY(splitView.bounds), widths[index], NSHeight(splitView.bounds));
		x += widths[index] + splitView.dividerThickness;
	}
}

- (void)displaySampleReset:(BOOL)reset
{
	auto const& sample = _availableSamples[_sampleLanguage.indexOfSelectedItem];
	NSString* file = to_ns(sample.file);
	if(_currentSample && _preview.document)
		_sampleEdits[_currentSample] = _preview.document.content;
	if(reset)
		[_sampleEdits removeObjectForKey:file];
	NSError* error;
	NSString* content = _sampleEdits[file];
	if(!content)
	{
		NSURL* url = [NSBundle.mainBundle URLForResource:file withExtension:nil subdirectory:@"Samples"];
		content = url ? [NSString stringWithContentsOfURL:url encoding:NSUTF8StringEncoding error:&error] : nil;
	}
	if(!content)
	{
		_sampleStatus.stringValue = @"The sample could not be loaded. Rebuild or reinstall Theme Editor.";
		return;
	}

	std::string const& scope = sample.scopes.front();
	OakDocument* document = [OakDocument documentWithString:content fileType:to_ns(scope) customName:file];
	document.virtualPath = file;
	document.recentTrackingDisabled = YES;
	document.keepBackupFile = NO;
	// Set the type after inserting the content; first-line sniffing may change it.
	document.fileType = to_ns(scope);
	_preview.document = document;
	_preview.textView.showInvisibles = _previewShowInvisibles;
	_baseFont = _preview.textView.font;
	_currentSample = file;
	[NSUserDefaults.standardUserDefaults setObject:file forKey:@"ThemeEditorSampleLanguage"];
	_sampleStatus.stringValue = scope == "text.plain"
		? @"No supported language bundles are installed. Install a language bundle in TextMate, then reopen Theme Editor."
		: @"Command-click text to select its style rule.";
	if(_draft)
		[self changed:_dirty];
}

- (void)sampleLanguageChanged:(id)sender { [self displaySampleReset:NO]; }
- (void)resetSample:(id)sender { [self displaySampleReset:YES]; }
- (void)togglePreviewInvisibles:(NSButton*)sender
{
	_previewShowInvisibles = sender.state == NSControlStateValueOn;
	_preview.textView.showInvisibles = _previewShowInvisibles;
}

- (void)createMenu
{
	NSMenu* menu = [NSMenu new];
	NSMenuItem* app = [NSMenuItem new];
	app.submenu = [NSMenu new];
	[app.submenu addItemWithTitle:@"Quit Theme Editor" action:@selector(terminate:) keyEquivalent:@"q"];
	[menu addItem:app];
	NSMenuItem* file = [NSMenuItem new];
	file.title = @"File";
	file.submenu = [[NSMenu alloc] initWithTitle:@"File"];
	for(NSArray* entry in @[ @[ @"New Theme", NSStringFromSelector(@selector(newTheme:)), @"n" ], @[ @"Import Colors…", NSStringFromSelector(@selector(importSwatches:)), @"" ], @[ @"Save Theme", NSStringFromSelector(@selector(saveTheme:)), @"s" ], @[ @"Export Theme…", NSStringFromSelector(@selector(exportTheme:)), @"" ] ])
	{
		NSMenuItem* item = [file.submenu addItemWithTitle:entry[0] action:NSSelectorFromString(entry[1]) keyEquivalent:entry[2]];
		item.target = self;
	}
	[menu addItem:file];
	NSMenuItem* edit = [NSMenuItem new];
	edit.title = @"Edit";
	edit.submenu = [[NSMenu alloc] initWithTitle:@"Edit"];
	[edit.submenu addItemWithTitle:@"Undo" action:@selector(undo:) keyEquivalent:@"z"];
	[edit.submenu addItemWithTitle:@"Cut" action:@selector(cut:) keyEquivalent:@"x"];
	[edit.submenu addItemWithTitle:@"Copy" action:@selector(copy:) keyEquivalent:@"c"];
	[edit.submenu addItemWithTitle:@"Paste" action:@selector(paste:) keyEquivalent:@"v"];
	[edit.submenu addItemWithTitle:@"Select All" action:@selector(selectAll:) keyEquivalent:@"a"];
	[menu addItem:edit];
	NSApp.mainMenu = menu;
}

- (void)rebuildThemesSelecting:(bundles::item_ptr)itemToSelect
{
	_rebuildingThemes = YES;
	std::sort(_themes.begin(), _themes.end(), [](auto const& lhs, auto const& rhs) { return [to_ns(lhs->name()) localizedCaseInsensitiveCompare:to_ns(rhs->name())] == NSOrderedAscending; });
	_themeRows.clear();
	_selectedRow = -1;
	for(auto category : { theme_editor::light, theme_editor::dark, theme_editor::unspecified })
	{
		NSString* heading = category == theme_editor::light ? @"Light" : category == theme_editor::dark ? @"Dark" : @"Unspecified";
		_themeRows.push_back({ heading, bundles::item_ptr() });
		for(auto const& item : _themes)
		{
			auto appearance = item == _source && _draft ? theme_editor::appearance(plist::convert((__bridge CFPropertyListRef)_draft)) : theme_editor::appearance(item->value_for_field(bundles::kFieldSemanticClass));
			if(appearance != category)
				continue;
			if(itemToSelect && item->uuid() == itemToSelect->uuid())
				_selectedRow = _themeRows.size();
			NSString* title = item == _source && _draft ? _draft[@"name"] : to_ns(item->name());
			_themeRows.push_back({ [title stringByAppendingString:item->paths().empty() ? @" (unsaved)" : @""], item });
		}
	}
	[_table reloadData];
	if(_selectedRow >= 0)
	{
		[_table selectRowIndexes:[NSIndexSet indexSetWithIndex:_selectedRow] byExtendingSelection:NO];
		[_table scrollRowToVisible:_selectedRow];
	}
	_rebuildingThemes = NO;
}

- (NSInteger)numberOfRowsInTableView:(NSTableView*)tableView { return _themeRows.size(); }
- (BOOL)tableView:(NSTableView*)tableView isGroupRow:(NSInteger)row { return !_themeRows[row].item; }
- (NSView*)tableView:(NSTableView*)tableView viewForTableColumn:(NSTableColumn*)column row:(NSInteger)row
{
	NSTableCellView* cell = [NSTableCellView new];
	NSTextField* label = [ThemeNameField labelWithString:_themeRows[row].title];
	label.font = _themeRows[row].item ? [NSFont systemFontOfSize:NSFont.systemFontSize] : [NSFont boldSystemFontOfSize:NSFont.smallSystemFontSize];
	label.textColor = _themeRows[row].item ? NSColor.labelColor : NSColor.secondaryLabelColor;
	label.lineBreakMode = NSLineBreakByTruncatingTail;
	if(_themeRows[row].item)
	{
		label.identifier = @"theme-name";
		label.toolTip = @"Press Return or Command-click to rename this theme.";
		label.delegate = self;
		label.target = self;
		label.action = @selector(finishRenamingTheme:);
	}
	label.translatesAutoresizingMaskIntoConstraints = NO;
	[cell addSubview:label];
	cell.textField = label;
	[NSLayoutConstraint activateConstraints:@[
		[label.leadingAnchor constraintEqualToAnchor:cell.leadingAnchor],
		[label.trailingAnchor constraintEqualToAnchor:cell.trailingAnchor],
		[label.centerYAnchor constraintEqualToAnchor:cell.centerYAnchor],
		[label.heightAnchor constraintEqualToConstant:ceil(label.intrinsicContentSize.height)],
	]];
	return cell;
}

- (void)appearanceChanged:(id)sender
{
	auto value = plist::convert((__bridge CFPropertyListRef)_draft);
	theme_editor::set_appearance(value, static_cast<theme_editor::appearance_t>(_appearance.selectedSegment));
	_draft = MutableTheme(value);
	[self changed:YES];
	[self rebuildThemesSelecting:_source];
}

- (void)invertTheme:(id)sender
{
	[_window makeFirstResponder:nil];
	if(_invalidFields.count)
	{
		NSBeep();
		return;
	}
	auto const& style = _preview.textView.theme->styles_for_scope(scope::scope_t("theme-editor.default"));
	NSColorSpace* space = theme_editor::color_space(_draft);
	theme_editor::invert_theme_colors(_draft,
		theme_editor::color_string([NSColor colorWithCGColor:style.foreground()], space),
		theme_editor::color_string([NSColor colorWithCGColor:style.background()], space),
		theme_editor::color_string([NSColor colorWithCGColor:style.caret()], space));
	auto value = plist::convert((__bridge CFPropertyListRef)_draft);
	auto appearance = theme_editor::appearance(value);
	if(appearance != theme_editor::unspecified)
		theme_editor::set_appearance(value, appearance == theme_editor::light ? theme_editor::dark : theme_editor::light);
	_draft = MutableTheme(value);
	_appearance.selectedSegment = theme_editor::appearance(value);
	[self changed:YES];
	[self reloadRules:_rules.indexOfSelectedItem];
	[self rebuildThemesSelecting:_source];
}

- (BOOL)confirmDiscard
{
	[_window makeFirstResponder:nil];
	if(!_dirty)
		return YES;
	NSAlert* alert = [self themeAlert];
	alert.messageText = @"Save changes to this theme?";
	[alert addButtonWithTitle:@"Save"];
	[alert addButtonWithTitle:@"Cancel"];
	alert.informativeText = @"Changes appear immediately in the preview. Save to keep them, or revert to the last saved version before leaving this theme.";
	[alert addButtonWithTitle:@"Revert Changes"];
	NSModalResponse response = [alert runModal];
	if(response == NSAlertThirdButtonReturn)
	{
		if(_newTheme)
			[self removeCurrentThemeFromList];
		else
			[self loadSource];
		// Closing the window is followed by another termination check. A fresh
		// placeholder, when no themes remain, also has no edits to discard.
		[self changed:NO];
		return YES;
	}
	return response == NSAlertFirstButtonReturn && [self saveCurrentTheme];
}

- (BOOL)tableView:(NSTableView*)tableView shouldSelectRow:(NSInteger)row
{
	if(_pendingThemeSelection || row < 0 || row >= (NSInteger)_themeRows.size() || !_themeRows[row].item)
		return NO;
	if(_rebuildingThemes || row == _selectedRow)
		return YES;
	auto uuid = _themeRows[row].item->uuid();
	if(![self confirmDiscard])
		return NO;
	if(row < (NSInteger)_themeRows.size() && _themeRows[row].item && _themeRows[row].item->uuid() == uuid)
		return YES;
	// Saving reordered the rows. Finish AppKit's navigation before selecting
	// the intended UUID, and reject any other rows it probes in this event.
	_pendingThemeSelection = YES;
	[NSTimer scheduledTimerWithTimeInterval:0 repeats:NO block:^(NSTimer*) {
		_pendingThemeSelection = NO;
		for(size_t index = 0; index < _themeRows.size(); ++index)
		{
			if(_themeRows[index].item && _themeRows[index].item->uuid() == uuid)
			{
				[self selectThemeAtRow:index];
				break;
			}
		}
	}];
	return NO;
}

- (void)tableViewSelectionDidChange:(NSNotification*)notification
{
	if(_rebuildingThemes)
		return;
	NSInteger row = _table.selectedRow;
	if(row < 0 || row >= (NSInteger)_themeRows.size() || !_themeRows[row].item)
		return;
	_selectedRow = row;
	if(_source && _source->uuid() == _themeRows[row].item->uuid())
		return;
	_source = _themeRows[row].item;
	[self loadSource];
}

- (BOOL)selectThemeAtRow:(NSInteger)row
{
	if(row < 0 || row >= (NSInteger)_themeRows.size() || !_themeRows[row].item)
		return NO;
	if(row != _table.selectedRow)
	{
		auto uuid = _themeRows[row].item->uuid();
		if(![self confirmDiscard])
			return NO;
		// Saving the previous draft can change the sorted rows.
		row = -1;
		for(size_t index = 0; index < _themeRows.size(); ++index)
		{
			if(_themeRows[index].item && _themeRows[index].item->uuid() == uuid)
			{
				row = index;
				break;
			}
		}
		if(row < 0)
			return NO;
		[_table selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
	}
	return YES;
}

- (void)renameThemeAtRow:(NSInteger)row
{
	if(![self selectThemeAtRow:row])
		return;
	row = _table.selectedRow;
	[_table scrollRowToVisible:row];
	NSTextField* label = ((NSTableCellView*)[_table viewAtColumn:0 row:row makeIfNecessary:YES]).textField;
	if(label.currentEditor)
	{
		[label selectText:nil];
		return;
	}
	_renameOriginalName = [_draft[@"name"] copy];
	_renameWasDirty = _dirty;
	label.stringValue = _draft[@"name"] ?: @"Untitled Theme";
	label.editable = YES;
	label.selectable = YES;
	label.drawsBackground = YES;
	label.backgroundColor = NSColor.textBackgroundColor;
	label.textColor = NSColor.textColor;
	[label selectText:nil];
}

- (void)finishRenamingTheme:(id)sender
{
	[_window makeFirstResponder:_table];
}

- (BOOL)control:(NSControl*)control textView:(NSTextView*)textView doCommandBySelector:(SEL)command
{
	if(![control.identifier isEqualToString:@"theme-name"] || command != @selector(cancelOperation:))
		return NO;
	if(_renameOriginalName)
	{
		textView.string = _renameOriginalName;
		((NSTextField*)control).stringValue = _renameOriginalName;
		[self nameChanged:control];
		[self changed:_renameWasDirty];
	}
	[_window makeFirstResponder:_table];
	return YES;
}

- (void)controlTextDidEndEditing:(NSNotification*)notification
{
	NSTextField* field = notification.object;
	if(![field.identifier isEqualToString:@"theme-name"])
		return;
	[self nameChanged:field];
	field.editable = NO;
	field.selectable = NO;
	field.bordered = NO;
	field.drawsBackground = NO;
	field.textColor = NSColor.labelColor;
	NSInteger row = [_table rowForView:field];
	if(row >= 0 && row < (NSInteger)_themeRows.size())
		field.stringValue = _themeRows[row].title;
	_renameOriginalName = nil;
}

- (void)loadSource
{
	[self loadSourceSelectingRule:0];
}

- (void)loadSourceSelectingRule:(NSInteger)selectedRule
{
	_draft = MutableTheme(_source->plist());
	[self reloadPalette];
	_newTheme = _source->paths().empty();
	_originalFiles = [NSMutableDictionary dictionary];
	for(auto const& path : _source->paths())
	{
		NSString* file = to_ns(path);
		_originalFiles[file] = [NSData dataWithContentsOfFile:file] ?: [NSData data];
	}
	NSMutableArray* rules = _draft[@"settings"];
	if(!rules)
		_draft[@"settings"] = rules = [NSMutableArray array];
	BOOL hasGlobal = NO;
	for(NSDictionary* rule in rules)
		hasGlobal |= ![rule[@"scope"] length];
	_hasPlaceholderGlobalRule = !hasGlobal;
	if(_hasPlaceholderGlobalRule)
		[rules insertObject:[@{ @"settings": [NSMutableDictionary dictionary] } mutableCopy] atIndex:0];
	_loadedTheme = plist::convert((__bridge CFPropertyListRef)_draft);
	_appearance.selectedSegment = theme_editor::appearance(_source->plist());
	[self reloadRules:std::clamp(selectedRule, NSInteger(0), NSInteger(rules.count) - 1)];
	[self changed:NO];
	[self rebuildThemesSelecting:_source];
}

- (BOOL)textView:(OakTextView*)textView inspectScope:(NSString*)scope
{
	[_window makeFirstResponder:textView];
	if(_invalidFields.count)
	{
		NSBeep();
		return YES;
	}
	auto value = plist::convert((__bridge CFPropertyListRef)_draft);
	plist::array_t rules;
	plist::get_key_path(value, "settings", rules);
	if(auto match = theme_editor::matching_rule(rules, scope::scope_t(to_s(scope))))
	{
		[_rules selectItemAtIndex:*match];
		[self ruleSelected:nil];
		[_rules scrollRectToVisible:_rules.bounds];
	}
	return YES;
}

- (NSMutableDictionary*)currentRule { return _draft[@"settings"][_rules.indexOfSelectedItem]; }
- (void)reloadRules:(NSInteger)selected
{
	[_rules removeAllItems];
	for(NSDictionary* rule in _draft[@"settings"])
	{
		NSString* title = [rule[@"name"] length] ? rule[@"name"] : ([rule[@"scope"] length] ? rule[@"scope"] : @"Global editor style");
		// addItemWithTitle coalesces duplicate titles; preserve one entry per rule.
		[_rules.menu addItem:[[NSMenuItem alloc] initWithTitle:title action:nil keyEquivalent:@""]];
	}
	[_rules selectItemAtIndex:selected];
	[self ruleSelected:nil];
}

- (void)ruleSelected:(id)sender
{
	if(sender && _invalidFields.count)
	{
		[_rules selectItemAtIndex:_activeRule];
		NSBeep();
		return;
	}
	_activeRule = _rules.indexOfSelectedItem;
	NSMutableDictionary* rule = [self currentRule];
	_removeRuleButton.enabled = [rule[@"scope"] length] > 0 && [_draft[@"settings"] count] > 1;
	BOOL isGlobalRule = ![rule[@"scope"] length];
	_gutterHeading.hidden = !isGlobalRule;
	_gutterControls.superview.superview.hidden = !isGlobalRule;
	NSDictionary* settings = rule[@"settings"];
	for(NSString* key in _fields)
	{
		_fields[key].textColor = NSColor.controlTextColor;
		id value = [key hasPrefix:@"gutter."] ? _draft[@"gutterSettings"][[key substringFromIndex:7]] : settings[key];
		_fields[key].stringValue = value ? [value description] : @"";
		_wells[key].color = theme_editor::parse_color(_fields[key].stringValue, theme_editor::color_space(_draft)) ?: NSColor.clearColor;
	}
	[self updateInheritedGutterColors];
	[self reloadFontControls];
}

- (void)updateInheritedGutterColors
{
	if(theme_ptr theme = _preview.textView.theme)
	{
		auto const& gutter = theme->gutter_styles();
		NSDictionary<NSString*, NSColor*>* colors = @{
			@"gutter.foreground": [NSColor colorWithCGColor:gutter.foreground],
			@"gutter.background": [NSColor colorWithCGColor:gutter.background],
			@"gutter.divider": [NSColor colorWithCGColor:gutter.divider],
			@"gutter.selectionForeground": [NSColor colorWithCGColor:gutter.selectionForeground],
			@"gutter.selectionBackground": [NSColor colorWithCGColor:gutter.selectionBackground],
		};
		for(NSString* key in colors)
		{
			// An unset override inherits a visible color; it is not transparent.
			if(!_fields[key].stringValue.length)
				_wells[key].color = colors[key];
		}
	}
}

- (void)changed:(BOOL)dirty
{
	_dirty = dirty && (_newTheme || _invalidFields.count || plist::convert((__bridge CFPropertyListRef)_draft) != _loadedTheme);
	if(!dirty)
		[_invalidFields removeAllObjects];
	_window.documentEdited = _dirty;
	_window.title = [NSString stringWithFormat:@"%@ — Theme Editor", _draft[@"name"] ?: @"untitled"];
	NSString* fontName = _baseFont.fontName;
	CGFloat fontSize = _baseFont.pointSize;
	for(NSDictionary* rule in _draft[@"settings"])
	{
		if([rule[@"scope"] length])
			continue;
		NSDictionary* settings = rule[@"settings"];
		fontName = settings[@"fontName"] ?: fontName;
		if(auto size = theme::parse_font_size(to_s([settings[@"fontSize"] description])))
			fontSize = *size > 0 ? *size : fontSize * -*size;
	}
	_preview.textView.font = [NSFont fontWithName:fontName size:fontSize] ?: _baseFont;
	// Use a private UUID and uncached styles so drafts cannot affect installed themes.
	auto value = plist::convert((__bridge CFPropertyListRef)_draft);
	value["uuid"] = to_s(_previewUUID);
	// The preview's base font already includes global sizes. Keep scoped sizes
	// in the private theme so relative overrides apply once to that base.
	for(auto& entry : plist::get<plist::array_t>(value["settings"]))
	{
		auto& rule = plist::get<plist::dictionary_t>(entry);
		std::string scope;
		plist::get_key_path(rule, "scope", scope);
		if(scope.empty())
			plist::get<plist::dictionary_t>(rule["settings"]).erase("fontSize");
	}
	auto item = std::make_shared<bundles::item_t>(_previewUUID, bundles::item_ptr(), bundles::kItemTypeTheme);
	item->set_plist(value);
	_preview.textView.theme = std::make_shared<theme_t>(item, NULL_STR, 12, false);
	[self updateInheritedGutterColors];
}

- (void)controlTextDidChange:(NSNotification*)notification
{
	NSTextField* field = notification.object;
	if([field.identifier isEqualToString:@"theme-name"])
		[self updateThemeName:field.currentEditor.string ?: field.stringValue forField:field];
	else
		[self settingChanged:field];
}

- (void)renameRule:(id)sender
{
	if(![_window makeFirstResponder:nil] || _invalidFields.count) { NSBeep(); return; }
	NSMutableDictionary* rule = [self currentRule];
	NSAlert* alert = [self themeAlert];
	alert.messageText = @"Edit style rule";
	alert.informativeText = [rule[@"scope"] length] ? @"Edit the rule’s name and scope selector. A scope selector is required; clear the name to display the selector in the menu." : @"Edit the global rule’s name. Its empty scope applies to the whole editor.";
	[alert addButtonWithTitle:@"Apply"];
	[alert addButtonWithTitle:@"Cancel"];
	NSTextField* name = [NSTextField textFieldWithString:rule[@"name"] ?: @""];
	name.frame = NSMakeRect(0, 70, 300, 24);
	name.identifier = @"rename-rule-name";
	name.accessibilityLabel = @"Rule name";
	name.placeholderString = [rule[@"scope"] length] ? rule[@"scope"] : @"Global editor style";
	BOOL scoped = [rule[@"scope"] length] > 0;
	NSTextField* scope = [NSTextField textFieldWithString:rule[@"scope"] ?: @""];
	scope.frame = NSMakeRect(0, 0, 300, 24);
	scope.identifier = @"edit-rule-scope";
	scope.accessibilityLabel = @"Scope selector";
	scope.enabled = scoped;
	scope.placeholderString = scoped ? @"e.g. string.quoted or comment" : @"Global editor style (all scopes)";
	NSView* form = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 300, 116)];
	NSTextField* nameLabel = [NSTextField labelWithString:@"Rule name"];
	nameLabel.frame = NSMakeRect(0, 96, 300, 20);
	NSTextField* scopeLabel = [NSTextField labelWithString:@"Scope selector"];
	scopeLabel.frame = NSMakeRect(0, 26, 300, 20);
	for(NSView* view in @[ nameLabel, name, scopeLabel, scope ])
		[form addSubview:view];
	name.nextKeyView = scoped ? scope : name;
	scope.nextKeyView = name;
	alert.accessoryView = form;
	alert.window.initialFirstResponder = name;
	NSString* title;
	NSString* selector;
	while(YES)
	{
		if([alert runModal] != NSAlertFirstButtonReturn)
			return;
		[alert.window makeFirstResponder:nil];
		title = [name.stringValue stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
		selector = [scope.stringValue stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
		if(!scoped || selector.length)
			break;
		alert.informativeText = @"Enter a scope selector before applying the rule (for example, string or comment).";
		alert.window.initialFirstResponder = scope;
	}
	if([title isEqualToString:rule[@"name"] ?: @""] && (!scoped || [selector isEqualToString:rule[@"scope"]]))
		return;
	if(scoped)
		rule[@"scope"] = selector;
	if(title.length)
		rule[@"name"] = title;
	else
		[rule removeObjectForKey:@"name"];
	[self reloadRules:_rules.indexOfSelectedItem];
	[self changed:YES];
}

- (void)nameChanged:(id)sender
{
	NSTextField* field = sender;
	[self updateThemeName:field.stringValue forField:field];
}

- (void)updateThemeName:(NSString*)name forField:(NSTextField*)field
{
	NSInteger row = [_table rowForView:field];
	if(row != _selectedRow || row < 0 || [_draft[@"name"] isEqualToString:name])
		return;
	_draft[@"name"] = [name copy];
	_themeRows[row].title = [name stringByAppendingString:_newTheme ? @" (unsaved)" : @""];
	[self changed:YES];
}

- (void)settingChanged:(NSTextField*)field
{
	NSString* key = field.identifier;
	NSString* value = field.stringValue;
	BOOL valid = YES;
	if(value.length && _wells[key])
		valid = theme_editor::parse_color(value, theme_editor::color_space(_draft)) != nil;
	if(value.length && [key isEqualToString:@"fontSize"])
	{
		auto size = theme::parse_font_size(to_s(value));
		CGFloat base = [[self currentRule][@"scope"] length] ? _preview.textView.font.pointSize : _baseFont.pointSize;
		double points = size ? (*size > 0 ? *size : base * -*size) : 0;
		valid = points >= 4 && points <= 200;
	}
	if(value.length && [key isEqualToString:@"fontName"])
		valid = [NSFont fontWithName:value size:12] != nil;
	field.textColor = valid ? NSColor.controlTextColor : NSColor.systemRedColor;
	if(!valid)
	{
		[_invalidFields addObject:key];
		_dirty = YES;
		_window.documentEdited = YES;
		return;
	}
	[_invalidFields removeObject:key];
	NSMutableDictionary* rule = [key hasPrefix:@"gutter."] ? _draft : [self currentRule];
	NSString* settingsKey = [key hasPrefix:@"gutter."] ? @"gutterSettings" : @"settings";
	NSString* valueKey = [key hasPrefix:@"gutter."] ? [key substringFromIndex:7] : key;
	NSMutableDictionary* settings = rule[settingsKey];
	if(!settings)
		rule[settingsKey] = settings = [NSMutableDictionary dictionary];
	if(value.length)
		settings[valueKey] = value;
	else
		[settings removeObjectForKey:valueKey];
	if(NSColor* color = theme_editor::parse_color(value, theme_editor::color_space(_draft)))
		_wells[key].color = color;
	[self changed:YES];
}

- (void)colorChanged:(NSColorWell*)well
{
	NSString* value = theme_editor::color_string(well.color, theme_editor::color_space(_draft));
	if(!value)
		return;
	_fields[well.identifier].stringValue = value;
	[self settingChanged:_fields[well.identifier]];
}

- (void)reloadFontControls
{
	NSFont* font = [NSFont fontWithName:_fields[@"fontName"].stringValue size:12];
	[_fontFamily selectItemAtIndex:0];
	if(font)
	{
		if([_fontFamily indexOfItemWithTitle:font.familyName] < 0)
			[_fontFamily addItemWithTitle:font.familyName];
		[_fontFamily selectItemWithTitle:font.familyName];
	}
	[_fontTypeface removeAllItems];
	[_fontTypeface addItemWithTitle:font ? @"Default" : @"Inherited"];
	_fontTypeface.enabled = font != nil;
	for(NSArray* member in (font ? [NSFontManager.sharedFontManager availableMembersOfFontFamily:font.familyName] : @[]))
	{
		NSMenuItem* item = [[NSMenuItem alloc] initWithTitle:member[1] action:nil keyEquivalent:@""];
		item.representedObject = member[0];
		[_fontTypeface.menu addItem:item];
		if([member[0] isEqualToString:font.fontName])
			[_fontTypeface selectItem:item];
	}
	NSArray* styles = [_fields[@"fontStyle"].stringValue componentsSeparatedByString:@" "];
	NSArray* keys = @[ @"bold", @"italic", @"underline", @"strikethrough" ];
	for(NSInteger i = 0; i < 4; ++i)
		[_fontStyles setSelected:[styles containsObject:keys[i]] forSegment:i];
	_fontStyles.toolTip = _fields[@"fontStyle"].stringValue.length ? @"Explicit formatting override" : @"Formatting inherited from other matching rules";
}

- (void)fontFamilyChanged:(id)sender
{
	NSString* name = @"";
	if(_fontFamily.indexOfSelectedItem > 0)
		name = [NSFontManager.sharedFontManager fontWithFamily:_fontFamily.titleOfSelectedItem traits:0 weight:5 size:12].fontName ?: @"";
	_fields[@"fontName"].stringValue = name;
	[self settingChanged:_fields[@"fontName"]];
	[self reloadFontControls];
}

- (void)fontTypefaceChanged:(id)sender
{
	NSFont* current = [NSFont fontWithName:_fields[@"fontName"].stringValue size:12];
	NSFont* regular = [NSFontManager.sharedFontManager fontWithFamily:current.familyName traits:0 weight:5 size:12];
	_fields[@"fontName"].stringValue = _fontTypeface.selectedItem.representedObject ?: regular.fontName ?: @"";
	[self settingChanged:_fields[@"fontName"]];
	[self reloadFontControls];
}

- (void)fontStylesChanged:(id)sender
{
	NSMutableArray* styles = [NSMutableArray array];
	NSArray* keys = @[ @"bold", @"italic", @"underline", @"strikethrough" ];
	for(NSInteger i = 0; i < 4; ++i)
		if([_fontStyles isSelectedForSegment:i])
			[styles addObject:keys[i]];
	_fields[@"fontStyle"].stringValue = styles.count ? [styles componentsJoinedByString:@" "] : @"plain";
	[self settingChanged:_fields[@"fontStyle"]];
	[self reloadFontControls];
}

- (void)resetFontStyle:(id)sender
{
	_fields[@"fontStyle"].stringValue = @"";
	[self settingChanged:_fields[@"fontStyle"]];
	[self reloadFontControls];
}

- (void)addRule:(id)sender
{
	if(_invalidFields.count) { NSBeep(); return; }
	NSAlert* alert = [self themeAlert];
	alert.messageText = @"Add a style rule";
	alert.informativeText = @"Enter a name and scope selector for the new rule (for example, string for strings or comment for comments).";
	[alert addButtonWithTitle:@"Add rule"];
	[alert addButtonWithTitle:@"Cancel"];
	NSTextField* name = [NSTextField textFieldWithString:@"New rule"];
	name.frame = NSMakeRect(0, 70, 300, 24);
	name.identifier = @"new-rule-name";
	name.accessibilityLabel = @"Rule name";
	NSTextField* scope = [NSTextField textFieldWithString:@""];
	scope.frame = NSMakeRect(0, 0, 300, 24);
	scope.identifier = @"new-rule-scope";
	scope.accessibilityLabel = @"Scope selector";
	scope.placeholderString = @"e.g. string.quoted or comment";
	NSView* form = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 300, 116)];
	NSTextField* nameLabel = [NSTextField labelWithString:@"Rule name"];
	nameLabel.frame = NSMakeRect(0, 96, 300, 20);
	NSTextField* scopeLabel = [NSTextField labelWithString:@"Scope selector"];
	scopeLabel.frame = NSMakeRect(0, 26, 300, 20);
	for(NSView* view in @[ nameLabel, name, scopeLabel, scope ])
		[form addSubview:view];
	name.nextKeyView = scope;
	scope.nextKeyView = name;
	alert.accessoryView = form;
	alert.window.initialFirstResponder = name;
	NSString* title;
	NSString* selector;
	while(YES)
	{
		[name selectText:nil];
		if([alert runModal] != NSAlertFirstButtonReturn)
			return;
		[alert.window makeFirstResponder:nil];
		title = [name.stringValue stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
		selector = [scope.stringValue stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
		if(title.length && selector.length)
			break;
		alert.informativeText = title.length ? @"Enter a scope selector before adding the rule (for example, string or comment)." : @"Enter a rule name before adding the rule.";
	}
	[_draft[@"settings"] addObject:[@{ @"name": title, @"scope": selector, @"settings": [NSMutableDictionary dictionary] } mutableCopy]];
	[self reloadRules:[_draft[@"settings"] count] - 1];
	[self changed:YES];
}

- (void)removeRule:(id)sender
{
	if(_invalidFields.count) { NSBeep(); return; }
	NSMutableArray* rules = _draft[@"settings"];
	if(rules.count <= 1 || ![[self currentRule][@"scope"] length])
		return;
	NSAlert* alert = [self themeAlert];
	alert.alertStyle = NSAlertStyleWarning;
	alert.messageText = [NSString stringWithFormat:@"Remove the “%@” rule?", _rules.titleOfSelectedItem];
	alert.informativeText = @"This removes the rule from the theme draft. Save the theme to apply the change.";
	[alert addButtonWithTitle:@"Cancel"];
	[alert addButtonWithTitle:@"Remove"];
	if([alert runModal] != NSAlertSecondButtonReturn)
		return;
	[rules removeObjectAtIndex:_rules.indexOfSelectedItem];
	[self reloadRules:0];
	[self changed:YES];
}

- (void)createThemeDuplicating:(BOOL)duplicate
{
	if(![self confirmDiscard])
		return;
	plist::dictionary_t value;
	if(duplicate && _source)
		value = _source->plist();
	else
		value = plist::dictionary_t{ { "settings", plist::array_t{ plist::dictionary_t{ { "settings", plist::dictionary_t{ { "foreground", "#202020" }, { "background", "#FFFFFF" }, { "caret", "#202020" }, { "selection", "#B5D5FF" } } } } } } };
	if(!duplicate)
		theme_editor::set_appearance(value, theme_editor::light);
	std::string sourceName = "Untitled Theme";
	plist::get_key_path(value, "name", sourceName);
	value["name"] = duplicate ? sourceName + " Copy" : "Untitled Theme";
	[self createThemeWithPropertyList:value];
}

- (void)createThemeWithPropertyList:(plist::dictionary_t)value
{
	oak::uuid_t uuid;
	uuid.generate();
	value["uuid"] = to_s(uuid);
	// Keep one bundle object even before any of its themes has been saved.
	if(!_themeEditorBundle)
	{
		oak::uuid_t const bundleUUID = "1C514B9A-BA40-4E6C-A79B-E169CE2AF4D2";
		_themeEditorBundle = bundles::lookup(bundleUUID);
		if(!_themeEditorBundle)
		{
			_themeEditorBundle = std::make_shared<bundles::item_t>(bundleUUID, bundles::item_ptr(), bundles::kItemTypeBundle);
			_themeEditorBundle->set_plist({ { "uuid", to_s(bundleUUID) }, { "name", "Theme Editor" } });
		}
	}
	auto item = std::make_shared<bundles::item_t>(uuid, _themeEditorBundle, bundles::kItemTypeTheme);
	item->set_plist(value);
	_themes.push_back(item);
	_dirty = NO;
	_source = item;
	[self loadSource];
	[self changed:YES];
}

- (void)reloadPalette
{
	[_paletteColors removeAllItems];
	id palette = _draft[@"themeEditorPalette"];
	if([palette isKindOfClass:NSArray.class])
	{
		for(id swatch in palette)
		{
			if(![swatch isKindOfClass:NSDictionary.class] || ![swatch[@"color"] isKindOfClass:NSString.class])
				continue;
			NSColor* color = theme_editor::parse_color(swatch[@"color"], NSColorSpace.sRGBColorSpace);
			if(!color) continue;
			NSString* name = [swatch[@"name"] isKindOfClass:NSString.class] ? swatch[@"name"] : @"Color";
			// Add menu items directly to retain duplicate swatch names.
			NSMenuItem* item = [[NSMenuItem alloc] initWithTitle:[NSString stringWithFormat:@"%@ — %@", name, swatch[@"color"]] action:nil keyEquivalent:@""];
			item.representedObject = swatch[@"color"];
			item.image = [NSImage imageWithSize:NSMakeSize(14, 14) flipped:NO drawingHandler:^BOOL(NSRect rect) {
				[color setFill];
				[[NSBezierPath bezierPathWithRoundedRect:NSInsetRect(rect, 1, 1) xRadius:3 yRadius:3] fill];
				return YES;
			}];
			[_paletteColors.menu addItem:item];
		}
	}
	_paletteControls.superview.superview.hidden = !_paletteColors.numberOfItems;
	_paletteHeading.hidden = !_paletteColors.numberOfItems;
	if(_paletteColors.numberOfItems) [_paletteColors selectItemAtIndex:0];
}

- (void)applyPaletteColor:(id)sender
{
	if(![_window makeFirstResponder:nil] || _invalidFields.count) { NSBeep(); return; }
	NSString* hex = _paletteColors.selectedItem.representedObject;
	NSColor* color = theme_editor::parse_color(hex, NSColorSpace.sRGBColorSpace);
	if(!color) return;
	NSArray* keys = @[ @"foreground", @"background", @"caret", @"selection", @"invisibles" ];
	NSTextField* field = _fields[keys[_paletteTarget.indexOfSelectedItem]];
	field.stringValue = theme_editor::color_string(color, theme_editor::color_space(_draft));
	[self settingChanged:field];
}

- (BOOL)importSwatchesFromURL:(NSURL*)url
{
	NSError* error = nil;
	NSData* data = [NSData dataWithContentsOfURL:url options:0 error:&error];
	NSDictionary* theme = nil;
	NSString* message = error.localizedDescription;
	if(data)
	{
		try
		{
			bool dark = _appearance.selectedSegment == theme_editor::dark;
			bool ase = data.length >= 4 && memcmp(data.bytes, "ASEF", 4) == 0;
			NSDictionary* palette = ase || [url.pathExtension.lowercaseString isEqual:@"ase"] ? theme_editor::read_ase(data) : theme_editor::read_dtcg(data, dark);
			NSString* name = url.lastPathComponent.stringByDeletingPathExtension;
			if([name.pathExtension.lowercaseString isEqual:@"tokens"]) name = name.stringByDeletingPathExtension;
			theme = theme_editor::theme_from_palette(palette, name, dark);
		}
		catch(std::exception const& exception) { message = to_ns(exception.what()); }
	}
	if(!theme)
	{
		NSAlert* alert = [self themeAlert];
		alert.messageText = @"The colors could not be imported.";
		alert.informativeText = message ?: @"Choose an Adobe Swatch Exchange (.ase) or DTCG (.tokens.json, .json, .tokens) file.";
		[alert runModal];
		return NO;
	}
	if(![self confirmDiscard]) return NO;
	[self createThemeWithPropertyList:plist::convert((__bridge CFPropertyListRef)theme)];
	return YES;
}

- (void)importSwatches:(id)sender
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];
	panel.title = @"Import Colors";
	panel.prompt = @"Import";
	panel.allowedContentTypes = @[ [UTType typeWithFilenameExtension:@"ase"] ?: UTTypeData, UTTypeJSON, [UTType typeWithFilenameExtension:@"tokens"] ?: UTTypeData ];
	panel.allowsMultipleSelection = NO;
	panel.canChooseDirectories = NO;
	if([panel runModal] == NSModalResponseOK)
		[self importSwatchesFromURL:panel.URL];
}

- (void)newTheme:(id)sender { [self createThemeDuplicating:NO]; }
- (void)duplicateTheme:(id)sender { [self createThemeDuplicating:YES]; }

- (NSMenu*)themeMenuForRow:(NSInteger)row
{
	if(row < 0 || row >= (NSInteger)_themeRows.size() || !_themeRows[row].item)
		return nil;
	NSMenu* menu = [NSMenu new];
	for(NSArray<NSString*>* entry in @[ @[ @"Duplicate", NSStringFromSelector(@selector(duplicateThemeFromMenu:)) ], @[ @"Delete", NSStringFromSelector(@selector(deleteThemeFromMenu:)) ], @[ @"Export…", NSStringFromSelector(@selector(exportThemeFromMenu:)) ] ])
	{
		NSMenuItem* item = [menu addItemWithTitle:entry[0] action:NSSelectorFromString(entry[1]) keyEquivalent:@""];
		item.target = self;
		item.representedObject = to_ns(to_s(_themeRows[row].item->uuid()));
	}
	return menu;
}

- (BOOL)selectThemeForMenuItem:(NSMenuItem*)item
{
	for(size_t row = 0; row < _themeRows.size(); ++row)
	{
		if(_themeRows[row].item && [to_ns(to_s(_themeRows[row].item->uuid())) isEqual:item.representedObject])
			return [self selectThemeAtRow:row];
	}
	return NO;
}

- (void)duplicateThemeFromMenu:(NSMenuItem*)item
{
	if([self selectThemeForMenuItem:item])
		[self duplicateTheme:item];
}

- (void)exportThemeFromMenu:(NSMenuItem*)item
{
	for(auto const& theme : _themes)
	{
		if([to_ns(to_s(theme->uuid())) isEqual:item.representedObject])
		{
			[self exportThemeWithPropertyList:ns::to_dictionary(theme->plist())];
			return;
		}
	}
}

- (NSDictionary*)themeForSerialization
{
	// The inspector's empty global placeholder is not part of the source theme.
	if(!_hasPlaceholderGlobalRule || [_draft[@"settings"][0][@"settings"] count] || [_draft[@"settings"][0][@"name"] length])
		return _draft;
	NSMutableDictionary* theme = [_draft mutableCopy];
	NSArray* rules = _draft[@"settings"];
	theme[@"settings"] = [rules subarrayWithRange:NSMakeRange(1, rules.count - 1)];
	return theme;
}

- (BOOL)writeThemeToURL:(NSURL*)url error:(NSError**)error
{
	return [self writeTheme:[self themeForSerialization] toURL:url error:error];
}

- (BOOL)writeTheme:(NSDictionary*)theme toURL:(NSURL*)url error:(NSError**)error
{
	NSMutableDictionary* exportedTheme = [theme mutableCopy];
	exportedTheme[@"uuid"] = NSUUID.UUID.UUIDString;
	NSData* data = [NSPropertyListSerialization dataWithPropertyList:exportedTheme format:NSPropertyListXMLFormat_v1_0 options:0 error:error];
	return data && [data writeToURL:url options:NSDataWritingAtomic error:error];
}

- (void)exportTheme:(id)sender
{
	if(!_source || ![_window makeFirstResponder:nil])
		return;
	NSString* name = [[_draft[@"name"] description] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
	if(!name.length || _invalidFields.count)
	{
		NSAlert* alert = [self themeAlert];
		alert.messageText = !name.length ? @"Enter a theme name before exporting." : @"Correct the highlighted setting before exporting.";
		[alert runModal];
		return;
	}
	[self exportThemeWithPropertyList:[self themeForSerialization]];
}

- (void)exportThemeWithPropertyList:(NSDictionary*)theme
{
	NSString* name = [theme[@"name"] isKindOfClass:NSString.class] ? [theme[@"name"] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet] : nil;
	if(!name.length)
		name = @"Untitled Theme";
	NSSavePanel* panel = [NSSavePanel savePanel];
	panel.title = @"Export Theme";
	panel.allowedContentTypes = @[ [UTType typeWithFilenameExtension:@"tmTheme"] ];
	panel.canCreateDirectories = YES;
	panel.nameFieldStringValue = [[name stringByReplacingOccurrencesOfString:@"/" withString:@":"] stringByAppendingPathExtension:@"tmTheme"];
	if([panel runModal] != NSModalResponseOK)
		return;
	NSError* error = nil;
	if(![self writeTheme:theme toURL:panel.URL error:&error])
	{
		NSAlert* alert = [self themeAlert];
		alert.messageText = @"The theme could not be exported.";
		alert.informativeText = error.localizedDescription ?: @"Choose another location and try again.";
		[alert runModal];
	}
}

- (void)deleteThemeFromMenu:(NSMenuItem*)item
{
	if(![self selectThemeForMenuItem:item])
		return;
	[self deleteTheme:item];
}

- (void)deleteTheme:(id)sender
{
	if(!_source)
		return;
	[_window makeFirstResponder:nil];
	NSAlert* alert = [self themeAlert];
	alert.alertStyle = NSAlertStyleWarning;
	alert.messageText = [NSString stringWithFormat:@"Delete the theme “%@”?", _draft[@"name"] ?: @"untitled"];
	alert.informativeText = @"This removes the theme from TextMate. Any unsaved changes to this theme will be discarded.";
	[alert addButtonWithTitle:@"Cancel"];
	[alert addButtonWithTitle:@"Delete"];
	if([alert runModal] != NSAlertSecondButtonReturn)
		return;
	auto deleted = _source;
	auto original = deleted->plist();
	if(!deleted->move_to_trash())
	{
		deleted->set_plist(original);
		NSAlert* failure = [self themeAlert];
		failure.messageText = @"The theme could not be deleted.";
		failure.informativeText = @"Check that your TextMate bundle folder is writable. The theme remains selected.";
		[failure runModal];
		return;
	}
	bundles::remove_item(deleted);
	[self removeCurrentThemeFromList];
}

- (void)removeCurrentThemeFromList
{
	bundles::item_ptr nextTheme;
	auto row = std::find_if(_themeRows.begin(), _themeRows.end(), [&](auto const& row) { return row.item == _source; });
	if(row != _themeRows.end())
	{
		// Follow the displayed groups, skipping headings; at the end, go back.
		for(auto next = std::next(row); next != _themeRows.end(); ++next)
		{
			if(next->item)
			{
				nextTheme = next->item;
				break;
			}
		}
		while(!nextTheme && row != _themeRows.begin())
		{
			--row;
			nextTheme = row->item;
		}
	}
	auto position = std::find(_themes.begin(), _themes.end(), _source);
	if(position != _themes.end())
		_themes.erase(position);
	_source.reset();
	_draft = nil;
	_dirty = NO;
	if(_themes.empty())
		[self createThemeDuplicating:NO];
	else
	{
		_source = nextTheme ? nextTheme : _themes.front();
		[self loadSource];
	}
}

- (BOOL)saveCurrentTheme
{
	if(!_source)
		return NO;
	[_window makeFirstResponder:nil];
	if(![[_draft[@"name"] description] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet].length)
	{
		NSAlert* alert = [self themeAlert];
		alert.messageText = @"Enter a theme name before saving.";
		[alert runModal];
		return NO;
	}
	for(NSString* key in _fields)
	{
		if([_invalidFields containsObject:key])
		{
			NSAlert* alert = [self themeAlert];
			alert.messageText = @"Correct the highlighted setting before saving.";
			[alert runModal];
			return NO;
		}
	}
	for(NSString* path in _originalFiles)
	{
		if(![[_originalFiles objectForKey:path] isEqual:[NSData dataWithContentsOfFile:path]])
		{
			NSAlert* alert = [self themeAlert];
			alert.messageText = @"This theme changed on disk.";
			alert.informativeText = @"Your draft has been kept. Reopen Theme Editor to load the latest version before saving.";
			[alert runModal];
			return NO;
		}
	}
	// Detect local overrides created by another TextMate process after launch.
	auto current = theme_editor::load_bundle_index();
	for(auto const& item : current.first)
	{
		if(item->uuid() == _source->uuid() && bundles::erase_false_values(item->plist()) != bundles::erase_false_values(_source->plist()))
		{
			NSAlert* alert = [self themeAlert];
			alert.messageText = @"This theme changed in another editor.";
			alert.informativeText = @"Your draft has been kept. Reopen Theme Editor to load the latest version.";
			[alert runModal];
			return NO;
		}
	}
	NSInteger const selectedRule = _rules.indexOfSelectedItem;
	auto original = _source->plist();
	_source->set_plist(plist::convert((__bridge CFPropertyListRef)[self themeForSerialization]));
	if(!_source->save())
	{
		_source->set_plist(original);
		NSAlert* alert = [self themeAlert];
		alert.messageText = @"The theme could not be saved.";
		alert.informativeText = @"Check that your TextMate bundle folder is writable. Your draft has been kept.";
		[alert runModal];
		return NO;
	}
	if(_newTheme)
	{
		if(!bundles::lookup(_source->bundle_uuid()))
			bundles::add_item(_source->bundle());
		bundles::add_item(_source);
	}
	// One guarded refresh retains the theme by UUID and the current style rule.
	[self loadSourceSelectingRule:selectedRule];
	return YES;
}

- (void)saveTheme:(id)sender { [_window makeFirstResponder:nil]; [self saveCurrentTheme]; }
- (BOOL)windowShouldClose:(NSWindow*)sender { if(![self confirmDiscard]) return NO; [NSApp terminate:nil]; return NO; }
- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender { return [self confirmDiscard] ? NSTerminateNow : NSTerminateCancel; }
@end

#ifndef THEME_EDITOR_TESTING
int main (int argc, char const* argv[])
{
	@autoreleasepool
	{
		oak::application_t application(argc, argv);
		oak::application_t::set_name("TextMate");
		// Resolve symlinks so invocation through PATH still finds the containing app.
		uint32_t length = 0;
		_NSGetExecutablePath(nullptr, &length);
		std::vector<char> executablePath(length);
		_NSGetExecutablePath(executablePath.data(), &length);
		NSString* executable = to_ns(path::resolve(executablePath.data()));
		NSBundle* bundle = [NSBundle bundleWithPath:executable.stringByDeletingLastPathComponent.stringByDeletingLastPathComponent.stringByDeletingLastPathComponent];
		NSBundle* containingBundle = [NSBundle bundleWithPath:bundle.bundlePath.stringByDeletingLastPathComponent.stringByDeletingLastPathComponent.stringByDeletingLastPathComponent];
		if([containingBundle.bundleIdentifier hasPrefix:@"com.macromates.TextMate"])
			bundle = containingBundle;
		if(bundle.bundleIdentifier)
			oak::application_t::set_path(to_s(bundle.bundlePath));
		else
			bundle = NSBundle.mainBundle;
		if(bundle.bundleIdentifier && ![bundle.bundleIdentifier isEqualToString:NSBundle.mainBundle.bundleIdentifier])
			[NSUserDefaults.standardUserDefaults addSuiteNamed:bundle.bundleIdentifier];
		RegisterDefaults();
		settings_t::set_global_settings_path(oak::application_t::support("Global.tmProperties"));
		if(NSString* defaults = [bundle pathForResource:@"Default" ofType:@"tmProperties"])
			settings_t::set_default_settings_path(to_s(defaults));
		[NSApplication sharedApplication];
		[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
		NSColorPanel.sharedColorPanel.showsAlpha = YES;
		ThemeEditorController* controller = [ThemeEditorController new];
		NSApp.delegate = controller;
		[NSApp run];
	}
	return 0;
}
#endif
