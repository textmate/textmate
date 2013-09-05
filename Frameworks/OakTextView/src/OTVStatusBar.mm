#import "OTVStatusBar.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSString Additions.h>
#import <bundles/bundles.h>
#import <text/ctype.h>

static NSTextField* OakCreateTextField (NSString* label)
{
	NSTextField* res = [[NSTextField alloc] initWithFrame:NSZeroRect];
	[res setBordered:NO];
	[res setEditable:NO];
	[res setSelectable:NO];
	[res setBezeled:NO];
	[res setDrawsBackground:NO];
	[res setFont:OakStatusBarFont()];
	[res setStringValue:label];
	[res setAlignment:NSRightTextAlignment];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[[res cell] setLineBreakMode:NSLineBreakByTruncatingMiddle];
	return res;
}

static NSButton* OakCreateImageToggleButton (NSImage* image)
{
	NSButton* res = [NSButton new];

	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSToggleButton];
	[res setBezelStyle:NSRecessedBezelStyle];
	[res setBordered:NO];

	[res setImage:image];
	[res setImagePosition:NSImageOnly];

	return res;
}

@interface OTVStatusBar () <NSMenuDelegate>
@property (nonatomic) CGFloat recordingTime;
@property (nonatomic) NSTimer* recordingTimer;

@property (nonatomic) NSTextField*   selectionField;
@property (nonatomic) NSPopUpButton* grammarPopUp;
@property (nonatomic) NSPopUpButton* tabSizePopUp;
@property (nonatomic) NSPopUpButton* bundleItemsPopUp;
@property (nonatomic) NSPopUpButton* symbolPopUp;
@property (nonatomic) NSButton*      macroRecordingButton;
@end

@implementation OTVStatusBar
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithGradient:[[NSGradient alloc] initWithColorsAndLocations: [NSColor colorWithCalibratedWhite:1.000 alpha:0.68], 0.0, [NSColor colorWithCalibratedWhite:1.000 alpha:0.5], 0.0416, [NSColor colorWithCalibratedWhite:1.000 alpha:0.0], 1.0, nil] inactiveGradient:[[NSGradient alloc] initWithColorsAndLocations: [NSColor colorWithCalibratedWhite:1.000 alpha:0.68], 0.0, [NSColor colorWithCalibratedWhite:1.000 alpha:0.5], 0.0416, [NSColor colorWithCalibratedWhite:1.000 alpha:0.0], 1.0, nil]])
	{
		self.selectionField               = OakCreateTextField(@"1:1");
		self.grammarPopUp                 = OakCreateStatusBarPopUpButton(@"");
		self.tabSizePopUp                 = OakCreateStatusBarPopUpButton();
		self.tabSizePopUp.pullsDown       = YES;
		self.bundleItemsPopUp             = OakCreateStatusBarPopUpButton();
		self.symbolPopUp                  = OakCreateStatusBarPopUpButton(@"");
		self.macroRecordingButton         = OakCreateImageToggleButton([NSImage imageNamed:@"Recording" inSameBundleAsClass:[self class]]);
		self.macroRecordingButton.action  = @selector(toggleMacroRecording:);
		self.macroRecordingButton.toolTip = @"Click to start recording a macro";

		[self.grammarPopUp.cell         accessibilitySetOverrideValue:@"Grammar"        forAttribute:NSAccessibilityDescriptionAttribute];
		[self.symbolPopUp.cell          accessibilitySetOverrideValue:@"Symbol"         forAttribute:NSAccessibilityDescriptionAttribute];
		[self.macroRecordingButton.cell accessibilitySetOverrideValue:@"Record a macro" forAttribute:NSAccessibilityDescriptionAttribute];

		// ===========================
		// = Wrap/Clip Bundles PopUp =
		// ===========================

		NSMenuItem* item = [[NSMenuItem alloc] initWithTitle:@"" action:@selector(nop:) keyEquivalent:@""];
		item.image = [NSImage imageNamed:NSImageNameActionTemplate];
		[[self.bundleItemsPopUp cell] setUsesItemFromMenu:NO];
		[[self.bundleItemsPopUp cell] setMenuItem:item];
		[[self.bundleItemsPopUp cell] accessibilitySetOverrideValue:@"Bundle Item" forAttribute:NSAccessibilityDescriptionAttribute];

		NSView* wrappedBundleItemsPopUpButton = [NSView new];
		[wrappedBundleItemsPopUpButton addSubview:self.bundleItemsPopUp];
		[self.bundleItemsPopUp setTranslatesAutoresizingMaskIntoConstraints:NO];
		[wrappedBundleItemsPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[popup]|" options:0 metrics:nil views:@{ @"popup" : self.bundleItemsPopUp }]];
		[wrappedBundleItemsPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[popup]|" options:0 metrics:nil views:@{ @"popup" : self.bundleItemsPopUp }]];

		// =======================
		// = Setup Tab Size Menu =
		// =======================

		NSMenu* tabSizeMenu = self.tabSizePopUp.menu;
		[tabSizeMenu removeAllItems];
		[tabSizeMenu addItemWithTitle:@"Current Indent" action:@selector(nop:) keyEquivalent:@""];
		[tabSizeMenu addItemWithTitle:@"Indent Size" action:@selector(nop:) keyEquivalent:@""];
		for(auto size : { 2, 3, 4, 8 })
			[[tabSizeMenu addItemWithTitle:[NSString stringWithFormat:@"\u2003%d", size] action:@selector(takeTabSizeFrom:) keyEquivalent:@""] setTag:size];
		[tabSizeMenu addItemWithTitle:@"\u2003Other…" action:@selector(showTabSizeSelectorPanel:) keyEquivalent:@""];
		[tabSizeMenu addItem:[NSMenuItem separatorItem]];
		[tabSizeMenu addItemWithTitle:@"Indent Using" action:@selector(nop:) keyEquivalent:@""];
		[tabSizeMenu addItemWithTitle:@"\u2003Tabs" action:@selector(setIndentWithTabs:) keyEquivalent:@""];
		[tabSizeMenu addItemWithTitle:@"\u2003Spaces" action:@selector(setIndentWithSpaces:) keyEquivalent:@""];

		// =======================

		NSDictionary* views = @{
			@"line"         : OakCreateTextField(@"Line:"),
			@"selection"    : self.selectionField,
			@"dividerOne"   : OakCreateDividerImageView(),
			@"grammar"      : self.grammarPopUp,
			@"dividerTwo"   : OakCreateDividerImageView(),
			@"items"        : wrappedBundleItemsPopUpButton,
			@"dividerThree" : OakCreateDividerImageView(),
			@"tabSize"      : self.tabSizePopUp,
			@"dividerFour"  : OakCreateDividerImageView(),
			@"symbol"       : self.symbolPopUp,
			@"dividerFive"  : OakCreateDividerImageView(),
			@"recording"    : self.macroRecordingButton,
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self.selectionField setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self.selectionField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow+2 forOrientation:NSLayoutConstraintOrientationHorizontal];
		[[self.selectionField cell] setLineBreakMode:NSLineBreakByTruncatingTail];

		[self.grammarPopUp setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow+1 forOrientation:NSLayoutConstraintOrientationHorizontal];

		[self.symbolPopUp setContentHuggingPriority:NSLayoutPriorityDefaultLow-1 forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self.symbolPopUp setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow-1 forOrientation:NSLayoutConstraintOrientationHorizontal];
		
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-10-[line]-[selection(>=50,<=225)]-8-[dividerOne]-(-2)-[grammar(>=125,<=225)]-5-[dividerTwo]-(-2)-[tabSize(<=102)]-4-[dividerThree]-5-[items(==30)]-4-[dividerFour]-(-2)-[symbol(>=125)]-5-[dividerFive]-6-[recording]-7-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[line]-5-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[selection]-5-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[grammar(==dividerOne,==dividerTwo,==dividerThree,==dividerFour,==dividerFive,==items,==tabSize,==symbol,==recording)]|" options:0 metrics:nil views:views]];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(grammarPopUpButtonWillPopUp:) name:NSPopUpButtonWillPopUpNotification object:self.grammarPopUp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(bundleItemsPopUpButtonWillPopUp:) name:NSPopUpButtonWillPopUpNotification object:self.bundleItemsPopUp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(symbolPopUpButtonWillPopUp:) name:NSPopUpButtonWillPopUpNotification object:self.symbolPopUp];
	}
	return self;
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, 24);
}

- (void)drawRect:(NSRect)aRect
{
	if([self.window contentBorderThicknessForEdge:NSMinYEdge] < NSMaxY(self.frame))
	{
		[[NSColor windowBackgroundColor] set];
		NSRectFill(aRect);
		[super drawRect:aRect];
	}
}

- (void)updateMacroRecordingAnimation:(NSTimer*)aTimer
{
	CGFloat fraction = oak::cap(0.00, 0.70 + 0.30 * cos(M_PI + _recordingTime), 1.0);
	self.macroRecordingButton.alphaValue = fraction;
	_recordingTime += 0.075;
}

- (void)grammarPopUpButtonWillPopUp:(NSNotification*)aNotification
{
	NSMenu* grammarMenu = self.grammarPopUp.menu;
	[grammarMenu removeAllItems];

	std::multimap<std::string, bundles::item_ptr, text::less_t> grammars;
	for(auto item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
		grammars.emplace(item->name(), item);

	for(auto pair : grammars)
	{
		if(!pair.second->hidden_from_user())
		{
			NSMenuItem* item = [grammarMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:@selector(takeGrammarUUIDFrom:) keyEquivalent:@""];
			[item setKeyEquivalentCxxString:key_equivalent(pair.second)];
			[item setRepresentedObject:[NSString stringWithCxxString:pair.second->uuid()]];
		}
	}

	if(grammars.empty())
		[grammarMenu addItemWithTitle:@"No Grammars Loaded" action:@selector(nop:) keyEquivalent:@""];

	[grammarMenu update];

	for(NSMenuItem* item in grammarMenu.itemArray)
	{
		if([item state] == NSOnState)
			[self.grammarPopUp selectItem:item];
	}
}

- (void)bundleItemsPopUpButtonWillPopUp:(NSNotification*)aNotification
{
	[self.delegate showBundleItemSelector:self.bundleItemsPopUp];
}

- (void)symbolPopUpButtonWillPopUp:(NSNotification*)aNotification
{
	[self.delegate showSymbolSelector:self.symbolPopUp];
}

// ===========
// = Actions =
// ===========

- (void)showBundlesMenu:(id)sender
{
	[self.bundleItemsPopUp performClick:self];
}

// ==============
// = Properties =
// ==============

- (void)setSelectionString:(NSString*)newSelectionString
{
	if(_selectionString == newSelectionString || [_selectionString isEqualToString:newSelectionString])
		return;
	_selectionString = newSelectionString;

	newSelectionString = [newSelectionString stringByReplacingOccurrencesOfString:@"&" withString:@", "];
	newSelectionString = [newSelectionString stringByReplacingOccurrencesOfString:@"x" withString:@"×"];
	self.selectionField.stringValue = newSelectionString;
}

- (void)setGrammarName:(NSString*)newGrammarName
{
	if(_grammarName == newGrammarName || [_grammarName isEqualToString:newGrammarName])
		return;
	_grammarName = newGrammarName;
	[self.grammarPopUp.menu removeAllItems];
	[self.grammarPopUp addItemWithTitle:newGrammarName ?: @"(no grammar)"];
}

- (void)setSymbolName:(NSString*)newSymbolName
{
	if(_symbolName == newSymbolName || [_symbolName isEqualToString:newSymbolName])
		return;
	_symbolName = newSymbolName;
	[self.symbolPopUp.menu removeAllItems];
	[self.symbolPopUp addItemWithTitle:newSymbolName ?: @"Symbols"];
}

- (void)setRecordingTimer:(NSTimer*)aTimer
{
	if(_recordingTimer != aTimer)
	{
		[_recordingTimer invalidate];
		_recordingTimer = aTimer;
	}
}

- (void)setIsMacroRecording:(BOOL)flag
{
	_isMacroRecording = flag;
	if(_isMacroRecording)
	{
		self.recordingTimer = [NSTimer scheduledTimerWithTimeInterval:0.02 target:self selector:@selector(updateMacroRecordingAnimation:) userInfo:nil repeats:YES];
	}
	else
	{
		self.recordingTimer = nil;
		_recordingTime = 0;
		[self updateMacroRecordingAnimation:nil];
	}
}

- (void)updateTabSettings
{
	self.tabSizePopUp.title = [NSString stringWithFormat:@"%@:\u2003%d", _softTabs ? @"Soft Tabs" : @"Tab Size", _tabSize];
}

- (void)setTabSize:(int32_t)size
{
	_tabSize = size;
	[self updateTabSettings];
}

- (void)setSoftTabs:(BOOL)flag
{
	_softTabs = flag;
	[self updateTabSettings];
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	self.recordingTimer = nil;
}
@end
