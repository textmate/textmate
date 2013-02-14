#import "OTVStatusBar.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
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
	[res setFont:[NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]]];
	[res setStringValue:label];
	[res setAlignment:NSRightTextAlignment];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	return res;
}

static NSPopUpButton* OakCreatePopUpButton (NSString* initialItem = nil)
{
	NSPopUpButton* res = [NSPopUpButton new];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setBordered:NO];
	[res setFont:[NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]]];

	if(initialItem)
		[[res cell] setMenuItem:[[NSMenuItem alloc] initWithTitle:initialItem action:@selector(nop:) keyEquivalent:@""]];

	return res;
}

static NSButton* OakCreateImageButton (NSImage* image)
{
	NSButton* res = [NSButton new];

	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBezelStyle:NSRecessedBezelStyle];
	[res setBordered:NO];

	[res setImage:image];
	[res setImagePosition:NSImageOnly];

	return res;
}

static NSImageView* OakCreateImageView (NSImage* image)
{
	NSImageView* res = [[NSImageView alloc] initWithFrame:NSZeroRect];
	[res setImage:image];
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
		self.grammarPopUp                 = OakCreatePopUpButton(@"");
		self.tabSizePopUp                 = OakCreatePopUpButton();
		self.tabSizePopUp.pullsDown       = YES;
		self.bundleItemsPopUp             = OakCreatePopUpButton();
		self.symbolPopUp                  = OakCreatePopUpButton(@"");
		self.macroRecordingButton         = OakCreateImageButton([NSImage imageNamed:@"Recording" inSameBundleAsClass:[self class]]);
		self.macroRecordingButton.action  = @selector(toggleMacroRecording:);
		self.macroRecordingButton.toolTip = @"Click to start recording a macro";

		// ===========================
		// = Wrap/Clip Bundles PopUp =
		// ===========================

		NSMenuItem* item = [[NSMenuItem alloc] initWithTitle:@"" action:@selector(nop:) keyEquivalent:@""];
		item.image = [NSImage imageNamed:NSImageNameActionTemplate];
		[[self.bundleItemsPopUp cell] setUsesItemFromMenu:NO];
		[[self.bundleItemsPopUp cell] setMenuItem:item];

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

		NSImage* dividerImage = [NSImage imageNamed:@"Divider" inSameBundleAsClass:[self class]];
		NSDictionary* views = @{
			@"line"         : OakCreateTextField(@"Line:"),
			@"selection"    : self.selectionField,
			@"dividerOne"   : OakCreateImageView(dividerImage),
			@"grammar"      : self.grammarPopUp,
			@"dividerTwo"   : OakCreateImageView(dividerImage),
			@"items"        : wrappedBundleItemsPopUpButton,
			@"dividerThree" : OakCreateImageView(dividerImage),
			@"tabSize"      : self.tabSizePopUp,
			@"dividerFour"  : OakCreateImageView(dividerImage),
			@"symbol"       : self.symbolPopUp,
			@"dividerFive"  : OakCreateImageView(dividerImage),
			@"recording"    : self.macroRecordingButton,
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self.symbolPopUp setContentHuggingPriority:NSLayoutPriorityDefaultLow-1 forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self.selectionField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[[self.selectionField cell] setLineBreakMode:NSLineBreakByTruncatingTail];
		
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-16-[line]-[selection(>=32,<=225)]-8-[dividerOne(==1)]-(-2)-[grammar(==115)]-5-[dividerTwo(==1)]-(-2)-[tabSize]-4-[dividerThree(==1)]-5-[items(==30)]-4-[dividerFour(==1)]-(-2)-[symbol(>=125)]-5-[dividerFive(==1)]-6-[recording]-14-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[line]-5-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[selection]-5-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[grammar(==dividerOne,==items,==tabSize,==symbol,==recording)]|" options:0 metrics:nil views:views]];

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
		grammars.insert(std::make_pair(item->name(), item));

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
	self.selectionField.stringValue = newSelectionString;
}

- (void)setGrammarName:(NSString*)newGrammarName
{
	if(_grammarName == newGrammarName || [_grammarName isEqualToString:newGrammarName])
		return;
	_grammarName = newGrammarName;
	self.grammarPopUp.title = newGrammarName ?: @"(no grammar)";
}

- (void)setSymbolName:(NSString*)newSymbolName
{
	if(_symbolName == newSymbolName || [_symbolName isEqualToString:newSymbolName])
		return;
	_symbolName = newSymbolName;
	self.symbolPopUp.title = newSymbolName ?: @"Symbols";;
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
