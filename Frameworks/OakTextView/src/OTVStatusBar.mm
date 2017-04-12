#import "OTVStatusBar.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSString Additions.h>
#import <bundles/bundles.h>
#import <text/ctype.h>
#import <ns/ns.h>

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

static NSButton* OakCreateImageToggleButton (NSImage* image, NSObject* accessibilityLabel)
{
	NSButton* res = [NSButton new];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSToggleButton];
	[res setBordered:NO];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];
	OakSetAccessibilityLabel(res, accessibilityLabel);
	return res;
}

static NSMenuItem* OakCreateIndentMenuItem (NSString* title, SEL action, id target)
{
	NSMenuItem* res = [[NSMenuItem alloc] initWithTitle:title action:action keyEquivalent:@""];
	[res setTarget:target];
	[res setIndentationLevel:1];
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
	if(self = [super initWithFrame:aRect])
	{
		[self setupStatusBarBackground];

		self.selectionField               = OakCreateTextField(@"1:1");
		self.grammarPopUp                 = OakCreateStatusBarPopUpButton(@"", @"Grammar");
		self.tabSizePopUp                 = OakCreateStatusBarPopUpButton();
		self.tabSizePopUp.pullsDown       = YES;
		self.bundleItemsPopUp             = OakCreateStatusBarPopUpButton(nil, @"Bundle Item");
		self.symbolPopUp                  = OakCreateStatusBarPopUpButton(@"", @"Symbol");
		self.macroRecordingButton         = OakCreateImageToggleButton([NSImage imageNamed:@"Recording" inSameBundleAsClass:[self class]], @"Record a macro");
		self.macroRecordingButton.action  = @selector(toggleMacroRecording:);
		self.macroRecordingButton.toolTip = @"Click to start recording a macro";

		[self setupTabSizeMenu:self];

		// ===========================
		// = Wrap/Clip Bundles PopUp =
		// ===========================

		NSMenuItem* item = [[NSMenuItem alloc] initWithTitle:@"" action:NULL keyEquivalent:@""];
		item.image = [NSImage imageNamed:NSImageNameActionTemplate];
		[[self.bundleItemsPopUp cell] setUsesItemFromMenu:NO];
		[[self.bundleItemsPopUp cell] setMenuItem:item];

		NSView* wrappedBundleItemsPopUpButton = [NSView new];
		OakAddAutoLayoutViewsToSuperview(@[ self.bundleItemsPopUp ], wrappedBundleItemsPopUpButton);
		[wrappedBundleItemsPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[popup]|" options:0 metrics:nil views:@{ @"popup" : self.bundleItemsPopUp }]];
		[wrappedBundleItemsPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[popup]|" options:0 metrics:nil views:@{ @"popup" : self.bundleItemsPopUp }]];

		NSTextField* line    = OakCreateTextField(@"Line:");

		NSView* dividerOne   = OakCreateDividerImageView();
		NSView* dividerTwo   = OakCreateDividerImageView();
		NSView* dividerThree = OakCreateDividerImageView();
		NSView* dividerFour  = OakCreateDividerImageView();
		NSView* dividerFive  = OakCreateDividerImageView();

		NSDictionary* views = @{
			@"line"         : line,
			@"selection"    : self.selectionField,
			@"dividerOne"   : dividerOne,
			@"grammar"      : self.grammarPopUp,
			@"dividerTwo"   : dividerTwo,
			@"items"        : wrappedBundleItemsPopUpButton,
			@"dividerThree" : dividerThree,
			@"tabSize"      : self.tabSizePopUp,
			@"dividerFour"  : dividerFour,
			@"symbol"       : self.symbolPopUp,
			@"dividerFive"  : dividerFive,
			@"recording"    : self.macroRecordingButton,
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], self);
		OakSetupKeyViewLoop(@[ self, _grammarPopUp, _tabSizePopUp, _bundleItemsPopUp, _symbolPopUp, _macroRecordingButton ], NO);

		[self.selectionField setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self.selectionField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow+2 forOrientation:NSLayoutConstraintOrientationHorizontal];
		[[self.selectionField cell] setLineBreakMode:NSLineBreakByTruncatingTail];

		[self.grammarPopUp setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow+1 forOrientation:NSLayoutConstraintOrientationHorizontal];

		[self.symbolPopUp setContentHuggingPriority:NSLayoutPriorityDefaultLow-1 forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self.symbolPopUp setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow-1 forOrientation:NSLayoutConstraintOrientationHorizontal];

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-10-[line]-[selection(>=50,<=225)]-8-[dividerOne]-(-2)-[grammar(>=125@400,>=50,<=225)]-5-[dividerTwo]-(-2)-[tabSize(<=102)]-4-[dividerThree]-5-[items(==30)]-4-[dividerFour]-(-2)-[symbol(>=125@450,>=50)]-5-[dividerFive]-6-[recording]-7-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[dividerOne(==dividerTwo,==dividerThree,==dividerFour,==dividerFive)]|" options:0 metrics:nil views:views]];

		[self addConstraint:[NSLayoutConstraint constraintWithItem:dividerTwo   attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeTop multiplier:1.0 constant:0.0]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:dividerThree attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeTop multiplier:1.0 constant:0.0]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:dividerFour  attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeTop multiplier:1.0 constant:0.0]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:dividerFive  attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeTop multiplier:1.0 constant:0.0]];

		// Baseline align text-controls
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[line]-[selection]-(>=1)-[grammar]-(>=1)-[tabSize]-(>=1)-[symbol]" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		// Center non-text control
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[selection]-(>=1)-[dividerOne]-(>=1)-[items]-(>=1)-[recording]" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(grammarPopUpButtonWillPopUp:) name:NSPopUpButtonWillPopUpNotification object:self.grammarPopUp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(bundleItemsPopUpButtonWillPopUp:) name:NSPopUpButtonWillPopUpNotification object:self.bundleItemsPopUp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(symbolPopUpButtonWillPopUp:) name:NSPopUpButtonWillPopUpNotification object:self.symbolPopUp];
	}
	return self;
}

- (void)setupTabSizeMenu:(id)sender
{
	NSMenu* tabSizeMenu = self.tabSizePopUp.menu;
	[tabSizeMenu removeAllItems];
	[tabSizeMenu addItemWithTitle:@"Current Indent" action:NULL keyEquivalent:@""];
	[tabSizeMenu addItemWithTitle:@"Indent Size" action:@selector(nop:) keyEquivalent:@""];
	for(auto size : { 2, 3, 4, 8 })
	{
		NSMenuItem* item = OakCreateIndentMenuItem([NSString stringWithFormat:@"%d", size], @selector(takeTabSizeFrom:), self.target);
		[item setTag:size];
		[tabSizeMenu addItem:item];
	}
	[tabSizeMenu addItem:OakCreateIndentMenuItem(@"Other…", @selector(showTabSizeSelectorPanel:), self.target)];
	[tabSizeMenu addItem:[NSMenuItem separatorItem]];
	[[tabSizeMenu addItemWithTitle:@"Indent Using" action:@selector(nop:) keyEquivalent:@""] setTarget:self.target];
	[tabSizeMenu addItem:OakCreateIndentMenuItem(@"Tabs", @selector(setIndentWithTabs:), self.target)];
	[tabSizeMenu addItem:OakCreateIndentMenuItem(@"Spaces", @selector(setIndentWithSpaces:), self.target)];
}

- (void)setTarget:(id)newTarget
{
	_target = newTarget;
	[self setupTabSizeMenu:self];
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
	{
		if(item->value_for_field(bundles::kFieldGrammarScope) != NULL_STR)
			grammars.emplace(item->name(), item);
	}

	for(auto pair : grammars)
	{
		if(!pair.second->hidden_from_user())
		{
			NSMenuItem* item = [grammarMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:@selector(takeGrammarUUIDFrom:) keyEquivalent:@""];
			[item setKeyEquivalentCxxString:key_equivalent(pair.second)];
			[item setRepresentedObject:[NSString stringWithCxxString:pair.second->uuid()]];
			[item setTarget:self.target];
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

- (void)setFileType:(NSString*)newFileType
{
	if(_fileType == newFileType)
		return;

	_fileType = newFileType;
	for(auto const& item : bundles::query(bundles::kFieldGrammarScope, to_s(newFileType)))
		self.grammarName = [NSString stringWithCxxString:item->name()];
}

- (void)setRecordingTimer:(NSTimer*)aTimer
{
	if(_recordingTimer != aTimer)
	{
		[_recordingTimer invalidate];
		_recordingTimer = aTimer;
	}
}

- (void)setRecordingMacro:(BOOL)flag
{
	_recordingMacro = flag;
	if(_recordingMacro)
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
	self.tabSizePopUp.title = [NSString stringWithFormat:@"%@:\u2003%lu", _softTabs ? @"Soft Tabs" : @"Tab Size", _tabSize];
}

- (void)setTabSize:(NSUInteger)size
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
