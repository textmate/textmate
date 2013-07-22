#import "FindWindowController.h"
#import "FFFolderMenu.h"
#import "Strings.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakPasteboardSelector.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakHistoryList.h>
#import <OakFoundation/OakFoundation.h>
#import <Preferences/Keys.h>
#import <ns/ns.h>
#import <io/path.h>
#import <regexp/regexp.h>

NSString* const kUserDefaultsFolderOptionsKey     = @"Folder Search Options";
NSString* const kUserDefaultsFindResultsHeightKey = @"findResultsHeight";

@interface OakAutoSizingTextField : NSTextField
@property (nonatomic) NSSize myIntrinsicContentSize;
@end

@implementation OakAutoSizingTextField
- (NSSize)intrinsicContentSize
{
	return NSEqualSizes(self.myIntrinsicContentSize, NSZeroSize) ? [super intrinsicContentSize] : self.myIntrinsicContentSize;
}

- (void)updateIntrinsicContentSizeToEncompassString:(NSString*)aString
{
	NSTextFieldCell* cell = [self.cell copy];
	cell.stringValue = aString;

	self.myIntrinsicContentSize = NSMakeSize(NSViewNoInstrinsicMetric, MAX(22, MIN([cell cellSizeForBounds:NSMakeRect(0, 0, NSWidth([self bounds]), CGFLOAT_MAX)].height, 225)));
	[self invalidateIntrinsicContentSize];
}
@end

static OakAutoSizingTextField* OakCreateTextField (id <NSTextFieldDelegate> delegate)
{
	OakAutoSizingTextField* res = [[OakAutoSizingTextField alloc] initWithFrame:NSZeroRect];
	res.font = OakControlFont();
	[[res cell] setWraps:YES];
	res.delegate = delegate;
	return res;
}

static NSButton* OakCreateHistoryButton ()
{
	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	res.bezelStyle = NSRoundedDisclosureBezelStyle;
	res.buttonType = NSMomentaryLightButton;
	res.title      = @"";
	return res;
}

static NSOutlineView* OakCreateOutlineView (NSScrollView** scrollViewOut)
{
	NSOutlineView* res = [[NSOutlineView alloc] initWithFrame:NSZeroRect];
	res.focusRingType                      = NSFocusRingTypeNone;
	res.allowsMultipleSelection            = YES;
	res.autoresizesOutlineColumn           = NO;
	res.usesAlternatingRowBackgroundColors = YES;
	res.headerView                         = nil;

	NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"checkbox"];
	NSButtonCell* dataCell = [NSButtonCell new];
	dataCell.buttonType    = NSSwitchButton;
	dataCell.controlSize   = NSSmallControlSize;
	dataCell.imagePosition = NSImageOnly;
	dataCell.font          = [NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]];
	tableColumn.dataCell = dataCell;
	tableColumn.width    = 50;
	[res addTableColumn:tableColumn];
	[res setOutlineTableColumn:tableColumn];

	tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"match"];
	[tableColumn setEditable:NO];
	NSTextFieldCell* cell = tableColumn.dataCell;
	cell.font = [NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]];
	[res addTableColumn:tableColumn];

	res.rowHeight = 14;

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = NO;
	scrollView.borderType            = NSNoBorder;
	scrollView.documentView          = res;

	if(scrollViewOut)
		*scrollViewOut = scrollView;

	return res;
}

static NSProgressIndicator* OakCreateProgressIndicator ()
{
	NSProgressIndicator* res = [[NSProgressIndicator alloc] initWithFrame:NSZeroRect];
	res.style                = NSProgressIndicatorSpinningStyle;
	res.controlSize          = NSSmallControlSize;
	res.displayedWhenStopped = NO;
	return res;
}

static NSButton* OakCreateStopSearchButton ()
{
	NSButton* res     = [[NSButton alloc] initWithFrame:NSZeroRect];
	res.buttonType    = NSMomentaryChangeButton;
	res.bordered      = NO;
	res.image         = [NSImage imageNamed:NSImageNameStopProgressFreestandingTemplate];
	res.imagePosition = NSImageOnly;
	res.toolTip       = @"Stop Search";
	res.keyEquivalent = @".";
	res.keyEquivalentModifierMask = NSCommandKeyMask;
	[res.cell setImageScaling:NSImageScaleProportionallyDown];
	return res;
}

@interface FindWindowController () <NSTextFieldDelegate, NSWindowDelegate, NSMenuDelegate>
{
	BOOL _wrapAround;
	BOOL _ignoreCase;
}
@property (nonatomic) NSTextField*              findLabel;
@property (nonatomic) OakAutoSizingTextField*   findTextField;
@property (nonatomic) NSButton*                 findHistoryButton;

@property (nonatomic) NSButton*                 countButton;

@property (nonatomic) NSTextField*              replaceLabel;
@property (nonatomic) OakAutoSizingTextField*   replaceTextField;
@property (nonatomic) NSButton*                 replaceHistoryButton;

@property (nonatomic) NSTextField*              optionsLabel;
@property (nonatomic) NSButton*                 ignoreCaseCheckBox;
@property (nonatomic) NSButton*                 ignoreWhitespaceCheckBox;
@property (nonatomic) NSButton*                 regularExpressionCheckBox;
@property (nonatomic) NSButton*                 wrapAroundCheckBox;

@property (nonatomic) NSTextField*              whereLabel;
@property (nonatomic) NSPopUpButton*            wherePopUpButton;
@property (nonatomic) NSTextField*              matchingLabel;
@property (nonatomic) NSComboBox*               globTextField;
@property (nonatomic) NSPopUpButton*            actionsPopUpButton;

@property (nonatomic) NSView*                   resultsTopDivider;
@property (nonatomic) NSScrollView*             resultsScrollView;
@property (nonatomic, readwrite) NSOutlineView* resultsOutlineView;
@property (nonatomic) NSView*                   resultsBottomDivider;

@property (nonatomic) NSProgressIndicator*      progressIndicator;
@property (nonatomic) NSTextField*              statusTextField;

@property (nonatomic, readwrite) NSButton*      findAllButton;
@property (nonatomic, readwrite) NSButton*      replaceAllButton;
@property (nonatomic, readwrite) NSButton*      replaceAndFindButton;
@property (nonatomic, readwrite) NSButton*      findPreviousButton;
@property (nonatomic, readwrite) NSButton*      findNextButton;
@property (nonatomic, readwrite) NSButton*      stopSearchButton;

@property (nonatomic) NSPopover*                findStringPopver;

@property (nonatomic) NSObjectController*       objectController;
@property (nonatomic) OakHistoryList*           globHistoryList;
@property (nonatomic) OakHistoryList*           recentFolders;
@property (nonatomic) NSMutableArray*           myConstraints;

@property (nonatomic) BOOL                      folderSearch;
@property (nonatomic, readonly) BOOL            canIgnoreWhitespace;
@property (nonatomic) CGFloat                   findResultsHeight;
@end

@implementation FindWindowController
+ (NSSet*)keyPathsForValuesAffectingCanIgnoreWhitespace { return [NSSet setWithObject:@"regularExpression"]; }
+ (NSSet*)keyPathsForValuesAffectingIgnoreWhitespace    { return [NSSet setWithObject:@"regularExpression"]; }

- (id)init
{
	NSRect r = [[NSScreen mainScreen] visibleFrame];
	if((self = [super initWithWindow:[[NSPanel alloc] initWithContentRect:NSMakeRect(NSMidX(r)-100, NSMidY(r)+100, 200, 200) styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask) backing:NSBackingStoreBuffered defer:NO]]))
	{
		self.window.title              = @"Find";
		self.window.frameAutosaveName  = @"Find";
		self.window.hidesOnDeactivate  = NO;
		self.window.collectionBehavior = self.window.collectionBehavior | NSWindowCollectionBehaviorMoveToActiveSpace;
		self.window.delegate           = self;

		self.findLabel                 = OakCreateLabel(@"Find:");
		self.findTextField             = OakCreateTextField(self);
		self.findHistoryButton         = OakCreateHistoryButton();
		self.countButton               = OakCreateButton(@"Σ", NSSmallSquareBezelStyle);

		self.replaceLabel              = OakCreateLabel(@"Replace:");
		self.replaceTextField          = OakCreateTextField(self);
		self.replaceHistoryButton      = OakCreateHistoryButton();

		self.optionsLabel              = OakCreateLabel(@"Options:");

		self.ignoreCaseCheckBox        = OakCreateCheckBox(@"Ignore Case");
		self.ignoreWhitespaceCheckBox  = OakCreateCheckBox(@"Ignore Whitespace");
		self.regularExpressionCheckBox = OakCreateCheckBox(@"Regular Expression");
		self.wrapAroundCheckBox        = OakCreateCheckBox(@"Wrap Around");

		self.whereLabel                = OakCreateLabel(@"In:");
		self.wherePopUpButton          = OakCreatePopUpButton();
		self.matchingLabel             = OakCreateLabel(@"matching");
		self.globTextField             = OakCreateComboBox();
		self.actionsPopUpButton        = OakCreatePopUpButton(YES /* pulls down */);

		NSScrollView* resultsScrollView = nil;
		self.resultsTopDivider         = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1]);
		self.resultsOutlineView        = OakCreateOutlineView(&resultsScrollView);
		self.resultsScrollView         = resultsScrollView;
		self.resultsBottomDivider      = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1]);

		self.progressIndicator         = OakCreateProgressIndicator();
		self.statusTextField           = OakCreateSmallLabel();

		[self.statusTextField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self.statusTextField.cell setLineBreakMode:NSLineBreakByTruncatingTail];

		self.findAllButton             = OakCreateButton(@"Find All");
		self.replaceAllButton          = OakCreateButton(@"Replace All");
		self.replaceAndFindButton      = OakCreateButton(@"Replace & Find");
		self.findPreviousButton        = OakCreateButton(@"Previous");
		self.findNextButton            = OakCreateButton(@"Next");
		self.stopSearchButton          = OakCreateStopSearchButton();

		[self updateSearchInPopUpMenu];

		// =============================
		// = Create action pop-up menu =
		// =============================

		NSMenu* actionMenu = self.actionsPopUpButton.menu;
		[actionMenu removeAllItems];

		NSMenuItem* titleItem = [actionMenu addItemWithTitle:@"" action:@selector(nop:) keyEquivalent:@""];
		titleItem.image = [NSImage imageNamed:NSImageNameActionTemplate];

		[actionMenu addItemWithTitle:@"Follow Symbolic Links" action:@selector(toggleFollowSymbolicLinks:) keyEquivalent:@""];
		[actionMenu addItemWithTitle:@"Search Hidden Folders" action:@selector(toggleSearchHiddenFolders:) keyEquivalent:@""];
		[actionMenu addItem:[NSMenuItem separatorItem]];
		NSMenuItem* collapseExpandItem = [actionMenu addItemWithTitle:@"Collapse/Expand Results" action:@selector(takeLevelToFoldFrom:) keyEquivalent:@"1"];
		collapseExpandItem.keyEquivalentModifierMask = NSAlternateKeyMask|NSCommandKeyMask;
		collapseExpandItem.tag = -1;

		NSMenuItem* selectResultItem = [actionMenu addItemWithTitle:@"Select Result" action:NULL keyEquivalent:@""];
		selectResultItem.submenu = [NSMenu new];
		selectResultItem.submenu.delegate = self;

		[actionMenu addItem:[NSMenuItem separatorItem]];
		[actionMenu addItemWithTitle:@"Copy Matching Parts"                action:@selector(copyMatchingParts:)             keyEquivalent:@""];
		[actionMenu addItemWithTitle:@"Copy Matching Parts With Filenames" action:@selector(copyMatchingPartsWithFilename:) keyEquivalent:@""];
		[actionMenu addItemWithTitle:@"Copy Entire Lines"                  action:@selector(copyEntireLines:)               keyEquivalent:@""];
		[actionMenu addItemWithTitle:@"Copy Entire Lines With Filenames"   action:@selector(copyEntireLinesWithFilename:)   keyEquivalent:@""];

		// =============================

		self.findHistoryButton.action     = @selector(showFindHistory:);
		self.replaceHistoryButton.action  = @selector(showReplaceHistory:);
		self.countButton.action           = @selector(countOccurrences:);
		self.findAllButton.action         = @selector(findAll:);
		self.replaceAllButton.action      = @selector(replaceAll:);
		self.replaceAndFindButton.action  = @selector(replaceAndFind:);
		self.findPreviousButton.action    = @selector(findPrevious:);
		self.findNextButton.action        = @selector(findNext:);
		self.stopSearchButton.action      = @selector(stopSearch:);

		self.objectController = [[NSObjectController alloc] initWithContent:self];
		self.globHistoryList  = [[OakHistoryList alloc] initWithName:@"Find in Folder Globs.default" stackSize:10 defaultItems:@"*", @"*.txt", @"*.{c,h}", nil];
		self.recentFolders    = [[OakHistoryList alloc] initWithName:@"findRecentPlaces" stackSize:6];

		[self.findTextField             bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.findString"           options:@{ NSContinuouslyUpdatesValueBindingOption: @YES }];
		[self.replaceTextField          bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.replaceString"        options:@{ NSContinuouslyUpdatesValueBindingOption: @YES }];
		[self.globTextField             bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.globHistoryList.head" options:nil];
		[self.globTextField             bind:NSContentValuesBinding toObject:_objectController withKeyPath:@"content.globHistoryList.list" options:nil];
		[self.globTextField             bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.folderSearch"         options:nil];
		[self.actionsPopUpButton        bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.folderSearch"         options:nil];
		[self.ignoreCaseCheckBox        bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.ignoreCase"           options:nil];
		[self.ignoreWhitespaceCheckBox  bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.ignoreWhitespace"     options:nil];
		[self.regularExpressionCheckBox bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.regularExpression"    options:nil];
		[self.wrapAroundCheckBox        bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.wrapAround"           options:nil];
		[self.ignoreWhitespaceCheckBox  bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.canIgnoreWhitespace"  options:nil];
		[self.statusTextField           bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.statusString"         options:nil];
		[self.replaceAndFindButton      bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.folderSearch"         options:@{ NSValueTransformerNameBindingOption: @"NSNegateBoolean" }];

		NSView* contentView = self.window.contentView;
		for(NSView* view in [self.allViews allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[contentView addSubview:view];
		}

		for(NSView* view in @[ self.resultsTopDivider, self.resultsScrollView, self.resultsBottomDivider, self.stopSearchButton, self.progressIndicator ])
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];

		[self updateConstraints];

		self.window.initialFirstResponder = self.findTextField;
		self.window.defaultButtonCell     = self.findNextButton.cell;

		self.searchIn = FFSearchInDocument;

		// setup find/replace strings/options
		[self userDefaultsDidChange:nil];
		[self findClipboardDidChange:nil];
		[self replaceClipboardDidChange:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(findClipboardDidChange:) name:OakPasteboardDidChangeNotification object:[OakPasteboard pasteboardWithName:NSFindPboard]];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(replaceClipboardDidChange:) name:OakPasteboardDidChangeNotification object:[OakPasteboard pasteboardWithName:NSReplacePboard]];

		[self.window addObserver:self forKeyPath:@"firstResponder" options:0 context:NULL];
	}
	return self;
}

- (void)dealloc
{
	[self.window removeObserver:self forKeyPath:@"firstResponder"];
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	[aMenu removeAllItems];
	[NSApp sendAction:@selector(updateGoToMenu:) to:nil from:aMenu];
}

- (NSDictionary*)allViews
{
	NSDictionary* views = @{
		@"findLabel"         : self.findLabel,
		@"find"              : self.findTextField,
		@"findHistory"       : self.findHistoryButton,
		@"count"             : self.countButton,
		@"replaceLabel"      : self.replaceLabel,
		@"replace"           : self.replaceTextField,
		@"replaceHistory"    : self.replaceHistoryButton,

		@"optionsLabel"      : self.optionsLabel,
		@"regularExpression" : self.regularExpressionCheckBox,
		@"ignoreWhitespace"  : self.ignoreWhitespaceCheckBox,
		@"ignoreCase"        : self.ignoreCaseCheckBox,
		@"wrapAround"        : self.wrapAroundCheckBox,

		@"whereLabel"        : self.whereLabel,
		@"where"             : self.wherePopUpButton,
		@"matching"          : self.matchingLabel,
		@"glob"              : self.globTextField,
		@"actions"           : self.actionsPopUpButton,

		@"status"            : self.statusTextField,

		@"findAll"           : self.findAllButton,
		@"replaceAll"        : self.replaceAllButton,
		@"replaceAndFind"    : self.replaceAndFindButton,
		@"previous"          : self.findPreviousButton,
		@"next"              : self.findNextButton,
	};

	if(self.showsResultsOutlineView)
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:views];
		[dict addEntriesFromDictionary:@{
			@"resultsTopDivider"    : self.resultsTopDivider,
			@"results"              : self.resultsScrollView,
			@"resultsBottomDivider" : self.resultsBottomDivider,
		}];
		views = dict;
	}

	if(self.isBusy)
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:views];
		[dict addEntriesFromDictionary:@{
			@"busy" : self.progressIndicator,
			@"stopSearch" : self.stopSearchButton,
		}];
		views = dict;
	}

	return views;
}

#ifndef CONSTRAINT
#define CONSTRAINT(str, align) [_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:str options:align metrics:nil views:views]]
#endif

- (void)updateConstraints
{
	if(_myConstraints)
		[self.window.contentView removeConstraints:_myConstraints];
	self.myConstraints = [NSMutableArray array];

	NSDictionary* views = self.allViews;

	CONSTRAINT(@"H:|-(>=20,==20@75)-[findLabel]-[find(>=100)]",        0);
	CONSTRAINT(@"H:[find]-(5)-[findHistory]-[count(==findHistory)]-|", NSLayoutFormatAlignAllTop);
	CONSTRAINT(@"V:[count(==21)]",                                     NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight);
	CONSTRAINT(@"H:|-(>=20,==20@75)-[replaceLabel]-[replace]",         0);
	CONSTRAINT(@"H:[replace]-(5)-[replaceHistory]",                    NSLayoutFormatAlignAllTop);
	CONSTRAINT(@"V:|-[find]-[replace]",                                NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight);

	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.findLabel attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self.findTextField attribute:NSLayoutAttributeTop multiplier:1 constant:3]];
	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.replaceLabel attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self.replaceTextField attribute:NSLayoutAttributeTop multiplier:1 constant:3]];

	CONSTRAINT(@"H:|-(>=20,==20@75)-[optionsLabel]-[regularExpression]-[ignoreWhitespace]-(>=20)-|", NSLayoutFormatAlignAllBaseline);
	CONSTRAINT(@"H:[ignoreCase(==regularExpression)]-[wrapAround(==ignoreWhitespace)]",              NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom);
	CONSTRAINT(@"V:[replace]-[regularExpression]-[ignoreCase]",                                      NSLayoutFormatAlignAllLeft);
	CONSTRAINT(@"V:[replace]-[ignoreWhitespace]-[wrapAround]",                                       0);

	CONSTRAINT(@"H:|-(>=20,==20@75)-[whereLabel]-[where(<=180)]-[matching]", NSLayoutFormatAlignAllBaseline);
	CONSTRAINT(@"H:[matching]-[glob]",                                       0);
	CONSTRAINT(@"H:[where]-(>=8)-[glob]-[actions]",                          NSLayoutFormatAlignAllTop);
	CONSTRAINT(@"V:[ignoreCase]-[where]",                                    NSLayoutFormatAlignAllLeft);
	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.replaceTextField attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:self.globTextField attribute:NSLayoutAttributeRight multiplier:1 constant:0]];

	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.findLabel attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:self.replaceLabel attribute:NSLayoutAttributeRight multiplier:1 constant:0]];
	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.findLabel attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:self.optionsLabel attribute:NSLayoutAttributeRight multiplier:1 constant:0]];
	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.findLabel attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:self.whereLabel   attribute:NSLayoutAttributeRight multiplier:1 constant:0]];

	if(self.showsResultsOutlineView)
	{
		CONSTRAINT(@"H:|[results(==resultsTopDivider,==resultsBottomDivider)]|", 0);
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[where]-[resultsTopDivider][results(>=50,==height@490)][resultsBottomDivider]-[status]" options:0 metrics:@{ @"height" : @(self.findResultsHeight) } views:views]];
	}
	else
	{
		CONSTRAINT(@"V:[where]-[status]", 0);
	}

	if(self.isBusy)
	{
		CONSTRAINT(@"H:|-[stopSearch(==13)]-(==6)-[busy]-[status]-|", NSLayoutFormatAlignAllCenterY);
	}
	else
	{
		CONSTRAINT(@"H:|-[status]-|", 0);
	}

	CONSTRAINT(@"H:|-[findAll]-[replaceAll]-(>=8)-[replaceAndFind]-[previous]-[next]-|", NSLayoutFormatAlignAllBottom);
	CONSTRAINT(@"V:[status(==14)]-[findAll]-|", 0);

	[self.window.contentView addConstraints:_myConstraints];

	if(self.showsResultsOutlineView)
	{
		NSView* keyViewLoop[] = { self.findTextField, self.replaceTextField, self.globTextField, self.actionsPopUpButton, self.countButton, self.regularExpressionCheckBox, self.ignoreWhitespaceCheckBox, self.ignoreCaseCheckBox, self.wrapAroundCheckBox, self.wherePopUpButton, self.resultsOutlineView, self.findAllButton, self.replaceAllButton, self.replaceAndFindButton, self.findPreviousButton, self.findNextButton };
		for(size_t i = 0; i < sizeofA(keyViewLoop); ++i)
			keyViewLoop[i].nextKeyView = keyViewLoop[(i + 1) % sizeofA(keyViewLoop)];
	}
	else
	{
		NSView* keyViewLoop[] = { self.findTextField, self.replaceTextField, self.globTextField, self.countButton, self.regularExpressionCheckBox, self.ignoreWhitespaceCheckBox, self.ignoreCaseCheckBox, self.wrapAroundCheckBox, self.wherePopUpButton, self.findAllButton, self.replaceAllButton, self.replaceAndFindButton, self.findPreviousButton, self.findNextButton };
		for(size_t i = 0; i < sizeofA(keyViewLoop); ++i)
			keyViewLoop[i].nextKeyView = keyViewLoop[(i + 1) % sizeofA(keyViewLoop)];
	}
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.ignoreCase = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindIgnoreCase];
	self.wrapAround = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindWrapAround];

	if(NSDictionary* options = [[NSUserDefaults standardUserDefaults] dictionaryForKey:kUserDefaultsFolderOptionsKey])
	{
		self.followSymbolicLinks = [[options objectForKey:@"followLinks"] boolValue];
		self.searchHiddenFolders = [[options objectForKey:@"searchHiddenFolders"] boolValue];
	}
}

- (void)findClipboardDidChange:(NSNotification*)aNotification
{
	OakPasteboardEntry* entry = [[OakPasteboard pasteboardWithName:NSFindPboard] current];
	self.findString        = entry.string;
	self.regularExpression = entry.regularExpression;
	self.ignoreWhitespace  = entry.ignoreWhitespace;
	self.fullWords         = entry.fullWordMatch;
}

- (void)replaceClipboardDidChange:(NSNotification*)aNotification
{
	self.replaceString = [[[OakPasteboard pasteboardWithName:NSReplacePboard] current] string];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if([keyPath isEqualToString:@"firstResponder"])
	{
		NSResponder* firstResponder = [self.window firstResponder];
		if(![firstResponder isKindOfClass:[NSTextView class]])
			self.showReplacementPreviews = firstResponder == self.replaceTextField;
	}
}

- (void)showWindow:(id)sender
{
	if([self isWindowLoaded] && ![self.window isVisible])
	{
		self.showsResultsOutlineView = self.folderSearch;
		if(!self.folderSearch)
			self.statusString = @"";
	}

	BOOL isVisibleAndKey = [self isWindowLoaded] && [self.window isVisible] && [self.window isKeyWindow];
	[super showWindow:sender];
	if(!isVisibleAndKey || ![[self.window firstResponder] isKindOfClass:[NSTextView class]])
		[self.window makeFirstResponder:self.findTextField];
}

- (BOOL)commitEditing
{
	id currentResponder = [[self window] firstResponder];
	id view = [currentResponder isKindOfClass:[NSTextView class]] ? [currentResponder delegate] : currentResponder;
	BOOL res = [self.objectController commitEditing];
	if([[self window] firstResponder] != currentResponder && view)
		[[self window] makeFirstResponder:view];

	// =====================
	// = Update Pasteboard =
	// =====================

	NSDictionary* newOptions = @{
		OakFindRegularExpressionOption : @(self.regularExpression),
		OakFindIgnoreWhitespaceOption  : @(self.ignoreWhitespace),
		OakFindFullWordsOption         : @(self.fullWords),
	};

	if(NSNotEmptyString(_findString))
	{
		OakPasteboardEntry* oldEntry = [[OakPasteboard pasteboardWithName:NSFindPboard] current];
		if(!oldEntry || ![oldEntry.string isEqualToString:_findString])
			[[OakPasteboard pasteboardWithName:NSFindPboard] addEntry:[OakPasteboardEntry pasteboardEntryWithString:_findString andOptions:newOptions]];
		else if(![oldEntry.options isEqualToDictionary:newOptions])
			oldEntry.options = newOptions;
	}

	if(_replaceString)
	{
		NSString* oldReplacement = [[[OakPasteboard pasteboardWithName:NSReplacePboard] current] string];
		if(!oldReplacement || ![oldReplacement isEqualToString:_replaceString])
			[[OakPasteboard pasteboardWithName:NSReplacePboard] addEntry:[OakPasteboardEntry pasteboardEntryWithString:_replaceString]];
	}

	return res;
}

- (void)windowDidResize:(NSNotification*)aNotification
{
	if(self.showsResultsOutlineView)
		self.findResultsHeight = NSHeight(self.resultsScrollView.frame);
}

- (void)windowDidResignKey:(NSNotification*)aNotification
{
	[self commitEditing];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self commitEditing];
}

// ==============================
// = Create “where” pop-up menu =
// ==============================

- (NSString*)displayNameForFolder:(NSString*)path
{
	std::vector<std::string> paths;
	for(NSUInteger i = 0; i < [self.recentFolders count]; ++i)
		paths.push_back(to_s((NSString*)[self.recentFolders objectAtIndex:i]));
	if(NSString* folder = self.searchFolder)
		paths.push_back(to_s(folder));
	paths.push_back(to_s(self.projectFolder));

	auto it = std::find(paths.begin(), paths.end(), to_s(path));
	if(it != paths.end())
		return [NSString stringWithCxxString:path::display_name(*it, path::disambiguate(paths)[it - paths.begin()])];
	return [[NSFileManager defaultManager] displayNameAtPath:path];
}

- (void)updateSearchInPopUpMenu
{
	NSMenu* whereMenu = self.wherePopUpButton.menu;
	[whereMenu removeAllItems];
	[[whereMenu addItemWithTitle:@"Document" action:@selector(orderFrontFindPanel:) keyEquivalent:@"f"] setTag:1];
	[[whereMenu itemAtIndex:0] setRepresentedObject:FFSearchInDocument];
	[[whereMenu addItemWithTitle:@"Selection" action:@selector(takeSearchInFrom:) keyEquivalent:@""] setRepresentedObject:FFSearchInSelection];
	[whereMenu addItem:[NSMenuItem separatorItem]];
	[[whereMenu addItemWithTitle:@"Open Files" action:@selector(nop:) keyEquivalent:@""] setRepresentedObject:FFSearchInOpenFiles];
	[[whereMenu addItemWithTitle:@"Project Folder" action:@selector(orderFrontFindPanel:) keyEquivalent:@"F"] setTag:3];
	[whereMenu addItemWithTitle:@"Other Folder…" action:@selector(showFolderSelectionPanel:) keyEquivalent:@""];

	if([self.searchIn isEqualToString:FFSearchInDocument])
		[self.wherePopUpButton selectItemAtIndex:0];
	else if([self.searchIn isEqualToString:FFSearchInSelection])
		[self.wherePopUpButton selectItemAtIndex:1];
	else if([self.searchIn isEqualToString:FFSearchInOpenFiles])
		[self.wherePopUpButton selectItemAtIndex:3];

	if(NSString* folder = self.searchFolder ?: self.projectFolder)
	{
		[whereMenu addItem:[NSMenuItem separatorItem]];
		NSMenuItem* folderMenuItem = [whereMenu addItemWithTitle:[self displayNameForFolder:folder] action:@selector(takeSearchInFrom:) keyEquivalent:@""];
		[folderMenuItem setIconForFile:folder];
		[folderMenuItem setRepresentedObject:folder];
		[FFFolderMenu addFolderSubmenuToMenuItem:folderMenuItem];

		if(self.searchFolder)
			[self.wherePopUpButton selectItem:folderMenuItem];
	}

	// =================
	// = Recent Places =
	// =================

	[whereMenu addItem:[NSMenuItem separatorItem]];
	[whereMenu addItemWithTitle:@"Recent Places" action:@selector(nop:) keyEquivalent:@""];

	for(NSUInteger i = 0; i < [self.recentFolders count]; ++i)
	{
		NSString* path = [self.recentFolders objectAtIndex:i];
		if([path isEqualToString:self.searchFolder] || [path isEqualToString:self.projectFolder])
			continue;

		NSMenuItem* recentItem = [whereMenu addItemWithTitle:[self displayNameForFolder:path] action:@selector(takeSearchInFrom:) keyEquivalent:@""];
		[recentItem setIconForFile:path];
		[recentItem setRepresentedObject:path];
	}
}

- (void)takeSearchInFrom:(NSMenuItem*)menuItem
{
	self.searchIn = [menuItem representedObject];
}

// ==============================

- (IBAction)showFindHistory:(id)sender
{
	if(![[[OakPasteboardSelector sharedInstance] window] isVisible])
		[[OakPasteboard pasteboardWithName:NSFindPboard] selectItemForControl:self.findTextField];
	// if the panel is visible it will automatically be hidden due to the mouse click
}

- (IBAction)showReplaceHistory:(id)sender
{
	if(![[[OakPasteboardSelector sharedInstance] window] isVisible])
		[[OakPasteboard pasteboardWithName:NSReplacePboard] selectItemForControl:self.replaceTextField];
	// if the panel is visible it will automatically be hidden due to the mouse click
}

- (void)setFindErrorString:(NSString*)aString
{
	if(_findErrorString == aString || [_findErrorString isEqualToString:aString])
		return;

	if(_findErrorString = aString)
	{
		if(!self.findStringPopver)
		{
			NSViewController* viewController = [NSViewController new];
			viewController.view = OakCreateLabel(@"");

			self.findStringPopver = [NSPopover new];
			self.findStringPopver.behavior = NSPopoverBehaviorApplicationDefined;
			self.findStringPopver.contentViewController = viewController;
		}

		NSTextField* textField = (NSTextField*)self.findStringPopver.contentViewController.view;
		textField.stringValue = _findErrorString;
		[textField sizeToFit];

		[self.findStringPopver showRelativeToRect:NSZeroRect ofView:self.findTextField preferredEdge:NSMaxYEdge];
	}
	else
	{
		[self.findStringPopver close];
		self.findStringPopver = nil;
	}
}

- (void)updateFindErrorString
{
	NSString* errorString = nil;
	if(self.regularExpression)
	{
		std::string const& error = regexp::validate(to_s(self.findString));
		if(error != NULL_STR)
			errorString = [NSString stringWithCxxString:text::format("Invalid regular expression: %s.", error.c_str())];
	}
	self.findErrorString = errorString;
}

- (void)setShowsResultsOutlineView:(BOOL)flag
{
	if(_showsResultsOutlineView == flag)
		return;

	BOOL isWindowLoaded = [self isWindowLoaded];
	CGFloat desiredHeight = self.findResultsHeight;

	for(NSView* view in @[ self.resultsTopDivider, self.resultsScrollView, self.resultsBottomDivider ])
	{
		if(_showsResultsOutlineView = flag)
				[self.window.contentView addSubview:view];
		else	[view removeFromSuperview];
	}

	[self updateConstraints];

	if(flag && isWindowLoaded)
	{
		[self.window layoutIfNeeded];

		NSRect screenFrame = [[self.window screen] visibleFrame];
		NSRect windowFrame = self.window.frame;
		CGFloat minY = NSMinY(windowFrame);
		CGFloat maxY = NSMaxY(windowFrame);

		CGFloat currentHeight = NSHeight(self.resultsScrollView.frame);
		minY -= desiredHeight - currentHeight;

		if(minY < NSMinY(screenFrame))
			maxY += NSMinY(screenFrame) - minY;
		if(maxY > NSMaxY(screenFrame))
			minY -= maxY - NSMaxY(screenFrame);

		minY = MAX(minY, NSMinY(screenFrame));
		maxY = MIN(maxY, NSMaxY(screenFrame));

		windowFrame.origin.y    = minY;
		windowFrame.size.height = maxY - minY;

		[self.window setFrame:windowFrame display:YES];
	}

	self.window.defaultButtonCell = flag ? self.findAllButton.cell : self.findNextButton.cell;
}

- (void)setDisableResultsCheckBoxes:(BOOL)flag
{
	if(_disableResultsCheckBoxes == flag)
		return;
	_disableResultsCheckBoxes = flag;

	[[self.resultsOutlineView tableColumnWithIdentifier:@"checkbox"] setHidden:flag];
	[self.resultsOutlineView setOutlineTableColumn:[self.resultsOutlineView tableColumnWithIdentifier:flag ? @"match" : @"checkbox"]];
}

- (void)setShowResultsCollapsed:(BOOL)flag
{
	if(_showResultsCollapsed = flag)
			[self.resultsOutlineView collapseItem:nil collapseChildren:YES];
	else	[self.resultsOutlineView expandItem:nil expandChildren:YES];
	[self.resultsOutlineView setNeedsDisplay:YES];
}

- (void)setShowReplacementPreviews:(BOOL)flag
{
	if(_showReplacementPreviews != flag)
	{
		_showReplacementPreviews = flag;
		[self.resultsOutlineView reloadData];
	}
}

- (void)setBusy:(BOOL)busyFlag
{
	if(_busy == busyFlag)
		return;

	if(_busy = busyFlag)
	{
		[self.window.contentView addSubview:self.stopSearchButton];
		[self.window.contentView addSubview:self.progressIndicator];
		[self.progressIndicator startAnimation:self];
	}
	else
	{
		[self.stopSearchButton removeFromSuperview];
		[self.progressIndicator stopAnimation:self];
		[self.progressIndicator removeFromSuperview];
	}
	[self updateConstraints];
}

- (NSString*)searchFolder
{
	return [@[ FFSearchInDocument, FFSearchInSelection, FFSearchInOpenFiles ] containsObject:self.searchIn] ? nil : self.searchIn;
}

- (void)setSearchIn:(NSString*)aString
{
	if(_searchIn == aString || [_searchIn isEqualToString:aString])
	{
		for(NSMenuItem* menuItem in [self.wherePopUpButton.menu itemArray])
		{
			if([[menuItem representedObject] isEqualTo:aString])
				[self.wherePopUpButton selectItem:menuItem];
		}
		return;
	}

	_searchIn = aString;
	self.folderSearch = self.searchFolder != nil;
	self.window.title = self.searchFolder ? [NSString localizedStringWithFormat:MSG_FIND_IN_FOLDER_WINDOW_TITLE, [self.searchFolder stringByAbbreviatingWithTildeInPath]] : MSG_WINDOW_TITLE;
	if(NSString* folder = self.searchFolder)
		[self.recentFolders addObject:folder];
	[self updateSearchInPopUpMenu];
}

- (IBAction)goToParentFolder:(id)sender
{
	if(NSString* parent = [self.searchFolder stringByDeletingLastPathComponent])
		self.searchIn = parent;
}

- (void)setFolderSearch:(BOOL)flag
{
	if(_folderSearch == flag)
		return;

	_folderSearch = flag;
	self.showsResultsOutlineView = flag;
}

- (void)setFindString:(NSString*)aString
{
	if(_findString == aString || [_findString isEqualToString:aString])
		return;

	_findString = aString ?: @"";
	[self.findTextField updateIntrinsicContentSizeToEncompassString:_findString];

	if(self.findErrorString)
		[self updateFindErrorString];
}

- (void)setReplaceString:(NSString*)aString
{
	if(_replaceString == aString || [_replaceString isEqualToString:aString])
		return;

	_replaceString = aString ?: @"";
	[self.replaceTextField updateIntrinsicContentSizeToEncompassString:_replaceString];
}

- (void)setFindResultsHeight:(CGFloat)height { [[NSUserDefaults standardUserDefaults] setInteger:height forKey:kUserDefaultsFindResultsHeightKey]; }
- (CGFloat)findResultsHeight                 { return [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsFindResultsHeightKey] ?: 200; }

- (void)setRegularExpression:(BOOL)flag { _regularExpression = flag; if(self.findErrorString) [self updateFindErrorString]; }
- (void)setIgnoreCase:(BOOL)flag        { if(_ignoreCase != flag) [[NSUserDefaults standardUserDefaults] setObject:@(_ignoreCase = flag) forKey:kUserDefaultsFindIgnoreCase]; }
- (void)setWrapAround:(BOOL)flag        { if(_wrapAround != flag) [[NSUserDefaults standardUserDefaults] setObject:@(_wrapAround = flag) forKey:kUserDefaultsFindWrapAround]; }
- (BOOL)ignoreWhitespace                { return _ignoreWhitespace && self.canIgnoreWhitespace; }
- (BOOL)canIgnoreWhitespace             { return _regularExpression == NO; }

- (NSString*)globString                 { [self commitEditing]; return _globHistoryList.head; }

- (void)setProjectFolder:(NSString*)aFolder
{
	if(_projectFolder != aFolder && ![_projectFolder isEqualToString:aFolder])
	{
		_projectFolder = aFolder ?: @"";
		self.globHistoryList = [[OakHistoryList alloc] initWithName:[NSString stringWithFormat:@"Find in Folder Globs.%@", _projectFolder] stackSize:10 defaultItems:@"*", @"*.txt", @"*.{c,h}", nil];
		[self updateSearchInPopUpMenu];
	}
}

- (void)updateFolderSearchUserDefaults
{
	[[NSUserDefaults standardUserDefaults] setObject:@{
		@"followLinks"         : @(self.followSymbolicLinks),
		@"searchHiddenFolders" : @(self.searchHiddenFolders),
	} forKey:kUserDefaultsFolderOptionsKey];
}

- (void)setFollowSymbolicLinks:(BOOL)flag { if(_followSymbolicLinks != flag) { _followSymbolicLinks = flag; [self updateFolderSearchUserDefaults]; } }
- (void)setSearchHiddenFolders:(BOOL)flag { if(_searchHiddenFolders != flag) { _searchHiddenFolders = flag; [self updateFolderSearchUserDefaults]; } }

- (IBAction)toggleFollowSymbolicLinks:(id)sender { self.followSymbolicLinks = !self.followSymbolicLinks; }
- (IBAction)toggleSearchHiddenFolders:(id)sender { self.searchHiddenFolders = !self.searchHiddenFolders; }
- (IBAction)takeLevelToFoldFrom:(id)sender       { self.showResultsCollapsed = !self.showResultsCollapsed; }

- (IBAction)selectNextResult:(id)sender
{
	if([self.resultsOutlineView numberOfRows] == 0)
		return;

	NSInteger row = [self.resultsOutlineView selectedRow];
	while(true)
	{
		if(++row == [self.resultsOutlineView numberOfRows])
		{
			if(!self.wrapAround)
				return;
			row = -1;
			continue;
		}

		if([[self.resultsOutlineView delegate] respondsToSelector:@selector(outlineView:isGroupItem:)] && [[self.resultsOutlineView delegate] outlineView:self.resultsOutlineView isGroupItem:[self.resultsOutlineView itemAtRow:row]])
		{
			[self.resultsOutlineView expandItem:[self.resultsOutlineView itemAtRow:row]];
			continue;
		}

		[self.resultsOutlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[self.resultsOutlineView scrollRowToVisible:row];

		break;
	}
}

- (IBAction)selectPreviousResult:(id)sender
{
	if([self.resultsOutlineView numberOfRows] == 0)
		return;

	self.showResultsCollapsed = NO;

	NSInteger row = [self.resultsOutlineView selectedRow];
	if(row == -1)
		row = [self.resultsOutlineView numberOfRows];

	while(true)
	{
		if(--row == 0)
		{
			if(!self.wrapAround)
				return;
			row = [self.resultsOutlineView numberOfRows];
			continue;
		}

		if([[self.resultsOutlineView delegate] respondsToSelector:@selector(outlineView:isGroupItem:)] && [[self.resultsOutlineView delegate] outlineView:self.resultsOutlineView isGroupItem:[self.resultsOutlineView itemAtRow:row]])
			continue;

		[self.resultsOutlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[self.resultsOutlineView scrollRowToVisible:row];

		break;
	}
}

- (BOOL)control:(NSControl*)control textView:(NSTextView*)textView doCommandBySelector:(SEL)command
{
	if(command == @selector(moveDown:))
	{
		NSRange insertionPoint = [[textView.selectedRanges lastObject] rangeValue];
		NSRange lastNewline    = [textView.string rangeOfString:@"\n" options:NSBackwardsSearch];

		if(lastNewline.location == NSNotFound || lastNewline.location < NSMaxRange(insertionPoint))
		{
			if(control == self.findTextField)
				return [self showFindHistory:control], YES;
			else if(control == self.replaceTextField)
				return [self showReplaceHistory:control], YES;
		}
	}
	return NO;
}

- (void)controlTextDidChange:(NSNotification*)aNotification
{
	OakAutoSizingTextField* textField = [aNotification object];
	NSDictionary* userInfo = [aNotification userInfo];
	NSTextView* textView = userInfo[@"NSFieldEditor"];

	if(textView && textField)
		[textField updateIntrinsicContentSizeToEncompassString:textView.string];

	if(textField == self.replaceTextField && self.showReplacementPreviews)
		[self.resultsOutlineView reloadData];
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	BOOL res = YES;
	if(aMenuItem.action == @selector(toggleFollowSymbolicLinks:))
		[aMenuItem setState:self.followSymbolicLinks ? NSOnState : NSOffState];
	else if(aMenuItem.action == @selector(toggleSearchHiddenFolders:))
		[aMenuItem setState:self.searchHiddenFolders ? NSOnState : NSOffState];
	else if(aMenuItem.action == @selector(takeLevelToFoldFrom:) && aMenuItem.tag == -1)
		[aMenuItem setTitle:self.showResultsCollapsed ? @"Expand Results" : @"Collapse Results"];
	else if(aMenuItem.action == @selector(goToParentFolder:))
		res = self.searchFolder != nil;
	return res;
}
@end
