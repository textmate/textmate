#import "FindWindowController.h"
#import "FFResultsViewController.h"
#import "FFFolderMenu.h"
#import "CommonAncestor.h"
#import "Strings.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakSyntaxFormatter.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakHistoryList.h>
#import <OakFoundation/OakFoundation.h>
#import <MenuBuilder/MenuBuilder.h>
#import <Preferences/Keys.h>
#import <ns/ns.h>
#import <io/path.h>
#import <regexp/regexp.h>

NSString* const kUserDefaultsFolderOptionsKey     = @"Folder Search Options";
NSString* const kUserDefaultsFindResultsHeightKey = @"findResultsHeight";
NSString* const kUserDefaultsDefaultFindGlobsKey  = @"defaultFindInFolderGlobs";

NSButton* OakCreateClickableStatusBar ()
{
	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	[res.cell setLineBreakMode:NSLineBreakByTruncatingTail];
	res.alignment  = NSTextAlignmentLeft;
	res.bordered   = NO;
	res.buttonType = NSToggleButton;
	res.title      = @"";

	[res setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationVertical];
	return res;
}

static NSComboBox* OakCreateFindPanelComboBox (id <NSComboBoxDelegate> delegate, NSView* labelView)
{
	NSComboBox* res = OakCreateComboBox(labelView);
	res.delegate = delegate;

	res.cell.wraps = YES;
	res.numberOfVisibleItems = 12;
	[res setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationVertical];
	return res;
}

static NSProgressIndicator* OakCreateProgressIndicator ()
{
	NSProgressIndicator* res = [[NSProgressIndicator alloc] initWithFrame:NSZeroRect];
	res.style                = NSProgressIndicatorSpinningStyle;
	res.controlSize          = NSControlSizeSmall;
	res.displayedWhenStopped = NO;
	return res;
}

static NSButton* OakCreateStopSearchButton ()
{
	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	res.buttonType    = NSMomentaryChangeButton;
	res.bordered      = NO;
	res.image         = [NSImage imageNamed:NSImageNameStopProgressFreestandingTemplate];
	res.imagePosition = NSImageOnly;
	res.toolTip       = @"Stop Search";
	res.keyEquivalent = @".";
	res.keyEquivalentModifierMask = NSEventModifierFlagCommand;
	[res.cell setImageScaling:NSImageScaleProportionallyDown];
	res.accessibilityLabel = res.toolTip;
	return res;
}

static OakSyntaxFormatter* OakCreateSyntaxFormatter (NSString* grammarName)
{
	OakSyntaxFormatter* res = [[OakSyntaxFormatter alloc] initWithGrammarName:grammarName];
	res.enabled = YES;
	return res;
}

@interface FindWindowController () <NSComboBoxDelegate, NSWindowDelegate, NSMenuDelegate, NSPopoverDelegate, NSTextStorageDelegate>
@property (nonatomic) NSTextField*              findLabel;
@property (nonatomic) NSComboBox*               findComboBox;
@property (nonatomic) OakSyntaxFormatter*       findStringFormatter;
@property (nonatomic) OakSyntaxFormatter*       findCellFormatter;
@property (nonatomic) OakSyntaxFormatter*       findHistoryFormatter;
@property (nonatomic) BOOL                      findBoxListIsOpen;

@property (nonatomic) NSButton*                 countButton;

@property (nonatomic) NSTextField*              replaceLabel;
@property (nonatomic) NSComboBox*               replaceComboBox;
@property (nonatomic) OakSyntaxFormatter*       replaceStringFormatter;
@property (nonatomic) OakSyntaxFormatter*       replaceCellFormatter;
@property (nonatomic) BOOL                      replaceBoxListIsOpen;

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

@property (nonatomic) NSProgressIndicator*      progressIndicator;
@property (nonatomic) NSButton*                 statusTextField;

@property (nonatomic, readwrite) NSButton*      findAllButton;
@property (nonatomic, readwrite) NSButton*      replaceAllButton;
@property (nonatomic, readwrite) NSButton*      replaceButton;
@property (nonatomic, readwrite) NSButton*      replaceAndFindButton;
@property (nonatomic, readwrite) NSButton*      findPreviousButton;
@property (nonatomic, readwrite) NSButton*      findNextButton;
@property (nonatomic, readwrite) NSButton*      stopSearchButton;

@property (nonatomic) NSPopover*                findStringPopver;

@property (nonatomic) NSObjectController*       objectController;
@property (nonatomic) OakHistoryList*           globHistoryList;
@property (nonatomic) OakHistoryList*           recentFolders;
@property (nonatomic) NSMutableArray*           myConstraints;

@property (nonatomic, readonly) BOOL            canIgnoreWhitespace;
@property (nonatomic) CGFloat                   findResultsHeight;
@end

@implementation FindWindowController
+ (NSSet*)keyPathsForValuesAffectingCanIgnoreWhitespace  { return [NSSet setWithObject:@"regularExpression"]; }
+ (NSSet*)keyPathsForValuesAffectingIgnoreWhitespace     { return [NSSet setWithObject:@"regularExpression"]; }
+ (NSSet*)keyPathsForValuesAffectingCanEditGlob          { return [NSSet setWithObject:@"searchTarget"]; }
+ (NSSet*)keyPathsForValuesAffectingCanReplaceInDocument { return [NSSet setWithObject:@"searchTarget"]; }

+ (void)initialize
{
	[NSUserDefaults.standardUserDefaults registerDefaults:@{
		kUserDefaultsDefaultFindGlobsKey: @[ @"*", @"*.txt", @"*.{c,h}" ],
	}];
}

- (id)init
{
	NSRect r = [[NSScreen mainScreen] visibleFrame];
	if((self = [super initWithWindow:[[NSPanel alloc] initWithContentRect:NSMakeRect(NSMidX(r)-100, NSMidY(r)+100, 200, 200) styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskMiniaturizable) backing:NSBackingStoreBuffered defer:NO]]))
	{
		_projectFolder = NSHomeDirectory();

		self.resultsViewController     = [FFResultsViewController new];

		self.window.frameAutosaveName  = @"Find";
		self.window.hidesOnDeactivate  = NO;
		self.window.collectionBehavior = NSWindowCollectionBehaviorMoveToActiveSpace|NSWindowCollectionBehaviorFullScreenAuxiliary;
		self.window.delegate           = self;
		self.window.restorable         = NO;

		self.findLabel                 = OakCreateLabel(@"Find:");
		self.findComboBox              = OakCreateFindPanelComboBox(self, self.findLabel);
		self.findStringFormatter       = OakCreateSyntaxFormatter(@"source.regexp.oniguruma");
		self.findCellFormatter         = [_findStringFormatter copy];
		_findComboBox.formatter        = _findCellFormatter;
		self.findHistoryFormatter      = [_findStringFormatter copy];

		self.countButton               = OakCreateButton(@"Σ", NSSmallSquareBezelStyle);
		self.countButton.toolTip = @"Show Results Count";
		self.countButton.accessibilityLabel = self.countButton.toolTip;

		self.replaceLabel              = OakCreateLabel(@"Replace:");
		self.replaceComboBox           = OakCreateFindPanelComboBox(self, self.replaceLabel);
		self.replaceStringFormatter    = OakCreateSyntaxFormatter(@"textmate.format-string");
		self.replaceCellFormatter      = [_replaceStringFormatter copy];
		_replaceComboBox.formatter     = _replaceStringFormatter;

		self.optionsLabel              = OakCreateLabel(@"Options:");

		self.ignoreCaseCheckBox        = OakCreateCheckBox(@"Ignore Case");
		self.ignoreWhitespaceCheckBox  = OakCreateCheckBox(@"Ignore Whitespace");
		self.regularExpressionCheckBox = OakCreateCheckBox(@"Regular Expression");
		self.wrapAroundCheckBox        = OakCreateCheckBox(@"Wrap Around");

		self.whereLabel                = OakCreateLabel(@"In:");
		self.wherePopUpButton          = OakCreatePopUpButton(NO, nil, self.whereLabel);
		self.matchingLabel             = OakCreateLabel(@"matching");
		self.globTextField             = OakCreateComboBox(self.matchingLabel);
		self.actionsPopUpButton        = OakCreateActionPopUpButton(YES /* bordered */);

		self.progressIndicator         = OakCreateProgressIndicator();
		self.statusTextField           = OakCreateClickableStatusBar();

		self.findAllButton             = OakCreateButton(@"Find All");
		self.replaceAllButton          = OakCreateButton(@"Replace All");
		self.replaceButton             = OakCreateButton(@"Replace");
		self.replaceAndFindButton      = OakCreateButton(@"Replace & Find");
		self.findPreviousButton        = OakCreateButton(@"Previous");
		self.findNextButton            = OakCreateButton(@"Next");
		self.stopSearchButton          = OakCreateStopSearchButton();

		[self updateWindowTitle];
		[self updateSearchInPopUpMenu];

		// =============================
		// = Create action pop-up menu =
		// =============================

		MBMenu const items = {
			{ /* Placeholder */ },
			{ @"Search",                               @selector(nop:)                                    },
			{ @"Binary Files",                         @selector(toggleSearchBinaryFiles:),   .indent = 1 },
			{ @"Hidden Folders",                       @selector(toggleSearchHiddenFolders:), .indent = 1 },
			{ @"Symbolic Links to Folders",            @selector(toggleSearchFolderLinks:),   .indent = 1 },
			{ @"Symbolic Links to Files",              @selector(toggleSearchFileLinks:),     .indent = 1 },
			{ /* -------- */ },
			{ @"Collapse Results",                     @selector(toggleCollapsedState:),      @"1", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .target = self.resultsViewController },
			{ @"Select Result",                        .delegate = self                                   },
			{ /* -------- */ },
			{ @"Copy Matching Parts",                  @selector(copyMatchingParts:)                      },
			{ @"Copy Matching Parts With Filenames",   @selector(copyMatchingPartsWithFilename:)          },
			{ @"Copy Entire Lines",                    @selector(copyEntireLines:)                        },
			{ @"Copy Entire Lines With Filenames",     @selector(copyEntireLinesWithFilename:)            },
			{ @"Copy Replacements",                    @selector(copyReplacements:)                       },
			{ /* -------- */ },
			{ @"Check All",                            @selector(checkAll:)                               },
			{ @"Uncheck All",                          @selector(uncheckAll:)                             },
		};

		if(NSMenu* actionMenu = MBCreateMenu(items))
			self.actionsPopUpButton.menu = actionMenu;

		// =============================

		self.countButton.action           = @selector(countOccurrences:);
		self.findAllButton.action         = @selector(findAll:);
		self.replaceAllButton.action      = @selector(replaceAll:);
		self.replaceButton.action         = @selector(replace:);
		self.replaceAndFindButton.action  = @selector(replaceAndFind:);
		self.findPreviousButton.action    = @selector(findPrevious:);
		self.findNextButton.action        = @selector(findNext:);
		self.stopSearchButton.action      = @selector(stopSearch:);

		self.objectController = [[NSObjectController alloc] initWithContent:self];
		self.globHistoryList  = [[OakHistoryList alloc] initWithName:@"Find in Folder Globs.default" stackSize:10 fallbackUserDefaultsKey:kUserDefaultsDefaultFindGlobsKey];
		self.recentFolders    = [[OakHistoryList alloc] initWithName:@"findRecentPlaces" stackSize:21];

		[self.findComboBox              bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.findString"           options:
			@{ NSContinuouslyUpdatesValueBindingOption: @YES,
		          NSValueTransformerNameBindingOption: _findStringFormatter.transformerName }];
		[self.replaceComboBox           bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.replaceString"        options:
			@{ NSContinuouslyUpdatesValueBindingOption: @YES,
		          NSValueTransformerNameBindingOption: _replaceStringFormatter.transformerName }];
		[self.globTextField             bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.globHistoryList.head" options:nil];
		[self.globTextField             bind:NSContentValuesBinding toObject:_objectController withKeyPath:@"content.globHistoryList.list" options:nil];
		[self.globTextField             bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.canEditGlob"          options:nil];
		[self.ignoreCaseCheckBox        bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.ignoreCase"           options:nil];
		[self.ignoreWhitespaceCheckBox  bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.ignoreWhitespace"     options:nil];
		[self.regularExpressionCheckBox bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.regularExpression"    options:nil];
		[self.wrapAroundCheckBox        bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.wrapAround"           options:nil];
		[self.ignoreWhitespaceCheckBox  bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.canIgnoreWhitespace"  options:nil];
		[self.replaceButton             bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.canReplaceInDocument" options:nil];
		[self.replaceAndFindButton      bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.canReplaceInDocument" options:nil];

		[self.countButton               bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];
		[self.findAllButton             bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];
		[self.replaceAllButton          bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];
		[self.replaceAndFindButton      bind:@"enabled2"            toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];
		[self.findPreviousButton        bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];
		[self.findNextButton            bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];

		[self.resultsViewController     bind:@"replaceString"       toObject:_objectController withKeyPath:@"content.replaceString"        options:nil];

		[[OakPasteboard pasteboardWithName:NSFindPboard] bindComboBoxToPasteboardHistory:self.findComboBox valueTransformerName:self.findHistoryFormatter.pasteboardEntryRegexOnlyTransformerName];
		[[OakPasteboard pasteboardWithName:OakReplacePboard] bindComboBoxToPasteboardHistory:self.replaceComboBox valueTransformerName:self.replaceStringFormatter.pasteboardEntryTransformerName];

		NSView* contentView = self.window.contentView;
		OakAddAutoLayoutViewsToSuperview([self.allViews allValues], contentView);

		[self updateConstraints];

		self.window.defaultButtonCell = self.findNextButton.cell;

		// setup find/replace strings/options
		[self userDefaultsDidChange:nil];
		[self findClipboardDidChange:nil];
		[self replaceClipboardDidChange:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(findClipboardDidChange:) name:OakPasteboardDidChangeNotification object:[OakPasteboard pasteboardWithName:NSFindPboard]];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(replaceClipboardDidChange:) name:OakPasteboardDidChangeNotification object:[OakPasteboard pasteboardWithName:OakReplacePboard]];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(textViewWillPerformFindOperation:) name:@"OakTextViewWillPerformFindOperation" object:nil];

		// Register to application activation/deactivation notification so we can tweak our collection behavior
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidActivate:) name:NSApplicationDidBecomeActiveNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidDeactivate:) name:NSApplicationDidResignActiveNotification object:nil];

		[self.window addObserver:self forKeyPath:@"firstResponder" options:0 context:NULL];

		[self initializeFormatterEnabledStates];
	}
	return self;
}

- (void)dealloc
{
	[[OakPasteboard pasteboardWithName:NSFindPboard] unbindComboBoxFromPasteboardHistory:_findComboBox];
	[[OakPasteboard pasteboardWithName:OakReplacePboard] unbindComboBoxFromPasteboardHistory:_replaceComboBox];
	[self.window removeObserver:self forKeyPath:@"firstResponder"];
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)applicationDidActivate:(NSNotification*)notification
{
	// Starting with 10.11 behavior must be changed after current event loop cycle <rdar://23587833>
	dispatch_async(dispatch_get_main_queue(), ^{
		self.window.collectionBehavior |= NSWindowCollectionBehaviorMoveToActiveSpace;
	});
}

- (void)applicationDidDeactivate:(NSNotification*)notification
{
	// Starting with 10.11 behavior must be changed after current event loop cycle <rdar://23587833>
	dispatch_async(dispatch_get_main_queue(), ^{
		self.window.collectionBehavior &= ~NSWindowCollectionBehaviorMoveToActiveSpace;
	});
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	[aMenu removeAllItems];
	[NSApp sendAction:@selector(updateSelectTabMenu:) to:nil from:aMenu];
}

- (NSDictionary*)allViews
{
	NSDictionary* views = @{
		@"findLabel":         self.findLabel,
		@"find":              self.findComboBox,
		@"count":             self.countButton,
		@"replaceLabel":      self.replaceLabel,
		@"replace":           self.replaceComboBox,

		@"optionsLabel":      self.optionsLabel,
		@"regularExpression": self.regularExpressionCheckBox,
		@"ignoreWhitespace":  self.ignoreWhitespaceCheckBox,
		@"ignoreCase":        self.ignoreCaseCheckBox,
		@"wrapAround":        self.wrapAroundCheckBox,

		@"whereLabel":        self.whereLabel,
		@"where":             self.wherePopUpButton,
		@"matching":          self.matchingLabel,
		@"glob":              self.globTextField,
		@"actions":           self.actionsPopUpButton,

		@"results":           self.showsResultsOutlineView ? self.resultsViewController.view : [NSNull null],
		@"status":            self.statusTextField,

		@"findAll":           self.findAllButton,
		@"replaceAll":        self.replaceAllButton,
		@"replaceButton":     self.replaceButton,
		@"replaceAndFind":    self.replaceAndFindButton,
		@"previous":          self.findPreviousButton,
		@"next":              self.findNextButton,
	};

	if(self.isBusy)
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:views];
		[dict addEntriesFromDictionary:@{
			@"busy":       self.progressIndicator,
			@"stopSearch": self.stopSearchButton,
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
	CONSTRAINT(@"H:[find]-(5)-[count(==21)]-|",                        NSLayoutFormatAlignAllTop);
	CONSTRAINT(@"V:[count(==21)]",                                     NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight);
	CONSTRAINT(@"H:|-(>=20,==20@75)-[replaceLabel]-[replace]",         0);
	CONSTRAINT(@"V:|-[find(==20)]-[replace(==20)]",                                NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight);

	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.findLabel attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self.findComboBox attribute:NSLayoutAttributeTop multiplier:1 constant:3]];
	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.replaceLabel attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self.replaceComboBox attribute:NSLayoutAttributeTop multiplier:1 constant:3]];

	CONSTRAINT(@"H:|-(>=20,==20@75)-[optionsLabel]-[regularExpression]-[ignoreWhitespace]-(>=20)-|", NSLayoutFormatAlignAllBaseline);
	CONSTRAINT(@"H:[ignoreCase(==regularExpression)]-[wrapAround(==ignoreWhitespace)]",              NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom);
	CONSTRAINT(@"V:[replace]-[regularExpression]-[ignoreCase]",                                      NSLayoutFormatAlignAllLeft);
	CONSTRAINT(@"V:[replace]-[ignoreWhitespace]-[wrapAround]",                                       0);

	CONSTRAINT(@"H:|-(>=20,==20@75)-[whereLabel]-[where(<=180)]-[matching]", NSLayoutFormatAlignAllBaseline);
	CONSTRAINT(@"H:[matching]-[glob]-[actions]-(>=20@25)-|",                 0);
	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.actionsPopUpButton attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self.globTextField    attribute:NSLayoutAttributeTop multiplier:1 constant:0]];
	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.actionsPopUpButton attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self.wherePopUpButton attribute:NSLayoutAttributeTop multiplier:1 constant:0]];

	CONSTRAINT(@"V:[ignoreCase]-[where]",                                    NSLayoutFormatAlignAllLeft);
	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.countButton attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:self.actionsPopUpButton attribute:NSLayoutAttributeRight multiplier:1 constant:0]];

	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.findLabel attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:self.replaceLabel attribute:NSLayoutAttributeRight multiplier:1 constant:0]];
	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.findLabel attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:self.optionsLabel attribute:NSLayoutAttributeRight multiplier:1 constant:0]];
	[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self.findLabel attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:self.whereLabel   attribute:NSLayoutAttributeRight multiplier:1 constant:0]];

	if(self.showsResultsOutlineView)
	{
		CONSTRAINT(@"H:|[results]|", 0);
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[where]-[results(>=50,==height@490)]-(8)-[status]" options:0 metrics:@{ @"height": @(self.findResultsHeight) } views:views]];
	}
	else
	{
		CONSTRAINT(@"V:[where]-(8)-[status]", 0);
	}

	if(self.isBusy)
	{
		CONSTRAINT(@"H:|-[stopSearch(==13)]-(==6)-[busy]-[status]-|", NSLayoutFormatAlignAllCenterY);
	}
	else
	{
		CONSTRAINT(@"H:|-[status]-|", 0);
	}

	CONSTRAINT(@"H:|-[findAll]-[replaceAll]-(>=8)-[replaceButton]-[replaceAndFind]-[previous]-[next]-|", NSLayoutFormatAlignAllBottom);
	CONSTRAINT(@"V:[status]-(8)-[findAll]-|", 0);

	[self.window.contentView addConstraints:_myConstraints];

	if(self.showsResultsOutlineView)
			OakSetupKeyViewLoop(@[ self.findComboBox, self.replaceComboBox, self.countButton, self.regularExpressionCheckBox, self.ignoreWhitespaceCheckBox, self.ignoreCaseCheckBox, self.wrapAroundCheckBox, self.wherePopUpButton, self.globTextField, self.actionsPopUpButton, self.resultsViewController.outlineView, self.findAllButton, self.replaceAllButton, self.replaceButton, self.replaceAndFindButton, self.findPreviousButton, self.findNextButton ]);
	else	OakSetupKeyViewLoop(@[ self.findComboBox, self.replaceComboBox, self.countButton, self.regularExpressionCheckBox, self.ignoreWhitespaceCheckBox, self.ignoreCaseCheckBox, self.wrapAroundCheckBox, self.wherePopUpButton, self.globTextField, self.actionsPopUpButton, self.findAllButton, self.replaceAllButton, self.replaceButton, self.replaceAndFindButton, self.findPreviousButton, self.findNextButton ]);
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.ignoreCase = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindIgnoreCase];
	self.wrapAround = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindWrapAround];

	NSDictionary* options = [[NSUserDefaults standardUserDefaults] dictionaryForKey:kUserDefaultsFolderOptionsKey];
	self.searchHiddenFolders = [[options objectForKey:@"searchHiddenFolders"] boolValue];
	self.searchFolderLinks   = [[options objectForKey:@"searchFolderLinks"] boolValue];
	self.searchFileLinks     = ![[options objectForKey:@"skipFileLinks"] boolValue];
	self.searchBinaryFiles   = [[options objectForKey:@"searchBinaryFiles"] boolValue];
}

- (void)findClipboardDidChange:(NSNotification*)aNotification
{
	OakPasteboardEntry* entry = [[OakPasteboard pasteboardWithName:NSFindPboard] current];
	self.findString        = entry.string;
	self.regularExpression = entry.regularExpression;
	self.ignoreWhitespace  = entry.ignoreWhitespace;
	self.fullWords         = entry.fullWordMatch;

	[[OakPasteboard pasteboardWithName:NSFindPboard] updateBoundComboBoxNow:_findComboBox valueTransformerName:_findHistoryFormatter.pasteboardEntryRegexOnlyTransformerName];
}

- (void)replaceClipboardDidChange:(NSNotification*)aNotification
{
	self.replaceString = [[[OakPasteboard pasteboardWithName:OakReplacePboard] current] string];

	[[OakPasteboard pasteboardWithName:OakReplacePboard] updateBoundComboBoxNow:_replaceComboBox valueTransformerName:_replaceStringFormatter.pasteboardEntryTransformerName];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if([keyPath isEqualToString:@"firstResponder"])
	{
		NSResponder* firstResponder = [self.window firstResponder];
		self.resultsViewController.showReplacementPreviews = firstResponder == self.replaceComboBox || firstResponder == self.replaceComboBox.currentEditor;

		if([firstResponder isKindOfClass:[NSTextView class]])
		{
			NSTextView* textView = (NSTextView*)firstResponder;
			if(textView.isFieldEditor)
			{
				BOOL enable = _findComboBox.currentEditor || _replaceComboBox.currentEditor;
				if(textView.textStorage.delegate = enable ? self : nil)
					[self addStylesToFieldEditor];
			}
		}
	}
}

- (void)updateWindowTitle
{
	if(NSString* folder = self.searchFolder)
		self.window.title = [NSString localizedStringWithFormat:MSG_FIND_IN_FOLDER_WINDOW_TITLE, [folder stringByAbbreviatingWithTildeInPath]];
	else if(_searchTarget == FFSearchTargetOpenFiles)
		self.window.title = MSG_FIND_IN_OPEN_FILES_WINDOW_TITLE;
	else
		self.window.title = MSG_WINDOW_TITLE;
}

- (void)showWindow:(id)sender
{
	BOOL isVisibleAndKey = [self isWindowLoaded] && [self.window isVisible] && [self.window isKeyWindow];
	[super showWindow:sender];
	if(!isVisibleAndKey || ![[self.window firstResponder] isKindOfClass:[NSTextView class]])
		[self.window makeFirstResponder:self.findComboBox];
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
		OakFindRegularExpressionOption: @(self.regularExpression),
		OakFindIgnoreWhitespaceOption:  @(self.ignoreWhitespace),
		OakFindFullWordsOption:         @(self.fullWords),
	};

	if(OakNotEmptyString(_findString))
	{
		OakPasteboardEntry* oldEntry = [[OakPasteboard pasteboardWithName:NSFindPboard] current];
		if(oldEntry && [oldEntry.string isEqualToString:_findString])
		{
			if(![oldEntry.options isEqualToDictionary:newOptions])
				oldEntry.options = newOptions;
			oldEntry.date = [NSDate date];
		}
		else
		{
			[[OakPasteboard pasteboardWithName:NSFindPboard] addEntryWithString:_findString andOptions:newOptions];
		}
	}

	if(_replaceString)
	{
		OakPasteboardEntry* oldEntry = [[OakPasteboard pasteboardWithName:OakReplacePboard] current];
		if(oldEntry && [oldEntry.string isEqualToString:_replaceString])
				oldEntry.date = [NSDate date];
		else	[[OakPasteboard pasteboardWithName:OakReplacePboard] addEntryWithString:_replaceString];
	}

	return res;
}

- (void)windowDidResize:(NSNotification*)aNotification
{
	if(self.showsResultsOutlineView)
		self.findResultsHeight = NSHeight(self.resultsViewController.view.frame);
}

- (void)windowDidResignKey:(NSNotification*)aNotification
{
	[self commitEditing];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self commitEditing];
}

- (void)textViewWillPerformFindOperation:(NSNotification*)aNotification
{
	if([self isWindowLoaded] && [self.window isVisible] && [self.window isKeyWindow])
		[self commitEditing];
}

// ==============================
// = Create “where” pop-up menu =
// ==============================

- (NSString*)displayNameForFolder:(NSString*)path
{
	std::vector<std::string> paths;
	for(NSUInteger i = 0; i < [self.recentFolders count]; ++i)
		paths.push_back(to_s([self.recentFolders objectAtIndex:i]));
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
	NSMenu* whereMenu = _wherePopUpButton.menu;
	[whereMenu removeAllItems];

	NSMenuItem* documentItem    = [whereMenu addItemWithTitle:@"Document"           action:@selector(takeSearchTargetFrom:) keyEquivalent:@"f"];
	NSMenuItem* selectionItem   = [whereMenu addItemWithTitle:@"Selection"          action:@selector(takeSearchTargetFrom:) keyEquivalent:@""];
	[whereMenu addItem:[NSMenuItem separatorItem]];
	NSMenuItem* openFilesItem   = [whereMenu addItemWithTitle:@"Open Files"         action:@selector(takeSearchTargetFrom:) keyEquivalent:@""];
	NSMenuItem* projectItem     = [whereMenu addItemWithTitle:@"Project Folder"     action:@selector(takeSearchTargetFrom:) keyEquivalent:@"F"];
	NSMenuItem* fileBrowserItem = [whereMenu addItemWithTitle:@"File Browser Items" action:@selector(takeSearchTargetFrom:) keyEquivalent:@""];
	NSMenuItem* otherItem       = [whereMenu addItemWithTitle:@"Other Folder…"      action:@selector(showFolderSelectionPanel:) keyEquivalent:@""];
	[whereMenu addItem:[NSMenuItem separatorItem]];
	NSMenuItem* folderItem      = [whereMenu addItemWithTitle:@"«Last Folder»"      action:@selector(takeSearchTargetFrom:) keyEquivalent:@""];
	[whereMenu addItem:[NSMenuItem separatorItem]];

	documentItem.tag    = FFSearchTargetDocument;
	selectionItem.tag   = FFSearchTargetSelection;
	openFilesItem.tag   = FFSearchTargetOpenFiles;
	projectItem.tag     = FFSearchTargetProject;
	fileBrowserItem.tag = FFSearchTargetFileBrowserItems;
	otherItem.tag       = FFSearchTargetOther;

	NSString* lastFolder = self.searchFolder ?: self.projectFolder;
	if(lastFolder)
	{
		[folderItem setTitle:[self displayNameForFolder:lastFolder]];
		[folderItem setIconForFile:lastFolder];
		[folderItem setRepresentedObject:lastFolder];
		[FFFolderMenu addFolderSubmenuToMenuItem:folderItem];
	}

	if(_searchTarget == FFSearchTargetProject || _searchTarget == FFSearchTargetOther || (_searchTarget == FFSearchTargetFileBrowserItems && _fileBrowserItems.count == 1))
			[_wherePopUpButton selectItem:folderItem];
	else	[_wherePopUpButton selectItemWithTag:_searchTarget];

	// =================
	// = Recent Places =
	// =================

	[whereMenu addItem:[NSMenuItem separatorItem]];
	[whereMenu addItemWithTitle:@"Recent Places" action:@selector(nop:) keyEquivalent:@""];

	for(NSUInteger i = 0; i < [self.recentFolders count]; ++i)
	{
		NSString* path = [self.recentFolders objectAtIndex:i];
		if([path isEqualToString:lastFolder] || [path isEqualToString:self.projectFolder])
			continue;
		if(![[NSFileManager defaultManager] fileExistsAtPath:path])
			continue;

		NSMenuItem* recentItem = [whereMenu addItemWithTitle:[self displayNameForFolder:path] action:@selector(takeSearchTargetFrom:) keyEquivalent:@""];
		[recentItem setIconForFile:path];
		[recentItem setRepresentedObject:path];
	}
}

- (void)setSearchTarget:(FFSearchTarget)newTarget
{
	_searchTarget = newTarget;

	BOOL isFolderSearch = _searchTarget != FFSearchTargetDocument && _searchTarget != FFSearchTargetSelection;
	self.showsResultsOutlineView = isFolderSearch;

	BOOL isDirectory = NO;
	if(_searchTarget == FFSearchTargetOther && [[NSFileManager defaultManager] fileExistsAtPath:self.otherFolder isDirectory:&isDirectory] && isDirectory)
		[self.recentFolders addObject:self.otherFolder];

	[self updateSearchInPopUpMenu];
	[self updateWindowTitle];
}

- (void)takeSearchTargetFrom:(NSMenuItem*)menuItem
{
	if(NSString* folder = menuItem.representedObject)
	{
		self.otherFolder = folder;
		self.searchTarget = FFSearchTargetOther;
	}
	else
	{
		self.searchTarget = FFSearchTarget(menuItem.tag);
	}
}

// ==============================

- (void)expandComboBox:(NSComboBox*)comboBox
{
	// https://stackoverflow.com/questions/4499262/how-to-programmatically-open-an-nscomboboxs-list
	id accessibilityComboBox = NSAccessibilityUnignoredDescendant(comboBox);
	if(![accessibilityComboBox isAccessibilityExpanded]) {
		[self.window makeFirstResponder:comboBox];
		[accessibilityComboBox setAccessibilityExpanded:YES];
	}
}

- (IBAction)showFindHistory:(id)sender
{
	[self expandComboBox:self.findComboBox];
}

- (IBAction)showReplaceHistory:(id)sender
{
	[self expandComboBox:self.replaceComboBox];
}

- (void)popoverDidClose:(NSNotification*)aNotification
{
	self.findErrorString = nil;
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
			viewController.view = OakCreateLabel();

			self.findStringPopver = [NSPopover new];
			self.findStringPopver.behavior = NSPopoverBehaviorTransient;
			self.findStringPopver.contentViewController = viewController;
			self.findStringPopver.delegate = self;
		}

		NSTextField* textField = (NSTextField*)self.findStringPopver.contentViewController.view;
		textField.stringValue = _findErrorString;
		[textField sizeToFit];

		[self.findStringPopver showRelativeToRect:NSZeroRect ofView:self.findComboBox preferredEdge:NSMaxYEdge];
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

	NSView* view = self.resultsViewController.view;
	if(_showsResultsOutlineView = flag)
			OakAddAutoLayoutViewsToSuperview(@[ view ], self.window.contentView);
	else	[view removeFromSuperview];

	[self updateConstraints];

	if(flag && isWindowLoaded)
	{
		[self.window layoutIfNeeded];

		NSRect screenFrame = [[self.window screen] visibleFrame];
		NSRect windowFrame = self.window.frame;
		CGFloat minY = NSMinY(windowFrame);
		CGFloat maxY = NSMaxY(windowFrame);

		CGFloat currentHeight = NSHeight(self.resultsViewController.view.frame);
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

- (void)setBusy:(BOOL)busyFlag
{
	if(_busy == busyFlag)
		return;

	NSArray* busyViews = @[ self.stopSearchButton, self.progressIndicator ];
	if(_busy = busyFlag)
	{
		OakAddAutoLayoutViewsToSuperview(busyViews, self.window.contentView);
		[self.progressIndicator startAnimation:self];
	}
	else
	{
		[self.progressIndicator stopAnimation:self];
		[busyViews makeObjectsPerformSelector:@selector(removeFromSuperview)];
	}
	[self updateConstraints];
}

- (id)formatStatusString:(NSString*)aString
{
	if(OakIsEmptyString(aString))
		return @" ";

	static NSAttributedString* const lineJoiner = [[NSAttributedString alloc] initWithString:@"¬" attributes:@{ NSForegroundColorAttributeName: [NSColor lightGrayColor] }];
	static NSAttributedString* const tabJoiner  = [[NSAttributedString alloc] initWithString:@"‣" attributes:@{ NSForegroundColorAttributeName: [NSColor lightGrayColor] }];

	NSMutableAttributedString* res = [[NSMutableAttributedString alloc] init];

	__block bool firstLine = true;
	[aString enumerateLinesUsingBlock:^(NSString* line, BOOL* stop){
		if(!std::exchange(firstLine, false))
			[res appendAttributedString:lineJoiner];

		bool firstTab = true;
		for(NSString* str in [line componentsSeparatedByString:@"\t"])
		{
			if(!std::exchange(firstTab, false))
				[res appendAttributedString:tabJoiner];
			[res appendAttributedString:[[NSAttributedString alloc] initWithString:str]];
		}
	}];

	NSMutableParagraphStyle* paragraphStyle = [[NSMutableParagraphStyle alloc] init];
	[paragraphStyle setLineBreakMode:NSLineBreakByTruncatingMiddle];
	NSDictionary* globalAttrs = @{
		NSParagraphStyleAttributeName:  paragraphStyle,
		NSForegroundColorAttributeName: [NSColor textColor],
		NSFontAttributeName:            OakStatusBarFont(),
	};
	[res addAttributes:globalAttrs range:NSMakeRange(0, [[res string] length])];

	return res;
}

- (void)setStatusString:(NSString*)aString
{
	self.statusTextField.attributedTitle = [self formatStatusString:_statusString = aString];
	self.alternateStatusString = nil;
}

- (void)setAlternateStatusString:(NSString*)aString
{
	self.statusTextField.attributedAlternateTitle = (_alternateStatusString = aString) ? [self formatStatusString:aString] : nil;
}

- (NSString*)searchFolder
{
	if(_searchTarget == FFSearchTargetProject)
		return self.projectFolder;
	else if(_searchTarget == FFSearchTargetFileBrowserItems && _fileBrowserItems.count == 1)
		return _fileBrowserItems.firstObject;
	else if(_searchTarget == FFSearchTargetOther)
		return self.otherFolder;
	return nil;
}

- (IBAction)goToParentFolder:(id)sender
{
	if(_searchTarget == FFSearchTargetFileBrowserItems)
	{
		self.otherFolder = CommonAncestor(_fileBrowserItems);
		self.searchTarget = FFSearchTargetOther;
	}
	else if(NSString* parent = [self.searchFolder stringByDeletingLastPathComponent])
	{
		self.otherFolder = parent;
		self.searchTarget = FFSearchTargetOther;
	}
}

- (void)setFindString:(NSString*)aString
{
	if(_findString == aString || [_findString isEqualToString:aString])
		return;

	_findString = aString ?: @"";

	if(self.findErrorString)
		[self updateFindErrorString];
}

- (void)setReplaceString:(NSString*)aString
{
	_replaceString = aString ?: @"";
}

- (void)setFindResultsHeight:(CGFloat)height { [[NSUserDefaults standardUserDefaults] setInteger:height forKey:kUserDefaultsFindResultsHeightKey]; }
- (CGFloat)findResultsHeight                 { return [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsFindResultsHeightKey] ?: 200; }

- (void)setRegularExpression:(BOOL)flag
{
	if(_regularExpression == flag)
		return;

	_regularExpression = flag;
	if(self.findErrorString)
		[self updateFindErrorString];

	_findStringFormatter.enabled    = flag;
	_findCellFormatter.enabled      = flag && !_findBoxListIsOpen;
	_replaceStringFormatter.enabled = flag;
	_replaceCellFormatter.enabled   = flag && !_replaceBoxListIsOpen;

	// Re-format current value
	if(!_findComboBox.currentEditor)
	{
		_findComboBox.objectValue = nil;
		_findComboBox.objectValue = _findString;
	}

	if(!_replaceComboBox.currentEditor)
	{
		_replaceComboBox.objectValue = nil;
		_replaceComboBox.objectValue = _replaceString;
	}

	[self addStylesToFieldEditor];
}

- (void)textStorageDidProcessEditing:(NSNotification*)aNotification
{
	[self addStylesToFieldEditor];
}

- (void)initializeFormatterEnabledStates
{
	_findStringFormatter.enabled    = _regularExpression;
	_findCellFormatter.enabled      = _regularExpression;
	_replaceStringFormatter.enabled = _regularExpression;
	_replaceCellFormatter.enabled   = _regularExpression;
}

- (void)addStylesToTextFieldIfHasFieldEditor:(NSTextField*)textField syntaxFormatter:(OakSyntaxFormatter*)syntaxFormatter cellFormatter:(OakSyntaxFormatter*)cellFormatter listIsOpen:(BOOL)listIsOpen
{
	cellFormatter.enabled = _regularExpression && !listIsOpen;
	if(NSTextView* editorTextView = (NSTextView*)textField.currentEditor)
		[syntaxFormatter addStylesToString:editorTextView.textStorage];
}

- (void)addStylesToFieldEditor
{
	[self addStylesToTextFieldIfHasFieldEditor:_findComboBox syntaxFormatter:_findStringFormatter cellFormatter:_findCellFormatter listIsOpen:_findBoxListIsOpen];
	[self addStylesToTextFieldIfHasFieldEditor:_replaceComboBox syntaxFormatter:_replaceStringFormatter cellFormatter:_replaceCellFormatter listIsOpen:_replaceBoxListIsOpen];
}

- (void)comboBoxWillPopUp:(NSNotification*)notification
{
	id comboBox = notification.object;
	if(comboBox == _findComboBox)
		_findBoxListIsOpen = YES;
	else if(comboBox == _replaceComboBox)
		_replaceBoxListIsOpen = YES;

	dispatch_async(dispatch_get_main_queue(), ^{
		// By next runloop cycle, the menu has *just* appeared onscreen
		[comboBox scrollItemAtIndexToTop:0];
	});
}
- (void)comboBoxWillDismiss:(NSNotification*)notification
{
	id comboBox = notification.object;
	if(comboBox == _findComboBox)
		_findBoxListIsOpen = NO;
	else if(comboBox == _replaceComboBox)
		_replaceBoxListIsOpen = NO;
}

- (void)setIgnoreCase:(BOOL)flag        { if(_ignoreCase != flag) [[NSUserDefaults standardUserDefaults] setObject:@(_ignoreCase = flag) forKey:kUserDefaultsFindIgnoreCase]; }
- (void)setWrapAround:(BOOL)flag        { if(_wrapAround != flag) [[NSUserDefaults standardUserDefaults] setObject:@(_wrapAround = flag) forKey:kUserDefaultsFindWrapAround]; }
- (BOOL)ignoreWhitespace                { return _ignoreWhitespace && self.canIgnoreWhitespace; }
- (BOOL)canIgnoreWhitespace             { return _regularExpression == NO; }

- (BOOL)canEditGlob                     { return _searchTarget != FFSearchTargetDocument && _searchTarget != FFSearchTargetSelection; }
- (BOOL)canReplaceInDocument            { return _searchTarget == FFSearchTargetDocument || _searchTarget == FFSearchTargetSelection; }

- (NSString*)globString                 { [self commitEditing]; return _globHistoryList.head; }
- (void)setGlobString:(NSString*)aGlob  { [_globHistoryList addObject:aGlob]; }

- (void)setProjectFolder:(NSString*)aFolder
{
	if(_projectFolder != aFolder && ![_projectFolder isEqualToString:aFolder])
	{
		_projectFolder = aFolder ?: @"";
		self.globHistoryList = [[OakHistoryList alloc] initWithName:[NSString stringWithFormat:@"Find in Folder Globs.%@", _projectFolder] stackSize:10 fallbackUserDefaultsKey:kUserDefaultsDefaultFindGlobsKey];
		[self updateSearchInPopUpMenu];
	}
}

- (void)updateFolderSearchUserDefaults
{
	NSMutableDictionary* options = [NSMutableDictionary dictionary];

	if(self.searchHiddenFolders) options[@"searchHiddenFolders"] = @YES;
	if(self.searchFolderLinks)   options[@"searchFolderLinks"]   = @YES;
	if(!self.searchFileLinks)    options[@"skipFileLinks"]       = @YES;
	if(self.searchBinaryFiles)   options[@"searchBinaryFiles"]   = @YES;

	if([options count])
			[[NSUserDefaults standardUserDefaults] setObject:options forKey:kUserDefaultsFolderOptionsKey];
	else	[[NSUserDefaults standardUserDefaults] removeObjectForKey:kUserDefaultsFolderOptionsKey];
}

- (void)setSearchHiddenFolders:(BOOL)flag { if(_searchHiddenFolders != flag) { _searchHiddenFolders = flag; [self updateFolderSearchUserDefaults]; } }
- (void)setSearchFolderLinks:(BOOL)flag   { if(_searchFolderLinks != flag)   { _searchFolderLinks   = flag; [self updateFolderSearchUserDefaults]; } }
- (void)setSearchFileLinks:(BOOL)flag     { if(_searchFileLinks != flag)     { _searchFileLinks     = flag; [self updateFolderSearchUserDefaults]; } }
- (void)setSearchBinaryFiles:(BOOL)flag   { if(_searchBinaryFiles != flag)   { _searchBinaryFiles   = flag; [self updateFolderSearchUserDefaults]; } }

- (IBAction)toggleSearchHiddenFolders:(id)sender { self.searchHiddenFolders = !self.searchHiddenFolders; }
- (IBAction)toggleSearchFolderLinks:(id)sender   { self.searchFolderLinks   = !self.searchFolderLinks;   }
- (IBAction)toggleSearchFileLinks:(id)sender     { self.searchFileLinks     = !self.searchFileLinks;     }
- (IBAction)toggleSearchBinaryFiles:(id)sender   { self.searchBinaryFiles   = !self.searchBinaryFiles;   }

- (IBAction)takeLevelToFoldFrom:(id)sender       { [self.resultsViewController toggleCollapsedState:sender];                    }
- (IBAction)selectNextResult:(id)sender          { [self.resultsViewController selectNextResultWrapAround:self.wrapAround];     }
- (IBAction)selectPreviousResult:(id)sender      { [self.resultsViewController selectPreviousResultWrapAround:self.wrapAround]; }
- (IBAction)selectNextTab:(id)sender             { [self.resultsViewController selectNextDocument:sender];                      }
- (IBAction)selectPreviousTab:(id)sender         { [self.resultsViewController selectPreviousDocument:sender];                  }

- (BOOL)control:(NSControl*)control textView:(NSTextView*)textView doCommandBySelector:(SEL)command
{
	if(command == @selector(moveDown:))
	{
		NSRange insertionPoint = [[textView.selectedRanges lastObject] rangeValue];
		NSRange lastNewline    = [textView.string rangeOfString:@"\n" options:NSBackwardsSearch];

		if(lastNewline.location == NSNotFound || lastNewline.location < NSMaxRange(insertionPoint))
		{
			if(control == self.findComboBox)
				return [self showFindHistory:control], YES;
			else if(control == self.replaceComboBox)
				return [self showReplaceHistory:control], YES;
		}
	}
	return NO;
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	BOOL res = YES;
	if(aMenuItem.action == @selector(toggleSearchHiddenFolders:))
		[aMenuItem setState:self.searchHiddenFolders ? NSOnState : NSOffState];
	else if(aMenuItem.action == @selector(toggleSearchFolderLinks:))
		[aMenuItem setState:self.searchFolderLinks ? NSOnState : NSOffState];
	else if(aMenuItem.action == @selector(toggleSearchFileLinks:))
		[aMenuItem setState:self.searchFileLinks ? NSOnState : NSOffState];
	else if(aMenuItem.action == @selector(toggleSearchBinaryFiles:))
		[aMenuItem setState:self.searchBinaryFiles ? NSOnState : NSOffState];
	else if(aMenuItem.action == @selector(goToParentFolder:))
		res = self.searchFolder != nil || _searchTarget == FFSearchTargetFileBrowserItems && CommonAncestor(_fileBrowserItems);
	return res;
}
@end
