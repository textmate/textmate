#import "Find.h"
#import "FFResultNode.h"
#import "FFResultsViewController.h"
#import "FFDocumentSearch.h"
#import "CommonAncestor.h"
#import "FFFolderMenu.h"
#import "FFStatusBarViewController.h"
#import <OakFoundation/OakFindProtocol.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/OakHistoryList.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakPasteboardSelector.h>
#import <OakAppKit/OakSyntaxFormatter.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <MenuBuilder/MenuBuilder.h>
#import <Preferences/Keys.h>
#import <ns/ns.h>
#import <text/types.h>
#import <text/utf8.h>
#import <regexp/format_string.h>
#import <regexp/regexp.h>
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>
#import <io/path.h>
#import <settings/settings.h>

OAK_DEBUG_VAR(Find_Base);

static NSString* const kUserDefaultsFolderOptionsKey               = @"Folder Search Options";
static NSString* const kUserDefaultsFindResultsHeightKey           = @"findResultsHeight";
static NSString* const kUserDefaultsDefaultFindGlobsKey            = @"defaultFindInFolderGlobs";
static NSString* const kUserDefaultsKeepSearchResultsOnDoubleClick = @"keepSearchResultsOnDoubleClick";
static NSString* const kSearchMarkIdentifier                       = @"search";

enum FindActionTag
{
	FindActionFindNext = 1,
	FindActionFindPrevious,
	FindActionCountMatches,
	FindActionFindAll,
	FindActionReplaceAll,
	FindActionReplaceAndFind,
	FindActionReplaceSelected,
	FindActionReplace,
};

@implementation FindMatch
- (instancetype)initWithUUID:(NSUUID*)uuid firstRange:(text::range_t const&)firstRange lastRange:(text::range_t const&)lastRange
{
	if(self = [super init])
	{
		_UUID       = uuid;
		_firstRange = firstRange;
		_lastRange  = lastRange;
	}
	return self;
}
@end

// ====================
// = Various controls =
// ====================

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

	self.myIntrinsicContentSize = NSMakeSize(NSViewNoIntrinsicMetric, MAX(22, MIN([cell cellSizeForBounds:NSMakeRect(0, 0, NSWidth([self bounds]), CGFLOAT_MAX)].height, 225)));
	[self invalidateIntrinsicContentSize];
}
@end

static OakAutoSizingTextField* OakCreateTextField (id <NSTextFieldDelegate> delegate, NSView* labelView, NSString* grammarName)
{
	OakAutoSizingTextField* res = [[OakAutoSizingTextField alloc] initWithFrame:NSZeroRect];
	res.font = OakControlFont();
	res.formatter = [[OakSyntaxFormatter alloc] initWithGrammarName:grammarName];
	[[res cell] setWraps:YES];
	res.accessibilityTitleUIElement = labelView;
	res.delegate = delegate;
	return res;
}

static NSButton* OakCreateHistoryButton (NSString* toolTip)
{
	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	res.bezelStyle = NSBezelStyleRoundedDisclosure;
	res.buttonType = NSMomentaryLightButton;
	res.title      = @"";
	res.toolTip    = toolTip;
	res.accessibilityLabel = toolTip;
	[res setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
	return res;
}

// ========================
// = FindWindowController =
// ========================

@interface FindWindowController : NSWindowController <NSTextFieldDelegate, NSWindowDelegate, NSMenuDelegate, NSPopoverDelegate, NSTextStorageDelegate>
{
	NSObjectController*        _objectController;

	OakAutoSizingTextField*    _findTextField;
	OakSyntaxFormatter*        _findStringFormatter;
	OakAutoSizingTextField*    _replaceTextField;
	OakSyntaxFormatter*        _replaceStringFormatter;
	NSPopUpButton*             _wherePopUpButton;
	NSButton*                  _findAllButton;
	NSButton*                  _findNextButton;

	FFStatusBarViewController* _statusBarViewController;
	NSGridView*                _gridView;
	NSStackView*               _actionButtonsStackView;

	BOOL                       _ignoreWhitespace;

	BOOL                       _findStringUpdated;
	BOOL                       _replaceStringUpdated;
}
@property (nonatomic, readonly)           FFResultsViewController* resultsViewController;
@property (nonatomic) BOOL                showsResultsOutlineView;

@property (nonatomic) FFSearchTarget      searchTarget;
@property (nonatomic) NSString*           projectFolder;
@property (nonatomic) NSArray*            fileBrowserItems;
@property (nonatomic) NSString*           otherFolder;

@property (nonatomic, readonly) NSString* searchFolder;

@property (nonatomic) NSString* findString;
@property (nonatomic) NSString* replaceString;
@property (nonatomic) NSString* globString;

@property (nonatomic) BOOL ignoreCase;
@property (nonatomic) BOOL ignoreWhitespace;
@property (nonatomic) BOOL regularExpression;
@property (nonatomic) BOOL wrapAround;
@property (nonatomic) BOOL fullWords; // not implemented

@property (nonatomic) BOOL searchHiddenFolders;
@property (nonatomic) BOOL searchFolderLinks;
@property (nonatomic) BOOL searchFileLinks;
@property (nonatomic) BOOL searchBinaryFiles;

@property (nonatomic, getter = isBusy) BOOL busy;
@property (nonatomic) NSString* statusString;
@property (nonatomic) NSString* alternateStatusString;
@property (nonatomic) NSString* findErrorString;

@property (nonatomic, readonly) NSButton*       replaceAllButton;
@property (nonatomic) NSPopover*                findStringPopver;

@property (nonatomic) OakHistoryList*           globHistoryList;
@property (nonatomic) OakHistoryList*           recentFolders;

@property (nonatomic, readonly) BOOL            canIgnoreWhitespace;
@property (nonatomic) CGFloat                   findResultsHeight;

@property (nonatomic) BOOL                      canEditGlob;
@property (nonatomic) BOOL                      canReplaceInDocument;
@end

@implementation FindWindowController
+ (NSSet*)keyPathsForValuesAffectingCanIgnoreWhitespace  { return [NSSet setWithObject:@"regularExpression"]; }
+ (NSSet*)keyPathsForValuesAffectingIgnoreWhitespace     { return [NSSet setWithObject:@"regularExpression"]; }

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
		_objectController = [[NSObjectController alloc] initWithContent:self];
		_projectFolder    = NSHomeDirectory();

		self.globHistoryList = [[OakHistoryList alloc] initWithName:@"Find in Folder Globs.default" stackSize:10 fallbackUserDefaultsKey:kUserDefaultsDefaultFindGlobsKey];
		self.recentFolders   = [[OakHistoryList alloc] initWithName:@"findRecentPlaces" stackSize:21];

		self.window.frameAutosaveName  = @"Find";
		self.window.hidesOnDeactivate  = NO;
		self.window.collectionBehavior = NSWindowCollectionBehaviorMoveToActiveSpace|NSWindowCollectionBehaviorFullScreenAuxiliary;
		self.window.delegate           = self;
		self.window.restorable         = NO;

		_resultsViewController = [[FFResultsViewController alloc] init];
		[_resultsViewController bind:@"replaceString" toObject:_objectController withKeyPath:@"content.replaceString" options:nil];

		_statusBarViewController = [[FFStatusBarViewController alloc] init];
		_statusBarViewController.stopAction = @selector(stopSearch:);
		_statusBarViewController.stopTarget = Find.sharedInstance;

		NSStackView* stackView = [NSStackView stackViewWithViews:@[
			self.gridView,
			_resultsViewController.view,
			_statusBarViewController.view,
			self.actionButtonsStackView,
		]];
		stackView.orientation = NSUserInterfaceLayoutOrientationVertical;
		stackView.alignment   = NSLayoutAttributeLeading;
		stackView.edgeInsets  = { .bottom = 20 };
		[stackView setHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationVertical];
		[stackView setHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		self.actionButtonsStackView.edgeInsets = { .left = 20, .right = 20 };

		_resultsViewController.view.hidden = YES;

		self.window.contentView = stackView;
		self.window.defaultButtonCell = _findNextButton.cell;

		[self updateWindowTitle];
		[self.window layoutIfNeeded]; // Incase autosaved window frame includes results, we want to shrink the frame

		OakSetupKeyViewLoop(@[ self.gridView, _resultsViewController.view, self.actionButtonsStackView ]);

		// setup find/replace strings/options
		[self userDefaultsDidChange:nil];
		[self findClipboardDidChange:nil];
		[self replaceClipboardDidChange:nil];

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:NSUserDefaults.standardUserDefaults];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(findClipboardDidChange:) name:OakPasteboardDidChangeNotification object:OakPasteboard.findPasteboard];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(replaceClipboardDidChange:) name:OakPasteboardDidChangeNotification object:OakPasteboard.replacePasteboard];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(textViewWillPerformFindOperation:) name:@"OakTextViewWillPerformFindOperation" object:nil];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(resultsFrameDidChange:) name:NSViewFrameDidChangeNotification object:_resultsViewController.view];

		// Register to application activation/deactivation notification so we can tweak our collection behavior
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(applicationDidActivate:) name:NSApplicationDidBecomeActiveNotification object:nil];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(applicationDidDeactivate:) name:NSApplicationDidResignActiveNotification object:nil];

		[self.window addObserver:self forKeyPath:@"firstResponder" options:0 context:NULL];
	}
	return self;
}

- (void)dealloc
{
	[self.window removeObserver:self forKeyPath:@"firstResponder"];
	[NSNotificationCenter.defaultCenter removeObserver:self];
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
	[NSApp sendAction:@selector(updateShowTabMenu:) to:nil from:aMenu];
}

- (NSGridView*)gridView
{
	if(!_gridView)
	{
		NSTextField* findLabel              = OakCreateLabel(@"Find:");
		_findTextField                      = OakCreateTextField(self, findLabel, @"source.regexp.oniguruma");
		_findStringFormatter                = _findTextField.formatter;
		NSButton* findHistoryButton         = OakCreateHistoryButton(@"Show Find History");

		NSButton* countButton               = OakCreateButton(@"Σ", NSBezelStyleSmallSquare);
		countButton.toolTip                 = @"Show Results Count";
		countButton.accessibilityLabel      = countButton.toolTip;

		NSTextField* replaceLabel           = OakCreateLabel(@"Replace:");
		_replaceTextField                   = OakCreateTextField(self, replaceLabel, @"textmate.format-string");
		_replaceStringFormatter             = _replaceTextField.formatter;
		NSButton* replaceHistoryButton      = OakCreateHistoryButton(@"Show Replace History");

		NSTextField* optionsLabel           = OakCreateLabel(@"Options:");

		NSButton* ignoreCaseCheckBox        = OakCreateCheckBox(@"Ignore Case");
		NSButton* ignoreWhitespaceCheckBox  = OakCreateCheckBox(@"Ignore Whitespace");
		NSButton* regularExpressionCheckBox = OakCreateCheckBox(@"Regular Expression");
		NSButton* wrapAroundCheckBox        = OakCreateCheckBox(@"Wrap Around");

		NSTextField* whereLabel             = OakCreateLabel(@"In:");
		_wherePopUpButton                   = OakCreatePopUpButton(NO, nil, whereLabel);
		NSTextField* matchingLabel          = OakCreateLabel(@"matching");
		NSComboBox* globTextField           = OakCreateComboBox(matchingLabel);
		NSPopUpButton* actionsPopUpButton   = OakCreateActionPopUpButton(YES /* bordered */);

		NSGridView* optionsGridView = [NSGridView gridViewWithViews:@[
			@[ regularExpressionCheckBox, ignoreWhitespaceCheckBox ],
			@[ ignoreCaseCheckBox,        wrapAroundCheckBox       ],
		]];

		optionsGridView.rowSpacing    = 8;
		optionsGridView.columnSpacing = 20;
		optionsGridView.rowAlignment  = NSGridRowAlignmentFirstBaseline;

		NSStackView* whereStackView = [NSStackView stackViewWithViews:@[
			_wherePopUpButton, matchingLabel, globTextField
		]];
		whereStackView.alignment = NSLayoutAttributeLastBaseline;
		[whereStackView setHuggingPriority:NSLayoutPriorityWindowSizeStayPut forOrientation:NSLayoutConstraintOrientationVertical];

		_gridView = [NSGridView gridViewWithViews:@[
			@[ findLabel,    _findTextField,    findHistoryButton,   countButton ],
			@[ replaceLabel, _replaceTextField, replaceHistoryButton             ],
			@[ optionsLabel, optionsGridView                                     ],
			@[ whereLabel,   whereStackView,    actionsPopUpButton               ],
		]];

		_gridView.rowSpacing    = 8;
		_gridView.columnSpacing = 4;
		_gridView.rowAlignment  = NSGridRowAlignmentFirstBaseline;

		[_gridView rowAtIndex:0].topPadding        = 20;
		[_gridView rowAtIndex:2].bottomPadding     = 12;
		[_gridView columnAtIndex:0].xPlacement     = NSGridCellPlacementTrailing;
		[_gridView columnAtIndex:0].leadingPadding = 20;
		[_gridView columnAtIndex:1].leadingPadding = 4;
		[_gridView columnAtIndex:3].leadingPadding = 4;
		[_gridView columnAtIndex:_gridView.numberOfColumns-1].trailingPadding = 20;

		[_gridView cellAtColumnIndex:2 rowIndex:0].yPlacement = NSGridCellPlacementTop;
		[_gridView cellAtColumnIndex:3 rowIndex:0].yPlacement = NSGridCellPlacementTop;
		[_gridView cellAtColumnIndex:2 rowIndex:1].yPlacement = NSGridCellPlacementTop;

		[_gridView mergeCellsInHorizontalRange:NSMakeRange(2, 2) verticalRange:NSMakeRange(3, 1)];
		[_gridView cellAtColumnIndex:2 rowIndex:3].xPlacement = NSGridCellPlacementFill;

		[_gridView rowAtIndex:3].rowAlignment = NSGridRowAlignmentNone;
		[_gridView rowAtIndex:3].yPlacement   = NSGridCellPlacementCenter;

		NSDictionary<NSNumber*, NSView*>* baselineViews = @{ @2: regularExpressionCheckBox, @3: matchingLabel };
		for(NSNumber* row in baselineViews)
		{
			NSGridCell* gridCell = [_gridView cellAtColumnIndex:0 rowIndex:row.integerValue];
			gridCell.rowAlignment = NSGridRowAlignmentNone;
			gridCell.yPlacement   = NSGridCellPlacementNone;
			gridCell.customPlacementConstraints = @[ [gridCell.contentView.firstBaselineAnchor constraintEqualToAnchor:baselineViews[row].firstBaselineAnchor constant:0] ];
		}

		[_gridView setContentHuggingPriority:NSLayoutPriorityWindowSizeStayPut forOrientation:NSLayoutConstraintOrientationVertical];

		[countButton.widthAnchor constraintEqualToAnchor:findHistoryButton.widthAnchor].active = YES;
		[countButton.heightAnchor constraintEqualToAnchor:findHistoryButton.heightAnchor].active = YES;
		[_wherePopUpButton addConstraint:[NSLayoutConstraint constraintWithItem:_wherePopUpButton attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationLessThanOrEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:150]];

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
			{ @"Collapse Results",                     @selector(toggleCollapsedState:),      @"1", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .target = _resultsViewController },
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
			actionsPopUpButton.menu = actionMenu;

		// =============================

		findHistoryButton.action    = @selector(showFindHistory:);
		replaceHistoryButton.action = @selector(showReplaceHistory:);
		countButton.action          = @selector(countOccurrences:);

		[_findTextField            bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.findString"           options:@{ NSContinuouslyUpdatesValueBindingOption: @YES }];
		[_replaceTextField         bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.replaceString"        options:@{ NSContinuouslyUpdatesValueBindingOption: @YES }];
		[globTextField             bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.globHistoryList.head" options:nil];
		[globTextField             bind:NSContentValuesBinding toObject:_objectController withKeyPath:@"content.globHistoryList.list" options:nil];
		[globTextField             bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.canEditGlob"          options:nil];
		[ignoreCaseCheckBox        bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.ignoreCase"           options:nil];
		[ignoreWhitespaceCheckBox  bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.ignoreWhitespace"     options:nil];
		[regularExpressionCheckBox bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.regularExpression"    options:nil];
		[wrapAroundCheckBox        bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.wrapAround"           options:nil];
		[ignoreWhitespaceCheckBox  bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.canIgnoreWhitespace"  options:nil];
		[countButton               bind:NSEnabledBinding       toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];

		OakSetupKeyViewLoop(@[ _gridView, _findTextField, _replaceTextField, countButton, regularExpressionCheckBox, ignoreWhitespaceCheckBox, ignoreCaseCheckBox, wrapAroundCheckBox, _wherePopUpButton, globTextField, actionsPopUpButton ]);
	}
	return _gridView;
}

- (NSStackView*)actionButtonsStackView
{
	if(!_actionButtonsStackView)
	{
		_findAllButton                 = OakCreateButton(@"Find All");
		_replaceAllButton              = OakCreateButton(@"Replace All");
		NSButton* replaceButton        = OakCreateButton(@"Replace");
		NSButton* replaceAndFindButton = OakCreateButton(@"Replace & Find");
		NSButton* findPreviousButton   = OakCreateButton(@"Previous");
		_findNextButton                = OakCreateButton(@"Next");

		_findAllButton.action          = @selector(findAll:);
		_replaceAllButton.action       = @selector(replaceAll:);
		replaceButton.action           = @selector(replace:);
		replaceAndFindButton.action    = @selector(replaceAndFind:);
		findPreviousButton.action      = @selector(findPrevious:);
		_findNextButton.action         = @selector(findNext:);

		[replaceButton         bind:NSEnabledBinding toObject:_objectController withKeyPath:@"content.canReplaceInDocument" options:nil];
		[replaceAndFindButton  bind:NSEnabledBinding toObject:_objectController withKeyPath:@"content.canReplaceInDocument" options:nil];
		[_findAllButton        bind:NSEnabledBinding toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];
		[_replaceAllButton     bind:NSEnabledBinding toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];
		[replaceAndFindButton  bind:@"enabled2"      toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];
		[findPreviousButton    bind:NSEnabledBinding toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];
		[_findNextButton       bind:NSEnabledBinding toObject:_objectController withKeyPath:@"content.findString.length"    options:nil];

		_actionButtonsStackView = [NSStackView stackViewWithViews:@[ _findAllButton, _replaceAllButton ]];
		[_actionButtonsStackView setViews:@[ replaceButton, replaceAndFindButton, findPreviousButton, _findNextButton ] inGravity:NSStackViewGravityTrailing];
		[_actionButtonsStackView setHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationVertical];

		OakSetupKeyViewLoop(@[ _actionButtonsStackView, _findAllButton, _replaceAllButton, replaceButton, replaceAndFindButton, findPreviousButton, _findNextButton ], NO);
	}
	return _actionButtonsStackView;
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.ignoreCase = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindIgnoreCase];
	self.wrapAround = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindWrapAround];

	NSDictionary* options = [NSUserDefaults.standardUserDefaults dictionaryForKey:kUserDefaultsFolderOptionsKey];
	self.searchHiddenFolders = [[options objectForKey:@"searchHiddenFolders"] boolValue];
	self.searchFolderLinks   = [[options objectForKey:@"searchFolderLinks"] boolValue];
	self.searchFileLinks     = ![[options objectForKey:@"skipFileLinks"] boolValue];
	self.searchBinaryFiles   = [[options objectForKey:@"searchBinaryFiles"] boolValue];
}

- (void)findClipboardDidChange:(NSNotification*)aNotification
{
	OakPasteboardEntry* entry = [OakPasteboard.findPasteboard current];
	self.findString        = entry.string;
	self.regularExpression = entry.regularExpression;
	self.ignoreWhitespace  = entry.ignoreWhitespace;
	self.fullWords         = entry.fullWordMatch;
	_findStringUpdated = NO;
}

- (void)replaceClipboardDidChange:(NSNotification*)aNotification
{
	self.replaceString    = [[OakPasteboard.replacePasteboard current] string];
	_replaceStringUpdated = NO;
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if([keyPath isEqualToString:@"firstResponder"])
	{
		NSResponder* firstResponder = [self.window firstResponder];
		_resultsViewController.showReplacementPreviews = firstResponder == _replaceTextField || firstResponder == _replaceTextField.currentEditor;

		if([firstResponder isKindOfClass:[NSTextView class]])
		{
			NSTextView* textView = (NSTextView*)firstResponder;
			if(textView.isFieldEditor)
			{
				BOOL enable = _findTextField.currentEditor || _replaceTextField.currentEditor;
				if(textView.textStorage.delegate = enable ? self : nil)
					[self addStylesToFieldEditor];
			}
		}
	}
}

- (void)updateWindowTitle
{
	if(NSString* folder = self.searchFolder)
		self.window.title = [NSString localizedStringWithFormat:@"Find — %@", [folder stringByAbbreviatingWithTildeInPath]];
	else if(_searchTarget == FFSearchTargetOpenFiles)
		self.window.title = @"Find — Open Files";
	else
		self.window.title = @"Find";
}

- (void)showWindow:(id)sender
{
	BOOL isVisibleAndKey = [self isWindowLoaded] && [self.window isVisible] && [self.window isKeyWindow];
	[super showWindow:sender];
	if(!isVisibleAndKey || ![[self.window firstResponder] isKindOfClass:[NSTextView class]])
		[self.window makeFirstResponder:_findTextField];
}

- (BOOL)commitEditing
{
	id currentResponder = [[self window] firstResponder];
	id view = [currentResponder isKindOfClass:[NSTextView class]] ? [currentResponder delegate] : currentResponder;
	BOOL res = [_objectController commitEditing];
	if([[self window] firstResponder] != currentResponder && view)
		[[self window] makeFirstResponder:view];

	// =====================
	// = Update Pasteboard =
	// =====================

	if(_findStringUpdated && OakNotEmptyString(_findString))
	{
		NSDictionary* newOptions = @{
			OakFindRegularExpressionOption: @(self.regularExpression),
			OakFindIgnoreWhitespaceOption:  @(self.ignoreWhitespace),
			OakFindFullWordsOption:         @(self.fullWords),
		};

		[OakPasteboard.findPasteboard addEntryWithString:_findString options:newOptions];
		_findStringUpdated = NO;
	}

	if(_replaceStringUpdated && _replaceString)
	{
		[OakPasteboard.replacePasteboard addEntryWithString:_replaceString];
		_replaceStringUpdated = NO;
	}

	return res;
}

- (void)resultsFrameDidChange:(NSNotification*)aNotification
{
	if(self.showsResultsOutlineView)
		self.findResultsHeight = NSHeight(_resultsViewController.view.frame);
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
	return [NSFileManager.defaultManager displayNameAtPath:path];
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
		if(![NSFileManager.defaultManager fileExistsAtPath:path])
			continue;

		NSMenuItem* recentItem = [whereMenu addItemWithTitle:[self displayNameForFolder:path] action:@selector(takeSearchTargetFrom:) keyEquivalent:@""];
		[recentItem setIconForFile:path];
		[recentItem setRepresentedObject:path];
	}
}

- (void)setSearchTarget:(FFSearchTarget)newTarget
{
	_searchTarget = newTarget;

	BOOL isDirectory = NO;
	if(_searchTarget == FFSearchTargetOther && [NSFileManager.defaultManager fileExistsAtPath:self.otherFolder isDirectory:&isDirectory] && isDirectory)
		[self.recentFolders addObject:self.otherFolder];

	self.canEditGlob          = _searchTarget != FFSearchTargetDocument && _searchTarget != FFSearchTargetSelection;
	self.canReplaceInDocument = _searchTarget == FFSearchTargetDocument || _searchTarget == FFSearchTargetSelection;

	[self updateSearchInPopUpMenu];
	[self updateWindowTitle];

	BOOL isFolderSearch = _searchTarget != FFSearchTargetDocument && _searchTarget != FFSearchTargetSelection;
	self.showsResultsOutlineView = isFolderSearch;
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

- (IBAction)showFindHistory:(id)sender
{
	if(![[OakPasteboardSelector.sharedInstance window] isVisible])
		[OakPasteboard.findPasteboard selectItemForControl:_findTextField];
	// if the panel is visible it will automatically be hidden due to the mouse click
}

- (IBAction)showReplaceHistory:(id)sender
{
	if(![[OakPasteboardSelector.sharedInstance window] isVisible])
		[OakPasteboard.replacePasteboard selectItemForControl:_replaceTextField];
	// if the panel is visible it will automatically be hidden due to the mouse click
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

		[self.findStringPopver showRelativeToRect:NSZeroRect ofView:_findTextField preferredEdge:NSMaxYEdge];
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
	_showsResultsOutlineView = flag;

	if(!self.isWindowLoaded)
		return;

	NSRect windowFrame = self.window.frame;
	CGFloat minY = NSMinY(windowFrame);
	CGFloat maxY = NSMaxY(windowFrame);

	if(_showsResultsOutlineView)
			minY -= MAX(50, self.findResultsHeight + 8);
	else	minY += NSHeight(_resultsViewController.view.frame) + 8;

	NSRect screenFrame = self.window.screen.visibleFrame;
	if(minY < NSMinY(screenFrame))
		maxY += NSMinY(screenFrame) - minY;
	if(maxY > NSMaxY(screenFrame))
		minY -= maxY - NSMaxY(screenFrame);

	minY = MAX(minY, NSMinY(screenFrame));
	maxY = MIN(maxY, NSMaxY(screenFrame));
	NSRect newWindowFrame = NSMakeRect(NSMinX(windowFrame), minY, NSWidth(windowFrame), maxY - minY);

	if(_showsResultsOutlineView)
		_resultsViewController.view.hidden = NO;
	[self.window setFrame:newWindowFrame display:YES animate:YES];
	if(!_showsResultsOutlineView)
		_resultsViewController.view.hidden = YES;

	self.window.defaultButtonCell = _showsResultsOutlineView ? _findAllButton.cell : _findNextButton.cell;
}

- (void)setBusy:(BOOL)busyFlag
{
	_busy = busyFlag;
	_statusBarViewController.progressIndicatorVisible = busyFlag;
}

- (void)setStatusString:(NSString*)aString
{
	_statusString = aString;
	_statusBarViewController.statusText = _statusString;
}

- (void)setAlternateStatusString:(NSString*)aString
{
	_alternateStatusString = aString;
	_statusBarViewController.alternateStatusText = _alternateStatusString;
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
	if(_searchTarget == FFSearchTargetFileBrowserItems && _fileBrowserItems.count > 1)
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
	_findStringUpdated = YES;
	[_findTextField updateIntrinsicContentSizeToEncompassString:_findString];

	if(self.findErrorString)
		[self updateFindErrorString];
}

- (void)setReplaceString:(NSString*)aString
{
	if(_replaceString == aString || [_replaceString isEqualToString:aString])
		return;

	_replaceString = aString ?: @"";
	_replaceStringUpdated = YES;
	[_replaceTextField updateIntrinsicContentSizeToEncompassString:_replaceString];
}

- (void)setFindResultsHeight:(CGFloat)height { [NSUserDefaults.standardUserDefaults setInteger:height forKey:kUserDefaultsFindResultsHeightKey]; }
- (CGFloat)findResultsHeight                 { return [NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsFindResultsHeightKey] ?: 200; }

- (void)setRegularExpression:(BOOL)flag
{
	if(_regularExpression == flag)
		return;

	_regularExpression = flag;
	_findStringUpdated = YES;
	if(self.findErrorString)
		[self updateFindErrorString];

	_findStringFormatter.enabled    = flag;
	_replaceStringFormatter.enabled = flag;

	// Re-format current value
	if(!_findTextField.currentEditor)
	{
		_findTextField.objectValue = nil;
		_findTextField.objectValue = _findString;
	}

	if(!_replaceTextField.currentEditor)
	{
		_replaceTextField.objectValue = nil;
		_replaceTextField.objectValue = _replaceString;
	}

	[self addStylesToFieldEditor];
}

- (void)setIgnoreWhitespace:(BOOL)flag
{
	if(_ignoreWhitespace == flag)
		return;

	_ignoreWhitespace  = flag;
	_findStringUpdated = YES;
}

- (void)setFullWords:(BOOL)flag
{
	if(_fullWords == flag)
		return;

	_fullWords         = flag;
	_findStringUpdated = YES;
}

- (void)textStorageDidProcessEditing:(NSNotification*)aNotification
{
	[self addStylesToFieldEditor];
}

- (void)addStylesToFieldEditor
{
	[_findStringFormatter addStylesToString:((NSTextView*)_findTextField.currentEditor).textStorage];
	[_replaceStringFormatter addStylesToString:((NSTextView*)_replaceTextField.currentEditor).textStorage];
}

- (void)setIgnoreCase:(BOOL)flag        { if(_ignoreCase != flag) [NSUserDefaults.standardUserDefaults setObject:@(_ignoreCase = flag) forKey:kUserDefaultsFindIgnoreCase]; }
- (void)setWrapAround:(BOOL)flag        { if(_wrapAround != flag) [NSUserDefaults.standardUserDefaults setObject:@(_wrapAround = flag) forKey:kUserDefaultsFindWrapAround]; }
- (BOOL)ignoreWhitespace                { return _ignoreWhitespace && self.canIgnoreWhitespace; }
- (BOOL)canIgnoreWhitespace             { return _regularExpression == NO; }

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
			[NSUserDefaults.standardUserDefaults setObject:options forKey:kUserDefaultsFolderOptionsKey];
	else	[NSUserDefaults.standardUserDefaults removeObjectForKey:kUserDefaultsFolderOptionsKey];
}

- (void)setSearchHiddenFolders:(BOOL)flag { if(_searchHiddenFolders != flag) { _searchHiddenFolders = flag; [self updateFolderSearchUserDefaults]; } }
- (void)setSearchFolderLinks:(BOOL)flag   { if(_searchFolderLinks != flag)   { _searchFolderLinks   = flag; [self updateFolderSearchUserDefaults]; } }
- (void)setSearchFileLinks:(BOOL)flag     { if(_searchFileLinks != flag)     { _searchFileLinks     = flag; [self updateFolderSearchUserDefaults]; } }
- (void)setSearchBinaryFiles:(BOOL)flag   { if(_searchBinaryFiles != flag)   { _searchBinaryFiles   = flag; [self updateFolderSearchUserDefaults]; } }

- (IBAction)toggleSearchHiddenFolders:(id)sender { self.searchHiddenFolders = !self.searchHiddenFolders; }
- (IBAction)toggleSearchFolderLinks:(id)sender   { self.searchFolderLinks   = !self.searchFolderLinks;   }
- (IBAction)toggleSearchFileLinks:(id)sender     { self.searchFileLinks     = !self.searchFileLinks;     }
- (IBAction)toggleSearchBinaryFiles:(id)sender   { self.searchBinaryFiles   = !self.searchBinaryFiles;   }

- (IBAction)takeLevelToFoldFrom:(id)sender       { [_resultsViewController toggleCollapsedState:sender];                    }
- (IBAction)selectNextResult:(id)sender          { [_resultsViewController selectNextResultWrapAround:self.wrapAround];     }
- (IBAction)selectPreviousResult:(id)sender      { [_resultsViewController selectPreviousResultWrapAround:self.wrapAround]; }
- (IBAction)selectNextTab:(id)sender             { [_resultsViewController selectNextDocument:sender];                      }
- (IBAction)selectPreviousTab:(id)sender         { [_resultsViewController selectPreviousDocument:sender];                  }

- (BOOL)control:(NSControl*)control textView:(NSTextView*)textView doCommandBySelector:(SEL)command
{
	if(command == @selector(moveDown:))
	{
		NSRange insertionPoint = [[textView.selectedRanges lastObject] rangeValue];
		NSRange lastNewline    = [textView.string rangeOfString:@"\n" options:NSBackwardsSearch];

		if(lastNewline.location == NSNotFound || lastNewline.location < NSMaxRange(insertionPoint))
		{
			if(control == _findTextField)
				return [self showFindHistory:control], YES;
			else if(control == _replaceTextField)
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
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	BOOL res = YES;
	if(aMenuItem.action == @selector(toggleSearchHiddenFolders:))
		[aMenuItem setState:self.searchHiddenFolders ? NSControlStateValueOn : NSControlStateValueOff];
	else if(aMenuItem.action == @selector(toggleSearchFolderLinks:))
		[aMenuItem setState:self.searchFolderLinks ? NSControlStateValueOn : NSControlStateValueOff];
	else if(aMenuItem.action == @selector(toggleSearchFileLinks:))
		[aMenuItem setState:self.searchFileLinks ? NSControlStateValueOn : NSControlStateValueOff];
	else if(aMenuItem.action == @selector(toggleSearchBinaryFiles:))
		[aMenuItem setState:self.searchBinaryFiles ? NSControlStateValueOn : NSControlStateValueOff];
	else if(aMenuItem.action == @selector(goToParentFolder:))
		res = self.searchFolder != nil || _searchTarget == FFSearchTargetFileBrowserItems && CommonAncestor(_fileBrowserItems);
	return res;
}
@end

// ========
// = Find =
// ========

@interface Find () <OakFindServerProtocol>
@property (nonatomic) FindWindowController* windowController;
@property (nonatomic) FFDocumentSearch* documentSearch;
@property (nonatomic) FFResultNode* results;
@property (nonatomic, weak) id <FindDelegate> resultsDelegate;
@property (nonatomic) NSUInteger countOfMatches;
@property (nonatomic) NSUInteger countOfExcludedMatches;
@property (nonatomic) NSUInteger countOfReadOnlyMatches;
@property (nonatomic) NSUInteger countOfExcludedReadOnlyMatches;
@property (nonatomic) BOOL closeWindowOnSuccess;
@property (nonatomic) BOOL performingFolderSearch;

// =========================
// = OakFindProtocolServer =
// =========================

@property (nonatomic) find_operation_t findOperation;
@property (nonatomic) find::options_t  findOptions;

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition wrapped:(BOOL)didWrap;
- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString;
@end

NSString* const FFFindWasTriggeredByEnter = @"FFFindWasTriggeredByEnter";

@implementation Find
+ (instancetype)sharedInstance
{
	static Find* sharedInstance = [self new];
	return sharedInstance;
}

- (FindWindowController*)windowController
{
	if(!_windowController)
	{
		_windowController = [FindWindowController new];
		_windowController.nextResponder = self;

		_windowController.resultsViewController.selectResultAction      = @selector(didSelectResult:);
		_windowController.resultsViewController.removeResultAction      = @selector(didRemoveResult:);
		_windowController.resultsViewController.doubleClickResultAction = @selector(didDoubleClickResult:);
		_windowController.resultsViewController.target                  = self;

		[_windowController.replaceAllButton bind:NSTitleBinding toObject:self withKeyPath:@"replaceAllButtonTitle" options:nil];
		[_windowController.replaceAllButton bind:@"enabled2" toObject:self withKeyPath:@"canReplaceAll" options:nil];

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(windowWillClose:) name:NSWindowWillCloseNotification object:_windowController.window];
	}
	return _windowController;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self stopSearch:self];
}

// ====================================
// = Actions for displaying the panel =
// ====================================

- (void)showWindow:(id)sender
{
	[self.windowController showWindow:self];
}

- (IBAction)showFolderSelectionPanel:(id)sender
{
	NSOpenPanel* openPanel = [NSOpenPanel openPanel];
	openPanel.title = @"Find in Folder";
	openPanel.canChooseFiles = NO;
	openPanel.canChooseDirectories = YES;
	if(NSString* folder = self.searchFolder)
		openPanel.directoryURL = [NSURL fileURLWithPath:folder];
	if([self.windowController isWindowLoaded] && [self.windowController.window isVisible])
	{
		[openPanel beginSheetModalForWindow:self.windowController.window completionHandler:^(NSModalResponse result) {
			if(result == NSModalResponseOK)
			{
				self.windowController.otherFolder = [[[[openPanel URLs] lastObject] filePathURL] path];
				self.windowController.searchTarget = FFSearchTargetOther;
			}
			else if([self isVisible]) // Reset selected item in pop-up button
				self.windowController.searchTarget = self.windowController.searchTarget;
		}];
	}
	else
	{
		[openPanel beginWithCompletionHandler:^(NSInteger result) {
			if(result == NSModalResponseOK)
			{
				self.windowController.otherFolder = [[[[openPanel URLs] lastObject] filePathURL] path];
				self.windowController.searchTarget = FFSearchTargetOther;
				[self showWindow:self];
			}
		}];
	}
}

// ================
// = Find actions =
// ================

+ (NSSet*)keyPathsForValuesAffectingCanReplaceAll         { return [NSSet setWithArray:@[ @"countOfMatches", @"countOfExcludedMatches", @"countOfReadOnlyMatches", @"countOfExcludedReadOnlyMatches", @"windowController.showsResultsOutlineView" ]]; }
+ (NSSet*)keyPathsForValuesAffectingReplaceAllButtonTitle { return [NSSet setWithArray:@[ @"countOfMatches", @"countOfExcludedMatches", @"countOfReadOnlyMatches", @"countOfExcludedReadOnlyMatches", @"windowController.showsResultsOutlineView" ]]; }

- (BOOL)canReplaceAll                { return _windowController.showsResultsOutlineView ? (_countOfExcludedMatches - _countOfExcludedReadOnlyMatches < _countOfMatches - _countOfReadOnlyMatches) : YES; }
- (NSString*)replaceAllButtonTitle   { return _windowController.showsResultsOutlineView && (_countOfExcludedMatches || _countOfReadOnlyMatches && _countOfReadOnlyMatches != _countOfMatches) ? @"Replace Selected" : @"Replace All"; }

- (IBAction)countOccurrences:(id)sender   { [self performFindAction:FindActionCountMatches   withWindowController:self.windowController]; }
- (IBAction)findAll:(id)sender            { [self performFindAction:FindActionFindAll        withWindowController:self.windowController]; }
- (IBAction)findAllInSelection:(id)sender { [self performFindAction:FindActionFindAll        withWindowController:self.windowController]; }
- (IBAction)findNext:(id)sender           { [self performFindAction:FindActionFindNext       withWindowController:self.windowController]; }
- (IBAction)findPrevious:(id)sender       { [self performFindAction:FindActionFindPrevious   withWindowController:self.windowController]; }
- (IBAction)replaceAll:(id)sender         { [self performFindAction:FindActionReplaceAll     withWindowController:self.windowController]; }
- (IBAction)replaceAndFind:(id)sender     { [self performFindAction:FindActionReplaceAndFind withWindowController:self.windowController]; }
- (IBAction)replace:(id)sender            { [self performFindAction:FindActionReplace        withWindowController:self.windowController]; }

- (IBAction)stopSearch:(id)sender
{
	if(_performingFolderSearch)
	{
		[_documentSearch stop];
		[self folderSearchDidFinish:nil];
		self.windowController.statusString = @"Stopped.";
	}
}

- (void)performFindAction:(FindActionTag)action withWindowController:(FindWindowController*)controller
{
	[controller updateFindErrorString];
	if(controller.findErrorString != nil)
		return;

	_findOptions = (controller.regularExpression ? find::regular_expression : find::none) | (controller.ignoreWhitespace ? find::ignore_whitespace : find::none) | (controller.fullWords ? find::full_words : find::none) | (controller.ignoreCase ? find::ignore_case : find::none) | (controller.wrapAround ? find::wrap_around : find::none);
	if(action == FindActionFindPrevious)
		_findOptions |= find::backwards;
	else if(action == FindActionCountMatches || action == FindActionFindAll || action == FindActionReplaceAll)
		_findOptions |= find::all_matches;

	FFSearchTarget searchTarget = controller.searchTarget;
	if(searchTarget != FFSearchTargetSelection && (searchTarget != FFSearchTargetDocument || action == FindActionFindAll && self.documentIdentifier))
	{
		switch(action)
		{
			case FindActionFindAll:
			{
				if(searchTarget == FFSearchTargetDocument && self.documentIdentifier)
				{
					if(OakDocument* document = [OakDocumentController.sharedInstance findDocumentWithIdentifier:self.documentIdentifier])
					{
						self.documentSearch = nil;
						self.windowController.showsResultsOutlineView              = YES;
						self.windowController.resultsViewController.hideCheckBoxes = YES;
						[self acceptMatches:[document matchesForString:controller.findString options:_findOptions]];
						[self folderSearchDidFinish:nil];
					}
				}
				else if(searchTarget == FFSearchTargetOpenFiles)
				{
					self.documentSearch = nil;
					self.windowController.showsResultsOutlineView              = YES;
					self.windowController.resultsViewController.hideCheckBoxes = NO;
					for(OakDocument* document in [OakDocumentController.sharedInstance openDocuments])
						[self acceptMatches:[document matchesForString:controller.findString options:_findOptions]];
					[self folderSearchDidFinish:nil];
				}
				else
				{
					NSArray* paths;
					if(searchTarget == FFSearchTargetProject)
						paths = @[ self.projectFolder ];
					else if(searchTarget == FFSearchTargetFileBrowserItems)
						paths = self.fileBrowserItems;
					else // searchTarget == FFSearchTargetOther
						paths = @[ _windowController.otherFolder ];

					FFDocumentSearch* folderSearch = [FFDocumentSearch new];
					folderSearch.searchBinaryFiles   = YES;
					folderSearch.searchString        = controller.findString;
					folderSearch.options             = _findOptions;
					folderSearch.paths               = paths;
					folderSearch.glob                = controller.globString;
					folderSearch.searchFolderLinks   = controller.searchFolderLinks;
					folderSearch.searchFileLinks     = controller.searchFileLinks;
					folderSearch.searchHiddenFolders = controller.searchHiddenFolders;
					folderSearch.searchBinaryFiles   = controller.searchBinaryFiles;

					self.documentSearch = folderSearch;
				}
			}
			break;

			case FindActionReplaceAll:
			case FindActionReplaceSelected:
			{
				NSUInteger replaceCount = 0, fileCount = 0;
				std::string replaceString = to_s(controller.replaceString);

				for(FFResultNode* parent in _results.children)
				{
					if(parent.countOfExcluded == parent.countOfLeafs)
						continue;

					std::multimap<std::pair<size_t, size_t>, std::string> replacements;
					for(FFResultNode* child in parent.children)
					{
						if(child.excluded)
							continue;
						child.replaceString = controller.replaceString;
						replacements.emplace(std::make_pair(child.match.first, child.match.last), controller.regularExpression ? format_string::expand(replaceString, child.match.captures) : replaceString);
					}

					if(OakDocument* doc = parent.document)
					{
						if(doc.isLoaded)
						{
							[doc performReplacements:replacements checksum:parent.match.checksum];
						}
						else
						{
							if(![doc performReplacements:replacements checksum:parent.match.checksum])
							{
								[parent.children setValue:nil forKey:@"replaceString"];
								continue;
							}

							[doc saveModalForWindow:self.windowController.window completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
								// TODO Indicate failure when result != OakDocumentIOResultSuccess
								if(!doc.isLoaded) // Ensure document is still closed
									doc.content = nil;
							}];
						}

						parent.readOnly = YES;
						replaceCount += replacements.size();
						++fileCount;
					}
				}
				self.windowController.statusString = [NSString stringWithFormat:@"%@ replacement%@ made across %@ file%@.", [NSNumberFormatter localizedStringFromNumber:@(replaceCount) numberStyle:NSNumberFormatterDecimalStyle], replaceCount == 1 ? @"" : @"s", [NSNumberFormatter localizedStringFromNumber:@(fileCount) numberStyle:NSNumberFormatterDecimalStyle], fileCount == 1 ? @"" : @"s"];
			}
			break;

			case FindActionFindNext:     [self.windowController selectNextResult:self];     break;
			case FindActionFindPrevious: [self.windowController selectPreviousResult:self]; break;
		}
	}
	else
	{
		bool onlySelection = searchTarget == FFSearchTargetSelection;
		switch(action)
		{
			case FindActionFindNext:
			case FindActionFindPrevious:
			case FindActionFindAll:        _findOperation = onlySelection ? kFindOperationFindInSelection       : kFindOperationFind;       break;
			case FindActionCountMatches:   _findOperation = onlySelection ? kFindOperationCountInSelection      : kFindOperationCount;      break;
			case FindActionReplaceAll:     _findOperation = onlySelection ? kFindOperationReplaceAllInSelection : kFindOperationReplaceAll; break;
			case FindActionReplaceAndFind: _findOperation = kFindOperationReplaceAndFind;                                                   break;
			case FindActionReplace:        _findOperation = kFindOperationReplace;                                                          break;
		}

		self.closeWindowOnSuccess = action == FindActionFindNext && [[NSApp currentEvent] type] == NSEventTypeKeyDown && to_s([NSApp currentEvent]) == utf8::to_s(NSCarriageReturnCharacter);
		self.findMatches = nil;
		[NSApp sendAction:@selector(performFindOperation:) to:nil from:self];
	}
}

- (NSString*)findString    { return self.windowController.findString    ?: @""; }
- (NSString*)replaceString { return self.windowController.replaceString ?: @""; }

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition wrapped:(BOOL)didWrap
{
	static std::string const formatStrings[4][3] = {
		{ "No more occurrences of “${found}”.", "Found “${found}”${line:+ at line ${line}, column ${column}}.",               "${count} occurrences of “${found}”." },
		{ "No more matches for “${found}”.",    "Found one match for “${found}”${line:+ at line ${line}, column ${column}}.", "${count} matches for “${found}”."    },
	};

	std::map<std::string, std::string> variables;
	variables["count"]  = to_s([NSNumberFormatter localizedStringFromNumber:@(aNumber) numberStyle:NSNumberFormatterDecimalStyle]);
	variables["found"]  = to_s(aFindString);
	variables["line"]   = aPosition ? std::to_string(aPosition.line + 1)   : NULL_STR;
	variables["column"] = aPosition ? std::to_string(aPosition.column + 1) : NULL_STR;
	self.windowController.statusString = [NSString stringWithCxxString:format_string::expand(formatStrings[(_findOptions & find::regular_expression) ? 1 : 0][std::min<size_t>(aNumber, 2)], variables)];

	NSResponder* keyView = [[NSApp keyWindow] firstResponder];
	id element = [keyView respondsToSelector:@selector(cell)] ? [keyView performSelector:@selector(cell)] : keyView;
	if([element respondsToSelector:@selector(isAccessibilityElement)] && [element isAccessibilityElement])
		NSAccessibilityPostNotificationWithUserInfo(element, NSAccessibilityAnnouncementRequestedNotification, @{ NSAccessibilityAnnouncementKey: self.windowController.statusString });

	if(self.closeWindowOnSuccess && aNumber != 0)
		return [self.windowController close];
}

- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString
{
	static NSString* const formatStrings[2][3] = {
		{ @"Nothing replaced (no occurrences of “%@”).", @"Replaced one occurrence of “%@”.", @"Replaced %2$@ occurrences of “%@”." },
		{ @"Nothing replaced (no matches for “%@”).",    @"Replaced one match of “%@”.",      @"Replaced %2$@ matches of “%@”."     }
	};
	NSString* format = formatStrings[(_findOptions & find::regular_expression) ? 1 : 0][aNumber > 2 ? 2 : aNumber];
	self.windowController.statusString = [NSString stringWithFormat:format, aFindString, [NSNumberFormatter localizedStringFromNumber:@(aNumber) numberStyle:NSNumberFormatterDecimalStyle]];
}

// =============
// = Accessors =
// =============

- (void)setSearchTarget:(FFSearchTarget)newTarget { self.windowController.searchTarget = newTarget; }
- (FFSearchTarget)searchTarget                    { return self.windowController.searchTarget; }
- (void)setProjectFolder:(NSString*)folder        { self.windowController.projectFolder = folder; }
- (NSString*)projectFolder                        { return self.windowController.projectFolder; }
- (void)setFileBrowserItems:(NSArray*)items       { self.windowController.fileBrowserItems = items; }
- (NSArray*)fileBrowserItems                      { return self.windowController.fileBrowserItems; }
- (NSString*)searchFolder                         { return self.windowController.searchFolder; }
- (BOOL)isVisible                                 { return self.windowController.window.isVisible; }

// ===========
// = Options =
// ===========

- (IBAction)takeFindOptionToToggleFrom:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);

	find::options_t option = find::options_t([sender tag]);
	switch(option)
	{
		case find::full_words:         self.windowController.fullWords         = !self.windowController.fullWords;         break;
		case find::ignore_case:        self.windowController.ignoreCase        = !self.windowController.ignoreCase;        break;
		case find::ignore_whitespace:  self.windowController.ignoreWhitespace  = !self.windowController.ignoreWhitespace;  break;
		case find::regular_expression: self.windowController.regularExpression = !self.windowController.regularExpression; break;
		case find::wrap_around:        self.windowController.wrapAround        = !self.windowController.wrapAround;        break;
		default:
			ASSERTF(false, "Unknown find option tag %d\n", option);
	}

	if([[[OakPasteboard.findPasteboard current] string] isEqualToString:self.windowController.findString])
		[self.windowController commitEditing]; // update the options on the pasteboard immediately if the find string has not been changed
}

// ====================
// = Search in Folder =
// ====================

- (void)clearMatches
{
	if(_results)
	{
		for(FFResultNode* parent in _results.children)
			[parent.document removeAllMarksOfType:kSearchMarkIdentifier];

		[self unbind:@"countOfMatches"];
		[self unbind:@"countOfExcludedMatches"];
		[self unbind:@"countOfReadOnlyMatches"];
		[self unbind:@"countOfExcludedReadOnlyMatches"];

		// Update UI dependent on “count of matches”
		self.countOfMatches = self.countOfExcludedMatches = self.countOfReadOnlyMatches = self.countOfExcludedReadOnlyMatches = 0;
	}

	_windowController.resultsViewController.results = _results = [FFResultNode new];
	_resultsDelegate = self.delegate;
}

- (void)setDocumentSearch:(FFDocumentSearch*)newSearcher
{
	[self clearMatches];

	if(_documentSearch)
	{
		[_documentSearch removeObserver:self forKeyPath:@"currentPath"];
		[NSNotificationCenter.defaultCenter removeObserver:self name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[NSNotificationCenter.defaultCenter removeObserver:self name:FFDocumentSearchDidFinishNotification object:_documentSearch];
		[_documentSearch stop];
	}

	if(_documentSearch = newSearcher)
	{
		self.windowController.busy                    = YES;
		self.windowController.statusString            = @"Searching…";
		self.windowController.showsResultsOutlineView = YES;
		self.windowController.resultsViewController.hideCheckBoxes = NO;

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(folderSearchDidReceiveResults:) name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(folderSearchDidFinish:) name:FFDocumentSearchDidFinishNotification object:_documentSearch];
		[_documentSearch addObserver:self forKeyPath:@"currentPath" options:(NSKeyValueObservingOptionNew|NSKeyValueObservingOptionOld) context:NULL];
		self.performingFolderSearch = YES;
		[_documentSearch start];
	}
}

- (void)acceptMatches:(NSArray<OakDocumentMatch*>*)matches
{
	NSUInteger countOfExistingItems = _results.children.count;

	FFResultNode* parent = nil;
	for(OakDocumentMatch* match in matches)
	{
		[match.document setMarkOfType:kSearchMarkIdentifier atPosition:match.range.from content:nil];

		FFResultNode* node = [FFResultNode resultNodeWithMatch:match];
		if(!parent || ![parent.document isEqual:node.document])
			[_results addResultNode:(parent = [FFResultNode resultNodeWithMatch:match baseDirectory:CommonAncestor(_documentSearch.paths)])];
		[parent addResultNode:node];
	}

	[_windowController.resultsViewController insertItemsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(countOfExistingItems, _results.children.count - countOfExistingItems)]];
}

- (void)folderSearchDidReceiveResults:(NSNotification*)aNotification
{
	[self acceptMatches:[aNotification userInfo][@"matches"]];
}

- (void)setUpFindMatches:(id)sender
{
	NSMutableArray* findMatches = [NSMutableArray array];
	for(FFResultNode* parent in _results.children)
		[findMatches addObject:[[FindMatch alloc] initWithUUID:parent.firstResultNode.document.identifier firstRange:parent.firstResultNode.match.range lastRange:parent.lastResultNode.match.range]];
	self.findMatches = findMatches;
}

- (void)folderSearchDidFinish:(NSNotification*)aNotification
{
	self.performingFolderSearch = NO;
	self.windowController.busy = NO;
	if(!_results)
		return;

	[self bind:@"countOfMatches" toObject:_results withKeyPath:@"countOfLeafs" options:nil];
	[self bind:@"countOfExcludedMatches" toObject:_results withKeyPath:@"countOfExcluded" options:nil];
	[self bind:@"countOfReadOnlyMatches" toObject:_results withKeyPath:@"countOfReadOnly" options:nil];
	[self bind:@"countOfExcludedReadOnlyMatches" toObject:_results withKeyPath:@"countOfExcludedReadOnly" options:nil];

	[self setUpFindMatches:self];

	NSString* fmt;
	switch(self.countOfMatches)
	{
		case 0:  fmt = @"No results found for “%@”.";     break;
		case 1:  fmt = @"Found one result for “%@”.";     break;
		default: fmt = @"Found %2$@ results for “%1$@”."; break;
	}

	NSString* searchString = [_documentSearch searchString] ?: self.windowController.findString;
	NSString* msg = [NSString stringWithFormat:fmt, searchString, [NSNumberFormatter localizedStringFromNumber:@(self.countOfMatches) numberStyle:NSNumberFormatterDecimalStyle]];
	if(_documentSearch)
	{
		NSNumberFormatter* formatter = [NSNumberFormatter new];
		formatter.numberStyle = NSNumberFormatterDecimalStyle;
		formatter.maximumFractionDigits = 1;
		NSString* seconds = [formatter stringFromNumber:@([_documentSearch searchDuration])];

		self.windowController.statusString          = [msg stringByAppendingFormat:([_documentSearch scannedFileCount] == 1 ? @" (searched one file in %@ seconds)" : @" (searched %2$@ files in %1$@ seconds)"), seconds, [NSNumberFormatter localizedStringFromNumber:@([_documentSearch scannedFileCount]) numberStyle:NSNumberFormatterDecimalStyle]];
		self.windowController.alternateStatusString = [msg stringByAppendingFormat:@" (searched %2$@ in %1$@ seconds)", seconds, [NSString stringWithCxxString:text::format_size([_documentSearch scannedByteCount])]];
	}
	else
	{
		self.windowController.statusString = msg;
	}

	__weak __block id observerId = [NSNotificationCenter.defaultCenter addObserverForName:OakPasteboardDidChangeNotification object:OakPasteboard.findPasteboard queue:nil usingBlock:^(NSNotification*){
		self.findMatches = nil;
		for(FFResultNode* parent in _results.children)
			[parent.document removeAllMarksOfType:kSearchMarkIdentifier];
		[NSNotificationCenter.defaultCenter removeObserver:observerId];
	}];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if([keyPath isEqualToString:@"currentPath"])
	{
		id newValue = [change objectForKey:NSKeyValueChangeNewKey], oldValue = [change objectForKey:NSKeyValueChangeOldKey];
		std::string searchPath     = [newValue respondsToSelector:@selector(UTF8String)] ? [newValue UTF8String] : "";
		std::string lastSearchPath = [oldValue respondsToSelector:@selector(UTF8String)] ? [oldValue UTF8String] : "";

		// Show only the directory part unless the file name hasn’t changed since last poll of the scanner
		if(searchPath != lastSearchPath && !path::is_directory(searchPath))
			searchPath = path::parent(searchPath);

		std::string relative = path::relative_to(searchPath, to_s(self.searchFolder));
		if(path::is_directory(searchPath))
			relative += "/";

		self.windowController.statusString = [NSString localizedStringWithFormat:@"Searching “%@”…", [NSString stringWithCxxString:relative]];
	}
}

// =============================
// = Selecting Results Actions =
// =============================

- (void)didSelectResult:(FFResultNode*)item
{
	OakDocument* doc = item.document;
	if(!doc.isOpen)
		doc.recentTrackingDisabled = YES;

	NSMutableDictionary* captures = [NSMutableDictionary dictionary];
	for(auto pair : item.match.captures)
		captures[to_ns(pair.first)] = to_ns(pair.second);
	doc.matchCaptures = [captures copy];

	[_resultsDelegate selectRange:item.match.range inDocument:doc];
}

- (void)didDoubleClickResult:(FFResultNode*)item
{
	if([[NSUserDefaults.standardUserDefaults objectForKey:kUserDefaultsKeepSearchResultsOnDoubleClick] boolValue])
		return;
	[_resultsDelegate bringToFront];
	[self.windowController close];
}

- (void)didRemoveResult:(FFResultNode*)item
{
	if(OakIsAlternateKeyOrMouseEvent())
	{
		if(item.document.path)
		{
			std::string path = path::relative_to(to_s(item.document.path), to_s(CommonAncestor(_documentSearch.paths)));
			NSString* newGlob = [_windowController.globString stringByAppendingFormat:@"~%@", [NSString stringWithCxxString:path]];
			_windowController.globString = newGlob;
		}
	}

	[item.document removeAllMarksOfType:kSearchMarkIdentifier];
	[self setUpFindMatches:self];

	NSString* fmt;
	switch(self.countOfMatches)
	{
		case 0:  fmt = @"No results for “%@”.";             break;
		case 1:  fmt = @"Showing one result for “%@”.";     break;
		default: fmt = @"Showing %2$@ results for “%1$@”."; break;
	}
	_windowController.statusString = [NSString stringWithFormat:fmt, [_documentSearch searchString], [NSNumberFormatter localizedStringFromNumber:@(self.countOfMatches) numberStyle:NSNumberFormatterDecimalStyle]];
}

// =====================
// = Show Tab… Submenu =
// =====================

- (IBAction)takeSelectedPathFrom:(id)sender
{
	FFResultNode* item = [sender representedObject];
	if([item isKindOfClass:[FFResultNode class]])
		[_windowController.resultsViewController showResultNode:item.firstResultNode];
}

- (void)updateShowTabMenu:(NSMenu*)aMenu
{
	if(self.countOfMatches == 0)
	{
		[[aMenu addItemWithTitle:@"No Results" action:@selector(nop:) keyEquivalent:@""] setEnabled:NO];
	}
	else
	{
		char key = 0;
		for(FFResultNode* parent in _results.children)
		{
			if(OakDocument* doc = parent.document)
			{
				NSMenuItem* item = [aMenu addItemWithTitle:(doc.path ? to_ns(path::relative_to(to_s(doc.path), to_s(self.searchFolder))) : doc.displayName) action:@selector(takeSelectedPathFrom:) keyEquivalent:key < 9 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];
				if(aMenu.propertiesToUpdate & NSMenuPropertyItemImage)
					[item setImage:parent.document.icon];
				[item setRepresentedObject:parent];
			}
		}
	}
}

// =====================
// = Copy Find Results =
// =====================

- (void)copyReplacements:(id)sender
{
	NSMutableArray* array = [NSMutableArray array];

	std::string const replacementString = to_s(_windowController.replaceString);
	for(FFResultNode* item in _windowController.resultsViewController.selectedResults)
	{
		auto const& captures = item.match.captures;
		[array addObject:captures.empty() ? _windowController.replaceString : to_ns(format_string::expand(replacementString, captures))];
	}

	[NSPasteboard.generalPasteboard clearContents];
	[NSPasteboard.generalPasteboard writeObjects:array];
}

- (void)copyEntireLines:(BOOL)entireLines withFilename:(BOOL)withFilename
{
	NSMutableArray* array = [NSMutableArray array];

	for(FFResultNode* item in _windowController.resultsViewController.selectedResults)
	{
		OakDocumentMatch* m = item.match;
		std::string str = to_s(m.excerpt);

		if(!entireLines)
			str = str.substr(m.first - m.excerptOffset, m.last - m.first);
		else if(str.size() && str.back() == '\n')
			str.erase(str.size()-1);

		if(withFilename)
			str = text::format("%s:%lu\t", [item.path UTF8String], m.lineNumber + 1) + str;

		[array addObject:to_ns(str)];
	}

	[NSPasteboard.generalPasteboard clearContents];
	[NSPasteboard.generalPasteboard writeObjects:array];
}

- (void)copy:(id)sender                          { [self copyEntireLines:YES withFilename:NO ]; }
- (void)copyMatchingParts:(id)sender             { [self copyEntireLines:NO  withFilename:NO ]; }
- (void)copyMatchingPartsWithFilename:(id)sender { [self copyEntireLines:NO  withFilename:YES]; }
- (void)copyEntireLines:(id)sender               { [self copyEntireLines:YES withFilename:NO ]; }
- (void)copyEntireLinesWithFilename:(id)sender   { [self copyEntireLines:YES withFilename:YES]; }

// =====================
// = Check/Uncheck All =
// =====================

- (void)allMatchesSetExclude:(BOOL)exclude
{
	_results.excluded = exclude;
}

- (IBAction)checkAll:(id)sender
{
	[self allMatchesSetExclude:NO];
}

- (IBAction)uncheckAll:(id)sender
{
	[self allMatchesSetExclude:YES];
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	static std::set<SEL> const copyActions = { @selector(copy:), @selector(copyReplacements:), @selector(copyMatchingParts:), @selector(copyMatchingPartsWithFilename:), @selector(copyEntireLines:), @selector(copyEntireLinesWithFilename:) };
	if(copyActions.find(aMenuItem.action) != copyActions.end())
		return [_results countOfLeafs] != 0;
	else if(aMenuItem.action == @selector(checkAll:))
		return _countOfExcludedMatches > _countOfExcludedReadOnlyMatches;
	else if(aMenuItem.action == @selector(uncheckAll:) )
		return _countOfExcludedMatches - _countOfExcludedReadOnlyMatches < _countOfMatches - _countOfReadOnlyMatches;
	return YES;
}
@end
