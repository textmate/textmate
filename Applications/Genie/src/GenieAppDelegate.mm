#import "GenieAppDelegate.h"
#import "GenieItemTableCellView.h"
#import "ClipboardHistory.h"
#import "GlobalHotkey.h"
#import <GenieManager/GenieManager.h>
#import <GenieManager/GenieRunner.h>
#import <GenieManager/GenieItem.h>
#import <GenieManager/GenieItemCollection.h>
#import <GenieManager/GenieLRUDatabase.h>
#import <MenuBuilder/MenuBuilder.h>
#import <SoftwareUpdate/SoftwareUpdate.h>
#import <OakFoundation/OakFoundation.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <text/ranker.h>
#import <text/utf8.h>
#import <ns/ns.h>
#import <oak/debug.h>
#import <os/log.h>
#import <WebKit/WebKit.h>
#import <ServiceManagement/ServiceManagement.h>

// ==========================
// = GenieHTMLTableCellView =
// ==========================

@interface WKWebViewDisableScrollGesture : WKWebView
@end

@implementation WKWebViewDisableScrollGesture
- (BOOL)acceptsFirstResponder
{
	return NO;
}

- (void)scrollWheel:(NSEvent*)anEvent
{
	if(ABS(anEvent.scrollingDeltaY) > ABS(anEvent.scrollingDeltaX))
			[self.nextResponder scrollWheel:anEvent];
	else	[super scrollWheel:anEvent];
}
@end

@interface GenieHTMLTableCellView : NSTableCellView <WKNavigationDelegate>
@property (nonatomic) WKWebView* webView;
@property (nonatomic) NSString* HTMLString;
@property (nonatomic) WKNavigation* currentNavigationRequest;
- (instancetype)init;
@end

static void* kHTMLBinding = &kHTMLBinding;
static NSString* HTMLItemDidUpdateHeightNotification = @"HTMLItemDidUpdateHeightNotification";

@implementation GenieHTMLTableCellView
- (instancetype)init
{
	if(self = [super init])
	{
		WKWebViewConfiguration* webConfig = [[WKWebViewConfiguration alloc] init];
		webConfig.suppressesIncrementalRendering = YES;

		_webView = [[WKWebViewDisableScrollGesture alloc] initWithFrame:NSZeroRect configuration:webConfig];
		_webView.navigationDelegate = self;

		NSDictionary* views = @{ @"html": _webView };
		OakAddAutoLayoutViewsToSuperview(views.allValues, self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[html]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[html]|" options:0 metrics:nil views:views]];

		// // Retain cycle! Could be solved by binding to WKWebView
		// [self bind:@"HTMLString" toObject:self withKeyPath:@"objectValue.html" options:nil];
	}
	return self;
}

- (void)setObjectValue:(id)newValue
{
	id oldValue = self.objectValue;
	if(oldValue)
	{
		[oldValue removeObserver:self forKeyPath:@"html" context:kHTMLBinding];
		if(_currentNavigationRequest)
		{
			_HTMLString = nil;
			_currentNavigationRequest = nil;
			[_webView stopLoading];
		}
	}

	[super setObjectValue:newValue];

	if(newValue)
		[newValue addObserver:self forKeyPath:@"html" options:NSKeyValueObservingOptionInitial context:kHTMLBinding];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)value change:(NSDictionary*)changeDictionary context:(void*)someContext
{
	if(someContext == kHTMLBinding && [keyPath isEqualToString:@"html"])
		self.HTMLString = [value valueForKey:@"html"];
}

- (void)setHTMLString:(NSString*)newHTMLString
{
	if([_HTMLString isEqualToString:newHTMLString])
		return;

	_currentNavigationRequest = nil;
	if(_HTMLString = newHTMLString)
	{
		@try {
			_currentNavigationRequest = [_webView loadHTMLString:_HTMLString baseURL:[NSURL URLWithString:@"file://localhost/"]];
		}
		@catch (NSException* e) {
			os_log_error(OS_LOG_DEFAULT, "exception loading HTML string: %@", e);
		}
	}
}

- (void)webView:(WKWebView*)webView didFinishNavigation:(WKNavigation*)navigation
{
	if(navigation != _currentNavigationRequest)
		return;
	_currentNavigationRequest = nil;

	if(NSDictionary* objectValue = self.objectValue)
	{
		__weak GenieHTMLTableCellView* weakSelf = self;
		[self.webView evaluateJavaScript:@"[ window.innerHeight, document.body.getBoundingClientRect().top, document.body.getBoundingClientRect().bottom ]" completionHandler:^(id res, NSError* error){
			if(weakSelf.objectValue == objectValue)
			{
				if([res isKindOfClass:[NSArray class]] && [res count] == 3)
				{
					// CGFloat curHeight = [[res objectAtIndex:0] doubleValue];
					CGFloat top       = [[res objectAtIndex:1] doubleValue];
					CGFloat bottom    = [[res objectAtIndex:2] doubleValue];
					CGFloat newHeight = bottom + top; // top gives us the top margin which we’ll re-use for bottom margin

					[[NSNotificationCenter defaultCenter] postNotificationName:HTMLItemDidUpdateHeightNotification object:objectValue userInfo:@{ @"height": @(newHeight) }];
				}
			}
		}];
	}
	else
	{
		NSLog(@"%s *** finished load HTML without any object value", sel_getName(_cmd));
	}
}
@end

// ==========================

static NSString* kEnableClipboardHistorySettingsKey = @"enableClipboardHistory";
static NSString* kDisableLaunchAtLoginSettingsKey   = @"disableLaunchAtLogin";
static NSString* kActivationKeyEventSettingsKey     = @"activationKeyEvent";

@interface MyTextField : NSTextField
@end

@implementation MyTextField
- (BOOL)mouseDownCanMoveWindow
{
	return YES;
}
@end

@interface MyFieldEditor : NSTextView
@end

@implementation MyFieldEditor
- (BOOL)mouseDownCanMoveWindow
{
	return YES;
}
@end

@interface GenieBorderlessWindow : NSPanel
@end

@implementation GenieBorderlessWindow
- (BOOL)canBecomeKeyWindow
{
	return YES;
}

- (BOOL)isMovableByWindowBackground
{
	return YES;
}
@end

@interface AppDelegate () <NSApplicationDelegate, QLPreviewPanelDelegate, QLPreviewPanelDataSource, NSWindowDelegate, NSTextFieldDelegate, NSTableViewDelegate, NSTableViewDataSource>
{
	BOOL _disableUserDefaultsNotification;
	GlobalHotkey* _hotkey;

	NSMutableArray<NSString*>* _historyArray;
	NSUInteger _nextHistoryIndex;

	NSWindow* _window;
	NSTextView* _sharedFieldEditor;

	NSProgressIndicator* _progressIndicator;
	NSTextField* _textField;
	NSTableView* _tableView;

	NSArray<id <QLPreviewItem>>* _previewItems;
}
@property (nonatomic) NSArray<GenieItemCollection*>* history;
@property (nonatomic, readonly) GenieItemCollection* collection;

@property (nonatomic) BOOL drawTableViewAsHighlighted;
@end

@implementation AppDelegate
+ (NSSet*)keyPathsForValuesAffectingCollection { return [NSSet setWithArray:@[ @"history" ]]; }

+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kEnableClipboardHistorySettingsKey : @NO,
		kDisableLaunchAtLoginSettingsKey   : @NO,
		kActivationKeyEventSettingsKey     : @"@ ",
	}];
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	if(_disableUserDefaultsNotification)
		return;
	_disableUserDefaultsNotification = YES;

	BOOL enableClipboardHistory = [[NSUserDefaults standardUserDefaults] boolForKey:kEnableClipboardHistorySettingsKey];
	if(![ClipboardHistory.sharedInstance trySetEnabled:enableClipboardHistory])
	{
		[[NSUserDefaults standardUserDefaults] setBool:ClipboardHistory.sharedInstance.enabled forKey:kEnableClipboardHistorySettingsKey];
		[[NSUserDefaults standardUserDefaults] synchronize];
		[[NSDistributedNotificationCenter defaultCenter] postNotificationName:NSUserDefaultsDidChangeNotification object:@"com.macromates.Genie" userInfo:nil deliverImmediately:YES];

		os_log_error(OS_LOG_DEFAULT, "Failed to monitor clipboard");
	}

	BOOL launchAtLogin = ![[NSUserDefaults standardUserDefaults] boolForKey:kDisableLaunchAtLoginSettingsKey];
	if(!SMLoginItemSetEnabled(CFSTR("com.macromates.GenieLauncher"), launchAtLogin))
	{
		[[NSUserDefaults standardUserDefaults] setBool:launchAtLogin forKey:kDisableLaunchAtLoginSettingsKey];
		[[NSUserDefaults standardUserDefaults] synchronize];
		[[NSDistributedNotificationCenter defaultCenter] postNotificationName:NSUserDefaultsDidChangeNotification object:@"com.macromates.Genie" userInfo:nil deliverImmediately:YES];

		os_log_error(OS_LOG_DEFAULT, "Failed to create login item for Genie");
	}

	NSString* activationKeyEvent = [[NSUserDefaults standardUserDefaults] stringForKey:kActivationKeyEventSettingsKey];
	if(activationKeyEvent && ![activationKeyEvent isEqualToString:_hotkey.eventString])
	{
		_hotkey = [GlobalHotkey globalHotkeyForEventString:activationKeyEvent handler:^OSStatus{
			[NSApp activateIgnoringOtherApps:YES];
			return noErr;
		}];
	}

	_disableUserDefaultsNotification = NO;
}

- (GenieItemCollection*)collection
{
	return _history.lastObject;
}

- (void)applicationWillFinishLaunching:(NSNotification*)aNotification
{
	MBMenu const items = {
		{ @"Genie",
			.submenu = {
				{ @"About Genie",      @selector(orderFrontStandardAboutPanel:)         },
				{ /* -------- */ },
				{ @"Preferences…",     @selector(orderFrontPreferences:),        @","   },
				{ /* -------- */ },
				{ @"Services", .systemMenu = MBMenuTypeServices                         },
				{ /* -------- */ },
				{ @"Hide Genie",       @selector(hide:),                         @"h"   },
				{ @"Hide Others",      @selector(hideOtherApplications:),        @"h", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Show All",         @selector(unhideAllApplications:)                },
				{ /* -------- */ },
				{ @"Quit Genie",       @selector(terminate:),                    @"q"   },
			}
		},
		{ @"File",
			.submenu = {
				{ @"New",             @selector(newDocument:),           @"n"   },
				{ @"Open…",           @selector(openDocument:),          @"o"   },
				{ @"Open Recent",
					.systemMenu = MBMenuTypeOpenRecent, .submenu = {
						{ @"Clear Menu", @selector(clearRecentDocuments:) },
					}
				},
				{ /* -------- */ },
				{ @"Close",           @selector(performClose:),          @"w"   },
				{ @"Close All",       @selector(closeAll:),              @"w", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .target = NSApp, .alternate = YES },
				{ @"Save…",           @selector(saveDocument:),          @"s"   },
				{ @"Save As…",        @selector(saveDocumentAs:),        @"S"   },
				{ @"Revert to Saved", @selector(revertDocumentToSaved:), @"r"   },
				{ /* -------- */ },
				{ @"Page Setup…",     @selector(runPageLayout:),         @"P"   },
				{ @"Print…",          @selector(print:),                 @"p"   },
			}
		},
		{ @"Edit",
			.submenu = {
				{ @"Undo",                  @selector(undo:),                  @"z"   },
				{ @"Redo",                  @selector(redo:),                  @"Z"   },
				{ /* -------- */ },
				{ @"Cut",                   @selector(cut:),                   @"x"   },
				{ @"Copy",                  @selector(copy:),                  @"c"   },
				{ @"Paste",                 @selector(paste:),                 @"v"   },
				{ @"Paste and Match Style", @selector(pasteAsPlainText:),      @"V", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Delete",                @selector(delete:)                        },
				{ @"Select All",            @selector(selectAllWithFallback:), @"a"   },
				{ @"Deselect All",          @selector(deselectAll:),           @"A"   },
				{ /* -------- */ },
				{ @"Find",
					.submenu = {
						{ @"Find…",                  @selector(performFindPanelAction:),       @"f", .tag = 1 },
						{ @"Find and Replace…",      @selector(performFindPanelAction:),       @"f", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = 12 },
						{ @"Find Next",              @selector(performFindPanelAction:),       @"g", .tag = 2 },
						{ @"Find Previous",          @selector(performFindPanelAction:),       @"G", .tag = 3 },
						{ @"Use Selection for Find", @selector(performFindPanelAction:),       @"e", .tag = 7 },
						{ @"Jump to Selection",      @selector(centerSelectionInVisibleArea:), @"j"   },
					}
				},
				{ @"Spelling and Grammar",
					.submenu = {
						{ @"Show Spelling and Grammar",      @selector(showGuessPanel:),                  @":" },
						{ @"Check Document Now",             @selector(checkSpelling:),                   @";" },
						{ /* -------- */ },
						{ @"Check Spelling While Typing",    @selector(toggleContinuousSpellChecking:)         },
						{ @"Check Grammar With Spelling",    @selector(toggleGrammarChecking:)                 },
						{ @"Correct Spelling Automatically", @selector(toggleAutomaticSpellingCorrection:)     },
					}
				},
				{ @"Substitutions",
					.submenu = {
						{ @"Show Substitutions", @selector(orderFrontSubstitutionsPanel:)     },
						{ /* -------- */ },
						{ @"Smart Copy/Paste",   @selector(toggleSmartInsertDelete:)          },
						{ @"Smart Quotes",       @selector(toggleAutomaticQuoteSubstitution:) },
						{ @"Smart Dashes",       @selector(toggleAutomaticDashSubstitution:)  },
						{ @"Smart Links",        @selector(toggleAutomaticLinkDetection:)     },
						{ @"Data Detectors",     @selector(toggleAutomaticDataDetection:)     },
						{ @"Text Replacement",   @selector(toggleAutomaticTextReplacement:)   },
					}
				},
				{ @"Transformations",
					.submenu = {
						{ @"Make Upper Case", @selector(uppercaseWord:)  },
						{ @"Make Lower Case", @selector(lowercaseWord:)  },
						{ @"Capitalize",      @selector(capitalizeWord:) },
					}
				},
				{ @"Speech",
					.submenu = {
						{ @"Start Speaking", @selector(startSpeaking:) },
						{ @"Stop Speaking",  @selector(stopSpeaking:)  },
					}
				},
			}
		},
		{ @"Format",
			.submenu = {
				{ @"Font",
					.systemMenu = MBMenuTypeFont, .submenu = {
						{ @"Show Fonts",  @selector(orderFrontFontPanel:),  @"t", .target = NSFontManager.sharedFontManager },
						{ @"Bold",        @selector(addFontTrait:),         @"b", .target = NSFontManager.sharedFontManager, .tag = 2 },
						{ @"Italic",      @selector(addFontTrait:),         @"i", .target = NSFontManager.sharedFontManager, .tag = 1 },
						{ @"Underline",   @selector(underline:),            @"u"   },
						{ /* -------- */ },
						{ @"Bigger",      @selector(modifyFont:),           @"+", .target = NSFontManager.sharedFontManager, .tag = 3 },
						{ @"Smaller",     @selector(modifyFont:),           @"-", .target = NSFontManager.sharedFontManager, .tag = 4 },
						{ /* -------- */ },
						{ @"Kern",
							.submenu = {
								{ @"Use Default", @selector(useStandardKerning:) },
								{ @"Use None",    @selector(turnOffKerning:)     },
								{ @"Tighten",     @selector(tightenKerning:)     },
								{ @"Loosen",      @selector(loosenKerning:)      },
							}
						},
						{ @"Ligatures",
							.submenu = {
								{ @"Use Default", @selector(useStandardLigatures:) },
								{ @"Use None",    @selector(turnOffLigatures:)     },
								{ @"Use All",     @selector(useAllLigatures:)      },
							}
						},
						{ @"Baseline",
							.submenu = {
								{ @"Use Default", @selector(unscript:)      },
								{ @"Superscript", @selector(superscript:)   },
								{ @"Subscript",   @selector(subscript:)     },
								{ @"Raise",       @selector(raiseBaseline:) },
								{ @"Lower",       @selector(lowerBaseline:) },
							}
						},
						{ /* -------- */ },
						{ @"Show Colors", @selector(orderFrontColorPanel:), @"C"   },
						{ /* -------- */ },
						{ @"Copy Style",  @selector(copyFont:),             @"c", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
						{ @"Paste Style", @selector(pasteFont:),            @"v", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
					}
				},
				{ @"Text",
					.submenu = {
						{ @"Align Left",  @selector(alignLeft:),    @"{"   },
						{ @"Center",      @selector(alignCenter:),  @"|"   },
						{ @"Justify",     @selector(alignJustified:)       },
						{ @"Align Right", @selector(alignRight:),   @"}"   },
						{ /* -------- */ },
						{ @"Writing Direction",
							.submenu = {
								{ @"Paragraph",                                                      .enabled = NO },
								{ @"Default",       @selector(makeBaseWritingDirectionNatural:),     .indent = 1 },
								{ @"Left to Right", @selector(makeBaseWritingDirectionLeftToRight:), .indent = 1 },
								{ @"Right to Left", @selector(makeBaseWritingDirectionRightToLeft:), .indent = 1 },
								{ /* -------- */ },
								{ @"Selection",                                                      .enabled = NO },
								{ @"Default",       @selector(makeTextWritingDirectionNatural:),     .indent = 1 },
								{ @"Left to Right", @selector(makeTextWritingDirectionLeftToRight:), .indent = 1 },
								{ @"Right to Left", @selector(makeTextWritingDirectionRightToLeft:), .indent = 1 },
							}
						},
						{ /* -------- */ },
						{ @"Show Ruler",  @selector(toggleRuler:)          },
						{ @"Copy Ruler",  @selector(copyRuler:),    @"c", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
						{ @"Paste Ruler", @selector(pasteRuler:),   @"v", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
					}
				},
			}
		},
		{ @"View",
			.submenu = {
				{ @"Show Toolbar",       @selector(toggleToolbarShown:),           @"t", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Customize Toolbar…", @selector(runToolbarCustomizationPalette:)       },
				{ /* -------- */ },
				{ @"Show Sidebar",       @selector(toggleSourceList:),             @"s", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
				{ @"Enter Full Screen",  @selector(toggleFullScreen:),             @"f", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
			}
		},
		{
			@"Go", .submenu = {
				{ @"Parent",        @selector(goToParentFolder:), .key = NSUpArrowFunctionKey   },
				{ @"Children",      @selector(goToChildItems:),   .key = NSDownArrowFunctionKey },
				{ @"Back to Start", @selector(goToRoot:),         @"/" },
				{ /* -------- */ },
				{ @"Back in History",    @selector(goToPreviousHistoryItem:), .key = NSUpArrowFunctionKey,   .modifierFlags = NSEventModifierFlagControl },
				{ @"Forward in History", @selector(goToNextHistoryItem:),     .key = NSDownArrowFunctionKey, .modifierFlags = NSEventModifierFlagControl },
			}
		},
		{ @"Window",
			.systemMenu = MBMenuTypeWindows, .submenu = {
				{ @"Minimize",           @selector(performMiniaturize:), @"m" },
				{ @"Zoom",               @selector(performZoom:)              },
				{ /* -------- */ },
				{ @"Bring All to Front", @selector(arrangeInFront:)           },
			}
		},
		{ @"Help",
			.systemMenu = MBMenuTypeHelp, .submenu = {
				{ @"Genie Help", @selector(showHelp:), @"?" },
			}
		},
	};

	if(NSMenu* menu = MBCreateMenu(items))
		NSApp.mainMenu = menu;

	[NSApp activateIgnoringOtherApps:YES];
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
	_historyArray     = [[[NSUserDefaults standardUserDefaults] arrayForKey:@"history"] mutableCopy] ?: [NSMutableArray array];
	_nextHistoryIndex = _historyArray.count;

	[self userDefaultsDidChange:nil];
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:@"com.macromates.GeniePrefs"];

	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(htmlItemDidUpdateHeight:) name:HTMLItemDidUpdateHeightNotification object:nil];

	_drawTableViewAsHighlighted = YES;

	_window = [[GenieBorderlessWindow alloc] initWithContentRect:NSMakeRect(0, 0, 400, 600) styleMask:NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskFullSizeContentView backing:NSBackingStoreBuffered defer:NO];
	_window.releasedWhenClosed         = NO;
	_window.titleVisibility            = NSWindowTitleHidden;
	_window.titlebarAppearsTransparent = YES;
	_window.delegate                   = self;

	[_window standardWindowButton:NSWindowCloseButton].hidden       = YES;
	[_window standardWindowButton:NSWindowMiniaturizeButton].hidden = YES;
	[_window standardWindowButton:NSWindowZoomButton].hidden        = YES;

	[_window center];
	_window.frameAutosaveName = @"Catalog";

	_textField = [MyTextField textFieldWithString:@""];
	_textField.delegate        = self;
	_textField.font            = [NSFont systemFontOfSize:21];
	_textField.bezeled         = NO;
	_textField.drawsBackground = NO;
	_textField.focusRingType   = NSFocusRingTypeNone;

	_progressIndicator = [[NSProgressIndicator alloc] initWithFrame:NSZeroRect];
	_progressIndicator.style                = NSProgressIndicatorSpinningStyle;
	_progressIndicator.displayedWhenStopped = NO;
	_progressIndicator.controlSize          = NSControlSizeSmall;

	static MBMenu const items = {
		{ /* Placeholder */ },
		{ @"Preferences…",          @selector(orderFrontPreferences:),      @"," },
		{ @"Check for Updates",     @selector(performSoftwareUpdateCheck:)       },
		{ @"Check for Test Builds", @selector(performSoftwareUpdateCheck:), .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .alternate = YES },
		{ /* -------- */ },
		{ @"Quick Look",            @selector(toggleQuickLookPreview:),     @"y" },
		{ @"Parent",                @selector(goToParentFolder:),           .key = NSUpArrowFunctionKey   },
		{ @"Children",              @selector(goToChildItems:),             .key = NSDownArrowFunctionKey },
		{ @"Back to Start",         @selector(goToRoot:),                   @"/" },
		{ /* -------- */ },
		{ @"Back in History",       @selector(goToPreviousHistoryItem:), .key = NSUpArrowFunctionKey,   .modifierFlags = NSEventModifierFlagControl },
		{ @"Forward in History",    @selector(goToNextHistoryItem:),     .key = NSDownArrowFunctionKey, .modifierFlags = NSEventModifierFlagControl },
		{ /* -------- */ },
		{ @"Quit Genie",            @selector(terminate:),                  @"q" },
	};

	NSPopUpButton* popUpButton = OakCreateActionPopUpButton();
	popUpButton.menu = MBCreateMenu(items);

	NSView* wrappedPopUpButton = [NSView new];
	OakAddAutoLayoutViewsToSuperview(@[ popUpButton ], wrappedPopUpButton);
	[wrappedPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[popup]|" options:0 metrics:nil views:@{ @"popup" : popUpButton }]];
	[wrappedPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[popup]|" options:0 metrics:nil views:@{ @"popup" : popUpButton }]];

	_tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
	_tableView.allowsMultipleSelection = YES;
	_tableView.allowsEmptySelection    = NO;
	_tableView.refusesFirstResponder   = YES;
	_tableView.headerView              = nil;
	_tableView.dataSource              = self;
	_tableView.delegate                = self;
	_tableView.doubleAction            = @selector(performDoubleClick:);
	[_tableView addTableColumn:[[NSTableColumn alloc] initWithIdentifier:@"title"]];
	[_tableView sizeLastColumnToFit];

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller = YES;
	scrollView.autohidesScrollers  = YES;
	scrollView.borderType          = NSNoBorder;
	scrollView.documentView        = _tableView;

	NSView* contentView = _window.contentView;

	NSDictionary* views = @{
		@"query":  _textField,
		@"busy":   _progressIndicator,
		@"popup":  wrappedPopUpButton,
		@"items":  scrollView,
	};
	OakAddAutoLayoutViewsToSuperview(views.allValues, contentView);
	_window.initialFirstResponder = _textField;

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(16)-[query]-[busy]-[popup(==30)]-(8)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[items(>=100)]|" options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(8)-[query]-(8)-[items(>=100)]|" options:0 metrics:nil views:views]];

	[_tableView bind:NSContentBinding toObject:self withKeyPath:@"collection.arrangedObjects" options:nil];
	[_tableView bind:NSSelectionIndexesBinding toObject:self withKeyPath:@"collection.selectionIndexes" options:nil];

	[_textField bind:NSValueBinding toObject:self withKeyPath:@"collection.queryString" options:@{ NSContinuouslyUpdatesValueBindingOption: @YES, NSNullPlaceholderBindingOption: @"Genie Search" }];
	[_progressIndicator bind:NSAnimateBinding toObject:self withKeyPath:@"collection.busy" options:nil];

	[self goToRoot:self];
	[_window makeKeyAndOrderFront:self];

	// =========================
	// = Setup Software Update =
	// =========================

	SoftwareUpdate* swUpdate = SoftwareUpdate.sharedInstance;
	NSOperatingSystemVersion osVersion = NSProcessInfo.processInfo.operatingSystemVersion;
	NSString* appVersion = [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleShortVersionString"];;
	NSString* parms = [NSString stringWithFormat:@"v=%@&os=%ld.%ld.%ld", [appVersion stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLQueryAllowedCharacterSet], osVersion.majorVersion, osVersion.minorVersion, osVersion.patchVersion];
	[swUpdate setSignee:key_chain_t::key_t("org.textmate.duff", "Allan Odgaard", "-----BEGIN PUBLIC KEY-----\nMIIBtjCCASsGByqGSM44BAEwggEeAoGBAPIE9PpXPK3y2eBDJ0dnR/D8xR1TiT9m\n8DnPXYqkxwlqmjSShmJEmxYycnbliv2JpojYF4ikBUPJPuerlZfOvUBC99ERAgz7\nN1HYHfzFIxVo1oTKWurFJ1OOOsfg8AQDBDHnKpS1VnwVoDuvO05gK8jjQs9E5LcH\ne/opThzSrI7/AhUAy02E9H7EOwRyRNLofdtPxpa10o0CgYBKDfcBscidAoH4pkHR\nIOEGTCYl3G2Pd1yrblCp0nCCUEBCnvmrWVSXUTVa2/AyOZUTN9uZSC/Kq9XYgqwj\nhgzqa8h/a8yD+ao4q8WovwGeb6Iso3WlPl8waz6EAPR/nlUTnJ4jzr9t6iSH9owS\nvAmWrgeboia0CI2AH++liCDvigOBhAACgYAFWO66xFvmF2tVIB+4E7CwhrSi2uIk\ndeBrpmNcZZ+AVFy1RXJelNe/cZ1aXBYskn/57xigklpkfHR6DGqpEbm6KC/47Jfy\ny5GEx+F/eBWEePi90XnLinytjmXRmS2FNqX6D15XNG1xJfjociA8bzC7s4gfeTUd\nlpQkBq2z71yitA==\n-----END PUBLIC KEY-----\n")];
	[swUpdate setChannels:@{
		kSoftwareUpdateChannelRelease : [NSURL URLWithString:[@"https://genie.macromates.com/version/release?" stringByAppendingString:parms]],
	}];

	if(NSString* oldVersion = [[NSUserDefaults standardUserDefaults] stringForKey:@"didUpdateFrom"])
	{
		NSUserNotification* notification = [[NSUserNotification alloc] init];
		notification.title           = @"Updated Successfully";
		notification.informativeText = [NSString stringWithFormat:@"You are now running version %@ (updated from %@)", [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleShortVersionString"], oldVersion];
		[[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:notification];

		[self launchPreferencesWithArguments:@[ @"-showChanges", @"YES" ]];
	}
}

- (void)applicationDidBecomeActive:(NSNotification*)aNotification
{
	self.collection.live = YES;
	[_window makeKeyAndOrderFront:self];
}

- (void)applicationDidResignActive:(NSNotification*)aNotification
{
	self.collection.live = NO;
}

- (id)windowWillReturnFieldEditor:(NSWindow*)aWindow toObject:(id)someObject
{
	if(someObject != _textField)
		return nil;

	if(!_sharedFieldEditor)
	{
		_sharedFieldEditor = [[MyFieldEditor alloc] initWithFrame:NSZeroRect];
		_sharedFieldEditor.fieldEditor = YES;
	}
	return _sharedFieldEditor;
}

- (void)windowDidBecomeKey:(NSNotification*)aNotification
{
	[_window makeFirstResponder:_window.initialFirstResponder];
}

- (BOOL)windowShouldClose:(id)sender
{
	[NSApp hide:self];
	return YES;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self addToHistory:self.collection.queryString];
	[[NSUserDefaults standardUserDefaults] setObject:_historyArray forKey:@"history"];

	// We do this after delay to avoid seeing the busy indicator when closing the window
	[self performSelector:@selector(goToRoot:) withObject:self afterDelay:0];

	// TODO Should set: collection.live = NO
}

- (void)performHideAndClose:(id)sender
{
	[NSApp hide:self];
	[_window performClose:self];
}

- (void)performTab:(id)sender
{
	if([self goToChildItems:sender] == 0)
	{
		if(GenieItem* item = self.collection.selectedObjects.firstObject)
		{
			if(NSString* value = item.value)
			{
				[self addToHistory:self.collection.queryString];
				[self setNewQueryString:value];
			}
		}
	}
}

- (void)performReturn:(id)sender
{
	[self executeItems:sender];
}

- (void)performDoubleClick:(id)sender
{
	[self executeItems:sender];
}

- (BOOL)control:(NSControl*)aControl textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aCommand
{
	NSEvent* currentEvent = [NSApp currentEvent];
	if(currentEvent.type == NSEventTypeKeyDown && to_s(currentEvent) == "@c")
		return [self tryToPerform:@selector(copy:) with:aTextView];

	if(aCommand == @selector(cancel:) || aCommand == @selector(cancelOperation:))
		return [self performHideAndClose:aTextView], YES;
	else if(aCommand == @selector(insertTab:))
		return [self performTab:aTextView], YES;
	else if(aCommand == @selector(insertBacktab:))
		return [self goToParentFolder:aTextView], YES;

	NSUInteger res = OakPerformTableViewActionFromSelector(_tableView, aCommand);
	if(res == OakMoveAcceptReturn)
		return [self performReturn:aTextView], YES;
	else if(res == OakMoveCancelReturn)
		return [self performHideAndClose:self], YES;
	else if(res == OakMoveNoActionReturn)
		NSLog(@"%s %@", sel_getName(_cmd), NSStringFromSelector(aCommand));
	return res != OakMoveNoActionReturn;
}

- (void)logAction:(NSString*)someAction forItems:(NSArray*)selectedItems
{
	for(GenieItem* item in selectedItems.reverseObjectEnumerator)
	{
		GenieFilter* filter = self.collection.filter;

		BOOL fullString = item.isFallback || item.kind == kGenieItemKindCommandResult;
		NSString* queryString  = fullString ? filter.string : filter.queryString;
		NSString* filterString = item.acceptsQuery ? filter.filterString : filter.normalizedString;

		NSMutableDictionary* event = [NSMutableDictionary dictionary];
		event[@"identifier"] = item.identifierWithContext;
		event[@"title"]      = item.title;
		event[@"date"]       = [NSDate date];
		event[@"event"]      = someAction;
		event[@"query"]      = OakNotEmptyString(queryString)  ? queryString  : nil;
		event[@"filter"]     = OakNotEmptyString(filterString) ? filterString : nil;

		[GenieLRUDatabase.sharedInstance addEvent:event];
	}
}

- (void)goToRoot:(id)sender
{
	self.collection.live = NO;
	self.history = @[ GenieItemCollection.defaultCollection ];
	self.collection.live = YES;

	[_tableView scrollRowToVisible:0];
}

- (BOOL)goToChildItems:(id)sender
{
	if(GenieItem* item = self.collection.selectedObjects.firstObject)
	{
		NSMutableArray* children = [NSMutableArray array];

		for(GenieItem* predicateItem in [GenieManager.sharedInstance.items filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"disabled = NO && kind = %ld", kGenieItemKindPredicateGroup]])
		{
			if([item matchesPredicate:[NSPredicate predicateWithFormat:[predicateItem staticValueForKey:@"predicate"]]])
			{
				for(GenieItem* predicateChild in predicateItem.children)
					[children addObject:[predicateChild copyWithNewParent:item]];
			}
		}

		if(NSArray* items = item.children)
			[children addObjectsFromArray:items];

		if(children.count)
		{
			[self logAction:@"tab" forItems:@[ item ]];

			self.collection.live = NO;
			self.history = [self.history arrayByAddingObject:[[GenieItemCollection alloc] initWithItems:children]];
			self.collection.live = YES;

			[_tableView scrollRowToVisible:0];

			return YES;
		}
	}
	return NO;
}

- (void)goToParentFolder:(id)sender
{
	if(self.history.count > 1)
	{
		[_tableView unbind:NSSelectionIndexesBinding];
		self.collection.live = NO;
		self.history = [self.history subarrayWithRange:NSMakeRange(0, self.history.count-1)];
		self.collection.live = YES;
		[_tableView bind:NSSelectionIndexesBinding toObject:self withKeyPath:@"collection.selectionIndexes" options:nil];

		NSIndexSet* indexSet = self.collection.selectionIndexes;
		[_tableView scrollRowToVisible:indexSet.count ? indexSet.firstIndex : 0];
	}
}

- (void)executeItems:(id)sender
{
	NSArray* selectedItems = self.collection.selectedObjects;
	if(selectedItems.count)
	{
		[self logAction:@"return" forItems:selectedItems];

		__weak __block id appDidDeactivateObserver = [[[NSWorkspace sharedWorkspace] notificationCenter] addObserverForName:NSWorkspaceDidDeactivateApplicationNotification object:nil queue:nil usingBlock:^(NSNotification* n){
			[_window close];
			[[[NSWorkspace sharedWorkspace] notificationCenter] removeObserver:appDidDeactivateObserver];

			if(!RunGenieItems(selectedItems))
				NSBeep();
		}];
		[NSApp hide:self];
	}
}

// ===================
// = History Support =
// ===================

- (void)setNewQueryString:(NSString*)newString
{
	self.collection.queryString = newString;
	if(_window.firstResponder == _sharedFieldEditor)
		[_sharedFieldEditor setSelectedRange:NSMakeRange(_sharedFieldEditor.string.length, 0)];
}

- (void)addToHistory:(NSString*)aString
{
	if(OakNotEmptyString(aString) && ![_historyArray.lastObject isEqualToString:aString])
		[_historyArray addObject:aString];
	_nextHistoryIndex = _historyArray.count;
}

- (void)goToPreviousHistoryItem:(id)sender
{
	if(_nextHistoryIndex > 0)
		[self setNewQueryString:_historyArray[--_nextHistoryIndex]];
}

- (void)goToNextHistoryItem:(id)sender
{
	if(_nextHistoryIndex < _historyArray.count)
	{
		if(++_nextHistoryIndex == _historyArray.count)
				[self setNewQueryString:@""];
		else	[self setNewQueryString:_historyArray[_nextHistoryIndex]];
	}
}

// ====================================
// = TableView Data Source / Delegate =
// ====================================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return self.collection.arrangedObjects.count;
}

- (NSView*)tableView:(NSTableView*)aTableView viewForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	id item = self.collection.arrangedObjects[rowIndex];

	BOOL regularItem = [item isKindOfClass:[GenieItem class]];
	NSString* identifier = regularItem ? @"GenieItem" : @"HTMLOutputItem";

	NSTableCellView* res = [aTableView makeViewWithIdentifier:identifier owner:self];
	if(!res)
	{
		if(regularItem)
				res = [[GenieItemTableCellView alloc] init];
		else	res = [[GenieHTMLTableCellView alloc] init];
		res.identifier = identifier;
	}
	else
	{
#if 0
		if(!regularItem)
		{
			// If we reuse a WebView we decrease height since content height is never less than frame height
			NSRect frame = res.frame;
			frame.size.height = [item[@"height"] doubleValue] ?: 1;
			res.frame = frame;
			[res layoutSubtreeIfNeeded];
		}
#endif
	}

	res.objectValue = item;
	return res;
}

- (NSTableRowView*)tableView:(NSTableView*)tableView rowViewForRow:(NSInteger)row
{
	return [GenieInactiveTableRowView new];
}

- (void)tableView:(NSTableView*)tableView didAddRowView:(NSTableRowView*)rowView forRow:(NSInteger)row
{
	[(GenieInactiveTableRowView*)rowView setDrawAsHighlighted:_drawTableViewAsHighlighted];
}

- (void)setDrawTableViewAsHighlighted:(BOOL)flag
{
	_drawTableViewAsHighlighted = flag;
	[_tableView enumerateAvailableRowViewsUsingBlock:^(NSTableRowView* rowView, NSInteger row){
		[(GenieInactiveTableRowView*)rowView setDrawAsHighlighted:flag];
	}];
}

- (CGFloat)tableView:(NSTableView*)aTableView heightOfRow:(NSInteger)rowIndex
{
	id item = self.collection.arrangedObjects[rowIndex];
	BOOL regularItem = [item isKindOfClass:[GenieItem class]];
	return regularItem ? 38 : ([[item valueForKey:@"height"] doubleValue] ?: 1);
}

- (BOOL)tableView:(NSTableView*)aTableView shouldSelectRow:(NSInteger)rowIndex
{
	id item = self.collection.arrangedObjects[rowIndex];
	return ![[item valueForKey:@"readOnly"] boolValue];
}

// ====================================

- (void)tableViewSelectionDidChange:(NSNotification*)aNotification
{
	if(QLPreviewPanel.sharedPreviewPanel.dataSource == self)
	{
		_previewItems = self.previewableItems;
		[QLPreviewPanel.sharedPreviewPanel reloadData];
	}

	NSArray<GenieItem*>* arrangedObjects = self.collection.arrangedObjects;
	if(_tableView.numberOfRows != arrangedObjects.count)
	{
		NSLog(@"%s *** table view not yet reloaded: %ld rows in table versus %ld in arranged objects", sel_getName(_cmd), _tableView.numberOfRows, arrangedObjects.count);
		return;
	}

	// FIXME Only ask to update HTML if we haven’t already asked in this “show/hide” session
	for(GenieItem* item in self.collection.selectedObjects)
	{
		if([item respondsToSelector:@selector(updateHTML)])
			[item updateHTML];
	}

	NSIndexSet* hiddenRows   = _tableView.hiddenRowIndexes;
	NSIndexSet* selectedRows = _tableView.selectedRowIndexes;

	NSMutableIndexSet* shouldShow = [NSMutableIndexSet indexSet];
	NSMutableIndexSet* shouldHide = [NSMutableIndexSet indexSet];

	for(NSUInteger i = 0; i < arrangedObjects.count; ++i)
	{
		GenieItem* item = arrangedObjects[i];
		if([item isKindOfClass:[GenieItem class]] && item.hasHTMLOutput)
		{
			if([selectedRows containsIndex:i])
			{
				if([hiddenRows containsIndex:i+1])
					[shouldShow addIndex:i+1];
			}
			else if(![hiddenRows containsIndex:i+1])
			{
				[shouldHide addIndex:i+1];
			}
		}
	}

	if(shouldShow.count || shouldHide.count)
	{
		[NSAnimationContext beginGrouping];
		[NSAnimationContext currentContext].duration = 0;
		[_tableView hideRowsAtIndexes:shouldHide withAnimation:NSTableViewAnimationEffectNone];
		[_tableView unhideRowsAtIndexes:shouldShow withAnimation:NSTableViewAnimationEffectNone];
		[NSAnimationContext endGrouping];
	}
}

- (void)htmlItemDidUpdateHeight:(NSNotification*)aNotification
{
	NSMutableDictionary* item = aNotification.object;
	NSNumber* height = aNotification.userInfo[@"height"];
	item[@"height"] = height;

	NSInteger rowIndex = [self.collection.arrangedObjects indexOfObject:(GenieItem*)item];
	if(rowIndex == NSNotFound)
	{
		NSLog(@"%s *** item not found, new height %.0f", sel_getName(_cmd), [height doubleValue]);
		return;
	}

	[_tableView noteHeightOfRowsWithIndexesChanged:[NSIndexSet indexSetWithIndex:rowIndex]];
}

// ================
// = Menu Actions =
// ================

- (void)selectAllWithFallback:(id)sender
{
	NSText* fieldEditor = (NSText*)_window.firstResponder;
	if([fieldEditor isKindOfClass:[NSText class]])
	{
		if(OakNotEmptyString(fieldEditor.string))
			return [fieldEditor selectAll:sender];
	}
	[_tableView selectAll:sender];
}

- (void)deselectAll:(id)sender
{
	if(_tableView.allowsEmptySelection)
	{
		[_tableView deselectAll:sender];
	}
	else if(_tableView.selectedRow != -1)
	{
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:0] byExtendingSelection:NO];
		[_tableView scrollRowToVisible:0];
	}
}

- (void)copy:(id)sender
{
	NSArray<GenieItem*>* selectedItems = self.collection.selectedObjects;
	if(selectedItems.count == 1)
	{
		if(NSArray* pboardItems = selectedItems.firstObject.clipboardRepresentations)
		{
			[[NSPasteboard generalPasteboard] clearContents];
			[[NSPasteboard generalPasteboard] writeObjects:pboardItems];
		}
	}
	else if(selectedItems.count > 1)
	{
		NSMutableString* string = [NSMutableString string];
		for(GenieItem* item in selectedItems)
		{
			for(id value in item.clipboardRepresentations)
			{
				if([value isKindOfClass:[NSString class]])
				{
					[string appendString:value];
					if(![value hasSuffix:@"\n"])
						[string appendString:@"\n"];
					break;
				}
			}
		}
		[[NSPasteboard generalPasteboard] clearContents];
		[[NSPasteboard generalPasteboard] writeObjects:@[ string ]];
	}
}

- (void)launchPreferencesWithArguments:(NSArray*)someArguments
{
	if(NSURL* url = [NSBundle.mainBundle URLForResource:@"GeniePrefs" withExtension:@"app"])
	{
		NSArray* baseArguments = @[ NSBundle.mainBundle.bundleIdentifier ];
		someArguments = someArguments ? [baseArguments arrayByAddingObjectsFromArray:someArguments] : baseArguments;

		[self performHideAndClose:self];
		[[NSWorkspace sharedWorkspace] launchApplicationAtURL:url options:NSWorkspaceLaunchDefault configuration:@{ NSWorkspaceLaunchConfigurationArguments: someArguments } error:nullptr];
	}
	else
	{
		os_log_error(OS_LOG_DEFAULT, "Failed to find GeniePrefs.app");
	}
}

- (void)orderFrontPreferences:(id)sender
{
	[self launchPreferencesWithArguments:nil];
}

- (void)performSoftwareUpdateCheck:(id)sender
{
	[SoftwareUpdate.sharedInstance checkForUpdates:self];
}

// ======================
// = Validate Menu Item =
// ======================

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if(aMenuItem.action == @selector(goToParentFolder:))
		return _history.count > 1;
	else if(aMenuItem.action == @selector(toggleQuickLookPreview:))
		return self.previewableItems.count;
	else if(aMenuItem.action == @selector(goToPreviousHistoryItem:))
		return _nextHistoryIndex > 0;
	else if(aMenuItem.action == @selector(goToNextHistoryItem:))
		return _nextHistoryIndex < _historyArray.count;
	return YES;
}

// =============
// = QuickLook =
// =============

- (NSArray<GenieItem*>*)previewableItems
{
	return [self.collection.selectedObjects filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"previewItemURL != nil"]];
}

- (void)toggleQuickLookPreview:(id)sender
{
	if([QLPreviewPanel sharedPreviewPanelExists] && [[QLPreviewPanel sharedPreviewPanel] isVisible])
			[[QLPreviewPanel sharedPreviewPanel] orderOut:nil];
	else	[[QLPreviewPanel sharedPreviewPanel] makeKeyAndOrderFront:nil];
}

- (BOOL)acceptsPreviewPanelControl:(QLPreviewPanel*)previewPanel
{
	return YES;
}

- (void)beginPreviewPanelControl:(QLPreviewPanel*)previewPanel
{
	_previewItems = self.previewableItems;
	previewPanel.delegate   = self;
	previewPanel.dataSource = self;
}

- (void)endPreviewPanelControl:(QLPreviewPanel*)previewPanel
{
	_previewItems = nil;
}

- (NSInteger)numberOfPreviewItemsInPreviewPanel:(QLPreviewPanel*)previewPanel
{
	return _previewItems.count;
}

- (id <QLPreviewItem>)previewPanel:(QLPreviewPanel*)panel previewItemAtIndex:(NSInteger)index
{
	return _previewItems[index];
}

- (NSRect)previewPanel:(QLPreviewPanel*)previewPanel sourceFrameOnScreenForPreviewItem:(id <QLPreviewItem>)item
{
	NSInteger row = [self.collection.arrangedObjects indexOfObject:item];
	if(row != NSNotFound)
	{
		GenieItemTableCellView* view = [_tableView viewAtColumn:0 row:row makeIfNecessary:YES];
		if([view isKindOfClass:[GenieItemTableCellView class]])
		{
			NSImageView* imageView = view.imageView;
			return [_window convertRectToScreen:[imageView convertRect:imageView.bounds toView:nil]];
		}
	}
	return NSZeroRect;
}

- (BOOL)previewPanel:(QLPreviewPanel*)previewPanel handleEvent:(NSEvent*)event
{
	std::string const eventString = to_s(event);
	if(event.type == NSEventTypeKeyDown && eventString == "@y")
	{
		[self toggleQuickLookPreview:self];
	}
	else if((event.type == NSEventTypeKeyUp || event.type == NSEventTypeKeyDown) && (eventString == utf8::to_s(NSUpArrowFunctionKey) || eventString == utf8::to_s(NSDownArrowFunctionKey)))
	{
		[_window sendEvent:event];
		_previewItems = self.previewableItems;
		[previewPanel reloadData];
	}
	else
	{
		return NO;
	}

	return YES;
}
@end
