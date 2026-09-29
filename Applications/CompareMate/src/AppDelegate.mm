#import "AppDelegate.h"
#import "NewComparisonWindowController.h"
#import "SettingsWindowController.h"
#import "WindowController.h"
#import <MenuBuilder/src/MenuBuilder.h>
#import <BundlesManager/src/BundlesManager.h>
#import <settings/src/settings.h>
#import <theme/src/theme.h>
#import <io/src/path.h>

@interface AppDelegate () <NSApplicationDelegate, NSWindowDelegate, NSMenuItemValidation>
@property (nonatomic) NSWindow* window;
@property (nonatomic) NewComparisonWindowController* comparisonChooserController;
@property (nonatomic) SettingsWindowController* settingsWindowController;
@property (nonatomic) BOOL applicationFinishedLaunching;
@property (nonatomic) BOOL applicationFinishedRestoringWindows;
@end

@implementation AppDelegate
+ (void)initialize
{
	[NSUserDefaults.standardUserDefaults registerDefaults:@{
		@"universalThemeUUID": @(kMacClassicThemeUUID),
		@"darkModeThemeUUID":  @(kTwilightThemeUUID),
		CompareMateIgnoredFileNamesDefaultsKey: @[ @".DS_Store", @".git" ],
	}];
}

- (void)applicationWillFinishLaunching:(NSNotification*)aNotification
{
	settings_t::set_default_settings_path([[[NSBundle mainBundle] pathForResource:@"Default" ofType:@"tmProperties"] fileSystemRepresentation]);
	settings_t::set_global_settings_path(path::join(path::home(), "Library/Application Support/TextMate/Global.tmProperties"));
	[BundlesManager.sharedInstance loadBundlesIndex];
	[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(applicationDidFinishRestoringWindows:) name:NSApplicationDidFinishRestoringWindowsNotification object:NSApp];

	MBMenu const items = {
		{ @"CompareMate",
			.submenu = {
				{ @"About CompareMate", @selector(orderFrontStandardAboutPanel:)         },
				{ /* -------- */ },
				{ @"Settings…",            @selector(showSettings:),                  @",", .target = self },
				{ /* -------- */ },
				{ @"Services", .systemMenu = MBMenuTypeServices                             },
				{ /* -------- */ },
				{ @"Hide CompareMate",      @selector(hide:),                         @"h"   },
				{ @"Hide Others",          @selector(hideOtherApplications:),        @"h", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Show All",             @selector(unhideAllApplications:)                },
				{ /* -------- */ },
				{ @"Quit CompareMate",      @selector(terminate:),                    @"q"   },
			}
		},
		{ @"File",
			.submenu = {
				{ @"New Comparison…", @selector(newComparison:),         @"n", .target = self },
				{ @"Open…",           @selector(openDocument:),          @"o"   },
				{ @"Open Recent",
					.systemMenu = MBMenuTypeOpenRecent, .submenu = {
						{ @"Clear Menu", @selector(clearRecentDocuments:) },
					}
				},
				{ /* -------- */ },
				{ @"Close",           @selector(performClose:),          @"w"   },
				{ @"Close All",       @selector(closeAll:),              @"w", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .target = NSApp, .alternate = YES },
				{ @"Save",            @selector(saveDocument:),          @"s", .target = self },
				{ @"Save As…",        @selector(saveDocumentAs:),        @"S", .target = self },
				{ @"Revert to Saved", @selector(revertDocumentToSaved:), @"r"   },
				{ /* -------- */ },
				{ @"Page Setup…",     @selector(runPageLayout:),         @"P"   },
				{ @"Print…",          @selector(print:),                 @"p"   },
			}
		},
		{ @"Edit",
			.submenu = {
				{ @"Undo",                  @selector(undo:),             @"z"   },
				{ @"Redo",                  @selector(redo:),             @"Z"   },
				{ /* -------- */ },
				{ @"Cut",                   @selector(cut:),              @"x"   },
				{ @"Copy",                  @selector(copy:),             @"c"   },
				{ @"Paste",                 @selector(paste:),            @"v"   },
				{ @"Paste and Match Style", @selector(pasteAsPlainText:), @"V", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Delete",                @selector(delete:)                   },
				{ @"Select All",            @selector(selectAll:),        @"a"   },
				{ /* -------- */ },
				{ @"Find",
					.submenu = {
						{ @"Find…",                  @selector(performTextFinderAction:),      @"f",                                                                        .tag = NSTextFinderActionShowFindInterface    },
						{ @"Find and Replace…",      @selector(performTextFinderAction:),      @"f", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = NSTextFinderActionShowReplaceInterface },
						{ @"Find Next",              @selector(performTextFinderAction:),      @"g",                                                                        .tag = NSTextFinderActionNextMatch            },
						{ @"Find Previous",          @selector(performTextFinderAction:),      @"G",                                                                        .tag = NSTextFinderActionPreviousMatch        },
						{ @"Use Selection for Find", @selector(performTextFinderAction:),      @"e",                                                                        .tag = NSTextFinderActionSetSearchString      },
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
		{ @"Changes",
			.submenu = {
				MBMenuItem{ @"Open File Comparison", @selector(openSelectedFile:) }
					.withModifierFlags(NSEventModifierFlagCommand)
					.withKey(NSDownArrowFunctionKey)
					.withTarget(self),
				{ /* -------- */ },
				MBMenuItem{ @"Previous Change", @selector(previousChange:) }
					.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption)
					.withKey(NSUpArrowFunctionKey)
					.withTarget(self),
				MBMenuItem{ @"Next Change", @selector(nextChange:) }
					.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption)
					.withKey(NSDownArrowFunctionKey)
					.withTarget(self),
				{ /* -------- */ },
				MBMenuItem{ @"Copy Change to Left", @selector(copyChangeToLeft:) }
					.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption)
					.withKey(NSLeftArrowFunctionKey)
					.withTarget(self),
				MBMenuItem{ @"Copy Change to Right", @selector(copyChangeToRight:) }
					.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption)
					.withKey(NSRightArrowFunctionKey)
					.withTarget(self),
				{ /* -------- */ },
				MBMenuItem{ @"Move File to Left", @selector(moveChangeToLeft:) }
					.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagShift)
					.withKey(NSLeftArrowFunctionKey)
					.withTarget(self),
				MBMenuItem{ @"Move File to Right", @selector(moveChangeToRight:) }
					.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagShift)
					.withKey(NSRightArrowFunctionKey)
					.withTarget(self),
			}
		},
		{ @"Format",
			.submenu = {
				{ @"Font",
					.systemMenu = MBMenuTypeFont, .submenu = {
						{ @"Show Fonts",  @selector(orderFrontFontPanel:),  @"t",                              .target = NSFontManager.sharedFontManager },
						{ @"Bold",        @selector(addFontTrait:),         @"b", .tag = NSBoldFontMask ,      .target = NSFontManager.sharedFontManager },
						{ @"Italic",      @selector(addFontTrait:),         @"i", .tag = NSItalicFontMask ,    .target = NSFontManager.sharedFontManager },
						{ @"Underline",   @selector(underline:),            @"u"   },
						{ /* -------- */ },
						{ @"Bigger",      @selector(modifyFont:),           @"+", .tag = NSSizeUpFontAction,   .target = NSFontManager.sharedFontManager },
						{ @"Smaller",     @selector(modifyFont:),           @"-", .tag = NSSizeDownFontAction, .target = NSFontManager.sharedFontManager },
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
				{ @"Show Toolbar",         @selector(toggleToolbarShown:),           @"t", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Customize Toolbar…",   @selector(runToolbarCustomizationPalette:)     },
				{ /* -------- */ },
				{ @"Show Sidebar",         @selector(toggleSourceList:),             @"s", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
				{ @"Enter Full Screen",    @selector(toggleFullScreen:),             @"f", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
				{ /* -------- */ },
				{ @"Customize Touch Bar…", @selector(toggleTouchBarCustomizationPalette:) },
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
				{ @"CompareMate Help", @selector(showHelp:), @"?" },
			}
		},
	};

	if(NSMenu* menu = MBCreateMenu(items))
		NSApp.mainMenu = menu;
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
	self.applicationFinishedLaunching = YES;
	[self showInitialWindowIfReady];
}

- (void)applicationDidFinishRestoringWindows:(NSNotification*)notification
{
	self.applicationFinishedRestoringWindows = YES;
	[self showInitialWindowIfReady];
}

- (void)showInitialWindowIfReady
{
	if(!self.applicationFinishedLaunching || !self.applicationFinishedRestoringWindows)
		return;

	for(NSWindow* window in NSApp.windows)
	{
		if([window.windowController isKindOfClass:WindowController.class] || [window.windowController isKindOfClass:FolderWindowController.class])
			return;
	}

	[self newComparison:self];
}

- (IBAction)showSettings:(id)sender
{
	if(!self.settingsWindowController)
		self.settingsWindowController = [[SettingsWindowController alloc] init];
	[self.settingsWindowController showWindow:sender];
	[self.settingsWindowController.window makeKeyAndOrderFront:sender];
}

- (IBAction)newComparison:(id)sender
{
	if(self.comparisonChooserController.window.isVisible)
	{
		[self.comparisonChooserController.window makeKeyAndOrderFront:sender];
		return;
	}

	__weak AppDelegate* weakSelf = self;
	self.comparisonChooserController = [[NewComparisonWindowController alloc] initWithCompletionHandler:^(NSString* leftPath, NSString* rightPath) {
		BOOL isDirectory = NO;
		[NSFileManager.defaultManager fileExistsAtPath:leftPath isDirectory:&isDirectory];
		NSWindowController* windowController = nil;
		if(isDirectory)
			windowController = [[FolderWindowController alloc] initWithLeftPath:leftPath rightPath:rightPath];
		else
		{
			windowController = [[WindowController alloc] initWithLeftPath:leftPath rightPath:rightPath];
			[windowController.window center];
		}
		[windowController showWindow:weakSelf];
	}];
	[self.comparisonChooserController showWindow:sender];
}

- (NSWindowController*)activeComparisonWindowController
{
	NSWindowController* windowController = NSApp.keyWindow.windowController;
	return [windowController isKindOfClass:WindowController.class] || [windowController isKindOfClass:FolderWindowController.class] ? windowController : nil;
}

- (IBAction)nextChange:(id)sender
{
	[(id)[self activeComparisonWindowController] nextChange:sender];
}

- (IBAction)openSelectedFile:(id)sender
{
	NSWindowController* controller = [self activeComparisonWindowController];
	if([controller isKindOfClass:FolderWindowController.class])
		[(FolderWindowController*)controller openSelectedFile:sender];
}

- (IBAction)previousChange:(id)sender
{
	[(id)[self activeComparisonWindowController] previousChange:sender];
}

- (IBAction)copyChangeToLeft:(id)sender
{
	[(id)[self activeComparisonWindowController] copyChangeToLeft:sender];
}

- (IBAction)copyChangeToRight:(id)sender
{
	[(id)[self activeComparisonWindowController] copyChangeToRight:sender];
}

- (IBAction)moveChangeToLeft:(id)sender
{
	NSWindowController* controller = [self activeComparisonWindowController];
	if([controller respondsToSelector:@selector(moveChangeToLeft:)])
		[(id)controller moveChangeToLeft:sender];
	else
		NSBeep();
}

- (IBAction)moveChangeToRight:(id)sender
{
	NSWindowController* controller = [self activeComparisonWindowController];
	if([controller respondsToSelector:@selector(moveChangeToRight:)])
		[(id)controller moveChangeToRight:sender];
	else
		NSBeep();
}

- (IBAction)saveDocument:(id)sender
{
	NSWindowController* controller = [self activeComparisonWindowController];
	if([controller respondsToSelector:@selector(saveDocument:)])
		[(id)controller saveDocument:sender];
}

- (IBAction)saveDocumentAs:(id)sender
{
	NSWindowController* controller = [self activeComparisonWindowController];
	if([controller respondsToSelector:@selector(saveDocumentAs:)])
		[(id)controller saveDocumentAs:sender];
}

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	NSWindowController* controller = [self activeComparisonWindowController];
	BOOL const isFolderComparison = [controller isKindOfClass:FolderWindowController.class];
	SEL const action = menuItem.action;
	if(action == @selector(openSelectedFile:))
		return isFolderComparison && [(FolderWindowController*)controller canOpenSelectedFile];
	if(action == @selector(copyChangeToLeft:))
	{
		menuItem.title = isFolderComparison ? @"Copy File to Left" : @"Copy Change to Left";
		return controller != nil;
	}
	if(action == @selector(copyChangeToRight:))
	{
		menuItem.title = isFolderComparison ? @"Copy File to Right" : @"Copy Change to Right";
		return controller != nil;
	}
	if(action == @selector(moveChangeToLeft:) || action == @selector(moveChangeToRight:))
		return isFolderComparison;
	if(action == @selector(saveDocument:) || action == @selector(saveDocumentAs:))
		return [controller isKindOfClass:WindowController.class];
	if(action == @selector(nextChange:) || action == @selector(previousChange:))
		return controller != nil;
	return YES;
}

- (BOOL)applicationSupportsSecureRestorableState:(NSApplication*)app
{
	return YES;
}
@end
