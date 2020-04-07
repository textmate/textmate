#import "AppController.h"
#import "OakMainMenu.h"
#import "Favorites.h"
#import "AboutWindowController.h"
#import "TMPlugInController.h"
#import "RMateServer.h"
#import <BundleEditor/BundleEditor.h>
#import <BundlesManager/BundlesManager.h>
#import <CrashReporter/CrashReporter.h>
#import <DocumentWindow/DocumentWindowController.h>
#import <Find/Find.h>
#import <CommitWindow/CommitWindow.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakSubmenuController.h>
#import <OakFilterList/BundleItemChooser.h>
#import <OakFoundation/NSString Additions.h>
#import <OakTextView/OakDocumentView.h>
#import <MenuBuilder/MenuBuilder.h>
#import <Preferences/Keys.h>
#import <Preferences/Preferences.h>
#import <Preferences/TerminalPreferences.h>
#import <SoftwareUpdate/SoftwareUpdate.h>
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>
#import <bundles/query.h>
#import <io/path.h>
#import <regexp/glob.h>
#import <network/tbz.h>
#import <ns/ns.h>
#import <license/LicenseManager.h>
#import <settings/settings.h>
#import <oak/debug.h>
#import <oak/compat.h>
#import <oak/oak.h>
#import <scm/scm.h>
#import <text/types.h>

OAK_DEBUG_VAR(AppController);

void OakOpenDocuments (NSArray* paths, BOOL treatFilePackageAsFolder)
{
	NSArray* const bundleExtensions = @[ @"tmbundle", @"tmcommand", @"tmdragcommand", @"tmlanguage", @"tmmacro", @"tmpreferences", @"tmsnippet", @"tmtheme" ];

	NSMutableArray<OakDocument*>* documents = [NSMutableArray array];
	NSMutableArray* itemsToInstall = [NSMutableArray array];
	NSMutableArray* plugInsToInstall = [NSMutableArray array];
	BOOL enableInstallHandler = treatFilePackageAsFolder == NO && ([NSEvent modifierFlags] & NSEventModifierFlagOption) == 0;
	for(NSString* path in paths)
	{
		BOOL isDirectory = NO;
		NSString* pathExt = [[path pathExtension] lowercaseString];
		if(enableInstallHandler && [bundleExtensions containsObject:pathExt])
		{
			[itemsToInstall addObject:path];
		}
		else if(enableInstallHandler && [pathExt isEqualToString:@"tmplugin"])
		{
			[plugInsToInstall addObject:path];
		}
		else if([[NSFileManager defaultManager] fileExistsAtPath:path isDirectory:&isDirectory] && isDirectory)
		{
			[OakDocumentController.sharedInstance showFileBrowserAtPath:path];
		}
		else
		{
			[documents addObject:[OakDocumentController.sharedInstance documentWithPath:path]];
		}
	}

	if([itemsToInstall count])
		[[BundlesManager sharedInstance] installBundleItemsAtPaths:itemsToInstall];

	for(NSString* path in plugInsToInstall)
		[[TMPlugInController sharedInstance] installPlugInAtPath:path];

	[OakDocumentController.sharedInstance showDocuments:documents];
}

BOOL HasDocumentWindow (NSArray* windows)
{
	for(NSWindow* window in windows)
	{
		if([window.delegate isKindOfClass:[DocumentWindowController class]])
			return YES;
	}
	return NO;
}

@interface AppController ()
@property (nonatomic) BOOL didFinishLaunching;
@property (nonatomic) BOOL currentResponderIsOakTextView;
@end

@implementation AppController
- (NSMenu*)mainMenu
{
	MBMenu const items = {
		{ @"TextMate",
			.submenu = {
				{ @"About TextMate",        @selector(orderFrontAboutPanel:)               },
				{ /* -------- */ },
				{ @"Preferences…",          @selector(showPreferences:),            @","   },
				{ @"Check for Update",      @selector(performSoftwareUpdateCheck:)         },
				{ @"Check for Test Build",  @selector(performSoftwareUpdateCheck:),       .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .alternate = YES },
				{ /* -------- */ },
				{ @"Services",              .systemMenu = MBMenuTypeServices               },
				{ /* -------- */ },
				{ @"Hide TextMate",         @selector(hide:),                       @"h"   },
				{ @"Hide Others",           @selector(hideOtherApplications:),      @"h", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Show All",              @selector(unhideAllApplications:),             },
				{ /* -------- */ },
				{ @"Quit TextMate",         @selector(terminate:),                  @"q"   },
			}
		},
		{ @"File",
			.submenu = {
				{ @"New",                     @selector(newDocument:),              @"n"   },
				{ @"New File Browser",        @selector(newFileBrowser:),           @"n", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl, .alternate = YES },
				{ @"New Tab",                 @selector(newDocumentInTab:),         @"n", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ /* -------- */ },
				{ @"Open…",                   @selector(openDocument:),             @"o"   },
				{ @"Open Quickly…",           @selector(goToFile:),                 @"t"   },
				{ @"Open Recent",
					.systemMenu = MBMenuTypeOpenRecent, .submenu = {
						{ @"Clear Menu", @selector(clearRecentDocuments:) },
					}
				},
				{ @"Open Recent Project…",    @selector(openFavorites:),            @"O"   },
				{ /* -------- */ },
				{ @"Close",                   @selector(performClose:),             @"w"   },
				{ @"Close Window",            @selector(performCloseWindow:),       @"W"   },
				{ @"Close All Tabs",          @selector(performCloseAllTabs:),      @"w", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl },
				{ @"Close Other Tabs",        @selector(performCloseOtherTabsXYZ:), @"w", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
				{ @"Close Tabs to the Right", @selector(performCloseTabsToTheRight:)       },
				{ /* -------- */ },
				{ @"Sticky",                  @selector(toggleSticky:)                     },
				{ /* -------- */ },
				{ @"Save",                    @selector(saveDocument:),             @"s"   },
				{ @"Save As…",                @selector(saveDocumentAs:),           @"S"   },
				{ @"Save All",                @selector(saveAllDocuments:),         @"s", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Revert",                  @selector(revertDocumentToSaved:)            },
				{ /* -------- */ },
				{ @"Page Setup…",             @selector(runPageLayout:),                  .target = NSApp.delegate },
				{ @"Print…",                  @selector(printDocument:),            @"p"   },
			}
		},
		{ @"Edit",
			.submenu = {
				{ @"Undo",   @selector(undo:),   @"z" },
				{ @"Redo",   @selector(redo:),   @"Z" },
				{ /* -------- */ },
				{ @"Cut",    @selector(cut:),    @"x" },
				{ @"Copy",   @selector(copy:),   @"c" },
				{ @"Paste",
					.submenu = {
						{ @"Paste",                   @selector(paste:),                @"v"   },
						{ @"Paste Without Indenting", @selector(pasteWithoutReindent:), @"v", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl, .alternate = YES },
						{ @"Paste Next",              @selector(pasteNext:),            @"v", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
						{ @"Paste Previous",          @selector(pastePrevious:),        @"V"   },
						{ /* -------- */ },
						{ @"Show History",            @selector(showClipboardHistory:), @"v", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl },
					}
				},
				{ @"Delete", @selector(delete:), .key = NSBackspaceCharacter },
				{ /* -------- */ },
				{ @"Macros",
					.submenu = {
						{ @"Start Recording", @selector(toggleMacroRecording:), @"m", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
						{ @"Replay Macro",    @selector(playScratchMacro:),     @"M"   },
						{ @"Save Macro…",     @selector(saveScratchMacro:),     @"m", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
					}
				},
				{ /* -------- */ },
				{ @"Select",
					.submenu = {
						{ @"Word",                    @selector(selectWord:)                  },
						{ @"Line",                    @selector(selectHardLine:)              },
						{ @"Paragraph",               @selector(selectParagraph:)             },
						{ @"Current Scope",           @selector(selectCurrentScope:)          },
						{ @"Enclosing Typing Pairs",  @selector(selectBlock:),           @"B" },
						{ @"All",                     @selector(selectAll:),             @"a" },
						{ /* -------- */ },
						{ @"Toggle Column Selection", @selector(toggleColumnSelection:), .modifierFlags = NSEventModifierFlagOption },
					}
				},
				{ @"Find",
					.submenu = {
						{ @"Find and Replace…",           @selector(orderFrontFindPanel:),          @"f", .tag = 1 },
						{ @"Find in Project…",            @selector(orderFrontFindPanel:),          @"F", .tag = 3 },
						{ @"Find in Folder…",             @selector(orderFrontFindPanel:),                .tag = 4 },
						{ /* -------- */ },
						{ @"Show Find History",           @selector(showFindHistory:),              @"f", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl },
						{ /* -------- */ },
						{ @"Incremental Search",          @selector(incrementalSearch:),            @"s", .modifierFlags = NSEventModifierFlagControl },
						{ @"Incremental Search Previous", @selector(incrementalSearchPrevious:),    @"S", .modifierFlags = NSEventModifierFlagControl },
						{ /* -------- */ },
						{ @"Find Next",                   @selector(findNext:),                     @"g"   },
						{ @"Find Previous",               @selector(findPrevious:),                 @"G"   },
						{ @"Find All",                    @selector(findAllInSelection:),           @"f", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
						{ /* -------- */ },
						{ @"Find Options",
							.submenu = {
								{ @"Ignore Case",        @selector(toggleFindOption:), @"c", .tag =   2, .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
								{ @"Regular Expression", @selector(toggleFindOption:), @"r", .tag =   8, .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
								{ @"Ignore Whitespace",  @selector(toggleFindOption:),       .tag =   4  },
								{ @"Wrap Around",        @selector(toggleFindOption:), @"a", .tag = 128, .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
							}
						},
						{ /* -------- */ },
						{ @"Replace",                     @selector(replace:),                      @"g", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
						{ @"Replace & Find",              @selector(replaceAndFind:)                       },
						{ @"Replace All",                 @selector(replaceAll:),                   @"g", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
						{ @"Replace All in Selection",    @selector(replaceAllInSelection:),        @"G", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
						{ /* -------- */ },
						{ @"Use Selection for Find",      @selector(copySelectionToFindPboard:),    @"e"   },
						{ @"Use Selection for Replace",   @selector(copySelectionToReplacePboard:), @"E"   },
					}
				},
				{ @"Spelling",
					.submenuRef = &spellingMenu, .submenu = {
						{ @"Spelling…",                   @selector(showGuessPanel:),                @":"   },
						{ @"Check Document Now",          @selector(checkSpelling:),                 @";"   },
						{ /* -------- */ },
						{ @"Check Spelling While Typing", @selector(toggleContinuousSpellChecking:), @";", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
						{ /* -------- */ },
					}
				},
			}
		},
		{ @"View",
			.submenu = {
				{ @"Font",
					.systemMenu = MBMenuTypeFont, .submenu = {
						{ @"Show Fonts",   @selector(orderFrontFontPanel:),      .target = NSFontManager.sharedFontManager },
						{ /* -------- */ },
						{ @"Bigger",       @selector(makeTextLarger:),       @"+" },
						{ @"Smaller",      @selector(makeTextSmaller:),      @"-" },
						{ @"Default Size", @selector(makeTextStandardSize:), @"0" },
					}
				},
				{ @"Show File Browser",      @selector(toggleFileBrowser:),    @"d", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl },
				{ @"Show HTML Output",       @selector(toggleHTMLOutput:),     @"h", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl },
				{ @"Show Line Numbers",      @selector(toggleLineNumbers:),    @"l", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ /* -------- */ },
				{ @"Show Invisibles",        @selector(toggleShowInvisibles:), @"i", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ /* -------- */ },
				{ @"Enable Soft Wrap",       @selector(toggleSoftWrap:),       @"w", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Show Wrap Column",       @selector(toggleShowWrapColumn:)         },
				{ @"Show Indent Guides",     @selector(toggleShowIndentGuides:)       },
				{ @"Wrap Column",
					.submenuRef = &wrapColumnMenu, .submenu = {
						{ @"Use Window Frame", @selector(takeWrapColumnFrom:)   },
						{ /* -------- */ },
						{ @"40",               @selector(takeWrapColumnFrom:), .tag = 40 },
						{ @"80",               @selector(takeWrapColumnFrom:), .tag = 80 },
						{ /* -------- */ },
						{ @"Other…",           @selector(takeWrapColumnFrom:), .tag = -1 },
					}
				},
				{ /* -------- */ },
				{ @"Tab Size",
					.submenu = {
						{ @"2",      @selector(takeTabSizeFrom:),        .tag = 2 },
						{ @"3",      @selector(takeTabSizeFrom:),        .tag = 3 },
						{ @"4",      @selector(takeTabSizeFrom:),        .tag = 4 },
						{ @"5",      @selector(takeTabSizeFrom:),        .tag = 5 },
						{ @"6",      @selector(takeTabSizeFrom:),        .tag = 6 },
						{ @"7",      @selector(takeTabSizeFrom:),        .tag = 7 },
						{ @"8",      @selector(takeTabSizeFrom:),        .tag = 8 },
						{ /* -------- */ },
						{ @"Other…", @selector(showTabSizeSelectorPanel:) },
					}
				},
				{ @"Theme",                  .submenuRef = &themesMenu                },
				{ /* -------- */ },
				{ @"Fold Current Block",     @selector(toggleCurrentFolding:), .key = NSF1FunctionKey, .modifierFlags = 0 },
				{ @"Toggle Foldings at Level",
					.submenu = {
						{ @"All Levels", @selector(takeLevelToFoldFrom:), @"0", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
						{ @"1",          @selector(takeLevelToFoldFrom:), @"1", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = 1 },
						{ @"2",          @selector(takeLevelToFoldFrom:), @"2", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = 2 },
						{ @"3",          @selector(takeLevelToFoldFrom:), @"3", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = 3 },
						{ @"4",          @selector(takeLevelToFoldFrom:), @"4", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = 4 },
						{ @"5",          @selector(takeLevelToFoldFrom:), @"5", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = 5 },
						{ @"6",          @selector(takeLevelToFoldFrom:), @"6", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = 6 },
						{ @"7",          @selector(takeLevelToFoldFrom:), @"7", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = 7 },
						{ @"8",          @selector(takeLevelToFoldFrom:), @"8", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = 8 },
						{ @"9",          @selector(takeLevelToFoldFrom:), @"9", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .tag = 9 },
					}
				},
				{ /* -------- */ },
				{ @"Toggle Scroll Past End", @selector(toggleScrollPastEnd:)          },
				{ /* -------- */ },
				{ @"View Source",            @selector(viewSource:),           @"u", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Enter Full Screen",      @selector(toggleFullScreen:),     @"f", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
				{ /* -------- */ },
				{ @"Customize Touch Bar…",   @selector(toggleTouchBarCustomizationPalette:) },
			}
		},
		{ @"Navigate",
			.submenu = {
				{ @"Jump to Line…",              @selector(orderFrontGoToLinePanel:),      @"l" },
				{ @"Jump to Symbol…",            @selector(showSymbolChooser:),            @"T" },
				{ @"Jump to Selection",          @selector(centerSelectionInVisibleArea:), @"j" },
				{ /* -------- */ },
				{ @"Set Bookmark",               @selector(toggleCurrentBookmark:),        .key = NSF2FunctionKey },
				{ @"Jump to Next Bookmark",      @selector(goToNextBookmark:),             .key = NSF2FunctionKey, .modifierFlags = 0 },
				{ @"Jump to Previous Bookmark",  @selector(goToPreviousBookmark:),         .key = NSF2FunctionKey, .modifierFlags = NSEventModifierFlagShift },
				{ @"Jump to Bookmark",           .delegate = OakSubmenuController.sharedInstance },
				{ /* -------- */ },
				{ @"Jump to Next Mark",          @selector(jumpToNextMark:),               .key = NSF3FunctionKey, .modifierFlags = 0 },
				{ @"Jump to Previous Mark",      @selector(jumpToPreviousMark:),           .key = NSF3FunctionKey, .modifierFlags = NSEventModifierFlagShift },
				{ /* -------- */ },
				{ @"Scroll",
					.submenu = {
						{ @"Line Up",      @selector(scrollLineUp:),      .key = NSUpArrowFunctionKey,    .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl },
						{ @"Line Down",    @selector(scrollLineDown:),    .key = NSDownArrowFunctionKey,  .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl },
						{ @"Column Left",  @selector(scrollColumnLeft:),  .key = NSLeftArrowFunctionKey,  .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl },
						{ @"Column Right", @selector(scrollColumnRight:), .key = NSRightArrowFunctionKey, .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl },
					}
				},
				{ /* -------- */ },
				{ @"Go to Related File",         @selector(goToRelatedFile:),              .key = NSUpArrowFunctionKey, .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ /* -------- */ },
				{ @"Move Focus to File Browser", @selector(moveFocus:),                    .key = NSTabCharacter, .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
			}
		},
		{ @"Text",
			.submenu = {
				{ @"Transpose",                            @selector(transpose:)                        },
				{ /* -------- */ },
				{ @"Move Selection",
					.submenu = {
						{ @"Up",    @selector(moveSelectionUp:),    .key = NSUpArrowFunctionKey,    .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
						{ @"Down",  @selector(moveSelectionDown:),  .key = NSDownArrowFunctionKey,  .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
						{ @"Left",  @selector(moveSelectionLeft:),  .key = NSLeftArrowFunctionKey,  .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
						{ @"Right", @selector(moveSelectionRight:), .key = NSRightArrowFunctionKey, .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
					}
				},
				{ /* -------- */ },
				{ @"Toggle Case of Character / Selection", @selector(changeCaseOfLetter:)               },
				{ @"Toggle Case of Word / Selection",      @selector(changeCaseOfWord:)                 },
				{ /* -------- */ },
				{ @"Uppercase Word / Selection",           @selector(uppercaseWord:)                    },
				{ @"Lowercase Word / Selection",           @selector(lowercaseWord:)                    },
				{ @"Titlecase Line / Selection",           @selector(capitalizeWord:)                   },
				{ /* -------- */ },
				{ @"Shift Left",                           @selector(shiftLeft:),                  @"[" },
				{ @"Shift Right",                          @selector(shiftRight:),                 @"]" },
				{ @"Indent Line / Selection",              @selector(indent:)                           },
				{ /* -------- */ },
				{ @"Reformat Text",                        @selector(reformatText:)                     },
				{ @"Reformat Text and Justify",            @selector(reformatTextAndJustify:)           },
				{ @"Unwrap Paragraph",                     @selector(unwrapText:)                       },
				{ /* -------- */ },
				{ @"Filter Through Command…",              @selector(orderFrontRunCommandWindow:), @"|" },
			}
		},
		{ @"File Browser",
			.submenu = {
				{ @"New File",         @selector(newDocumentInDirectory:), @"n", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
				{ @"New Folder",       @selector(newFolder:),              @"N"   },
				{ /* -------- */ },
				{ @"Back",             @selector(goBack:)                         },
				{ @"Forward",          @selector(goForward:)                      },
				{ @"Enclosing Folder", @selector(goToParentFolder:),       .key = NSUpArrowFunctionKey },
				{ /* -------- */ },
				{ @"Select Document",  @selector(revealFileInProject:),    @"r", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
				{ @"Select None",      @selector(deselectAll:),            @"A"   },
				{ /* -------- */ },
				{ @"Project Folder",   @selector(goToProjectFolder:),      @"P"   },
				{ @"SCM Status",       @selector(goToSCMDataSource:),      @"Y"   },
				{ @"Computer",         @selector(goToComputer:),           @"C"   },
				{ @"Home",             @selector(goToHome:),               @"H"   },
				{ @"Desktop",          @selector(goToDesktop:),            @"D"   },
				{ @"Favorites",        @selector(goToFavorites:)                  },
				{ /* -------- */ },
				{ @"Go to Folder…",    @selector(orderFrontGoToFolder:)           },
				{ @"Reload",           @selector(reload:)                         },
			}
		},
		{ @"Bundles",
			.submenuRef = &bundlesMenu, .submenu = {
				{ @"Select Bundle Item…", @selector(showBundleItemChooser:), @"t", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagControl },
				{ @"Edit Bundles…",       @selector(showBundleEditor:),      @"b", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl },
				{ /* -------- */ },
			}
		},
		{ @"Window",
			.systemMenu = MBMenuTypeWindows, .submenu = {
				{ @"Minimize",               @selector(miniaturize:),           @"m" },
				{ @"Zoom",                   @selector(performZoom:)                 },
				{ /* -------- */ },
				{ @"Show Previous Tab",      @selector(selectPreviousTab:),     .key = NSTabCharacter,          .modifierFlags = NSEventModifierFlagControl|NSEventModifierFlagShift },
				{ @"Show Next Tab",          @selector(selectNextTab:),         .key = NSTabCharacter,          .modifierFlags = NSEventModifierFlagControl },
				{ @"Show Previous Tab",      @selector(selectPreviousTab:),     .key = NSLeftArrowFunctionKey,  .modifierFlags = NSEventModifierFlagOption|NSEventModifierFlagCommand, .hidden = YES },
				{ @"Show Next Tab",          @selector(selectNextTab:),         .key = NSRightArrowFunctionKey, .modifierFlags = NSEventModifierFlagOption|NSEventModifierFlagCommand, .hidden = YES },
				{ @"Show Previous Tab",      @selector(selectPreviousTab:),     @"{", .hidden = YES },
				{ @"Show Next Tab",          @selector(selectNextTab:),         @"}", .hidden = YES },
				{ @"Show Tab",               .delegate = OakSubmenuController.sharedInstance },
				{ /* -------- */ },
				{ @"Move Tab to New Window", @selector(moveDocumentToNewWindow:)     },
				{ @"Merge All Windows",      @selector(mergeAllWindows:)             },
				{ /* -------- */ },
				{ @"Bring All to Front",     @selector(arrangeInFront:)              },
			}
		},
		{ @"Help",
			.systemMenu = MBMenuTypeHelp, .submenu = {
				{ @"TextMate Help", @selector(showHelp:), @"?" },
			}
		},
	};

	NSMenu* menu = MBCreateMenu(items, [[OakMainMenu alloc] initWithTitle:@"AMainMenu"]);
	bundlesMenu.delegate    = self;
	themesMenu.delegate     = self;
	spellingMenu.delegate   = self;
	wrapColumnMenu.delegate = self;
	return menu;
}

- (NSMenu*)applicationDockMenu:(NSApplication*)anApplication
{
	MBMenu const items = {
		{ @"New File", @selector(newDocumentAndActivate:),  .target = self },
		{ @"Open…",    @selector(openDocumentAndActivate:), .target = self },
	};
	return MBCreateMenu(items);
}

- (void)setCurrentResponderIsOakTextView:(BOOL)flag
{
	if(_currentResponderIsOakTextView != flag)
	{
		_currentResponderIsOakTextView = flag;

		NSMenu* mainMenu = [NSApp mainMenu];
		NSMenu* goMenu   = [[mainMenu itemWithTitle:@"File Browser"] submenu];
		NSMenu* textMenu = [[mainMenu itemWithTitle:@"Text"] submenu];

		NSMenuItem* backMenuItem       = [goMenu itemWithTitle:@"Back"];
		NSMenuItem* forwardMenuItem    = [goMenu itemWithTitle:@"Forward"];
		NSMenuItem* shiftLeftMenuItem  = [textMenu itemWithTitle:@"Shift Left"];
		NSMenuItem* shiftRightMenuItem = [textMenu itemWithTitle:@"Shift Right"];

		if(!backMenuItem || !forwardMenuItem || !shiftLeftMenuItem || !shiftRightMenuItem)
			return;

		if(_currentResponderIsOakTextView)
		{
			backMenuItem.keyEquivalent                   = @"";
			forwardMenuItem.keyEquivalent                = @"";

			shiftLeftMenuItem.keyEquivalent              = @"[";
			shiftLeftMenuItem.keyEquivalentModifierMask  = NSEventModifierFlagCommand;
			shiftRightMenuItem.keyEquivalent             = @"]";
			shiftRightMenuItem.keyEquivalentModifierMask = NSEventModifierFlagCommand;
		}
		else
		{
			shiftLeftMenuItem.keyEquivalent           = @"";
			shiftRightMenuItem.keyEquivalent          = @"";

			backMenuItem.keyEquivalent                = @"[";
			backMenuItem.keyEquivalentModifierMask    = NSEventModifierFlagCommand;
			forwardMenuItem.keyEquivalent             = @"]";
			forwardMenuItem.keyEquivalentModifierMask = NSEventModifierFlagCommand;
		}
	}
}

- (void)applicationDidUpdate:(NSNotification*)aNotification
{
	self.currentResponderIsOakTextView = [NSApp targetForAction:@selector(shiftLeft:) to:nil from:self] != nil;
}

- (void)userDefaultsDidChange:(id)sender
{
	BOOL disableRmate        = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableRMateServerKey];
	NSString* rmateInterface = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsRMateServerListenKey];
	int rmatePort            = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsRMateServerPortKey];
	setup_rmate_server(!disableRmate, rmatePort, [rmateInterface isEqualToString:kRMateServerListenRemote]);
}

- (void)applicationWillFinishLaunching:(NSNotification*)aNotification
{
	D(DBF_AppController, bug("\n"););
	if(NSMenu* menu = [self mainMenu])
		NSApp.mainMenu = menu;

	SoftwareUpdate* swUpdate = [SoftwareUpdate sharedInstance];
	NSString* parms = [NSString stringWithFormat:@"v=%@&os=%zu.%zu.%zu", [[[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleShortVersionString"] stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLQueryAllowedCharacterSet], oak::os_major(), oak::os_minor(), oak::os_patch()];
	[swUpdate setSignee:key_chain_t::key_t("org.textmate.duff", "Allan Odgaard", "-----BEGIN PUBLIC KEY-----\nMIIBtjCCASsGByqGSM44BAEwggEeAoGBAPIE9PpXPK3y2eBDJ0dnR/D8xR1TiT9m\n8DnPXYqkxwlqmjSShmJEmxYycnbliv2JpojYF4ikBUPJPuerlZfOvUBC99ERAgz7\nN1HYHfzFIxVo1oTKWurFJ1OOOsfg8AQDBDHnKpS1VnwVoDuvO05gK8jjQs9E5LcH\ne/opThzSrI7/AhUAy02E9H7EOwRyRNLofdtPxpa10o0CgYBKDfcBscidAoH4pkHR\nIOEGTCYl3G2Pd1yrblCp0nCCUEBCnvmrWVSXUTVa2/AyOZUTN9uZSC/Kq9XYgqwj\nhgzqa8h/a8yD+ao4q8WovwGeb6Iso3WlPl8waz6EAPR/nlUTnJ4jzr9t6iSH9owS\nvAmWrgeboia0CI2AH++liCDvigOBhAACgYAFWO66xFvmF2tVIB+4E7CwhrSi2uIk\ndeBrpmNcZZ+AVFy1RXJelNe/cZ1aXBYskn/57xigklpkfHR6DGqpEbm6KC/47Jfy\ny5GEx+F/eBWEePi90XnLinytjmXRmS2FNqX6D15XNG1xJfjociA8bzC7s4gfeTUd\nlpQkBq2z71yitA==\n-----END PUBLIC KEY-----\n")];
	[swUpdate setChannels:@{
		kSoftwareUpdateChannelRelease:    [NSURL URLWithString:[NSString stringWithFormat:@"%s/releases/release?%@", REST_API, parms]],
		kSoftwareUpdateChannelPrerelease: [NSURL URLWithString:[NSString stringWithFormat:@"%s/releases/beta?%@", REST_API, parms]],
		kSoftwareUpdateChannelCanary:     [NSURL URLWithString:[NSString stringWithFormat:@"%s/releases/nightly?%@", REST_API, parms]],
	}];

	settings_t::set_default_settings_path([[[NSBundle mainBundle] pathForResource:@"Default" ofType:@"tmProperties"] fileSystemRepresentation]);
	settings_t::set_global_settings_path(path::join(path::home(), "Library/Application Support/TextMate/Global.tmProperties"));

	// LEGACY location used prior to 2.0-alpha.9513
	std::string const src = path::join(path::home(), "Library/Application Support/TextMate/project-state.db");
	std::string const dst = path::join(path::home(), "Library/Application Support/TextMate/RecentProjects.db");
	if(path::exists(src) && !path::exists(dst))
		rename(src.c_str(), dst.c_str());

	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		@"NSRecentDocumentsLimit": @25,
		@"WebKitDeveloperExtras":  @YES,
	}];
	RegisterDefaults();

	// LEGACY format used prior to 2.0-beta.12.23
	if(NSDictionary* volumeSettings = [[NSUserDefaults standardUserDefaults] dictionaryForKey:@"volumeSettings"])
	{
		for(NSString* pathPrefix in volumeSettings)
		{
			id setting = volumeSettings[pathPrefix][@"extendedAttributes"];
			if(setting && [setting boolValue] == NO)
			{
				std::string const glob = path::glob_t::escape(to_s(pathPrefix)) + "**";
				settings_t::set(kSettingsDisableExtendedAttributesKey, true, NULL_STR, glob);
			}
		}
		[[NSUserDefaults standardUserDefaults] removeObjectForKey:@"volumeSettings"];
	}

	[[TMPlugInController sharedInstance] loadAllPlugIns:nil];

	std::string dest = path::join(path::home(), "Library/Application Support/TextMate/Managed");
	if(!path::exists(dest))
	{
		if(NSString* archive = [[NSBundle mainBundle] pathForResource:@"DefaultBundles" ofType:@"tbz"])
		{
			path::make_dir(dest);

			network::tbz_t tbz(dest);
			if(tbz)
			{
				int fd = open([archive fileSystemRepresentation], O_RDONLY|O_CLOEXEC);
				if(fd != -1)
				{
					char buf[4096];
					ssize_t len;
					while((len = read(fd, buf, sizeof(buf))) > 0)
					{
						if(write(tbz.input_fd(), buf, len) != len)
						{
							fprintf(stderr, "*** error writing bytes to tar\n");
							break;
						}
					}
					close(fd);
				}

				std::string output, error;
				if(!tbz.wait_for_tbz(&output, &error))
					fprintf(stderr, "%s: %s%s\n", getprogname(), output.c_str(), error.c_str());
			}
			else
			{
				fprintf(stderr, "%s: unable to launch tar\n", getprogname());
			}
		}
		else
		{
			fprintf(stderr, "%s: no ‘DefaultBundles.tbz’ in TextMate.app\n", getprogname());
		}
	}
	[[BundlesManager sharedInstance] loadBundlesIndex];

	if(BOOL restoreSession = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableSessionRestoreKey])
	{
		std::string const prematureTerminationDuringRestore = path::join(path::temp(), "textmate_session_restore");

		NSString* promptUser = nil;
		if(path::exists(prematureTerminationDuringRestore))
			promptUser = @"Previous attempt of restoring your session caused an abnormal exit. Would you like to skip session restore?";
		else if([NSEvent modifierFlags] & NSEventModifierFlagShift)
			promptUser = @"By holding down shift (⇧) you have indicated that you wish to disable restoring the documents which were open in last session.";

		if(promptUser)
		{
			NSAlert* alert        = [[NSAlert alloc] init];
			alert.messageText     = @"Disable Session Restore?";
			alert.informativeText = promptUser;
			[alert addButtons:@"Restore Documents", @"Disable", nil];
			if([alert runModal] == NSAlertSecondButtonReturn) // "Disable"
				restoreSession = NO;
		}

		if(restoreSession)
		{
			close(open(prematureTerminationDuringRestore.c_str(), O_CREAT|O_TRUNC|O_WRONLY|O_CLOEXEC));
			[DocumentWindowController restoreSession];
		}
		unlink(prematureTerminationDuringRestore.c_str());
	}
}

- (BOOL)applicationShouldOpenUntitledFile:(NSApplication*)anApplication
{
	D(DBF_AppController, bug("\n"););
	return self.didFinishLaunching;
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
	D(DBF_AppController, bug("\n"););

	NSWindow.allowsAutomaticWindowTabbing = NO;

	if([NSApp respondsToSelector:@selector(setAutomaticCustomizeTouchBarMenuItemEnabled)]) // MAC_OS_X_VERSION_10_12_1
		NSApp.automaticCustomizeTouchBarMenuItemEnabled = YES;

	if(!HasDocumentWindow([NSApp orderedWindows]))
	{
		BOOL disableUntitledAtStartupPrefs = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableNewDocumentAtStartupKey];
		BOOL showFavoritesInsteadPrefs     = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsShowFavoritesInsteadOfUntitledKey];

		if(showFavoritesInsteadPrefs)
			[self openFavorites:self];
		else if(!disableUntitledAtStartupPrefs)
			[self newDocument:self];
	}

	[self userDefaultsDidChange:nil]; // setup mate/rmate server
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];

	NSMenu* selectMenu = [[[[[NSApp mainMenu] itemWithTitle:@"Edit"] submenu] itemWithTitle:@"Select"] submenu];
	[[selectMenu itemWithTitle:@"Toggle Column Selection"] setActivationString:@"⌥" withFont:nil];

	[TerminalPreferences updateMateIfRequired];
	[AboutWindowController showChangesIfUpdated];

	[[CrashReporter sharedInstance] applicationDidFinishLaunching:aNotification];
	[[CrashReporter sharedInstance] postNewCrashReportsToURLString:[NSString stringWithFormat:@"%s/crashes", REST_API]];

	[OakCommitWindowServer sharedInstance]; // Setup server

	self.didFinishLaunching = YES;
}

- (void)applicationWillResignActive:(NSNotification*)aNotification
{
	scm::disable();
}

- (void)applicationWillBecomeActive:(NSNotification*)aNotification
{
	scm::enable();
}

// =========================
// = Past Startup Delegate =
// =========================

- (IBAction)newDocumentAndActivate:(id)sender
{
	[NSApp activateIgnoringOtherApps:YES];
	[self newDocument:sender];
}

- (IBAction)openDocumentAndActivate:(id)sender
{
	[NSApp activateIgnoringOtherApps:YES];
	[self openDocument:sender];
}

- (IBAction)orderFrontAboutPanel:(id)sender
{
	[[AboutWindowController sharedInstance] showAboutWindow:self];
}

- (IBAction)orderFrontFindPanel:(id)sender
{
	D(DBF_AppController, bug("\n"););
	Find* find = [Find sharedInstance];
	NSInteger mode = [sender respondsToSelector:@selector(tag)] ? [sender tag] : find_tags::in_document;
	switch(mode)
	{
		case find_tags::in_document:  find.searchTarget = FFSearchTargetDocument;  break;
		case find_tags::in_selection: find.searchTarget = FFSearchTargetSelection; break;
		case find_tags::in_project:   find.searchTarget = FFSearchTargetProject;   break;
		case find_tags::in_folder:    return [find showFolderSelectionPanel:self]; break;
	}
	[find showWindow:self];
}

- (IBAction)orderFrontGoToLinePanel:(id)sender;
{
	D(DBF_AppController, bug("\n"););
	if(id textView = [NSApp targetForAction:@selector(selectionString)])
		[goToLineTextField setStringValue:[textView selectionString]];
	[goToLinePanel makeKeyAndOrderFront:self];
}

- (IBAction)performGoToLine:(id)sender
{
	D(DBF_AppController, bug("\n"););
	[goToLinePanel orderOut:self];
	[NSApp sendAction:@selector(selectAndCenter:) to:nil from:[goToLineTextField stringValue]];
}

- (IBAction)performSoftwareUpdateCheck:(id)sender
{
	[[SoftwareUpdate sharedInstance] checkForUpdates:self];
}

- (IBAction)showPreferences:(id)sender
{
	D(DBF_AppController, bug("\n"););
	[[Preferences sharedInstance] showWindow:self];
}

- (IBAction)showBundleEditor:(id)sender
{
	D(DBF_AppController, bug("\n"););
	[[BundleEditor sharedInstance] showWindow:self];
}

- (IBAction)openFavorites:(id)sender
{
	FavoriteChooser* chooser = [FavoriteChooser sharedInstance];
	chooser.action = @selector(didSelectFavorite:);
	[chooser showWindow:self];
}

- (void)didSelectFavorite:(id)sender
{
	NSMutableArray* paths = [NSMutableArray array];
	for(id item in [sender selectedItems])
		[paths addObject:[item objectForKey:@"path"]];
	OakOpenDocuments(paths, YES);
}

// =======================
// = Bundle Item Chooser =
// =======================

- (IBAction)showBundleItemChooser:(id)sender
{
	BundleItemChooser* chooser = [BundleItemChooser sharedInstance];
	chooser.action     = @selector(bundleItemChooserDidSelectItems:);
	chooser.editAction = @selector(editBundleItem:);

	OakTextView* textView = [NSApp targetForAction:@selector(scopeContext)];
	chooser.scope        = textView ? [textView scopeContext] : scope::wildcard;
	chooser.hasSelection = [textView hasSelection];

	if(DocumentWindowController* controller = [NSApp targetForAction:@selector(selectedDocument)])
	{
		OakDocument* doc = controller.selectedDocument;
		chooser.path      = doc.path;
		chooser.directory = [doc.path stringByDeletingLastPathComponent] ?: doc.directory;
	}
	else
	{
		chooser.path      = nil;
		chooser.directory = nil;
	}

	[chooser showWindowRelativeToFrame:textView.window ? [textView.window convertRectToScreen:[textView convertRect:[textView visibleRect] toView:nil]] : [[NSScreen mainScreen] visibleFrame]];
}

- (void)bundleItemChooserDidSelectItems:(id)sender
{
	for(id item in [sender selectedItems])
		[NSApp sendAction:@selector(performBundleItemWithUUIDStringFrom:) to:nil from:@{ @"representedObject": [item valueForKey:@"uuid"] }];
}

// ===========================
// = Find options menu items =
// ===========================

- (IBAction)toggleFindOption:(id)sender
{
	[[Find sharedInstance] takeFindOptionToToggleFrom:sender];
}

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	BOOL enabled = YES;
	if([item action] == @selector(toggleFindOption:))
	{
		BOOL active = NO;
		if(OakPasteboardEntry* entry = [[OakPasteboard pasteboardWithName:NSFindPboard] current])
		{
			switch([item tag])
			{
				case find::ignore_case:        active = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindIgnoreCase]; break;
				case find::regular_expression: active = [entry regularExpression]; break;
				case find::full_words:         active = [entry fullWordMatch];     enabled = ![entry regularExpression]; break;
				case find::ignore_whitespace:  active = [entry ignoreWhitespace];  enabled = ![entry regularExpression]; break;
				case find::wrap_around:        active = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindWrapAround]; break;
			}
			[item setState:(active ? NSControlStateValueOn : NSControlStateValueOff)];
		}
		else
		{
			enabled = NO;
		}
	}
	else if([item action] == @selector(orderFrontGoToLinePanel:))
	{
		enabled = [NSApp targetForAction:@selector(setSelectionString:)] != nil;
	}
	else if([item action] == @selector(performBundleItemWithUUIDStringFrom:))
	{
		id menuItemValidator = [NSApp.keyWindow.delegate respondsToSelector:@selector(performBundleItem:)] ? NSApp.keyWindow.delegate : [NSApp targetForAction:@selector(performBundleItem:)];
		if(menuItemValidator != self && [menuItemValidator respondsToSelector:@selector(validateMenuItem:)])
			enabled = [menuItemValidator validateMenuItem:item];
	}
	return enabled;
}

- (void)editBundleItem:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(selectedItems)]);
	ASSERT([[sender selectedItems] count] == 1);

	if(NSString* uuid = [[[sender selectedItems] lastObject] valueForKey:@"uuid"])
	{
		[[BundleEditor sharedInstance] revealBundleItem:bundles::lookup(to_s(uuid))];
	}
	else if(NSString* path = [[[sender selectedItems] lastObject] valueForKey:@"file"])
	{
		OakDocument* doc = [OakDocumentController.sharedInstance documentWithPath:path];
		NSString* line = [[[sender selectedItems] lastObject] valueForKey:@"line"];
		[OakDocumentController.sharedInstance showDocument:doc andSelect:(line ? text::pos_t(to_s(line)) : text::pos_t::undefined) inProject:nil bringToFront:YES];
	}
}

- (void)editBundleItemWithUUIDString:(NSString*)uuidString
{
	[[BundleEditor sharedInstance] revealBundleItem:bundles::lookup(to_s(uuidString))];
}

// ============
// = Printing =
// ============

- (IBAction)runPageLayout:(id)sender
{
	[[NSPageLayout pageLayout] runModal];
}
@end
