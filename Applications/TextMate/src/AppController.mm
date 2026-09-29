#import "AppController.h"
#import "OakMainMenu.h"
#import "Favorites.h"
#import "AboutWindowController.h"
#import "TextMateResponderActions.h"
#import "TMPlugInController.h"
#import "RMateServer.h"
#import <BundleEditor/src/BundleEditor.h>
#import <BundlesManager/src/BundlesManager.h>
#import <CrashReporter/src/CrashReporter.h>
#import <DocumentWindow/src/DocumentWindowController.h>
#import <Find/src/Find.h>
#import <CommitWindow/src/CommitWindow.h>
#import <OakAppKit/src/NSAlert Additions.h>
#import <OakAppKit/src/NSMenuItem Additions.h>
#import <OakAppKit/src/OakAppKit.h>
#import <OakAppKit/src/OakPasteboard.h>
#import <OakFilterList/src/BundleItemChooser.h>
#import <OakFoundation/src/OakFoundation.h>
#import <OakFoundation/src/NSString Additions.h>
#import <OakTextView/src/OakDocumentView.h>
#import <MenuBuilder/src/MenuBuilder.h>
#import <MenuBuilder/src/MBMenuDelegate.h>
#import <Preferences/src/Keys.h>
#import <Preferences/src/Preferences.h>
#import <Preferences/src/TerminalPreferences.h>
#if 0
// App updates are disabled for this fork until we decide on a new update mechanism.
#import <SoftwareUpdate/src/SoftwareUpdate.h>
#endif
#import <document/src/OakDocument.h>
#import <document/src/OakDocumentController.h>
#import <bundles/src/query.h>
#import <io/src/path.h>
#import <regexp/src/glob.h>
#import <ns/src/ns.h>
#import <settings/src/settings.h>
#import <oak/debug.h>
#import <oak/oak.h>
#import <scm/src/scm.h>
#import <text/src/types.h>

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
		else if([NSFileManager.defaultManager fileExistsAtPath:path isDirectory:&isDirectory] && isDirectory)
		{
			[OakDocumentController.sharedInstance showFileBrowserAtPath:path];
		}
		else
		{
			[documents addObject:[OakDocumentController.sharedInstance documentWithPath:path]];
		}
	}

	if([itemsToInstall count])
		[BundlesManager.sharedInstance installBundleItemsAtPaths:itemsToInstall];

	for(NSString* path in plugInsToInstall)
		[TMPlugInController.sharedInstance installPlugInAtPath:path];

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

@interface AppController () <OakUserDefaultsObserver, NSMenuItemValidation>
@property (nonatomic) BOOL didFinishLaunching;
@property (nonatomic) BOOL keyWindowHasBackAndForwardActions;
@end

@implementation AppController
- (NSMenu*)mainMenu
{
	MBMenu const items = {
		MBMenuItem{ @"TextMate" }.withSubmenu({
				{ @"About TextMate",        @selector(orderFrontAboutPanel:)               },
				{ /* -------- */ },
				{ @"Preferences…",          @selector(showPreferences:),            @","   },
#if 0
				// App updates are disabled for this fork until we decide on a new update mechanism.
				{ @"Check for Update",      @selector(performSoftwareUpdateCheck:)         },
				MBMenuItem{ @"Check for Test Build", @selector(performSoftwareUpdateCheck:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withAlternate(YES),
#endif
				{ /* -------- */ },
				MBMenuItem{ @"Services" }.withSystemMenu(MBMenuTypeServices),
				{ /* -------- */ },
				{ @"Hide TextMate",         @selector(hide:),                       @"h"   },
				MBMenuItem{ @"Hide Others", @selector(hideOtherApplications:), @"h" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
				{ @"Show All",              @selector(unhideAllApplications:),             },
				{ /* -------- */ },
				{ @"Quit TextMate",         @selector(terminate:),                  @"q"   },
		}),
		MBMenuItem{ @"File" }.withSubmenu({
				{ @"New",                     @selector(newDocument:),              @"n"   },
				MBMenuItem{ @"New File Browser", @selector(newFileBrowser:), @"n" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl).withAlternate(YES),
				MBMenuItem{ @"New Tab", @selector(newDocumentInTab:), @"n" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
				{ /* -------- */ },
				{ @"Open…",                   @selector(openDocument:),             @"o"   },
				{ @"Open Quickly…",           @selector(goToFile:),                 @"t"   },
				MBMenuItem{ @"Open Recent" }.withSystemMenu(MBMenuTypeOpenRecent).withSubmenu({
						{ @"Clear Menu", @selector(clearRecentDocuments:) },
				}),
				{ @"Open Recent Project…",    @selector(openFavorites:),            @"O"   },
				{ /* -------- */ },
				{ @"Close",                   @selector(performClose:),             @"w"   },
				{ @"Close Window",            @selector(performCloseWindow:),       @"W"   },
				MBMenuItem{ @"Close All Tabs", @selector(performCloseAllTabs:), @"w" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl),
				MBMenuItem{ @"Close Other Tabs", @selector(performCloseOtherTabsXYZ:), @"w" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl),
				{ @"Close Tabs to the Right", @selector(performCloseTabsToTheRight:)       },
				MBMenuItem{ @"Close Tabs to the Left", @selector(performCloseTabsToTheLeft:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withAlternate(YES),
				{ /* -------- */ },
				{ @"Sticky",                  @selector(toggleSticky:)                     },
				{ /* -------- */ },
				{ @"Save",                    @selector(saveDocument:),             @"s"   },
				{ @"Save As…",                @selector(saveDocumentAs:),           @"S"   },
				MBMenuItem{ @"Save All", @selector(saveAllDocuments:), @"s" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
				{ @"Revert",                  @selector(revertDocumentToSaved:)            },
				{ /* -------- */ },
				MBMenuItem{ @"Page Setup…", @selector(runPageLayout:) }.withTarget(NSApp.delegate),
				{ @"Print…",                  @selector(printDocument:),            @"p"   },
		}),
		MBMenuItem{ @"Edit" }.withSubmenu({
				{ @"Undo",   @selector(undo:),   @"z" },
				{ @"Redo",   @selector(redo:),   @"Z" },
				{ /* -------- */ },
				{ @"Cut",    @selector(cut:),    @"x" },
				{ @"Copy",   @selector(copy:),   @"c" },
				MBMenuItem{ @"Paste" }.withSubmenu({
						{ @"Paste",                   @selector(paste:),                @"v"   },
						MBMenuItem{ @"Paste Without Indenting", @selector(pasteWithoutReindent:), @"v" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl).withAlternate(YES),
						MBMenuItem{ @"Paste Next", @selector(pasteNext:), @"v" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
						{ @"Paste Previous",          @selector(pastePrevious:),        @"V"   },
						{ /* -------- */ },
						MBMenuItem{ @"Show History", @selector(showClipboardHistory:), @"v" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl),
				}),
				MBMenuItem{ @"Delete", @selector(delete:) }.withKey(NSBackspaceCharacter),
				{ /* -------- */ },
				MBMenuItem{ @"Macros" }.withSubmenu({
						MBMenuItem{ @"Start Recording", @selector(toggleMacroRecording:), @"m" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
						{ @"Replay Macro",    @selector(playScratchMacro:),     @"M"   },
						MBMenuItem{ @"Save Macro…", @selector(saveScratchMacro:), @"m" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl),
				}),
				{ /* -------- */ },
				MBMenuItem{ @"Select" }.withSubmenu({
						{ @"Word",                    @selector(selectWord:)                  },
						{ @"Line",                    @selector(selectHardLine:)              },
						{ @"Paragraph",               @selector(selectParagraph:)             },
						{ @"Current Scope",           @selector(selectCurrentScope:)          },
						{ @"Enclosing Typing Pairs",  @selector(selectBlock:),           @"B" },
						{ @"All",                     @selector(selectAll:),             @"a" },
						{ /* -------- */ },
						MBMenuItem{ @"Toggle Column Selection", @selector(toggleColumnSelection:) }.withModifierFlags(NSEventModifierFlagOption),
				}),
				MBMenuItem{ @"Find" }.withSubmenu({
						MBMenuItem{ @"Find and Replace…", @selector(orderFrontFindPanel:), @"f" }.withTag(FFSearchTargetDocument),
						MBMenuItem{ @"Find in Project…", @selector(orderFrontFindPanel:), @"F" }.withTag(FFSearchTargetProject),
						MBMenuItem{ @"Find in Folder…", @selector(orderFrontFindPanel:) }.withTag(FFSearchTargetOther),
						{ /* -------- */ },
						MBMenuItem{ @"Show Find History", @selector(showFindHistory:), @"f" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl),
						{ /* -------- */ },
						MBMenuItem{ @"Incremental Search", @selector(incrementalSearch:), @"s" }.withModifierFlags(NSEventModifierFlagControl),
						MBMenuItem{ @"Incremental Search Previous", @selector(incrementalSearchPrevious:), @"S" }.withModifierFlags(NSEventModifierFlagControl),
						{ /* -------- */ },
						{ @"Find Next",                   @selector(findNext:),                     @"g"   },
						{ @"Find Previous",               @selector(findPrevious:),                 @"G"   },
						MBMenuItem{ @"Find All", @selector(findAllInSelection:), @"f" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
						{ /* -------- */ },
						MBMenuItem{ @"Find Options" }.withSubmenu({
								MBMenuItem{ @"Ignore Case", @selector(toggleFindOption:), @"c" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(2),
								MBMenuItem{ @"Regular Expression", @selector(toggleFindOption:), @"r" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(8),
								MBMenuItem{ @"Ignore Whitespace", @selector(toggleFindOption:) }.withTag(4),
								MBMenuItem{ @"Wrap Around", @selector(toggleFindOption:), @"a" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(128),
						}),
						{ /* -------- */ },
						MBMenuItem{ @"Replace", @selector(replace:), @"g" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
						{ @"Replace & Find",              @selector(replaceAndFind:)                       },
						MBMenuItem{ @"Replace All", @selector(replaceAll:), @"g" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl),
						MBMenuItem{ @"Replace All in Selection", @selector(replaceAllInSelection:), @"G" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl),
						{ /* -------- */ },
						{ @"Use Selection for Find",      @selector(copySelectionToFindPboard:),    @"e"   },
						{ @"Use Selection for Replace",   @selector(copySelectionToReplacePboard:), @"E"   },
				}),
				MBMenuItem{ @"Spelling" }.withSubmenuRef(&spellingMenu).withSubmenu({
						{ @"Spelling…",                   @selector(showGuessPanel:),                @":"   },
						{ @"Check Document Now",          @selector(checkSpelling:),                 @";"   },
						{ /* -------- */ },
						MBMenuItem{ @"Check Spelling While Typing", @selector(toggleContinuousSpellChecking:), @";" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
						{ /* -------- */ },
				}),
		}),
		MBMenuItem{ @"View" }.withSubmenu({
				MBMenuItem{ @"Font" }.withSystemMenu(MBMenuTypeFont).withSubmenu({
						MBMenuItem{ @"Show Fonts", @selector(orderFrontFontPanel:) }.withTarget(NSFontManager.sharedFontManager),
						{ /* -------- */ },
						{ @"Bigger",       @selector(makeTextLarger:),       @"+" },
						{ @"Smaller",      @selector(makeTextSmaller:),      @"-" },
						{ @"Default Size", @selector(makeTextStandardSize:), @"0" },
				}),
				MBMenuItem{ @"Show File Browser", @selector(toggleFileBrowser:), @"d" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl),
				MBMenuItem{ @"Show HTML Output", @selector(toggleHTMLOutput:), @"h" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl),
				MBMenuItem{ @"Show Line Numbers", @selector(toggleLineNumbers:), @"l" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
				{ @"Show Scope Bar", @selector(toggleScopeBar:) },
				{ /* -------- */ },
				MBMenuItem{ @"Show Invisibles", @selector(toggleShowInvisibles:), @"i" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
				{ /* -------- */ },
				MBMenuItem{ @"Enable Soft Wrap", @selector(toggleSoftWrap:), @"w" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
				{ @"Show Wrap Column",       @selector(toggleShowWrapColumn:)         },
				{ @"Show Indent Guides",     @selector(toggleShowIndentGuides:)       },
				MBMenuItem{ @"Wrap Column" }.withSubmenuRef(&wrapColumnMenu).withSubmenu({
						{ @"Use Window Frame", @selector(takeWrapColumnFrom:)   },
						{ /* -------- */ },
						MBMenuItem{ @"40", @selector(takeWrapColumnFrom:) }.withTag(40),
						MBMenuItem{ @"80", @selector(takeWrapColumnFrom:) }.withTag(80),
						{ /* -------- */ },
						MBMenuItem{ @"Other…", @selector(takeWrapColumnFrom:) }.withTag(-1),
				}),
				{ /* -------- */ },
				MBMenuItem{ @"Tab Size" }.withSubmenu({
						MBMenuItem{ @"2", @selector(takeTabSizeFrom:) }.withTag(2),
						MBMenuItem{ @"3", @selector(takeTabSizeFrom:) }.withTag(3),
						MBMenuItem{ @"4", @selector(takeTabSizeFrom:) }.withTag(4),
						MBMenuItem{ @"5", @selector(takeTabSizeFrom:) }.withTag(5),
						MBMenuItem{ @"6", @selector(takeTabSizeFrom:) }.withTag(6),
						MBMenuItem{ @"7", @selector(takeTabSizeFrom:) }.withTag(7),
						MBMenuItem{ @"8", @selector(takeTabSizeFrom:) }.withTag(8),
						{ /* -------- */ },
						{ @"Other…", @selector(showTabSizeSelectorPanel:) },
				}),
				MBMenuItem{ @"Theme" }.withSubmenuRef(&themesMenu),
				{ /* -------- */ },
				MBMenuItem{ @"Fold Current Block", @selector(toggleCurrentFolding:) }.withModifierFlags(0).withKey(NSF1FunctionKey),
				MBMenuItem{ @"Toggle Foldings at Level" }.withSubmenu({
						MBMenuItem{ @"All Levels", @selector(takeLevelToFoldFrom:), @"0" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
						MBMenuItem{ @"1", @selector(takeLevelToFoldFrom:), @"1" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(1),
						MBMenuItem{ @"2", @selector(takeLevelToFoldFrom:), @"2" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(2),
						MBMenuItem{ @"3", @selector(takeLevelToFoldFrom:), @"3" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(3),
						MBMenuItem{ @"4", @selector(takeLevelToFoldFrom:), @"4" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(4),
						MBMenuItem{ @"5", @selector(takeLevelToFoldFrom:), @"5" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(5),
						MBMenuItem{ @"6", @selector(takeLevelToFoldFrom:), @"6" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(6),
						MBMenuItem{ @"7", @selector(takeLevelToFoldFrom:), @"7" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(7),
						MBMenuItem{ @"8", @selector(takeLevelToFoldFrom:), @"8" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(8),
						MBMenuItem{ @"9", @selector(takeLevelToFoldFrom:), @"9" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withTag(9),
				}),
				{ /* -------- */ },
				{ @"Toggle Scroll Past End", @selector(toggleScrollPastEnd:)          },
				{ /* -------- */ },
				MBMenuItem{ @"View Source", @selector(viewSource:), @"u" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption),
				MBMenuItem{ @"Enter Full Screen", @selector(toggleFullScreen:), @"f" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl),
				{ /* -------- */ },
				{ @"Customize Touch Bar…",   @selector(toggleTouchBarCustomizationPalette:) },
		}),
		MBMenuItem{ @"Navigate" }.withSubmenu({
				{ @"Jump to Line…",              @selector(orderFrontGoToLinePanel:),      @"l" },
				{ @"Jump to Symbol…",            @selector(showSymbolChooser:),            @"T" },
				{ @"Jump to Selection",          @selector(centerSelectionInVisibleArea:), @"j" },
				{ /* -------- */ },
				MBMenuItem{ @"Set Bookmark", @selector(toggleCurrentBookmark:) }.withKey(NSF2FunctionKey),
				MBMenuItem{ @"Jump to Next Bookmark", @selector(goToNextBookmark:) }.withModifierFlags(0).withKey(NSF2FunctionKey),
				MBMenuItem{ @"Jump to Previous Bookmark", @selector(goToPreviousBookmark:) }.withModifierFlags(NSEventModifierFlagShift).withKey(NSF2FunctionKey),
				MBMenuItem{ @"Jump to Bookmark" }.withDelegate([MBMenuDelegate delegateUsingSelector:@selector(updateBookmarksMenu:)]),
				{ /* -------- */ },
				MBMenuItem{ @"Jump to Next Mark", @selector(jumpToNextMark:) }.withModifierFlags(0).withKey(NSF3FunctionKey),
				MBMenuItem{ @"Jump to Previous Mark", @selector(jumpToPreviousMark:) }.withModifierFlags(NSEventModifierFlagShift).withKey(NSF3FunctionKey),
				{ /* -------- */ },
				MBMenuItem{ @"Scroll" }.withSubmenu({
						MBMenuItem{ @"Line Up", @selector(scrollLineUp:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl).withKey(NSUpArrowFunctionKey),
						MBMenuItem{ @"Line Down", @selector(scrollLineDown:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl).withKey(NSDownArrowFunctionKey),
						MBMenuItem{ @"Column Left", @selector(scrollColumnLeft:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl).withKey(NSLeftArrowFunctionKey),
						MBMenuItem{ @"Column Right", @selector(scrollColumnRight:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl).withKey(NSRightArrowFunctionKey),
				}),
				{ /* -------- */ },
				MBMenuItem{ @"Go to Related File", @selector(goToRelatedFile:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withKey(NSUpArrowFunctionKey),
				{ /* -------- */ },
				MBMenuItem{ @"Move Focus to File Browser", @selector(moveFocus:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption).withKey(NSTabCharacter),
		}),
		MBMenuItem{ @"Text" }.withSubmenu({
				{ @"Transpose",                            @selector(transpose:)                        },
				{ /* -------- */ },
				MBMenuItem{ @"Move Selection" }.withSubmenu({
						MBMenuItem{ @"Up", @selector(moveSelectionUp:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl).withKey(NSUpArrowFunctionKey),
						MBMenuItem{ @"Down", @selector(moveSelectionDown:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl).withKey(NSDownArrowFunctionKey),
						MBMenuItem{ @"Left", @selector(moveSelectionLeft:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl).withKey(NSLeftArrowFunctionKey),
						MBMenuItem{ @"Right", @selector(moveSelectionRight:) }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl).withKey(NSRightArrowFunctionKey),
				}),
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
		}),
		MBMenuItem{ @"File Browser" }.withSubmenu({
				MBMenuItem{ @"New File", @selector(newDocumentInDirectory:), @"n" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl),
				{ @"New Folder",       @selector(newFolder:),              @"N"   },
				{ /* -------- */ },
				{ @"Back",             @selector(goBack:)                         },
				{ @"Forward",          @selector(goForward:)                      },
				MBMenuItem{ @"Enclosing Folder", @selector(goToParentFolder:) }.withKey(NSUpArrowFunctionKey),
				{ /* -------- */ },
				MBMenuItem{ @"Select Document", @selector(revealFileInProject:), @"r" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl),
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
		}),
		MBMenuItem{ @"Bundles" }.withSubmenuRef(&bundlesMenu).withSubmenu({
				MBMenuItem{ @"Select Bundle Item…", @selector(showBundleItemChooser:), @"t" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagControl),
				MBMenuItem{ @"Edit Bundles…", @selector(showBundleEditor:), @"b" }.withModifierFlags(NSEventModifierFlagCommand|NSEventModifierFlagOption|NSEventModifierFlagControl),
				{ /* -------- */ },
		}),
		MBMenuItem{ @"Window" }.withSystemMenu(MBMenuTypeWindows).withSubmenu({
				{ @"Minimize",               @selector(miniaturize:),           @"m" },
				{ @"Zoom",                   @selector(performZoom:)                 },
				{ /* -------- */ },
				MBMenuItem{ @"Show Previous Tab", @selector(selectPreviousTab:) }.withModifierFlags(NSEventModifierFlagControl|NSEventModifierFlagShift).withKey(NSTabCharacter),
				MBMenuItem{ @"Show Next Tab", @selector(selectNextTab:) }.withModifierFlags(NSEventModifierFlagControl).withKey(NSTabCharacter),
				MBMenuItem{ @"Show Previous Tab", @selector(selectPreviousTab:) }.withModifierFlags(NSEventModifierFlagOption|NSEventModifierFlagCommand).withKey(NSLeftArrowFunctionKey).withHidden(YES),
				MBMenuItem{ @"Show Next Tab", @selector(selectNextTab:) }.withModifierFlags(NSEventModifierFlagOption|NSEventModifierFlagCommand).withKey(NSRightArrowFunctionKey).withHidden(YES),
				MBMenuItem{ @"Show Previous Tab", @selector(selectPreviousTab:), @"{" }.withHidden(YES),
				MBMenuItem{ @"Show Next Tab", @selector(selectNextTab:), @"}" }.withHidden(YES),
				MBMenuItem{ @"Show Tab" }.withDelegate([MBMenuDelegate delegateUsingSelector:@selector(updateShowTabMenu:)]),
				{ /* -------- */ },
				{ @"Move Tab to New Window", @selector(moveDocumentToNewWindow:)     },
				{ @"Merge All Windows",      @selector(mergeAllWindows:)             },
				{ /* -------- */ },
				{ @"Bring All to Front",     @selector(arrangeInFront:)              },
		}),
		MBMenuItem{ @"Help" }.withSystemMenu(MBMenuTypeHelp).withSubmenu({
				{ @"TextMate Help", @selector(showHelp:), @"?" },
		}),
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
			MBMenuItem{ @"New File", @selector(newDocumentAndActivate:) }.withTarget(self),
			MBMenuItem{ @"Open…", @selector(openDocumentAndActivate:) }.withTarget(self),
		};
		return MBCreateMenu(items);
	}

- (void)setKeyWindowHasBackAndForwardActions:(BOOL)flag
{
	if(_keyWindowHasBackAndForwardActions == flag)
		return;
	_keyWindowHasBackAndForwardActions = flag;

	NSMenu* textMenu        = [NSApp.mainMenu itemWithTitle:@"Text"].submenu;
	NSMenu* fileBrowserMenu = [NSApp.mainMenu itemWithTitle:@"File Browser"].submenu;

	auto itemWithAction = ^NSMenuItem*(NSMenu* menu, SEL action){
		NSInteger index = [menu indexOfItemWithTarget:nil andAction:action];
		return index == -1 ? nil : menu.itemArray[index];
	};

	NSMenuItem* backMenuItem       = itemWithAction(fileBrowserMenu, @selector(goBack:));
	NSMenuItem* forwardMenuItem    = itemWithAction(fileBrowserMenu, @selector(goForward:));
	NSMenuItem* shiftLeftMenuItem  = itemWithAction(textMenu,        @selector(shiftLeft:));
	NSMenuItem* shiftRightMenuItem = itemWithAction(textMenu,        @selector(shiftRight:));

	if(!backMenuItem || !forwardMenuItem || !shiftLeftMenuItem || !shiftRightMenuItem)
		return;

	for(NSMenuItem* menuItem in @[ backMenuItem, forwardMenuItem, shiftLeftMenuItem, shiftRightMenuItem ])
		menuItem.keyEquivalent = @"";

	(flag ? backMenuItem : shiftLeftMenuItem).keyEquivalent                 = @"[";
	(flag ? backMenuItem : shiftLeftMenuItem).keyEquivalentModifierMask     = NSEventModifierFlagCommand;
	(flag ? forwardMenuItem : shiftRightMenuItem).keyEquivalent             = @"]";
	(flag ? forwardMenuItem : shiftRightMenuItem).keyEquivalentModifierMask = NSEventModifierFlagCommand;
}

- (void)applicationDidUpdate:(NSNotification*)aNotification
{
	BOOL foundBackAndForwardActions = NO;
	for(NSResponder* responder = NSApp.keyWindow.firstResponder; responder && !foundBackAndForwardActions; responder = responder.nextResponder)
	{
		if([responder respondsToSelector:@selector(shiftLeft:)])
			break;
		else if([responder respondsToSelector:@selector(goBack:)])
			foundBackAndForwardActions = YES;
	}
	self.keyWindowHasBackAndForwardActions = foundBackAndForwardActions;
}

- (void)userDefaultsDidChange:(id)sender
{
	BOOL disableRmate        = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisableRMateServerKey];
	NSString* rmateInterface = [NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsRMateServerListenKey];
	NSInteger rmatePort      = [NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsRMateServerPortKey];
	if(rmatePort < 0)
		rmatePort = 0;
	else if(rmatePort > 0xFFFF)
		rmatePort = 0xFFFF;
	setup_rmate_server(!disableRmate, static_cast<uint16_t>(rmatePort), [rmateInterface isEqualToString:kRMateServerListenRemote]);
}

- (void)applicationWillFinishLaunching:(NSNotification*)aNotification
{
	if(NSMenu* menu = [self mainMenu])
		NSApp.mainMenu = menu;

#if 0
	// App updates are disabled for this fork until we decide on a new update mechanism.
	NSOperatingSystemVersion osVersion = NSProcessInfo.processInfo.operatingSystemVersion;
	NSString* parms = [NSString stringWithFormat:@"v=%@&os=%ld.%ld.%ld", [[[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleShortVersionString"] stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLQueryAllowedCharacterSet], osVersion.majorVersion, osVersion.minorVersion, osVersion.patchVersion];

	SoftwareUpdate.sharedInstance.channels = @{
		kSoftwareUpdateChannelRelease:    [NSURL URLWithString:[NSString stringWithFormat:@"" REST_API "/releases/release?%@", parms]],
		kSoftwareUpdateChannelPrerelease: [NSURL URLWithString:[NSString stringWithFormat:@"" REST_API "/releases/beta?%@", parms]],
		kSoftwareUpdateChannelCanary:     [NSURL URLWithString:[NSString stringWithFormat:@"" REST_API "/releases/nightly?%@", parms]],
	};
#endif

	settings_t::set_default_settings_path([[[NSBundle mainBundle] pathForResource:@"Default" ofType:@"tmProperties"] fileSystemRepresentation]);
	settings_t::set_global_settings_path(path::join(path::home(), "Library/Application Support/TextMate/Global.tmProperties"));

	[NSUserDefaults.standardUserDefaults registerDefaults:@{
		@"NSRecentDocumentsLimit": @25,
		@"WebKitDeveloperExtras":  @YES,
	}];
	RegisterDefaults();

	[TMPlugInController.sharedInstance loadAllPlugIns:nil];

	[BundlesManager.sharedInstance loadBundlesIndex];

	if(BOOL restoreSession = ![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisableSessionRestoreKey])
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
	return self.didFinishLaunching;
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
	NSWindow.allowsAutomaticWindowTabbing = NO;

	if([NSApp respondsToSelector:@selector(setAutomaticCustomizeTouchBarMenuItemEnabled:)]) // MAC_OS_X_VERSION_10_12_1
		NSApp.automaticCustomizeTouchBarMenuItemEnabled = YES;

	if(!HasDocumentWindow([NSApp orderedWindows]))
	{
		BOOL disableUntitledAtStartupPrefs = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisableNewDocumentAtStartupKey];
		BOOL showFavoritesInsteadPrefs     = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsShowFavoritesInsteadOfUntitledKey];

		if(showFavoritesInsteadPrefs)
			[self openFavorites:self];
		else if(!disableUntitledAtStartupPrefs)
			[self newDocument:self];
	}

	[self userDefaultsDidChange:nil]; // setup mate/rmate server
	OakObserveUserDefaults(self);

	NSMenu* selectMenu = [[[[[NSApp mainMenu] itemWithTitle:@"Edit"] submenu] itemWithTitle:@"Select"] submenu];
	[[selectMenu itemWithTitle:@"Toggle Column Selection"] setActivationString:@"⌥" withFont:nil];

	[TerminalPreferences updateMateIfRequired];
	[AboutWindowController showChangesIfUpdated];

	[CrashReporter.sharedInstance postNewCrashReportsToURLString:[NSString stringWithFormat:@"%s/crashes", REST_API]];

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

- (void)applicationDidResignActive:(NSNotification*)aNotification
{
	// If the window to activate, when switching back to TextMate, has “Move to
	// Active Space” set, then the system will move this window to the current
	// space. This is not what we want for auxillary windows like the Find dialog
	// or HTML output, as these windows are tied to a document window.
	//
	// Starting with macOS 10.11 we have to change collection behavior after the
	// current event loop cycle, both when receiving the did become and did resign
	// active notification.

	dispatch_async(dispatch_get_main_queue(), ^{
		NSMutableArray* changedWindows = [NSMutableArray array];
		for(NSWindow* window in NSApp.windows)
		{
			if((window.collectionBehavior & (NSWindowCollectionBehaviorMoveToActiveSpace|NSWindowCollectionBehaviorFullScreenAuxiliary)) == (NSWindowCollectionBehaviorMoveToActiveSpace|NSWindowCollectionBehaviorFullScreenAuxiliary))
			{
				window.collectionBehavior &= ~NSWindowCollectionBehaviorMoveToActiveSpace;
				[changedWindows addObject:window];
			}
		}

		if(changedWindows.count)
		{
			__weak __block id token = [NSNotificationCenter.defaultCenter addObserverForName:NSApplicationDidBecomeActiveNotification object:NSApp queue:nil usingBlock:^(NSNotification*){
				[NSNotificationCenter.defaultCenter removeObserver:token];
				dispatch_async(dispatch_get_main_queue(), ^{
					for(NSWindow* window in changedWindows)
						window.collectionBehavior |= NSWindowCollectionBehaviorMoveToActiveSpace;
				});
			}];
		}
	});
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
	[AboutWindowController.sharedInstance showAboutWindow:self];
}

- (IBAction)orderFrontFindPanel:(id)sender
{
	Find* find = Find.sharedInstance;
	NSInteger mode = [sender respondsToSelector:@selector(tag)] ? [sender tag] : FFSearchTargetDocument;
	switch(mode)
	{
		case FFSearchTargetDocument:  find.searchTarget = FFSearchTargetDocument;  break;
		case FFSearchTargetSelection: find.searchTarget = FFSearchTargetSelection; break;
		case FFSearchTargetProject:   find.searchTarget = FFSearchTargetProject;   break;
		case FFSearchTargetOther:     return [find showFolderSelectionPanel:self]; break;
	}
	[find showWindow:self];
}

- (IBAction)orderFrontGoToLinePanel:(id)sender;
{
	if(id textView = [NSApp targetForAction:@selector(selectionString)])
		[goToLineTextField setStringValue:[textView selectionString]];
	[goToLinePanel makeKeyAndOrderFront:self];
}

- (IBAction)performGoToLine:(id)sender
{
	[goToLinePanel orderOut:self];
	[NSApp sendAction:@selector(selectAndCenter:) to:nil from:[goToLineTextField stringValue]];
}

#if 0
// App updates are disabled for this fork until we decide on a new update mechanism.
- (IBAction)performSoftwareUpdateCheck:(id)sender
{
	[SoftwareUpdate.sharedInstance checkForUpdate:self];
}
#endif

- (IBAction)showPreferences:(id)sender
{
	[Preferences.sharedInstance showWindow:self];
}

- (IBAction)showBundleEditor:(id)sender
{
	[BundleEditor.sharedInstance showWindow:self];
}

- (IBAction)openFavorites:(id)sender
{
	FavoriteChooser* chooser = FavoriteChooser.sharedInstance;
	chooser.action = @selector(didSelectFavorite:);
	[chooser showWindow:self];
}

- (void)didSelectFavorite:(id)sender
{
	NSMutableArray* paths = [NSMutableArray array];
	for(id item in [sender selectedItems])
		[paths addObject:[item valueForKey:@"path"]];
	OakOpenDocuments(paths, YES);
}

// =======================
// = Bundle Item Chooser =
// =======================

- (IBAction)showBundleItemChooser:(id)sender
{
	BundleItemChooser* chooser = BundleItemChooser.sharedInstance;
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
	[Find.sharedInstance takeFindOptionToToggleFrom:sender];
}

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	BOOL enabled = YES;
	if([item action] == @selector(toggleFindOption:))
	{
		BOOL active = NO;
		if(OakPasteboardEntry* entry = [OakPasteboard.findPasteboard current])
		{
			switch([item tag])
			{
				case find::ignore_case:        active = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindIgnoreCase]; break;
				case find::regular_expression: active = [entry regularExpression]; break;
				case find::full_words:         active = [entry fullWordMatch];     enabled = ![entry regularExpression]; break;
				case find::ignore_whitespace:  active = [entry ignoreWhitespace];  enabled = ![entry regularExpression]; break;
				case find::wrap_around:        active = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindWrapAround]; break;
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
	else
	{
		enabled = [self validateThemeMenuItem:item];
	}
	return enabled;
}

- (void)editBundleItem:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(selectedItems)]);
	ASSERT([[sender selectedItems] count] == 1);

	if(NSString* uuid = [[[sender selectedItems] lastObject] valueForKey:@"uuid"])
	{
		[BundleEditor.sharedInstance revealBundleItem:bundles::lookup(to_s(uuid))];
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
	[BundleEditor.sharedInstance revealBundleItem:bundles::lookup(to_s(uuidString))];
}

// ============
// = Printing =
// ============

- (IBAction)runPageLayout:(id)sender
{
	[[NSPageLayout pageLayout] runModal];
}
@end
