#import "AppDelegate.h"
#import "WindowController.h"
#import <MenuBuilder/MenuBuilder.h>

@interface AppDelegate () <NSApplicationDelegate, NSWindowDelegate>
@property (nonatomic) NSWindow* window;
@end

@implementation AppDelegate
- (void)applicationWillFinishLaunching:(NSNotification*)aNotification
{
	MBMenu const items = {
		{ @"NewApplication",
			.submenu = {
				{ @"About NewApplication", @selector(orderFrontStandardAboutPanel:)         },
				{ /* -------- */ },
				{ @"Preferences…",         NULL,                                     @","   },
				{ /* -------- */ },
				{ @"Services", .systemMenu = MBMenuTypeServices                             },
				{ /* -------- */ },
				{ @"Hide NewApplication",  @selector(hide:),                         @"h"   },
				{ @"Hide Others",          @selector(hideOtherApplications:),        @"h", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Show All",             @selector(unhideAllApplications:)                },
				{ /* -------- */ },
				{ @"Quit NewApplication",  @selector(terminate:),                    @"q"   },
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
				{ @"NewApplication Help", @selector(showHelp:), @"?" },
			}
		},
	};

	if(NSMenu* menu = MBCreateMenu(items))
		NSApp.mainMenu = menu;
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
	WindowController* windowController = [[WindowController alloc] init];
	[windowController showWindow:self];
}
@end
