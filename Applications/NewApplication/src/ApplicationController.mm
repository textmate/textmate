#import "ApplicationController.h"
#import <MenuBuilder/MenuBuilder.h>

@interface ApplicationController () <NSApplicationDelegate, NSWindowDelegate>
@property (nonatomic) NSWindow* window;
@end

@implementation ApplicationController
- (void)applicationWillFinishLaunching:(NSNotification*)aNotification
{
	MBMenu const items = {
		{ @"NewApplication",
			.submenu = {
				{ @"About NewApplication", @selector(orderFrontStandardAboutPanel:),       .target = NSApp },
				{ /* -------- */ },
				{ @"Preferences…",         NULL,                                     @","   },
				{ /* -------- */ },
				{ @"Services", .systemMenu = MBMenuTypeServices                             },
				{ /* -------- */ },
				{ @"Hide NewApplication",  @selector(hide:),                         @"h"   },
				{ @"Hide Others",          @selector(hideOtherApplications:),        @"h", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption },
				{ @"Show All",             @selector(unhideAllApplications:)                },
				{ /* -------- */ },
				{ @"Quit NewApplication",  @selector(terminate:),                    @"q", .target = NSApp },
			}
		},
		{ @"File",
			.submenu = {
				{ @"New",             @selector(newDocument:),         @"n"   },
				{ @"Open…",           @selector(openDocument:),        @"o"   },
				{ @"Open Recent",
					.systemMenu = MBMenuTypeOpenRecent, .submenu = {
						{ @"Clear Menu", @selector(clearRecentDocuments:) },
					}
				},
				{ /* -------- */ },
				{ @"Close",           @selector(performClose:),        @"w"   },
				{ @"Close All",       @selector(closeAll:),            @"w", .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .target = NSApp, .alternate = YES },
				{ @"Save",            @selector(saveDocument:),        @"s"   },
				{ @"Save As…",        @selector(saveDocumentAs:),      @"S"   },
				{ @"Revert to Saved", @selector(revertDocumentToSaved:)       },
				{ /* -------- */ },
				{ @"Page Setup...",   @selector(runPageLayout:),       @"P"   },
				{ @"Print…",          @selector(print:),               @"p"   },
			}
		},
		{ @"Edit",
			.submenu = {
				{ @"Undo",       @selector(undo:),      @"z" },
				{ @"Redo",       @selector(redo:),      @"Z" },
				{ /* -------- */ },
				{ @"Cut",        @selector(cut:),       @"x" },
				{ @"Copy",       @selector(copy:),      @"c" },
				{ @"Paste",      @selector(paste:),     @"v" },
				{ @"Delete",     @selector(delete:)          },
				{ @"Select All", @selector(selectAll:), @"a" },
				{ /* -------- */ },
				{ @"Find",
					.submenu = {
						{ @"Find…",                  NULL,                                     @"f", .tag = 1 },
						{ @"Find Next",              @selector(findNext:),                     @"g", .tag = 2 },
						{ @"Find Previous",          @selector(findPrevious:),                 @"G", .tag = 3 },
						{ @"Use Selection for Find", @selector(copySelectionToFindPboard:),    @"e", .tag = 7 },
						{ @"Jump to Selection",      @selector(centerSelectionInVisibleArea:), @"j"   },
					}
				},
				{ @"Spelling and Grammar",
					.submenu = {
						{ @"Show Spelling…",              @selector(showGuessPanel:),              @":" },
						{ @"Check Spelling",              @selector(checkSpelling:),               @";" },
						{ @"Check Spelling While Typing", @selector(toggleContinuousSpellChecking:)     },
						{ @"Check Grammar With Spelling", @selector(toggleGrammarChecking:)             },
					}
				},
				{ @"Substitutions",
					.submenu = {
						{ @"Smart Copy/Paste", @selector(toggleSmartInsertDelete:),          .tag = 1 },
						{ @"Smart Quotes",     @selector(toggleAutomaticQuoteSubstitution:), .tag = 2 },
						{ @"Smart Links",      @selector(toggleAutomaticLinkDetection:),     .tag = 3 },
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
						{ @"Ligature",
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
			.submenu = {
				{ @"NewApplication Help", @selector(showHelp:), @"?" },
			}
		},
	};

	if(NSMenu* menu = MBCreateMenu(items))
		NSApp.mainMenu = menu;
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
	_window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 400, 400) styleMask:(NSTitledWindowMask|NSResizableWindowMask|NSClosableWindowMask|NSMiniaturizableWindowMask) backing:NSBackingStoreBuffered defer:NO];
	_window.releasedWhenClosed = NO;
	_window.delegate           = self;
	_window.title              = @"New Application";
	[_window center];
	[_window makeKeyAndOrderFront:self];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[NSApp terminate:self];
}
@end
