#import "AppController.h"
#import "Favorites.h"
#import "AboutWindowController.h"
#import "TMPlugInController.h"
#import "RMateServer.h"
#import <BundleEditor/BundleEditor.h>
#import <BundlesManager/BundlesManager.h>
#import <CrashReporter/CrashReporter.h>
#import <DocumentWindow/DocumentController.h>
#import <Find/Find.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakFilterList/BundleItemChooser.h>
#import <OakFilterList/OakFilterList.h>
#import <OakFoundation/NSString Additions.h>
#import <OakTextView/OakDocumentView.h>
#import <Preferences/Keys.h>
#import <Preferences/Preferences.h>
#import <Preferences/TerminalPreferences.h>
#import <SoftwareUpdate/SoftwareUpdate.h>
#import <document/collection.h>
#import <bundles/query.h>
#import <io/path.h>
#import <network/tbz.h>
#import <ns/ns.h>
#import <oak/debug.h>
#import <oak/compat.h>
#import <oak/oak.h>
#import <scm/scm.h>
#import <text/types.h>

OAK_DEBUG_VAR(AppController);

void OakOpenDocuments (NSArray* paths)
{
	std::vector<document::document_ptr> documents;
	NSMutableArray* itemsToInstall = [NSMutableArray array];
	NSMutableArray* plugInsToInstall = [NSMutableArray array];
	BOOL enableInstallHandler = ([NSEvent modifierFlags] & NSAlternateKeyMask) == 0;
	for(NSString* path in paths)
	{
		static auto const tmItemExtensions = new std::set<std::string>{ "tmbundle", "tmcommand", "tmdragcommand", "tmlanguage", "tmmacro", "tmpreferences", "tmsnippet", "tmtheme" };
		std::string const pathExt = to_s([[path pathExtension] lowercaseString]);
		if(enableInstallHandler && tmItemExtensions->find(pathExt) != tmItemExtensions->end())
		{
			[itemsToInstall addObject:path];
		}
		else if(enableInstallHandler && pathExt == "tmplugin")
		{
			[plugInsToInstall addObject:path];
		}
		else if(path::is_directory(to_s(path)))
		{
			document::show_browser(to_s(path));
		}
		else
		{
			documents.push_back(document::create(to_s(path)));
		}
	}

	if([itemsToInstall count])
		[[BundlesManager sharedInstance] installBundleItemsAtPaths:itemsToInstall];

	for(NSString* path in plugInsToInstall)
		[[TMPlugInController sharedInstance] installPlugInAtPath:path];

	document::show(documents);
}

BOOL HasDocumentWindow (NSArray* windows)
{
	for(NSWindow* window in windows)
	{
		if([window.delegate isKindOfClass:[DocumentController class]])
			return YES;
	}
	return NO;
}

@interface AppController ()
@property (nonatomic) OakFilterWindowController* filterWindowController;
@property (nonatomic) BOOL didFinishLaunching;
@property (nonatomic) BOOL currentResponderIsOakTextView;
@end

@implementation AppController
- (void)setCurrentResponderIsOakTextView:(BOOL)flag
{
	if(_currentResponderIsOakTextView != flag)
	{
		_currentResponderIsOakTextView = flag;

		NSMenu* mainMenu = [NSApp mainMenu];
		NSMenu* goMenu   = [[mainMenu itemWithTitle:@"Go"] submenu];
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
			shiftLeftMenuItem.keyEquivalentModifierMask  = NSCommandKeyMask;
			shiftRightMenuItem.keyEquivalent             = @"]";
			shiftRightMenuItem.keyEquivalentModifierMask = NSCommandKeyMask;
		}
		else
		{
			shiftLeftMenuItem.keyEquivalent           = @"";
			shiftRightMenuItem.keyEquivalent          = @"";

			backMenuItem.keyEquivalent                = @"[";
			backMenuItem.keyEquivalentModifierMask    = NSCommandKeyMask;
			forwardMenuItem.keyEquivalent             = @"]";
			forwardMenuItem.keyEquivalentModifierMask = NSCommandKeyMask;
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

- (void)checkExpirationDate:(id)sender
{
	NSTimeInterval const kSecondsPerDay = 24*60*60;

	NSDate* currentDate    = [NSDate date];
	NSDate* compileDate    = [NSDate dateWithString:@COMPILE_DATE @" 00:00:00 +0000"];
	NSDate* warningDate    = [compileDate dateByAddingTimeInterval:45*kSecondsPerDay];
	NSDate* expirationDate = [compileDate dateByAddingTimeInterval:90*kSecondsPerDay];

	if([currentDate laterDate:expirationDate] == currentDate)
	{
		NSInteger choice = NSRunAlertPanel(@"TextMate is Outdated!", @"You can get a new version from https://macromates.com/download.", @"Visit Website", nil, nil);
		if(choice == NSAlertDefaultReturn) // "Visit Website"
			[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"https://macromates.com/download"]];
		[NSApp terminate:self];
	}
	else if([currentDate laterDate:warningDate] == currentDate)
	{
		NSInteger daysUntilExpiration = floor([expirationDate timeIntervalSinceNow] / kSecondsPerDay);
		NSInteger weeksSinceCompilation = floor(-[compileDate timeIntervalSinceNow] / kSecondsPerDay / 7);
		NSInteger choice = NSRunAlertPanel(@"TextMate is Outdated!", @"This version of TextMate is more than %ld weeks old and you should update to latest version. You can continue to use this version for another %ld day%s.", @"Continue", @"Visit Download Page", nil, weeksSinceCompilation, daysUntilExpiration, daysUntilExpiration == 1 ? "" : "s");
		if(choice == NSAlertAlternateReturn) // "Visit Website"
			[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"https://macromates.com/download"]];
	}
	[NSTimer scheduledTimerWithTimeInterval:kSecondsPerDay target:self selector:@selector(checkExpirationDate:) userInfo:nil repeats:NO];
}

- (void)applicationWillFinishLaunching:(NSNotification*)aNotification
{
	D(DBF_AppController, bug("\n"););
	[self checkExpirationDate:self];

	settings_t::set_default_settings_path([[[NSBundle mainBundle] pathForResource:@"Default" ofType:@"tmProperties"] fileSystemRepresentation]);
	settings_t::set_global_settings_path(path::join(path::home(), "Library/Application Support/TextMate/Global.tmProperties"));

	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		@"ApplePressAndHoldEnabled" : @NO,
		@"NSRecentDocumentsLimit"   : @25,
		@"WebKitDeveloperExtras"    : @YES,
	}];
	RegisterDefaults();

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
							fprintf(stderr, "*** error wrting bytes to tar\n");
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
	[[TMPlugInController sharedInstance] loadAllPlugIns:nil];

	BOOL restoreSession = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableSessionRestoreKey];
	if(restoreSession && ([NSEvent modifierFlags] & NSShiftKeyMask))
	{
		NSInteger choice = NSRunAlertPanel(@"Disable Session Restore?", @"By holding down shift (⇧) you have indicated that you wish to disable restoring the documents which were open in last session.", @"Disable", @"Restore Documents", nil);
		if(choice == NSAlertDefaultReturn) // "Disable"
			restoreSession = NO;
	}

	if(restoreSession)
		[DocumentController restoreSession];
}

- (BOOL)applicationShouldOpenUntitledFile:(NSApplication*)anApplication
{
	D(DBF_AppController, bug("\n"););
	return self.didFinishLaunching;
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
	D(DBF_AppController, bug("\n"););

	BOOL disableUntitledAtStartupPrefs = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableNewDocumentAtStartupKey];
	if(!disableUntitledAtStartupPrefs && !HasDocumentWindow([NSApp orderedWindows]))
		[self newDocument:self];

	SoftwareUpdate* swUpdate = [SoftwareUpdate sharedInstance];
	NSString* parms = [NSString stringWithFormat:@"v=%@&os=%zu.%zu.%zu", [[[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleShortVersionString"] stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding], oak::os_major(), oak::os_minor(), oak::os_patch()];
	[swUpdate setSignee:key_chain_t::key_t("org.textmate.duff", "Allan Odgaard", "-----BEGIN PUBLIC KEY-----\nMIIBtjCCASsGByqGSM44BAEwggEeAoGBAPIE9PpXPK3y2eBDJ0dnR/D8xR1TiT9m\n8DnPXYqkxwlqmjSShmJEmxYycnbliv2JpojYF4ikBUPJPuerlZfOvUBC99ERAgz7\nN1HYHfzFIxVo1oTKWurFJ1OOOsfg8AQDBDHnKpS1VnwVoDuvO05gK8jjQs9E5LcH\ne/opThzSrI7/AhUAy02E9H7EOwRyRNLofdtPxpa10o0CgYBKDfcBscidAoH4pkHR\nIOEGTCYl3G2Pd1yrblCp0nCCUEBCnvmrWVSXUTVa2/AyOZUTN9uZSC/Kq9XYgqwj\nhgzqa8h/a8yD+ao4q8WovwGeb6Iso3WlPl8waz6EAPR/nlUTnJ4jzr9t6iSH9owS\nvAmWrgeboia0CI2AH++liCDvigOBhAACgYAFWO66xFvmF2tVIB+4E7CwhrSi2uIk\ndeBrpmNcZZ+AVFy1RXJelNe/cZ1aXBYskn/57xigklpkfHR6DGqpEbm6KC/47Jfy\ny5GEx+F/eBWEePi90XnLinytjmXRmS2FNqX6D15XNG1xJfjociA8bzC7s4gfeTUd\nlpQkBq2z71yitA==\n-----END PUBLIC KEY-----\n")];
	[swUpdate setChannels:@{
		kSoftwareUpdateChannelRelease : [NSURL URLWithString:[NSString stringWithFormat:REST_API @"/releases/release?%@", parms]],
		kSoftwareUpdateChannelBeta    : [NSURL URLWithString:[NSString stringWithFormat:REST_API @"/releases/beta?%@", parms]],
		kSoftwareUpdateChannelNightly : [NSURL URLWithString:[NSString stringWithFormat:REST_API @"/releases/nightly?%@", parms]],
	}];

	[self userDefaultsDidChange:nil]; // setup mate/rmate server
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];

	bundlesMenu.delegate  = self;
	themesMenu.delegate   = self;
	spellingMenu.delegate = self;

	NSMenu* selectMenu = [[[[[NSApp mainMenu] itemWithTitle:@"Edit"] submenu] itemWithTitle:@"Select"] submenu];
	[[selectMenu itemWithTitle:@"Toggle Column Selection"] setActivationString:@"⌥" withFont:nil];

	[TerminalPreferences updateMateIfRequired];
	[AboutWindowController showChangesIfUpdated];
	[[BundlesManager sharedInstance] setAutoUpdateBundles:YES];

	[[CrashReporter sharedInstance] applicationDidFinishLaunching:aNotification];
	[[CrashReporter sharedInstance] postNewCrashReportsToURLString:REST_API @"/crashes"];

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
	NSInteger mode = [sender respondsToSelector:@selector(tag)] ? [sender tag] : find_tags::in_document;
	switch(mode)
	{
		case find_tags::in_document:  return [[Find sharedInstance] showFindWindowFor:FFSearchInDocument];
		case find_tags::in_selection: return [[Find sharedInstance] showFindWindowFor:FFSearchInSelection];
		case find_tags::in_project:   return [[Find sharedInstance] showFindWindowFor:NSHomeDirectory()];
		case find_tags::in_folder:    return [[Find sharedInstance] showFolderSelectionPanel:self];
	}
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
	[NSApp sendAction:@selector(setSelectionString:) to:nil from:[goToLineTextField stringValue]];
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
	OakFilterWindowController* controller = [OakFilterWindowController new];
	controller.dataSource              = [FavoritesDataSource favoritesDataSource];
	controller.action                  = @selector(didSelectFavorite:);
	controller.allowsMultipleSelection = YES;
	[controller showWindow:self];
}

- (void)didSelectFavorite:(id)sender
{
	NSMutableArray* paths = [NSMutableArray array];
	for(id item in [sender selectedItems])
		[paths addObject:[item objectForKey:@"path"]];
	OakOpenDocuments(paths);
}

// =======================
// = Bundle Item Chooser =
// =======================

- (void)setFilterWindowController:(OakFilterWindowController*)controller
{
	if(controller != _filterWindowController)
	{
		if(_filterWindowController)
		{
			[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowWillCloseNotification object:_filterWindowController.window];
			_filterWindowController.target = nil;
			[_filterWindowController close];
		}

		if(_filterWindowController = controller)
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(filterWindowWillClose:) name:NSWindowWillCloseNotification object:_filterWindowController.window];
	}
}

- (void)filterWindowWillClose:(NSNotification*)notification
{
	BundleItemChooser* dataSource = [_filterWindowController dataSource];
	bundleItemSearch.filter_string  = to_s([dataSource filterString]);
	bundleItemSearch.key_equivalent = [dataSource keyEquivalentSearch];
	bundleItemSearch.all_scopes     = [dataSource searchAllScopes];
	bundleItemSearch.search_type    = [dataSource searchType];
	self.filterWindowController     = nil;
}

- (IBAction)showBundleItemChooser:(id)sender
{
	self.filterWindowController            = [OakFilterWindowController new];
	OakTextView* textView                  = [NSApp targetForAction:@selector(scopeContext)];

	BundleItemChooser* dataSource          = [BundleItemChooser bundleItemChooserForScope:textView ? [textView scopeContext] : scope::wildcard];
	dataSource.searchType                  = search::type(bundleItemSearch.search_type);
	dataSource.keyEquivalentSearch         = bundleItemSearch.key_equivalent;
	dataSource.textViewHasSelection        = [textView hasSelection];
	dataSource.searchAllScopes             = bundleItemSearch.all_scopes;
	dataSource.filterString                = [NSString stringWithCxxString:bundleItemSearch.filter_string];

	_filterWindowController.dataSource      = dataSource;
	_filterWindowController.action          = @selector(bundleItemChooserDidSelectItems:);
	_filterWindowController.accessoryAction = @selector(editBundleItem:);
	[_filterWindowController showWindowRelativeToWindow:[textView window]];
}

- (void)bundleItemChooserDidSelectItems:(id)sender
{
	for(NSDictionary* item in [sender selectedItems])
	{
		if(OakIsAlternateKeyOrMouseEvent())
				[[BundleEditor sharedInstance] revealBundleItem:bundles::lookup(to_s((NSString*)[item objectForKey:@"uuid"]))];
		else	[NSApp sendAction:@selector(performBundleItemWithUUIDString:) to:nil from:[item objectForKey:@"uuid"]];
	}
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
			[item setState:(active ? NSOnState : NSOffState)];
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
		if(bundles::item_ptr bundleItem = bundles::lookup(to_s((NSString*)item.representedObject)))
		{
			if(id textView = [NSApp targetForAction:@selector(hasSelection)])
				[item updateTitle:[NSString stringWithCxxString:name_with_selection(bundleItem, [textView hasSelection])]];
		}
	}
	else if([item action] == @selector(printDocument:))
	{
		NSView* webView = [NSApp targetForAction:@selector(print:)];
		enabled = [webView isKindOfClass:[NSView class]] && [webView conformsToProtocol:@protocol(WebDocumentView)];
	}
	return enabled;
}

- (void)editBundleItem:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(selectedItems)]);
	ASSERT([[sender selectedItems] count] == 1);

	if(NSString* uuid = [[[sender selectedItems] lastObject] objectForKey:@"uuid"])
		[[BundleEditor sharedInstance] revealBundleItem:bundles::lookup(to_s(uuid))];

	self.filterWindowController = nil;
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

- (void)printDocument:(id)sender
{
	NSView* webView = [NSApp targetForAction:@selector(print:)];
	if([webView isKindOfClass:[NSView class]] && [webView conformsToProtocol:@protocol(WebDocumentView)])
	{
		NSPrintOperation* printer = [NSPrintOperation printOperationWithView:webView];
		[[printer printPanel] setOptions:[[printer printPanel] options] | NSPrintPanelShowsPaperSize | NSPrintPanelShowsOrientation];

		NSPrintInfo* info = [printer printInfo];

		NSRect display = NSIntersectionRect(info.imageablePageBounds, (NSRect){ NSZeroPoint, info.paperSize });
		info.leftMargin   = NSMinX(display);
		info.rightMargin  = info.paperSize.width - NSMaxX(display);
		info.topMargin    = info.paperSize.height - NSMaxY(display);
		info.bottomMargin = NSMinY(display);

		[printer runOperationModalForWindow:[webView window] delegate:nil didRunSelector:NULL contextInfo:nil];
	}
}
@end
