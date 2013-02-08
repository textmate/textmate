#import "AppController.h"
#import "Favorites.h"
#import "AboutWindowController.h"
#import "InstallBundleItems.h"
#import "TMPlugInController.h"
#import "RMateServer.h"
#import <BundleEditor/BundleEditor.h>
#import <BundlesManager/BundlesManager.h>
#import <CrashReporter/CrashReporter.h>
#import <DocumentWindow/DocumentController.h>
#import <Find/Find.h>
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
#import <io/path.h>
#import <network/tbz.h>
#import <ns/ns.h>
#import <oak/debug.h>
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
		InstallBundleItems(itemsToInstall);

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
@end

@implementation AppController
- (void)userDefaultsDidChange:(id)sender
{
	BOOL disableRmate        = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableRMateServerKey];
	NSString* rmateInterface = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsRMateServerListenKey];
	int rmatePort            = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsRMateServerPortKey];
	setup_rmate_server(!disableRmate, [rmateInterface isEqualToString:kRMateServerListenRemote] ? INADDR_ANY : INADDR_LOOPBACK, rmatePort);
}

- (void)applicationWillFinishLaunching:(NSNotification*)aNotification
{
	D(DBF_AppController, bug("\n"););
	settings_t::set_default_settings_path([[[NSBundle mainBundle] pathForResource:@"Default" ofType:@"tmProperties"] fileSystemRepresentation]);
	settings_t::set_global_settings_path(path::join(path::home(), "Library/Application Support/TextMate/Global.tmProperties"));

	[[NSUserDefaults standardUserDefaults] registerDefaults:[NSDictionary dictionaryWithObjectsAndKeys:
		@NO, @"ApplePressAndHoldEnabled",
		@25, @"NSRecentDocumentsLimit",
		nil]];
	RegisterDefaults();
	[[NSUserDefaults standardUserDefaults] setObject:@NO forKey:@"NSQuitAlwaysKeepsWindows"];

	std::string dest = path::join(path::home(), "Library/Application Support/TextMate/Managed");
	if(!path::exists(dest))
	{
		if(NSString* archive = [[NSBundle mainBundle] pathForResource:@"DefaultBundles" ofType:@"tbz"])
		{
			int input, output;
			std::string error;

			path::make_dir(dest);

			pid_t pid = network::launch_tbz(dest, input, output, error);
			if(pid != -1)
			{
				int fd = open([archive fileSystemRepresentation], O_RDONLY);
				if(fd != -1)
				{
					char buf[4096];
					ssize_t len;
					while((len = read(fd, buf, sizeof(buf))) > 0)
					{
						if(write(input, buf, len) != len)
						{
							fprintf(stderr, "*** error wrting bytes to tar\n");
							break;
						}
					}
					close(fd);
				}

				if(!network::finish_tbz(pid, input, output, error))
					fprintf(stderr, "%s\n", error.c_str());
			}
			else
			{
				fprintf(stderr, "%s\n", error.c_str());
			}
		}
	}

	bundles::build_index(path::join(path::home(), "Library/Application Support/TextMate/Cache"));

	[[TMPlugInController sharedInstance] loadAllPlugIns:nil];

	BOOL disableSessionRestoreKeyDown  = ([NSEvent modifierFlags] & NSShiftKeyMask) == NSShiftKeyMask;
	BOOL disableSessionRestorePrefs    = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableSessionRestoreKey];
	if(!disableSessionRestoreKeyDown && !disableSessionRestorePrefs)
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

	[BundlesManager sharedInstance]; // trigger periodic polling of remote bundle index

	SoftwareUpdate* swUpdate = [SoftwareUpdate sharedInstance];
	[swUpdate setSignee:key_chain_t::key_t("org.textmate.duff", "Allan Odgaard", "-----BEGIN PUBLIC KEY-----\nMIIBtjCCASsGByqGSM44BAEwggEeAoGBAPIE9PpXPK3y2eBDJ0dnR/D8xR1TiT9m\n8DnPXYqkxwlqmjSShmJEmxYycnbliv2JpojYF4ikBUPJPuerlZfOvUBC99ERAgz7\nN1HYHfzFIxVo1oTKWurFJ1OOOsfg8AQDBDHnKpS1VnwVoDuvO05gK8jjQs9E5LcH\ne/opThzSrI7/AhUAy02E9H7EOwRyRNLofdtPxpa10o0CgYBKDfcBscidAoH4pkHR\nIOEGTCYl3G2Pd1yrblCp0nCCUEBCnvmrWVSXUTVa2/AyOZUTN9uZSC/Kq9XYgqwj\nhgzqa8h/a8yD+ao4q8WovwGeb6Iso3WlPl8waz6EAPR/nlUTnJ4jzr9t6iSH9owS\nvAmWrgeboia0CI2AH++liCDvigOBhAACgYAFWO66xFvmF2tVIB+4E7CwhrSi2uIk\ndeBrpmNcZZ+AVFy1RXJelNe/cZ1aXBYskn/57xigklpkfHR6DGqpEbm6KC/47Jfy\ny5GEx+F/eBWEePi90XnLinytjmXRmS2FNqX6D15XNG1xJfjociA8bzC7s4gfeTUd\nlpQkBq2z71yitA==\n-----END PUBLIC KEY-----\n")];
	[swUpdate setChannels:[NSDictionary dictionaryWithObjectsAndKeys:
		[NSURL URLWithString:REST_API @"/releases/release"],  kSoftwareUpdateChannelRelease,
		[NSURL URLWithString:REST_API @"/releases/beta"],     kSoftwareUpdateChannelBeta,
		[NSURL URLWithString:REST_API @"/releases/nightly"],  kSoftwareUpdateChannelNightly,
		nil]];

	[self userDefaultsDidChange:nil]; // setup mate/rmate server
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];

	bundlesMenu.delegate  = self;
	themesMenu.delegate   = self;
	spellingMenu.delegate = self;

	[TerminalPreferences updateMateIfRequired];
	[AboutWindowController showChangesIfUpdated];

	[[CrashReporter sharedInstance] applicationDidFinishLaunching:aNotification];
	[[CrashReporter sharedInstance] postNewCrashReportsToURLString:REST_API @"/crashes"];

	self.didFinishLaunching = YES;
}

- (void)applicationWillResignActive:(NSNotification*)aNotification
{
	scm::ng::disable();
}

- (void)applicationWillBecomeActive:(NSNotification*)aNotification
{
	scm::ng::enable();
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

	int mode = [sender respondsToSelector:@selector(tag)] ? [sender tag] : find_tags::in_document;
	if(mode == find_tags::in_folder)
		return [find showFolderSelectionPanel:self];

	switch(mode)
	{
		case find_tags::in_document:
			find.searchScope = find::in::document;
			break;
		case find_tags::in_selection:
			find.searchScope = find::in::selection;
			break;
		case find_tags::in_project:
			find.searchFolder = find.projectFolder;
			find.searchScope  = find::in::folder;
			break;
	}
	[find showFindPanel:self];
}

- (IBAction)orderFrontGoToLinePanel:(id)sender;
{
	D(DBF_AppController, bug("\n"););
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
	return enabled;
}

- (void)editBundleItem:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(selectedItems)]);
	ASSERT([[sender selectedItems] count] == 1);

	self.filterWindowController = nil;

	if(NSString* uuid = [[[sender selectedItems] lastObject] objectForKey:@"uuid"])
		[[BundleEditor sharedInstance] revealBundleItem:bundles::lookup(to_s(uuid))];
}
@end
