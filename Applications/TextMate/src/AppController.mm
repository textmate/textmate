#import "AppController.h"
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
#import <OakFilterList/BundleItemChooser.h>
#import <OakFoundation/NSString Additions.h>
#import <OakTextView/OakDocumentView.h>
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

#if !defined(MAC_OS_X_VERSION_10_12) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_12)
@interface NSWindow (Sierra)
+ (void)setAllowsAutomaticWindowTabbing:(BOOL)flag;
@end
#endif

OAK_DEBUG_VAR(AppController);

void OakOpenDocuments (NSArray* paths, BOOL treatFilePackageAsFolder)
{
	NSArray* const bundleExtensions = @[ @"tmbundle", @"tmcommand", @"tmdragcommand", @"tmlanguage", @"tmmacro", @"tmpreferences", @"tmsnippet", @"tmtheme" ];

	NSMutableArray<OakDocument*>* documents = [NSMutableArray array];
	NSMutableArray* itemsToInstall = [NSMutableArray array];
	NSMutableArray* plugInsToInstall = [NSMutableArray array];
	BOOL enableInstallHandler = treatFilePackageAsFolder == NO && ([NSEvent modifierFlags] & NSAlternateKeyMask) == 0;
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

- (void)applicationWillFinishLaunching:(NSNotification*)aNotification
{
	D(DBF_AppController, bug("\n"););
	SoftwareUpdate* swUpdate = [SoftwareUpdate sharedInstance];
	NSString* parms = [NSString stringWithFormat:@"v=%@&os=%zu.%zu.%zu", [[[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleShortVersionString"] stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding], oak::os_major(), oak::os_minor(), oak::os_patch()];
	[swUpdate setSignee:key_chain_t::key_t("org.textmate.duff", "Allan Odgaard", "-----BEGIN PUBLIC KEY-----\nMIIBtjCCASsGByqGSM44BAEwggEeAoGBAPIE9PpXPK3y2eBDJ0dnR/D8xR1TiT9m\n8DnPXYqkxwlqmjSShmJEmxYycnbliv2JpojYF4ikBUPJPuerlZfOvUBC99ERAgz7\nN1HYHfzFIxVo1oTKWurFJ1OOOsfg8AQDBDHnKpS1VnwVoDuvO05gK8jjQs9E5LcH\ne/opThzSrI7/AhUAy02E9H7EOwRyRNLofdtPxpa10o0CgYBKDfcBscidAoH4pkHR\nIOEGTCYl3G2Pd1yrblCp0nCCUEBCnvmrWVSXUTVa2/AyOZUTN9uZSC/Kq9XYgqwj\nhgzqa8h/a8yD+ao4q8WovwGeb6Iso3WlPl8waz6EAPR/nlUTnJ4jzr9t6iSH9owS\nvAmWrgeboia0CI2AH++liCDvigOBhAACgYAFWO66xFvmF2tVIB+4E7CwhrSi2uIk\ndeBrpmNcZZ+AVFy1RXJelNe/cZ1aXBYskn/57xigklpkfHR6DGqpEbm6KC/47Jfy\ny5GEx+F/eBWEePi90XnLinytjmXRmS2FNqX6D15XNG1xJfjociA8bzC7s4gfeTUd\nlpQkBq2z71yitA==\n-----END PUBLIC KEY-----\n")];
	[swUpdate setChannels:@{
		kSoftwareUpdateChannelRelease : [NSURL URLWithString:[NSString stringWithFormat:@"%s/releases/release?%@", REST_API, parms]],
		kSoftwareUpdateChannelBeta    : [NSURL URLWithString:[NSString stringWithFormat:@"%s/releases/beta?%@", REST_API, parms]],
		kSoftwareUpdateChannelNightly : [NSURL URLWithString:[NSString stringWithFormat:@"%s/releases/nightly?%@", REST_API, parms]],
	}];

	settings_t::set_default_settings_path([[[NSBundle mainBundle] pathForResource:@"Default" ofType:@"tmProperties"] fileSystemRepresentation]);
	settings_t::set_global_settings_path(path::join(path::home(), "Library/Application Support/TextMate/Global.tmProperties"));

	// LEGACY location used prior to 2.0-alpha.9513
	std::string const src = path::join(path::home(), "Library/Application Support/TextMate/project-state.db");
	std::string const dst = path::join(path::home(), "Library/Application Support/TextMate/RecentProjects.db");
	if(path::exists(src) && !path::exists(dst))
		rename(src.c_str(), dst.c_str());

	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		@"NSRecentDocumentsLimit"   : @25,
		@"WebKitDeveloperExtras"    : @YES,
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
		else if([NSEvent modifierFlags] & NSShiftKeyMask)
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

	if([NSWindow respondsToSelector:@selector(setAllowsAutomaticWindowTabbing:)]) // MAC_OS_X_VERSION_10_12
		NSWindow.allowsAutomaticWindowTabbing = NO;

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

	bundlesMenu.delegate    = self;
	themesMenu.delegate     = self;
	spellingMenu.delegate   = self;
	wrapColumnMenu.delegate = self;

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
		[NSApp sendAction:@selector(performBundleItemWithUUIDStringFrom:) to:nil from:@{ @"representedObject" : [item valueForKey:@"uuid"] }];
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
