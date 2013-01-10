#import "AppController.h"
#import "Favorites.h"
#import "AboutWindowController.h"
#import "InstallBundleItems.h"
#import "TMPlugInController.h"
#import <oak/oak.h>
#import <oak/debug.h>
#import <Find/Find.h>
#import <io/path.h>
#import <OakFoundation/NSString Additions.h>
#import <BundleEditor/BundleEditor.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakFilterList/OakFilterList.h>
#import <OakFilterList/BundleItemChooser.h>
#import <OakTextView/OakDocumentView.h>
#import <Preferences/Preferences.h>
#import <text/types.h>
#import <document/collection.h>
#import <ns/ns.h>

OAK_DEBUG_VAR(AppController);

void OakOpenDocuments (NSArray* paths)
{
	std::vector<document::document_ptr> documents;
	NSMutableArray* itemsToInstall = [NSMutableArray array];
	NSMutableArray* plugInsToInstall = [NSMutableArray array];
	BOOL enableInstallHandler = ([NSEvent modifierFlags] & NSAlternateKeyMask) == 0;
	for(NSString* path in paths)
	{
		static std::set<std::string> const tmItemExtensions = { "tmbundle", "tmcommand", "tmdragcommand", "tmlanguage", "tmmacro", "tmpreferences", "tmsnippet", "tmtheme" };
		std::string const pathExt = to_s([[path pathExtension] lowercaseString]);
		if(enableInstallHandler && tmItemExtensions.find(pathExt) != tmItemExtensions.end())
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

@interface AppController ()
@property (nonatomic, retain) OakFilterWindowController* filterWindowController;
@property (nonatomic, retain) AboutWindowController* aboutWindowController;
@end

@implementation AppController
- (void)setup
{
	bundlesMenu.delegate  = self;
	themesMenu.delegate   = self;
	spellingMenu.delegate = self;

	[NSApp setDelegate:self];

	if([AboutWindowController shouldShowChangesWindow])
	{
		self.aboutWindowController = [[AboutWindowController alloc] init];
		[self.aboutWindowController performSelector:@selector(showChangesWindow:) withObject:self afterDelay:0];
	}
}

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
	if(!self.aboutWindowController)
		self.aboutWindowController = [[AboutWindowController alloc] init];
	[self.aboutWindowController showAboutWindow:self];
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
