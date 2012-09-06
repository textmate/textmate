#import "AppController.h"
#import "Favorites.h"
#import "CreditsWindowController.h"
#import <oak/CocoaSTL.h>
#import <oak/oak.h>
#import <oak/debug.h>
#import <Find/Find.h>
#import <io/path.h>
#import <OakFoundation/OakFoundation.h>
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
	for(NSString* path in paths)
	{
		if(path::is_directory(to_s(path)))
		{
			document::show_browser(to_s(path));
		}
		else
		{
			documents.push_back(document::create(to_s(path)));
		}
	}

	document::show(documents);
}

@interface AppController ()
@property (nonatomic, retain) OakFilterWindowController* filterWindowController;
@end

@implementation AppController
@synthesize filterWindowController;

- (void)setup
{
	bundlesMenu.delegate  = self;
	themesMenu.delegate   = self;
	spellingMenu.delegate = self;

	[NSApp setDelegate:self];
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
	OakFilterWindowController* controller = [OakFilterWindowController filterWindow];
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

- (IBAction)showCredits:(id)sender
{
	D(DBF_AppController, bug("\n"););
	[CreditsWindowController showPath:[[NSBundle mainBundle] pathForResource:@"Credits" ofType:@"html"]];
}

// =======================
// = Bundle Item Chooser =
// =======================

- (void)setFilterWindowController:(OakFilterWindowController*)controller
{
	if(controller != filterWindowController)
	{
		if(filterWindowController)
		{
			[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowWillCloseNotification object:filterWindowController.window];
			filterWindowController.target = nil;
			[filterWindowController close];
			[filterWindowController release];
		}
		if(filterWindowController = [controller retain])
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(filterWindowWillClose:) name:NSWindowWillCloseNotification object:filterWindowController.window];
	}
}

- (void)filterWindowWillClose:(NSNotification*)notification
{
	bundleItemSearch.filter_string  = [[[filterWindowController dataSource] filterString] UTF8String];
	bundleItemSearch.key_equivalent = [[filterWindowController dataSource] keyEquivalentSearch];
	bundleItemSearch.all_scopes     = [[filterWindowController dataSource] searchAllScopes];
	bundleItemSearch.search_type    = [[filterWindowController dataSource] searchType];
	self.filterWindowController     = nil;
}

- (IBAction)showBundleItemChooser:(id)sender
{
	self.filterWindowController            = [OakFilterWindowController filterWindow];
	OakTextView* textView                  = [NSApp targetForAction:@selector(scope)];
	BundleItemChooser* dataSource          = [BundleItemChooser bundleItemChooserForScope:textView ? [textView scopeContext] : scope::wildcard];
	dataSource.searchType                  = search::type(bundleItemSearch.search_type);
	dataSource.keyEquivalentSearch         = bundleItemSearch.key_equivalent;
	dataSource.textViewHasSelection        = [textView hasSelection];
	dataSource.searchAllScopes             = bundleItemSearch.all_scopes;
	dataSource.filterString                = [NSString stringWithCxxString:bundleItemSearch.filter_string];
	filterWindowController.dataSource      = dataSource;
	filterWindowController.action          = @selector(bundleItemChooserDidSelectItems:);
	filterWindowController.accessoryAction = @selector(editBundleItem:);
	[filterWindowController showWindowRelativeToWindow:[textView window]];
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
