#import "BundlesPreferences.h"
#import <BundlesManager/BundlesManager.h>
#import <OakFoundation/NSDate Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <regexp/format_string.h>
#import <text/ctype.h>
#import <text/decode.h>

static std::string textify (std::string str)
{
	str = format_string::replace(str, "\\A\\s+|<[^>]*>|\\s+\\z", "");
	str = format_string::replace(str, "\\s+", " ");
	str = decode::entities(str);
	return str;
}

@implementation BundlesPreferences
- (NSString*)identifier            { return @"Bundles"; }
- (NSImage*)toolbarItemImage       { return [[NSWorkspace sharedWorkspace] iconForFileType:@"tmbundle"]; }
- (NSString*)toolbarItemLabel      { return @"Bundles"; }
- (NSView*)initialKeyView          { return bundlesTableView; }

- (void)bundlesDidChange:(id)sender
{
	std::set<std::string, text::less_t> set;
	for(size_t i = 0; i < [bundlesManager numberOfBundles]; ++i)
	{
		bundles_db::bundle_ptr bundle = [bundlesManager bundleAtIndex:i];
		if(bundle->category() != NULL_STR)
				set.insert(bundle->category());
		else	NSLog(@"%s No category for bundle: %s", SELNAME(_cmd), bundle->name().c_str());;
	}

	if(categories != std::vector<std::string>(set.begin(), set.end()))
	{
		categories = std::vector<std::string>(set.begin(), set.end());
		[categoriesScopeBar reloadData];
	}

	bundles.clear();
	for(size_t i = 0; i < [bundlesManager numberOfBundles]; ++i)
	{
		bundles_db::bundle_ptr bundle = [bundlesManager bundleAtIndex:i];
		if(enabledCategories.empty() || enabledCategories.find(bundle->category()) != enabledCategories.end())
			bundles.push_back(bundle);
	}
	[bundlesTableView reloadData];
}

- (id)init
{
	if(self = [super initWithNibName:@"BundlesPreferences" bundle:[NSBundle bundleForClass:[self class]]])
	{
		bundlesManager = [BundlesManager sharedInstance];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(bundlesDidChange:) name:BundlesManagerBundlesDidChangeNotification object:bundlesManager];
		[self bundlesDidChange:self];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[super dealloc];
}

// =======================
// = MGScopeBar Delegate =
// =======================

- (int)numberOfGroupsInScopeBar:(MGScopeBar*)theScopeBar
{
	return 1;
}

- (NSArray*)scopeBar:(MGScopeBar*)theScopeBar itemIdentifiersForGroup:(int)groupNumber
{
	NSMutableArray* res = [NSMutableArray array];
	if(groupNumber == 0)
	{
		iterate(category, categories)
			[res addObject:[NSString stringWithCxxString:*category]];
	}
	return res;
}

- (NSString*)scopeBar:(MGScopeBar*)theScopeBar labelForGroup:(int)groupNumber
{
	return nil;
}

- (MGScopeBarGroupSelectionMode)scopeBar:(MGScopeBar*)theScopeBar selectionModeForGroup:(int)groupNumber
{
	return MGMultipleSelectionMode;
}

- (NSString*)scopeBar:(MGScopeBar*)theScopeBar titleOfItem:(NSString*)identifier inGroup:(int)groupNumber
{
	return identifier;
}

- (void)scopeBar:(MGScopeBar*)theScopeBar selectedStateChanged:(BOOL)selected forItem:(NSString*)identifier inGroup:(int)groupNumber
{
	if(selected)
			enabledCategories.insert(to_s(identifier));
	else	enabledCategories.erase(to_s(identifier));
	[self bundlesDidChange:self];
}

// ========================
// = NSTableView Delegate =
// ========================

- (BOOL)tableView:(NSTableView*)aTableView shouldEditTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if([[aTableColumn identifier] isEqualToString:@"installed"])
	{
		if([bundlesManager installStateForBundle:bundles[rowIndex]] != NSMixedState)
			return YES;
	}
	return NO;
}

- (BOOL)tableView:(NSTableView*)aTableView shouldSelectRow:(int)anInt
{
	return [aTableView clickedColumn] != 0;
}

// ==========================
// = NSTableView DataSource =
// ==========================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return bundles.size();
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	bundles_db::bundle_ptr bundle = bundles[rowIndex];
	if([[aTableColumn identifier] isEqualToString:@"installed"])
	{
		return @([bundlesManager installStateForBundle:bundle]);
	}
	else if([[aTableColumn identifier] isEqualToString:@"name"])
	{
		return [NSString stringWithCxxString:bundle->name()];
	}
	else if([[aTableColumn identifier] isEqualToString:@"date"])
	{
		oak::date_t updated = bundle->installed() ? bundle->path_updated() : bundle->url_updated();
		NSDate* date = [(id)CFDateCreate(kCFAllocatorDefault, updated.value()) autorelease];
		return [date humanReadableTimeElapsed];
	}
	else if([[aTableColumn identifier] isEqualToString:@"description"])
	{
		return [NSString stringWithCxxString:textify(bundle->description())];
	}
	return nil;
}

- (void)tableView:(NSTableView*)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if([[aTableColumn identifier] isEqualToString:@"installed"])
	{
		bundles_db::bundle_ptr bundle = bundles[rowIndex];
		if([anObject boolValue])
				[bundlesManager installBundle:bundle];
		else	[bundlesManager uninstallBundle:bundle];
	}
}
@end
