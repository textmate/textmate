#import "BundlesPreferences.h"
#import <BundlesManager/BundlesManager.h>
#import <OakFoundation/NSDate Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <MGScopeBar/MGScopeBar.h>
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

@interface BundlesPreferences ()
@property (nonatomic, retain) BundlesManager* bundlesManager;
@end

@implementation BundlesPreferences
- (NSString*)identifier            { return @"Bundles"; }
- (NSImage*)toolbarItemImage       { return [[NSWorkspace sharedWorkspace] iconForFileType:@"tmbundle"]; }
- (NSString*)toolbarItemLabel      { return @"Bundles"; }
- (NSView*)initialKeyView          { return bundlesTableView; }

- (void)bundlesDidChange:(id)sender
{
	std::set<std::string, text::less_t> set;
	for(size_t i = 0; i < [_bundlesManager numberOfBundles]; ++i)
	{
		bundles_db::bundle_ptr bundle = [_bundlesManager bundleAtIndex:i];
		if(bundle->category() != NULL_STR)
				set.insert(bundle->category());
		else	NSLog(@"%s No category for bundle: %s", sel_getName(_cmd), bundle->name().c_str());;
	}

	if(categories != std::vector<std::string>(set.begin(), set.end()))
	{
		categories = std::vector<std::string>(set.begin(), set.end());
		[categoriesScopeBar reloadData];
	}

	bundles.clear();
	for(size_t i = 0; i < [_bundlesManager numberOfBundles]; ++i)
	{
		bundles_db::bundle_ptr bundle = [_bundlesManager bundleAtIndex:i];
		if(enabledCategories.empty() || enabledCategories.find(bundle->category()) != enabledCategories.end())
			bundles.push_back(bundle);
	}
	for(NSTableColumn* tableColumn in [bundlesTableView tableColumns])
		[bundlesTableView setIndicatorImage:nil inTableColumn:tableColumn];
	[bundlesTableView setIndicatorImage:[NSImage imageNamed:@"NSAscendingSortIndicator"] inTableColumn:[bundlesTableView tableColumnWithIdentifier:@"name"]];
	[bundlesTableView reloadData];
}

- (id)init
{
	if(self = [super initWithNibName:@"BundlesPreferences" bundle:[NSBundle bundleForClass:[self class]]])
	{
		self.bundlesManager = [BundlesManager sharedInstance];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(bundlesDidChange:) name:BundlesManagerBundlesDidChangeNotification object:_bundlesManager];
		[self bundlesDidChange:self];
	}
	return self;
}

- (void)awakeFromNib
{
	[bundlesTableView setIndicatorImage:[NSImage imageNamed:@"NSAscendingSortIndicator"] inTableColumn:[bundlesTableView tableColumnWithIdentifier:@"name"]];
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
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

- (void)tableView:(NSTableView*)aTableView didClickTableColumn:(NSTableColumn*)aTableColumn
{
	text::less_t lessThan;

	if([[aTableColumn identifier] isEqualToString:@"installed"])
		std::stable_sort(bundles.begin(), bundles.end(), [](bundles_db::bundle_ptr lhs, bundles_db::bundle_ptr rhs){ return lhs.get()->installed() && !rhs.get()->installed(); });
	else if([[aTableColumn identifier] isEqualToString:@"name"])
		std::sort(bundles.begin(), bundles.end(), [&lessThan](bundles_db::bundle_ptr lhs, bundles_db::bundle_ptr rhs){ return lessThan(lhs.get()->name(), rhs.get()->name()); });
	else if([[aTableColumn identifier] isEqualToString:@"date"])
		std::sort(bundles.begin(), bundles.end(), [](bundles_db::bundle_ptr lhs, bundles_db::bundle_ptr rhs){ return (rhs.get()->installed() ? rhs.get()->path_updated() : rhs.get()->url_updated()) < (lhs.get()->installed() ? lhs.get()->path_updated() : lhs.get()->url_updated()); });
	else if([[aTableColumn identifier] isEqualToString:@"description"])
		std::sort(bundles.begin(), bundles.end(), [&lessThan](bundles_db::bundle_ptr lhs, bundles_db::bundle_ptr rhs){ return lessThan(textify(lhs.get()->description()), textify(rhs.get()->description())); });
	else
		return;

	BOOL sortDescending = [aTableView indicatorImageInTableColumn:aTableColumn] == [NSImage imageNamed:@"NSAscendingSortIndicator"];
	if(sortDescending)
		std::reverse(bundles.begin(), bundles.end());

	for(NSTableColumn* tableColumn in [aTableView tableColumns])
		[aTableView setIndicatorImage:nil inTableColumn:tableColumn];
	[aTableView setIndicatorImage:[NSImage imageNamed:(sortDescending ? @"NSDescendingSortIndicator" : @"NSAscendingSortIndicator")] inTableColumn:aTableColumn];

	[aTableView reloadData];
}

- (BOOL)tableView:(NSTableView*)aTableView shouldEditTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if([[aTableColumn identifier] isEqualToString:@"installed"])
	{
		if([_bundlesManager installStateForBundle:bundles[rowIndex]] != NSMixedState)
			return YES;
	}
	return NO;
}

- (BOOL)tableView:(NSTableView*)aTableView shouldSelectRow:(NSInteger)rowIndex
{
	NSInteger clickedColumn = [aTableView clickedColumn];
	return clickedColumn != [aTableView columnWithIdentifier:@"installed"] && clickedColumn != [aTableView columnWithIdentifier:@"link"];
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
		return @([_bundlesManager installStateForBundle:bundle]);
	}
	else if([[aTableColumn identifier] isEqualToString:@"name"])
	{
		return [NSString stringWithCxxString:bundle->name()];
	}
	else if([[aTableColumn identifier] isEqualToString:@"date"])
	{
		oak::date_t updated = bundle->installed() ? bundle->path_updated() : bundle->url_updated();
		return [[NSDate dateWithTimeIntervalSinceReferenceDate:updated.value()] humanReadableTimeElapsed];
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
				[_bundlesManager installBundle:bundle completionHandler:nil];
		else	[_bundlesManager uninstallBundle:bundle];
	}
}

- (IBAction)didClickBundleLink:(NSTableView*)aTableView
{
	NSInteger rowIndex = [aTableView clickedRow];
	bundles_db::bundle_ptr bundle = bundles[rowIndex];
	if(bundle->html_url() != NULL_STR)
		[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:[NSString stringWithCxxString:bundle->html_url()]]];
}
@end
