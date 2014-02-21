#import "FSOutlineViewDelegate.h"
#import "OakFSUtilities.h"
#import "io/FSDataSource.h"
#import "io/FSItem.h"
#import "ui/OFBPathInfoCell.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <ns/ns.h>
#import <io/path.h>
#import <text/utf8.h>
#import <oak/oak.h>

@interface NSCell (FSItemCell)
- (void)setImage:(NSImage*)anImage;
- (void)setLabelIndex:(NSInteger)anInteger;
@end

static NSArray* ConvertURLSetToStringArray (NSSet* aSet)
{
	NSMutableArray* res = [NSMutableArray array];
	for(NSURL* url in aSet)
		[res addObject:[url absoluteString]];
	[res sortUsingSelector:@selector(compare:)];
	return res;
}

static NSMutableSet* ConvertURLArrayToStringSet (NSArray* anArray)
{
	NSMutableSet* res = [NSMutableSet set];
	for(NSString* urlString in anArray)
		[res addObject:[NSURL URLWithString:urlString]];
	return res;
}

// ================================
// = OutlineView Helper Functions =
// ================================

static NSSet* VisibleURLs (NSOutlineView* outlineView, FSItem* root, NSMutableSet* res = [NSMutableSet set])
{
	for(FSItem* item in root.children)
	{
		[res addObject:item.url];
		if(!item.leaf && [outlineView isItemExpanded:item])
			VisibleURLs(outlineView, item, res);
	}
	return res;
}

static NSSet* ExpandedURLs (NSOutlineView* outlineView, FSItem* root, NSMutableSet* res = [NSMutableSet set])
{
	for(FSItem* item in root.children)
	{
		if(!item.leaf && [outlineView isItemExpanded:item])
		{
			[res addObject:item.url];
			ExpandedURLs(outlineView, item, res);
		}
	}
	return res;
}

static NSSet* SelectedURLs (NSOutlineView* outlineView, FSItem* root)
{
	NSMutableSet* selectedURLs = [NSMutableSet set];
	NSIndexSet* indexSet = [outlineView selectedRowIndexes];
	for(NSUInteger index = [indexSet firstIndex]; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
		[selectedURLs addObject:[[outlineView itemAtRow:index] url]];
	[selectedURLs intersectSet:VisibleURLs(outlineView, root)];

	return selectedURLs;
}

static void Snapshot (NSOutlineView* outlineView, FSItem* item, NSMutableSet* expandedURLs, NSMutableSet* selectedURLs)
{
	[expandedURLs unionSet:ExpandedURLs(outlineView, item)];
	[selectedURLs minusSet:VisibleURLs(outlineView, item)];
	[selectedURLs unionSet:SelectedURLs(outlineView, item)];
}

static NSSet* VisibleItems (NSOutlineView* outlineView, FSItem* root, NSMutableSet* res = [NSMutableSet set])
{
	for(FSItem* item in root.children)
	{
		[res addObject:item];
		if(!item.leaf && [outlineView isItemExpanded:item])
			VisibleItems(outlineView, item, res);
	}
	return res;
}

// ================================

@interface FSOutlineViewDelegate () <NSOutlineViewDelegate>
{
	IBOutlet NSOutlineView* outlineView;
	IBOutlet FSDataSource* dataSource;
	NSArray* openURLs;
	NSArray* modifiedURLs;

	NSMutableSet* expandedURLs;
	NSMutableSet* selectedURLs;

	NSInteger itemsReloading;
	NSInteger suppressCollapsing;
	BOOL suppressAutoExpansion;

	NSMutableSet* recursiveExpandPaths;
	NSSet* pendingSelectURLs;
	NSURL* pendingEditURL;
	NSURL* pendingMakeVisibleURL;
	CGFloat pendingScrollOffset;
}
- (void)applicationWillTerminate:(NSNotification*)aNotification;
@property (nonatomic, retain) NSSet* pendingSelectURLs;
@property (nonatomic, retain) NSURL* pendingEditURL;
@property (nonatomic, retain) NSURL* pendingMakeVisibleURL;
@property (nonatomic, retain) NSMutableSet* pendingExpandURLs;
@property (nonatomic, assign) CGFloat pendingScrollOffset;
@end

@implementation FSOutlineViewDelegate
@synthesize outlineView, dataSource, openURLs, modifiedURLs, pendingSelectURLs, pendingEditURL, pendingMakeVisibleURL, pendingExpandURLs, pendingScrollOffset;

- (id)init
{
	if((self = [super init]))
	{
		recursiveExpandPaths = [NSMutableSet new];

		expandedURLs = ConvertURLArrayToStringSet([[NSUserDefaults standardUserDefaults] arrayForKey:@"ExpandedURLs"]);
		selectedURLs = [NSMutableSet new];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	}
	return self;
}

- (void)dealloc
{
	[self applicationWillTerminate:nil];
	outlineView.dataSource = nil;
	outlineView.delegate   = nil;
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	static BOOL mergeWithUserDefaults = NO;
	[expandedURLs intersectSet:VisibleURLs(outlineView, dataSource.rootItem)];
	if(mergeWithUserDefaults)
		[expandedURLs unionSet:ConvertURLArrayToStringSet([[NSUserDefaults standardUserDefaults] arrayForKey:@"ExpandedURLs"])];

	Snapshot(outlineView, dataSource.rootItem, expandedURLs, selectedURLs);
	[[NSUserDefaults standardUserDefaults] setObject:ConvertURLSetToStringArray(expandedURLs) forKey:@"ExpandedURLs"];
	mergeWithUserDefaults = YES;
}

- (void)setOutlineView:(NSOutlineView*)anOutlineView
{
	if(outlineView != anOutlineView)
	{
		[outlineView setDelegate:nil];
		outlineView = anOutlineView;
		[outlineView setDelegate:self];
	}
}

- (void)expandAndSelectChildren:(FSItem*)anItem expandAll:(BOOL)flag
{
	for(FSItem* child in anItem.children)
	{
		if(flag && child.link)
			continue;

		if(!child.leaf && (flag || [expandedURLs containsObject:child.url]))
		{
			if(flag)
				[recursiveExpandPaths addObject:child.url];

			if(![outlineView isItemExpanded:child])
				[outlineView expandItem:child];
		}

		if([selectedURLs containsObject:child.url])
			[outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:[outlineView rowForItem:child]] byExtendingSelection:YES];

		if(!child.leaf && [outlineView isItemExpanded:child])
			[self expandAndSelectChildren:child expandAll:flag];
	}
}

- (void)setDataSource:(FSDataSource*)aDataSource
{
	self.pendingSelectURLs = nil;
	self.pendingEditURL = nil;
	self.pendingMakeVisibleURL = nil;
	self.pendingExpandURLs = nil;
	self.pendingScrollOffset = 0;

	if(dataSource)
	{
		Snapshot(outlineView, dataSource.rootItem, expandedURLs, selectedURLs);

		[outlineView deselectAll:self];
		[outlineView setDataSource:nil];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:FSItemDidReloadNotification object:dataSource];
	}

	itemsReloading = 0;

	if(dataSource = aDataSource)
	{
		if(NSArray* expandedByDefault = [dataSource expandedURLs])
			[expandedURLs addObjectsFromArray:expandedByDefault];
		[outlineView setDataSource:dataSource];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(itemDidReload:) name:FSItemDidReloadNotification object:dataSource];
		[self expandAndSelectChildren:dataSource.rootItem expandAll:NO];
	}
}

- (void)checkPendingSelectAndEditURLs
{
	if(pendingScrollOffset != 0 && pendingScrollOffset <= NSHeight([outlineView frame]) - NSHeight([outlineView visibleRect]))
	{
		[outlineView scrollPoint:NSMakePoint(0, pendingScrollOffset)];
		self.pendingScrollOffset = 0;
	}

	NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
	for(NSInteger i = 0; i < [outlineView numberOfRows] && pendingSelectURLs; ++i)
	{
		if([pendingSelectURLs containsObject:[[outlineView itemAtRow:i] url]])
			[indexSet addIndex:i];
	}

	if([indexSet count])
	{
		[outlineView selectRowIndexes:indexSet byExtendingSelection:NO];
		self.pendingSelectURLs = nil;
	}

	if(itemsReloading)
		return;

	for(NSInteger i = 0; i < [outlineView numberOfRows] && pendingEditURL; ++i)
	{
		if(![pendingEditURL isEqual:[[outlineView itemAtRow:i] url]])
			continue;

		[outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:i] byExtendingSelection:NO];
		[outlineView editColumn:0 row:i withEvent:nil select:YES];
		self.pendingEditURL = nil;
	}

	for(NSInteger i = 0; i < [outlineView numberOfRows] && pendingMakeVisibleURL; ++i)
	{
		if(![pendingMakeVisibleURL isEqual:[[outlineView itemAtRow:i] url]])
			continue;

		NSRect rowRect     = [outlineView rectOfRow:i];
		NSRect visibleRect = [outlineView visibleRect];
		if(NSMinY(rowRect) < NSMinY(visibleRect) || NSMaxY(rowRect) > NSMaxY(visibleRect))
			[outlineView scrollPoint:NSMakePoint(NSMinX(rowRect), round(NSMidY(rowRect) - NSHeight(visibleRect)/2))];

		self.pendingMakeVisibleURL = nil;
	}
	
	for(NSInteger i = 0; i < [outlineView numberOfRows] && [pendingExpandURLs count]; ++i)
	{
		id item = [outlineView itemAtRow:i];
		
		if(![pendingExpandURLs containsObject:[item url]])
			continue;
		
		[outlineView expandItem:item];
		
		[pendingExpandURLs removeObject:[item url]];
	}
	if([pendingExpandURLs count] == 0)
		self.pendingExpandURLs = nil;
}

- (void)selectURLs:(NSArray*)someURLs expandChildren:(BOOL)expandAncestors
{
	selectedURLs = [NSMutableSet new];
	[outlineView deselectAll:self];

	self.pendingSelectURLs = [NSSet setWithArray:someURLs];
	if([someURLs count] == 1)
		self.pendingMakeVisibleURL = [someURLs lastObject];
	
	if(expandAncestors)
	{
		[expandedURLs removeAllObjects];

		NSMutableSet* ancestors = [NSMutableSet set];
		NSURL* rootURL = dataSource.rootItem.url;
		
		for(NSURL* targetURL in someURLs)
		{
			NSMutableSet* currentAncestors = [NSMutableSet set];
			NSURL* currentURL;
			
			for(currentURL = ParentForURL(targetURL); currentURL; currentURL = ParentForURL(currentURL))
			{
				if([currentURL isEqual:rootURL])
					break;
				
				[currentAncestors addObject:currentURL];
			}
			
			if(currentURL)
				[ancestors unionSet:currentAncestors];
		}
		
		self.pendingExpandURLs = ancestors;
	}
	
	[self checkPendingSelectAndEditURLs];
}

- (void)editURL:(NSURL*)anURL
{
	self.pendingEditURL = anURL;
	[self checkPendingSelectAndEditURLs];
}

- (void)scrollToOffset:(CGFloat)anOffset
{
	self.pendingScrollOffset = anOffset;
	[self checkPendingSelectAndEditURLs];
}

- (void)setFieldEditorString:(NSString*)aString selectedRanges:(NSArray*)someRanges
{
	if(aString && [outlineView editedRow] != -1 && [[[outlineView window] firstResponder] isKindOfClass:[NSTextView class]])
	{
		NSTextView* textView = (NSTextView*)[[outlineView window] firstResponder];
		if(![[[textView textStorage] string] isEqualToString:aString])
		{
			// We go via NSResponder for undo support
			[textView selectAll:self];
			[textView insertText:aString];
		}
		[textView setSelectedRanges:someRanges];
	}
}

- (void)itemDidReload:(NSNotification*)aNotification
{
	FSDataSource* aDataSource = [aNotification object];
	if(aDataSource != dataSource)
		return;

	FSItem* item      = [[aNotification userInfo] objectForKey:@"item"];
	NSArray* children = [[aNotification userInfo] objectForKey:@"children"];
	BOOL requested    = [[[aNotification userInfo] objectForKey:@"requested"] boolValue];
	BOOL recursive    = [[[aNotification userInfo] objectForKey:@"recursive"] boolValue];

	if(requested)
		--itemsReloading;

	NSString* editedValue = nil;
	NSArray* selectedRanges = nil;
	if([outlineView editedRow] != -1)
	{
		if([[[outlineView window] firstResponder] isKindOfClass:[NSTextView class]])
		{
			NSTextView* textView = (NSTextView*)[[outlineView window] firstResponder];
			editedValue = [[[textView textStorage] string] copy];
			selectedRanges = [[textView selectedRanges] copy];
		}
		self.pendingEditURL = [[outlineView itemAtRow:[outlineView editedRow]] url];
		[outlineView cancelOperation:self];
	}

	if(recursive)
	{
		Snapshot(outlineView, item, expandedURLs, selectedURLs);
		for(FSItem* child in VisibleItems(outlineView, item))
			[outlineView deselectRow:[outlineView rowForItem:child]];
		item.children = children;
	}

	suppressAutoExpansion = YES;
	[outlineView reloadItem:(item == dataSource.rootItem ? nil : item) reloadChildren:recursive];
	suppressAutoExpansion = NO;

	if(!recursive)
	{
		[self checkPendingSelectAndEditURLs];
		[self setFieldEditorString:editedValue selectedRanges:selectedRanges];
		return;
	}

	BOOL recursiveExpand = [recursiveExpandPaths containsObject:item.url];
	[recursiveExpandPaths removeObject:item.url];

	[self expandAndSelectChildren:item expandAll:recursiveExpand];
	[self checkPendingSelectAndEditURLs];
	[self setFieldEditorString:editedValue selectedRanges:selectedRanges];
}

// =================================
// = Outline view delegate methods =
// =================================

- (void)outlineView:(NSOutlineView*)anOutlineView willDisplayCell:(NSCell*)cell forTableColumn:(NSTableColumn*)tableColumn item:(FSItem*)item
{
	if([cell respondsToSelector:@selector(setImage:)])
	{
		if([item.icon respondsToSelector:@selector(setModified:)])
			item.icon.modified = [modifiedURLs containsObject:item.url];
		[cell setImage:item.icon];
	}
	cell.stringValue       = item.name;
	// cell.textColor         = lstat([[item.url path] fileSystemRepresentation], &(struct stat){ 0 }) == 0 ? [NSColor textColor] : [NSColor redColor];
	// cell.target            = delegate;
	cell.representedObject = item;
	if([cell respondsToSelector:@selector(setLabelIndex:)])
		[cell setLabelIndex:item.labelIndex];
	if([cell respondsToSelector:@selector(setIsOpen:)])
		((OFBPathInfoCell*)cell).isOpen = [openURLs containsObject:item.url];
	// cell.isLoading         = item.isLoading;
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView shouldSelectItem:(id)item
{
	if([self outlineView:anOutlineView isGroupItem:item])
		return NO;

	NSInteger col = [anOutlineView clickedColumn];
	NSInteger row = [anOutlineView clickedRow];
	if(col != -1 && row != -1)
	{
		NSCell* cell = [anOutlineView preparedCellAtColumn:col row:row];
		NSUInteger hit = [cell hitTestForEvent:[NSApp currentEvent] inRect:[anOutlineView frameOfCellAtColumn:col row:row] ofView:anOutlineView];
		if(hit & (OFBPathInfoCellHitOpenItem | OFBPathInfoCellHitRevealItem | NSCellHitTrackableArea))
			return NO;
	}
	return YES;
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView shouldTrackCell:(NSCell*)cell forTableColumn:(NSTableColumn*)tableColumn item:(id)item
{
	return YES;
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView isGroupItem:(FSItem*)item
{
	return [item respondsToSelector:@selector(group)] ? item.group : NO;
}

- (NSString*)outlineView:(NSOutlineView*)outlineView toolTipForCell:(NSCell*)cell rect:(NSRectPointer)rect tableColumn:(NSTableColumn*)tc item:(FSItem*)item mouseLocation:(NSPoint)mouseLocation
{
	return [item respondsToSelector:@selector(toolTip)] ? item.toolTip : nil;
}

// ===========================
// = Expand Delegate Methods =
// ===========================

static BOOL MyEvent (NSEvent* anEvent, NSView* aView)
{
	if([anEvent window] == [aView window])
	{
		static std::set<std::string> const ArrowLeftRight = { "~" + utf8::to_s(NSLeftArrowFunctionKey), "~" + utf8::to_s(NSRightArrowFunctionKey) };
		if([anEvent type] == NSLeftMouseUp)
			return NSMouseInRect([aView convertPoint:[anEvent locationInWindow] fromView:nil], [aView frame], [aView isFlipped]);
		else if([anEvent type] == NSKeyDown && [[aView window] firstResponder] == aView)
			return ArrowLeftRight.find(to_s(anEvent)) != ArrowLeftRight.end();
	}
	return NO;
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView shouldExpandItem:(FSItem*)item
{
	// During drag’n’drop the system will repeatedly ask to expand expanded items
	if([anOutlineView isItemExpanded:item])
		return YES;

	if(suppressAutoExpansion && ![expandedURLs containsObject:item.url])
		return NO;

	if(![pendingExpandURLs containsObject:item.url] && MyEvent([NSApp currentEvent], anOutlineView) && OakIsAlternateKeyOrMouseEvent())
		[recursiveExpandPaths addObject:item.url];

	if([dataSource reloadItem:item])
		++itemsReloading;
	return YES;
}

- (void)outlineViewItemDidExpand:(NSNotification*)aNotification
{
	if(suppressAutoExpansion)
		return; // we issued a reload so don’t tinker with selection/expansion

	FSItem* item = [[aNotification userInfo] objectForKey:@"NSObject"];
	if([item isKindOfClass:[FSItem class]])
	   [self expandAndSelectChildren:item expandAll:NO];
}

// =============================
// = Collapse Delegate Methods =
// =============================

- (BOOL)outlineView:(NSOutlineView*)anOutlineView shouldCollapseItem:(FSItem*)item
{
	return suppressCollapsing == 0;
}

- (void)outlineViewItemWillCollapse:(NSNotification*)aNotification
{
	if(++suppressCollapsing != 1)
		return;

	FSItem* item = [[aNotification userInfo] objectForKey:@"NSObject"];
	Snapshot(outlineView, item, expandedURLs, selectedURLs);

	if(MyEvent([NSApp currentEvent], outlineView) && OakIsAlternateKeyOrMouseEvent())
		[expandedURLs minusSet:ExpandedURLs(outlineView, item)];
	[expandedURLs removeObject:item.url];
}

- (void)outlineViewItemDidCollapse:(NSNotification*)aNotification
{
	if(--suppressCollapsing == 0)
	{
		FSItem* item = [[aNotification userInfo] objectForKey:@"NSObject"];
		if([dataSource unloadItem:item])
			[outlineView reloadItem:item reloadChildren:YES];
	}
}
@end
