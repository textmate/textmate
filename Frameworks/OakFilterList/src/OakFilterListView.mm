#import "OakFilterList.h"
#import "OakFilterListView.h"
#import <oak/debug.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <oak/oak.h>
#import <oak/CocoaSTL.h>
#import "highlight_ranges.h"

OAK_DEBUG_VAR(FilterList_View);

NSString* const FLDataSourceItemsDidChangeNotification     = @"FLDataSourceItemsDidChangeNotification";
NSString* const FLDataSourceItemsShouldDescendNotification = @"FLDataSourceItemsShouldDescendNotification";
NSString* const FLDataSourceItemsShouldAscendNotification  = @"FLDataSourceItemsShouldAscendNotification";

@interface OakFilterWindowController (Private)
- (void)sendAction:(id)sender;
@end

@interface OakFilterListView ()
@property (nonatomic, assign) BOOL isWaitingForItems;
@property (nonatomic, retain) NSArray* items;
@property (nonatomic, retain) NSAttributedString* infoString;
@end

@implementation OakFilterListView
@synthesize filterDataSource, isWaitingForItems, items, infoString, sourceIndex;

// ==================
// = Setup/Teardown =
// ==================

- (void)setup
{
	self.dataSource              = self;
	self.delegate                = self;
	self.isWaitingForItems       = YES;
	self.columnAutoresizingStyle = NSTableViewFirstColumnOnlyAutoresizingStyle;
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(viewFrameDidChange:) name:NSViewFrameDidChangeNotification object:self];
}

- (id)initWithCoder:(NSCoder*)aCoder
{
	if(self = [super initWithCoder:aCoder])
		[self setup];
	return self;
}

- (id)init
{
	if(self = [super init])
		[self setup];
	return self;
}

- (void)dealloc
{
	self.filterDataSource = nil;
	self.items            = nil;
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[super dealloc];
}

- (void)awakeFromNib
{
	self.infoString = [[[NSAttributedString alloc] initWithString:@""] autorelease];
}

- (void)viewFrameDidChange:(NSNotification*)notification
{
	[self tableViewSelectionDidChange:nil];
}

// =============
// = Accessors =
// =============

- (void)setFilterDataSource:(id <FilterListDataSource>)aDataSource
{
	if(aDataSource != filterDataSource)
	{
		if(filterDataSource)
		{
			if([filterDataSource respondsToSelector:@selector(stopLoading)])
				[filterDataSource stopLoading];
			[[NSNotificationCenter defaultCenter] removeObserver:self name:FLDataSourceItemsDidChangeNotification object:filterDataSource];
			[[NSNotificationCenter defaultCenter] removeObserver:self name:FLDataSourceItemsShouldDescendNotification object:filterDataSource];
			[[NSNotificationCenter defaultCenter] removeObserver:self name:FLDataSourceItemsShouldAscendNotification object:filterDataSource];
			[filterDataSource autorelease];
		}
		if(filterDataSource = [aDataSource retain])
		{
			ASSERT([filterDataSource conformsToProtocol:@protocol(FilterListDataSource)]);
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(dataSourceHasMoreData:) name:FLDataSourceItemsDidChangeNotification object:filterDataSource];
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(dataSourceShouldDescend:) name:FLDataSourceItemsShouldDescendNotification object:filterDataSource];
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(dataSourceShouldAscend:) name:FLDataSourceItemsShouldAscendNotification object:filterDataSource];

			NSTextFieldCell* dataCell = nil;
			if([filterDataSource respondsToSelector:@selector(itemDataCell)])
				dataCell = [filterDataSource itemDataCell];
			if(!dataCell)
				dataCell = [[[NSTextFieldCell alloc] initTextCell:@""] autorelease];
			[[self.tableColumns objectAtIndex:0] setDataCell:dataCell];
			self.allowsMultipleSelection = [filterDataSource respondsToSelector:@selector(allowsMultipleSelection)] && [filterDataSource allowsMultipleSelection];
		}
		[self reloadData];
	}
}

- (NSButtonCell*)accessoryButton
{
	return [[self tableColumnWithIdentifier:@"accessoryColumn"] dataCell];
}

- (void)setAccessoryButton:(NSButtonCell*)button
{
	NSTableColumn* accessoryColumn = [self tableColumnWithIdentifier:@"accessoryColumn"];
	if(button)
	{
		[accessoryColumn setDataCell:button];
		accessoryColumn.minWidth = [accessoryColumn.dataCell cellSize].width;
		accessoryColumn.maxWidth = accessoryColumn.minWidth;
	}
	[accessoryColumn setHidden:button == nil];
	[self sizeToFit];
}

// =============
// = Filtering =
// =============

- (void)dataSourceShouldDescend:(NSNotification*)aNotification
{
	[filterDataSource descendIntoItem:[self.selectedItems lastObject]];
}

- (void)dataSourceShouldAscend:(NSNotification*)aNotification
{
	[filterDataSource descendIntoItem:nil];
}

- (void)dataSourceHasMoreData:(NSNotification*)notification
{
	[self reloadData];
}

- (void)reloadData
{
	NSArray* selectedItems = nil;
	if([filterDataSource respondsToSelector:@selector(preservesSelectionWhenFiltering)] && [filterDataSource preservesSelectionWhenFiltering])
		selectedItems = [self.items objectsAtIndexes:self.selectedRowIndexes];
	self.items = [filterDataSource items];
	if(![filterDataSource respondsToSelector:@selector(infoStringForItem:)])
	{
		NSMutableParagraphStyle* style = [[[NSParagraphStyle defaultParagraphStyle] mutableCopy] autorelease];
		[style setAlignment:NSCenterTextAlignment];
		NSAttributedString* text = [[[NSAttributedString alloc] initWithString:[NSString stringWithFormat:@"%lu items", self.items.count]
                                                                    attributes:[NSDictionary dictionaryWithObjectsAndKeys:[NSColor darkGrayColor], NSForegroundColorAttributeName, [NSFont controlContentFontOfSize:11], NSFontAttributeName, style, NSParagraphStyleAttributeName, nil]] autorelease];
		self.infoString = text;
	}
	[super reloadData];
	NSMutableIndexSet* indexesToSelect = [NSMutableIndexSet indexSet];
	if(self.selectedRowIndexes.count != 1 || self.selectedRowIndexes.firstIndex != 0)
	{
		for(id item in selectedItems)
		{
			NSUInteger index = [self.items indexOfObject:item];
			if(index != NSNotFound)
				[indexesToSelect addIndex:index];
		}
	}
	if(indexesToSelect.count == 0)
		[indexesToSelect addIndex:0];
	[self selectRowIndexes:indexesToSelect byExtendingSelection:NO];
	[self scrollRowToVisible:indexesToSelect.firstIndex];
	[self tableViewSelectionDidChange:nil];
	self.isWaitingForItems = [filterDataSource respondsToSelector:@selector(moreItemsToCome)] && [filterDataSource moreItemsToCome];
}

// ======================
// = NSTableDataSource  =
// ======================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)tableView
{
	return [items count];
}

- (id)tableView:(NSTableView*)tableView objectValueForTableColumn:(NSTableColumn*)tableColumn row:(NSInteger)rowIndex
{
	if([tableColumn.identifier isEqualToString:@"accessoryColumn"])
		return [items objectAtIndex:rowIndex];
	if([filterDataSource respondsToSelector:@selector(displayStringForItem:)])
	{
		NSMutableParagraphStyle* pStyle = [[[NSParagraphStyle defaultParagraphStyle] mutableCopy] autorelease];
		[pStyle setLineBreakMode:NSLineBreakByTruncatingMiddle];
		NSMutableAttributedString* text = [[[filterDataSource displayStringForItem:[items objectAtIndex:rowIndex]] mutableCopy] autorelease];
		[text addAttributes:[NSDictionary dictionaryWithObjectsAndKeys:[NSColor darkGrayColor], NSForegroundColorAttributeName, [NSFont controlContentFontOfSize:13], NSFontAttributeName, pStyle, NSParagraphStyleAttributeName, nil] range:NSMakeRange(0, [text length])];
		NSDictionary* highlightAttributes = [NSDictionary dictionaryWithObjectsAndKeys:
												@1,                   NSUnderlineStyleAttributeName,
												[NSColor blackColor], NSForegroundColorAttributeName,
												[[NSFontManager sharedFontManager] convertFont:[NSFont controlContentFontOfSize:13] toHaveTrait:NSBoldFontMask], NSFontAttributeName,
												nil];
		HighlightRangesWithAttribute(text, FLMatchingTextAttributeName, highlightAttributes);
		return text;
	}
	else
		return [items objectAtIndex:rowIndex];
}

- (void)tableViewSelectionDidChange:(NSNotification*)notification
{
	if([filterDataSource respondsToSelector:@selector(infoStringForItem:)])
	{
		if(self.selectedRow != -1 && [items count] > 0)
		{
			NSMutableAttributedString* text = [[[filterDataSource infoStringForItem:[items objectAtIndex:self.selectedRow]] mutableCopy] autorelease];
			NSMutableParagraphStyle* style = [[[NSParagraphStyle defaultParagraphStyle] mutableCopy] autorelease];
			[style setLineBreakMode:NSLineBreakByTruncatingHead];
			[text addAttributes:[NSDictionary dictionaryWithObjectsAndKeys:[NSColor darkGrayColor], NSForegroundColorAttributeName, [NSFont controlContentFontOfSize:11], NSFontAttributeName, style, NSParagraphStyleAttributeName, nil]
                       range:NSMakeRange(0, [text length])];
			NSDictionary* highlightAttributes = [NSDictionary dictionaryWithObjectsAndKeys:
													@1,                   NSUnderlineStyleAttributeName,
													[NSColor blackColor], NSForegroundColorAttributeName,
													[[NSFontManager sharedFontManager] convertFont:[NSFont controlContentFontOfSize:11] toHaveTrait:NSBoldFontMask], NSFontAttributeName,
													nil];
			HighlightRangesWithAttribute(text, FLMatchingTextAttributeName, highlightAttributes);

			NSUInteger tabIndex = [text.mutableString rangeOfString:@"\t"].location;
			if(tabIndex != NSNotFound)
			{
				CGFloat width = self.window.frame.size.width - [text attributedSubstringFromRange:NSMakeRange(tabIndex, text.length - tabIndex)].size.width - 10;
				NSMutableParagraphStyle* rightAlignStyle = [[[text attribute:NSParagraphStyleAttributeName atIndex:tabIndex effectiveRange:NULL] mutableCopy] autorelease];
				[rightAlignStyle setTabStops:@[ [[[NSTextTab alloc] initWithType:NSLeftTabStopType location:width + 1.0] autorelease] ]];
				[text addAttribute:NSParagraphStyleAttributeName value:rightAlignStyle range:NSMakeRange(0, text.length)];
			}

			self.infoString = text;
		}
		else
		{
			self.infoString = [[[NSAttributedString alloc] initWithString:@""] autorelease];
		}
	}
}

- (void)tableView:(NSTableView*)tableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn*)tableColumn row:(NSInteger)rowIndex
{
	if([filterDataSource respondsToSelector:@selector(willDisplayCell:forItem:)] && ![tableColumn.identifier isEqualToString:@"accessoryColumn"])
		[filterDataSource willDisplayCell:aCell forItem:[self.items objectAtIndex:rowIndex]];
}

// ===========
// = Actions =
// ===========

- (NSArray*)selectedItems
{
	return [self.items objectsAtIndexes:self.selectedRowIndexes];
}

- (void)waitForAllItems
{
	if([filterDataSource respondsToSelector:@selector(waitForAllItems)])
		[filterDataSource waitForAllItems];
}

- (void)makeSelectedItemsBestMatch
{
	if([filterDataSource respondsToSelector:@selector(makeItemsBestFitForCurrentSearch:)])
		[filterDataSource makeItemsBestFitForCurrentSearch:self.selectedItems];
}

- (BOOL)acceptsFirstResponder
{
	return NO;
}
@end
