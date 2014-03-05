#import "OakFilterList.h"
#import "OakFilterListView.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <oak/oak.h>
#import "highlight_ranges.h"

OAK_DEBUG_VAR(FilterList_View);

NSString* const FLDataSourceItemsDidChangeNotification     = @"FLDataSourceItemsDidChangeNotification";
NSString* const FLDataSourceItemsShouldDescendNotification = @"FLDataSourceItemsShouldDescendNotification";
NSString* const FLDataSourceItemsShouldAscendNotification  = @"FLDataSourceItemsShouldAscendNotification";

@interface OakFilterListView ()
@property (nonatomic) BOOL isWaitingForItems;
@property (nonatomic) NSArray* items;
@property (nonatomic) NSAttributedString* infoString;
@end

@implementation OakFilterListView
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
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)awakeFromNib
{
	self.infoString = [[NSAttributedString alloc] initWithString:@""];
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
	if(aDataSource != _filterDataSource)
	{
		if(_filterDataSource)
		{
			if([_filterDataSource respondsToSelector:@selector(stopLoading)])
				[_filterDataSource stopLoading];
			[[NSNotificationCenter defaultCenter] removeObserver:self name:FLDataSourceItemsDidChangeNotification object:_filterDataSource];
			[[NSNotificationCenter defaultCenter] removeObserver:self name:FLDataSourceItemsShouldDescendNotification object:_filterDataSource];
			[[NSNotificationCenter defaultCenter] removeObserver:self name:FLDataSourceItemsShouldAscendNotification object:_filterDataSource];

		}

		if(_filterDataSource = aDataSource)
		{
			ASSERT([_filterDataSource conformsToProtocol:@protocol(FilterListDataSource)]);
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(dataSourceHasMoreData:) name:FLDataSourceItemsDidChangeNotification object:_filterDataSource];
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(dataSourceShouldDescend:) name:FLDataSourceItemsShouldDescendNotification object:_filterDataSource];
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(dataSourceShouldAscend:) name:FLDataSourceItemsShouldAscendNotification object:_filterDataSource];

			NSTextFieldCell* dataCell = nil;
			if([_filterDataSource respondsToSelector:@selector(itemDataCell)])
				dataCell = [_filterDataSource itemDataCell];
			if(!dataCell)
				dataCell = [[NSTextFieldCell alloc] initTextCell:@""];
			[[self.tableColumns objectAtIndex:0] setDataCell:dataCell];
			self.allowsMultipleSelection = [_filterDataSource respondsToSelector:@selector(allowsMultipleSelection)] && [_filterDataSource allowsMultipleSelection];
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
	[_filterDataSource descendIntoItem:[self.selectedItems lastObject]];
}

- (void)dataSourceShouldAscend:(NSNotification*)aNotification
{
	[_filterDataSource descendIntoItem:nil];
}

- (void)dataSourceHasMoreData:(NSNotification*)notification
{
	[self reloadData];
}

- (void)reloadData
{
	NSArray* selectedItems = nil;
	if([_filterDataSource respondsToSelector:@selector(preservesSelectionWhenFiltering)] && [_filterDataSource preservesSelectionWhenFiltering])
	{
		if(self.selectedRowIndexes.count != 1 || self.selectedRowIndexes.firstIndex != 0)
			selectedItems = [self.items objectsAtIndexes:self.selectedRowIndexes];
	}
	self.items = [_filterDataSource items];
	if([_filterDataSource respondsToSelector:@selector(selectedItems)])
		selectedItems = [_filterDataSource selectedItems];
	if(![_filterDataSource respondsToSelector:@selector(infoStringForItem:)])
	{
		NSMutableParagraphStyle* style = [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
		[style setAlignment:NSCenterTextAlignment];
		NSAttributedString* text = [[NSAttributedString alloc] initWithString:[NSString stringWithFormat:@"%lu items", self.items.count] attributes:@{
			NSForegroundColorAttributeName    : [NSColor darkGrayColor],
			NSFontAttributeName               : OakStatusBarFont(),
			NSParagraphStyleAttributeName     : style
		}];
		self.infoString = text;
	}
	[super reloadData];
	NSMutableIndexSet* indexesToSelect = [NSMutableIndexSet indexSet];
	for(id item in selectedItems)
	{
		NSUInteger index = [self.items indexOfObject:item];
		if(index != NSNotFound)
			[indexesToSelect addIndex:index];
	}
	if(indexesToSelect.count == 0)
		[indexesToSelect addIndex:0];
	[self selectRowIndexes:indexesToSelect byExtendingSelection:NO];
	[self scrollRowToVisible:indexesToSelect.firstIndex];
	[self tableViewSelectionDidChange:nil];
	self.isWaitingForItems = [_filterDataSource respondsToSelector:@selector(moreItemsToCome)] && [_filterDataSource moreItemsToCome];
}

// ======================
// = NSTableDataSource  =
// ======================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)tableView
{
	return [_items count];
}

- (id)tableView:(NSTableView*)tableView objectValueForTableColumn:(NSTableColumn*)tableColumn row:(NSInteger)rowIndex
{
	if([tableColumn.identifier isEqualToString:@"accessoryColumn"])
		return [_items objectAtIndex:rowIndex];

	if([_filterDataSource respondsToSelector:@selector(displayStringForItem:)])
	{
		NSMutableParagraphStyle* pStyle = [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
		[pStyle setLineBreakMode:NSLineBreakByTruncatingMiddle];

		NSFont* baseFont = OakControlFont();
		NSFont* boldFont = [[NSFontManager sharedFontManager] convertFont:baseFont toHaveTrait:NSBoldFontMask];

		NSDictionary* baseAttributes      = @{ NSForegroundColorAttributeName : [NSColor darkGrayColor], NSFontAttributeName : baseFont, NSParagraphStyleAttributeName : pStyle };
		NSDictionary* highlightAttributes = @{ NSForegroundColorAttributeName : [NSColor blackColor],    NSFontAttributeName : boldFont, NSUnderlineStyleAttributeName : @1 };

		NSMutableAttributedString* text = [[_filterDataSource displayStringForItem:[_items objectAtIndex:rowIndex]] mutableCopy];
		[text addAttributes:baseAttributes range:NSMakeRange(0, [text length])];
		HighlightRangesWithAttribute(text, FLMatchingTextAttributeName, highlightAttributes);
		return text;
	}
	else
	{
		return [_items objectAtIndex:rowIndex];
	}
}

- (void)tableViewSelectionDidChange:(NSNotification*)notification
{
	if([_filterDataSource respondsToSelector:@selector(infoStringForItem:)])
	{
		if(self.selectedRow != -1 && [_items count] > 0)
		{
			NSMutableParagraphStyle* pStyle = [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
			[pStyle setLineBreakMode:NSLineBreakByTruncatingHead];

			NSFont* baseFont = [NSFont controlContentFontOfSize:11];
			NSFont* boldFont = [[NSFontManager sharedFontManager] convertFont:baseFont toHaveTrait:NSBoldFontMask];

			NSDictionary* baseAttributes      = @{ NSForegroundColorAttributeName : [NSColor darkGrayColor], NSFontAttributeName : baseFont, NSParagraphStyleAttributeName : pStyle };
			NSDictionary* highlightAttributes = @{ NSForegroundColorAttributeName : [NSColor blackColor],    NSFontAttributeName : boldFont, NSUnderlineStyleAttributeName : @1 };

			NSMutableAttributedString* text = [[_filterDataSource infoStringForItem:[_items objectAtIndex:self.selectedRow]] mutableCopy];
			[text addAttributes:baseAttributes range:NSMakeRange(0, [text length])];
			HighlightRangesWithAttribute(text, FLMatchingTextAttributeName, highlightAttributes);

			NSUInteger tabIndex = [text.mutableString rangeOfString:@"\t"].location;
			if(tabIndex != NSNotFound)
			{
				CGFloat width = self.window.frame.size.width - [text attributedSubstringFromRange:NSMakeRange(tabIndex, text.length - tabIndex)].size.width - 10;
				NSMutableParagraphStyle* rightAlignStyle = [[text attribute:NSParagraphStyleAttributeName atIndex:tabIndex effectiveRange:NULL] mutableCopy];
				[rightAlignStyle setTabStops:@[ [[NSTextTab alloc] initWithType:NSLeftTabStopType location:width + 1.0] ]];
				[text addAttribute:NSParagraphStyleAttributeName value:rightAlignStyle range:NSMakeRange(0, text.length)];
			}

			self.infoString = text;
		}
		else
		{
			self.infoString = [[NSAttributedString alloc] initWithString:@""];
		}
	}
}

- (void)tableView:(NSTableView*)tableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn*)tableColumn row:(NSInteger)rowIndex
{
	if([_filterDataSource respondsToSelector:@selector(willDisplayCell:forItem:)] && ![tableColumn.identifier isEqualToString:@"accessoryColumn"])
		[_filterDataSource willDisplayCell:aCell forItem:[self.items objectAtIndex:rowIndex]];
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
	if([_filterDataSource respondsToSelector:@selector(waitForAllItems)])
		[_filterDataSource waitForAllItems];
}

- (void)makeSelectedItemsBestMatch
{
	if([_filterDataSource respondsToSelector:@selector(makeItemsBestFitForCurrentSearch:)])
		[_filterDataSource makeItemsBestFitForCurrentSearch:self.selectedItems];
}

- (BOOL)acceptsFirstResponder
{
	return NO;
}
@end
