/*
	TODO Drag’n’drop
	TODO Show some sort of title
	TODO Filter string
*/

#import "OakPasteboardSelector.h"
#import <ns/ns.h>
#import <oak/oak.h>
#import <oak/debug.h>

static size_t line_count (std::string const& text)
{
	size_t line_count = std::count(text.begin(), text.end(), '\n') + 1;
	if(text.size() > 0 && text[text.size() - 1] == '\n')
		line_count -= 1;
	return line_count;
}

@interface OakPasteboardSelectorMultiLineCell : NSCell
{
	size_t maxLines;
}
@property (nonatomic, assign) size_t maxLines;
+ (id)cellWithMaxLines:(size_t)maxLines;
- (CGFloat)rowHeightForText:(NSString*)text;
@end

@implementation OakPasteboardSelectorMultiLineCell
@synthesize maxLines;

+ (id)cellWithMaxLines:(size_t)maxLines;
{
	OakPasteboardSelectorMultiLineCell* cell = [[self class] new];
	cell.maxLines = maxLines;
	return cell;
}

- (NSDictionary*)textAttributes;
{
	static NSMutableParagraphStyle* const style = [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
	[style setLineBreakMode:NSLineBreakByTruncatingTail];
	if([self isHighlighted])
	{
		static NSDictionary* const highlightedAttributes = [[NSDictionary alloc] initWithObjectsAndKeys:
			[NSColor alternateSelectedControlTextColor], NSForegroundColorAttributeName,
			style, NSParagraphStyleAttributeName,
			nil];
		return highlightedAttributes;
	}
	else
	{
		static NSDictionary* const attributes = [[NSDictionary alloc] initWithObjectsAndKeys:style, NSParagraphStyleAttributeName, nil];
		return attributes;
	}
}

- (NSSet*)myAccessibilityAttributeNames
{
	static NSSet* set = [NSSet setWithArray:@[
		NSAccessibilityRoleAttribute,
		NSAccessibilityValueAttribute,
	]];
	return set;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = [[[self myAccessibilityAttributeNames] setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
	return attributes;
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	if([[self myAccessibilityAttributeNames] containsObject:attribute])
		return NO;
	return [super accessibilityIsAttributeSettable:attribute];
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	id value = nil;
	if([attribute isEqualToString:NSAccessibilityRoleAttribute])
		value = NSAccessibilityStaticTextRole;
	else if([attribute isEqualToString:NSAccessibilityValueAttribute])
		value = [self objectValue];
	else
		value = [super accessibilityAttributeValue:attribute];
	return value;
}

- (size_t)lineCountForText:(NSString*)text
{
	return oak::cap<size_t>(1, line_count(to_s(text)), maxLines);
}

- (void)drawWithFrame:(NSRect)frame inView:(NSView*)controlView
{
	NSArray* lines        = [[self objectValue] componentsSeparatedByString:@"\n"];
	NSArray* clippedLines = [lines subarrayWithRange:NSMakeRange(0, [self lineCountForText:[self objectValue]])];
	NSRect rowFrame       = frame;
	rowFrame.size.height  = [[lines objectAtIndex:0] sizeWithAttributes:[self textAttributes]].height;
	for(size_t index = 0; index < [clippedLines count]; ++index)
	{
		if(index == [clippedLines count] - 1 && [clippedLines count] < [lines count])
		{
			NSString* moreLinesText           = [NSString stringWithFormat:@"%lu more line%s", [lines count] - [clippedLines count], ([lines count] - [clippedLines count]) != 1 ? "s" : ""];
			NSDictionary* moreLinesAttributes = @{ NSForegroundColorAttributeName : ([self isHighlighted] ? [NSColor darkGrayColor] : [NSColor lightGrayColor]) };
			NSAttributedString* moreLines     = [[NSAttributedString alloc] initWithString:moreLinesText attributes:moreLinesAttributes];
			NSSize size             = [moreLines size];
			NSRect moreLinesRect    = rowFrame;
			moreLinesRect.origin.x += frame.size.width - size.width;
			moreLinesRect.size      = size;
			rowFrame.size.width    -= size.width + 5;
			[moreLines drawInRect:moreLinesRect];
		}
		[[clippedLines objectAtIndex:index] drawInRect:rowFrame withAttributes:[self textAttributes]];
		rowFrame.origin.y += rowFrame.size.height;
	}
}

- (CGFloat)rowHeightForText:(NSString*)text;
{
	NSArray* lines = [text componentsSeparatedByString:@"\n"];
	CGFloat height = [self lineCountForText:text] * [[lines objectAtIndex:0] sizeWithAttributes:[self textAttributes]].height;
	if(height == 0)
		height = 15; // FIXME magic number (see commit log for details)
	return height;
}
@end

@interface OakPasteboardSelectorTableViewHelper : NSResponder <NSTableViewDataSource, NSTableViewDelegate>
{
	NSTableView* tableView;
	NSMutableArray* entries;
	BOOL shouldClose;
	BOOL shouldSendAction;
}
- (void)setTableView:(NSTableView*)aTableView;
@end

@implementation OakPasteboardSelectorTableViewHelper
- (id)initWithEntries:(NSArray*)someEntries
{
	if(self = [super init])
	{
		entries = [someEntries mutableCopy];
	}
	return self;
}

- (void)dealloc
{
	[self setTableView:nil];
}

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return [entries count];
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	return [[[entries objectAtIndex:rowIndex] string] stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
}

- (CGFloat)tableView:(NSTableView*)aTableView heightOfRow:(NSInteger)rowIndex
{
	NSString* text = [self tableView:aTableView objectValueForTableColumn:nil row:rowIndex];
	return [[[[aTableView tableColumns] objectAtIndex:0] dataCell] rowHeightForText:text];
}

- (void)setTableView:(NSTableView*)aTableView
{
	if(tableView && tableView == aTableView)
		return;

	if(tableView)
	{
		[tableView setDelegate:nil];
		[tableView setTarget:nil];
		[tableView setDataSource:nil];
		[tableView setNextResponder:[self nextResponder]];
	}

	if(tableView = aTableView)
	{
		[tableView setDataSource:self];
		[tableView setDelegate:self];
		[tableView reloadData];
		[tableView setUsesAlternatingRowBackgroundColors:YES];
		[[[tableView tableColumns] objectAtIndex:0] setDataCell:[OakPasteboardSelectorMultiLineCell cellWithMaxLines:3]];

		NSResponder* nextResponder = [tableView nextResponder];
		[tableView setNextResponder:self];
		[self setNextResponder:nextResponder];

		[tableView setTarget:self];
		[tableView setDoubleAction:@selector(didDoubleClickInTableView:)];
		[tableView setAction:NULL];
	}
}

- (void)setPerformsActionOnSingleClick
{
	[tableView setAction:@selector(didDoubleClickInTableView:)];
}

- (void)deleteBackward:(id)sender
{
	int selectedRow = [tableView selectedRow];
	if(selectedRow == -1 || [entries count] <= 1)
		return NSBeep();
	[entries removeObjectAtIndex:selectedRow];
	[tableView reloadData];
	if([entries count] > 0)
		[tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:oak::cap(0, selectedRow - 1, (int)[entries count]-1)] byExtendingSelection:NO];
}

- (void)deleteForward:(id)sender
{
	int selectedRow = [tableView selectedRow];
	if(selectedRow == -1 || [entries count] <= 1)
		return NSBeep();
	[entries removeObjectAtIndex:selectedRow];
	[tableView reloadData];
	if([entries count] > 0)
		[tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:MIN(selectedRow, [entries count]-1)] byExtendingSelection:NO];
}

- (void)insertNewline:(id)sender
{
	shouldSendAction = entries.count > 0 ? YES : NO;
	shouldClose = YES;
}

- (void)cancel:(id)sender
{
	shouldClose = YES;
}

- (void)keyDown:(NSEvent*)anEvent
{
	[self interpretKeyEvents:@[ anEvent ]];
}

- (void)moveSelectedRowByOffset:(NSInteger)anOffset
{
	if(tableView.numberOfRows > 0)
	{
		NSInteger row = oak::cap<NSInteger>(0, tableView.selectedRow + anOffset, tableView.numberOfRows - 1);
		[tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[tableView scrollRowToVisible:row];
	}
}

- (int)visibleRows                             { return (int)floorf(NSHeight([tableView visibleRect]) / ([tableView rowHeight]+[tableView intercellSpacing].height)) - 1; }

- (void)moveUp:(id)sender                      { [self moveSelectedRowByOffset:-1]; }
- (void)moveDown:(id)sender                    { [self moveSelectedRowByOffset:+1];}
- (void)movePageUp:(id)sender                  { [self moveSelectedRowByOffset:-[self visibleRows]]; }
- (void)movePageDown:(id)sender                { [self moveSelectedRowByOffset:+[self visibleRows]]; }
- (void)scrollToBeginningOfDocument:(id)sender { [self moveSelectedRowByOffset:-(INT_MAX >> 1)]; }
- (void)scrollToEndOfDocument:(id)sender       { [self moveSelectedRowByOffset:+(INT_MAX >> 1)]; }

- (void)pageUp:(id)sender                      { [self movePageUp:sender]; }
- (void)pageDown:(id)sender                    { [self movePageDown:sender]; }
- (void)scrollPageUp:(id)sender                { [self movePageUp:sender]; }
- (void)scrollPageDown:(id)sender              { [self movePageDown:sender]; }

- (void)didDoubleClickInTableView:(id)aTableView
{
	shouldClose = shouldSendAction = YES;
}

- (BOOL)shouldSendAction
{
	return shouldSendAction;
}

- (BOOL)shouldClose
{
	return shouldClose;
}

- (NSArray*)entries
{
	return entries;
}
@end

@implementation OakPasteboardSelector
+ (OakPasteboardSelector*)sharedInstance
{
	static OakPasteboardSelector* instance = [OakPasteboardSelector new];
	return instance;
}

- (id)init
{
	if(self = [super initWithWindowNibName:@"Pasteboard Selector"])
	{
		[self setShouldCascadeWindows:NO];
		[self window];
	}
	return self;
}

- (void)setIndex:(unsigned)index
{
	[tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:index] byExtendingSelection:NO];
}

- (void)setEntries:(NSArray*)entries
{
	[self setIndex:0];
	tableViewHelper = [[OakPasteboardSelectorTableViewHelper alloc] initWithEntries:entries];
	[tableViewHelper setTableView:tableView];
}

- (NSArray*)entries
{
	return [tableViewHelper entries];
}

- (unsigned)showAtLocation:(NSPoint)aLocation
{
	NSWindow* parentWindow = [NSApp keyWindow];
	NSWindow* window = [self window];
	[window setFrameTopLeftPoint:aLocation];
	[parentWindow addChildWindow:window ordered:NSWindowAbove];
	[window orderFront:self];
	[tableView scrollRowToVisible:tableView.selectedRow];

	while(NSEvent* event = [NSApp nextEventMatchingMask:NSAnyEventMask untilDate:[NSDate distantFuture] inMode:NSDefaultRunLoopMode dequeue:YES])
	{
		static std::set<NSEventType> const keyEvent   = { NSKeyDown, NSKeyUp };
		static std::set<NSEventType> const mouseEvent = { NSLeftMouseDown, NSLeftMouseUp, NSRightMouseDown, NSRightMouseUp, NSOtherMouseDown, NSOtherMouseUp };

		bool orderOutEvent = (keyEvent.find([event type]) != keyEvent.end() && [event window] != parentWindow) || (mouseEvent.find([event type]) != mouseEvent.end() && [event window] != window);
		if(!orderOutEvent && keyEvent.find([event type]) != keyEvent.end() && !([event modifierFlags] & NSCommandKeyMask))
				[window sendEvent:event];
		else	[NSApp sendEvent:event];

		if(orderOutEvent || [tableViewHelper shouldClose])
			break;
	}

	[parentWindow removeChildWindow:window];
	[window orderOut:self];

	return [tableView selectedRow];
}

- (void)setWidth:(CGFloat)width;
{
	NSRect frame = [[self window] frame];
	frame.size.width = width;
	[[self window] setFrame:frame display:NO];
}

- (void)setPerformsActionOnSingleClick
{
	[tableViewHelper setPerformsActionOnSingleClick];
}

- (BOOL)shouldSendAction
{
	return [tableViewHelper shouldSendAction];
}
@end
