#import "OakChoiceMenu.h"
#import <oak/algorithm.h>

NSUInteger const OakChoiceMenuKeyUnused   = 0;
NSUInteger const OakChoiceMenuKeyReturn   = 1;
NSUInteger const OakChoiceMenuKeyTab      = 2;
NSUInteger const OakChoiceMenuKeyCancel   = 3;
NSUInteger const OakChoiceMenuKeyMovement = 4;

@interface OakChoiceMenu ()
@property (nonatomic) NSWindow* window;
@end

enum action_t { kActionNop, kActionTab, kActionReturn, kActionCancel, kActionMoveUp, kActionMoveDown, kActionPageUp, kActionPageDown, kActionMoveToBeginning, kActionMoveToEnd };

@implementation OakChoiceMenu
@synthesize choices, choiceIndex, window;

- (id)init
{
	if((self = [super init]))
	{
		self.choices = [NSArray array];
		choiceIndex = NSNotFound;
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	self.window = nil;
}

- (void)sizeToFit
{
	CGFloat const kTableViewPadding = 4;
	CGFloat const kScrollBarWidth   = 15;

	CGFloat width = 60;
	for(NSInteger i = 0; i < tableView.numberOfRows; ++i)
		width = std::max(width, kTableViewPadding + [[tableView preparedCellAtColumn:0 row:i] cellSizeForBounds:NSMakeRect(0, 0, CGFLOAT_MAX, tableView.rowHeight)].width);

	if([choices count] > 10)
		width += kScrollBarWidth;

	CGFloat height = std::min<NSUInteger>([choices count], 10) * ([tableView rowHeight]+[tableView intercellSpacing].height);
	NSRect frame   = { { NSMinX(window.frame), NSMaxY(window.frame) - height }, { std::min<CGFloat>(ceil(width), 400), height } };
	[window setFrame:frame display:YES];
}

- (void)setWindow:(NSWindow*)aWindow
{
	if(aWindow == window)
		return;

	[[window parentWindow] removeChildWindow:window];
	window = aWindow;
}

- (void)viewBoundsDidChange:(NSNotification*)aNotification
{
	NSView* aView = [[aNotification object] documentView];
	[window setFrameTopLeftPoint:[[aView window] convertRectToScreen:[aView convertRect:(NSRect){ topLeftPosition, NSZeroSize } fromView:nil]].origin];
}

- (NSString*)selectedChoice
{
	return choiceIndex == NSNotFound ? nil : [choices objectAtIndex:choiceIndex];
}

- (void)setChoices:(NSArray*)newChoices
{
	if([choices isEqualToArray:newChoices])
		return;

	id oldSelection = self.selectedChoice;
	self.choiceIndex = NSNotFound;
	choices = newChoices;
	[tableView reloadData];
	self.choiceIndex = [choices indexOfObject:oldSelection];

	[self sizeToFit];
}

- (void)setChoiceIndex:(NSUInteger)newIndex
{
	if(choiceIndex != newIndex)
	{
		choiceIndex = newIndex;
		if(choiceIndex == NSNotFound)
		{
			[tableView deselectAll:self];
		}
		else
		{
			[tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:choiceIndex] byExtendingSelection:NO];
			[tableView scrollRectToVisible:[tableView rectOfRow:choiceIndex]];
		}
	}
}

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return [choices count];
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	return [choices objectAtIndex:rowIndex];
}

- (void)showAtTopLeftPoint:(NSPoint)aPoint forView:(NSView*)aView
{
	window = [[NSPanel alloc] initWithContentRect:NSMakeRect(aPoint.x, aPoint.y, 0, 0) styleMask:NSBorderlessWindowMask backing:NSBackingStoreBuffered defer:NO];
	[window setReleasedWhenClosed:NO];
	[window setOpaque:NO];
	window.alphaValue         = 0.97;
	window.backgroundColor    = [NSColor colorWithCalibratedRed:1.00 green:0.96 blue:0.76 alpha:1];
	window.hasShadow          = YES;
	window.level              = NSStatusWindowLevel;
	window.ignoresMouseEvents = YES;

	tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
	[tableView addTableColumn:[[NSTableColumn alloc] initWithIdentifier:@"mainColumn"]];
	tableView.headerView                         = nil;
	tableView.focusRingType                      = NSFocusRingTypeNone;
	tableView.autoresizingMask                   = NSViewWidthSizable|NSViewHeightSizable;
	tableView.usesAlternatingRowBackgroundColors = YES;
	tableView.allowsMultipleSelection            = YES;
	tableView.dataSource                         = self;
	// tableView.delegate                           = self;
	[tableView reloadData];

	NSScrollView* scrollView         = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = NO;
	scrollView.autohidesScrollers    = YES;
	scrollView.borderType            = NSNoBorder;
	scrollView.documentView          = tableView;
	scrollView.autoresizingMask      = NSViewWidthSizable|NSViewHeightSizable;

	[window setContentView:scrollView];

	if(choiceIndex != NSNotFound)
		[tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:choiceIndex] byExtendingSelection:NO];

	[self sizeToFit];

	topLeftPosition = [aView convertRect:[[aView window] convertRectFromScreen:(NSRect){ aPoint, NSZeroSize }] toView:nil].origin;
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(viewBoundsDidChange:) name:NSViewBoundsDidChangeNotification object:[[aView enclosingScrollView] contentView]];
	[[aView window] addChildWindow:window ordered:NSWindowAbove];

	[window orderFront:self];
}

- (BOOL)isVisible
{
	return [window isVisible];
}

- (NSUInteger)didHandleKeyEvent:(NSEvent*)anEvent
{
	NSUInteger res = OakChoiceMenuKeyUnused;
	if(!window)
		return res;

	keyAction = kActionNop;
	[self interpretKeyEvents:@[ anEvent ]];
	if(keyAction == kActionNop)
		return res;

	NSInteger offset = 0;
	NSInteger visibleRows = floor(NSHeight([tableView visibleRect]) / ([tableView rowHeight]+[tableView intercellSpacing].height)) - 1;
	res = OakChoiceMenuKeyMovement;
	switch(keyAction)
	{
		case kActionMoveUp:          offset = -1;                  break;
		case kActionMoveDown:        offset = +1;                  break;
		case kActionPageUp:          offset = -visibleRows;        break;
		case kActionPageDown:        offset = +visibleRows;        break;
		case kActionMoveToBeginning: offset = -(INT_MAX >> 1);     break;
		case kActionMoveToEnd:       offset = +(INT_MAX >> 1);     break;
		case kActionReturn:          res = OakChoiceMenuKeyReturn; break;
		case kActionTab:             res = OakChoiceMenuKeyTab;    break;
		case kActionCancel:          res = OakChoiceMenuKeyCancel; break;
	}

	if(res == OakChoiceMenuKeyMovement)
		self.choiceIndex = oak::cap<NSInteger>(0, (choiceIndex == NSNotFound ? (offset > 0 ? -1 : [choices count]) : choiceIndex) + offset, [choices count] - 1);

	return res;
}

- (void)doCommandBySelector:(SEL)aSelector
{
	if([self respondsToSelector:aSelector])
		[super doCommandBySelector:aSelector];
}

- (void)insertText:(id)aString                 { }

- (void)insertNewline:(id)sender               { keyAction = kActionReturn;           }
- (void)insertTab:(id)sender                   { keyAction = kActionTab;              }
- (void)cancelOperation:(id)sender             { keyAction = kActionCancel;           }

- (void)moveUp:(id)sender                      { keyAction = kActionMoveUp;           }
- (void)moveDown:(id)sender                    { keyAction = kActionMoveDown;         }

- (void)movePageUp:(id)sender                  { keyAction = kActionPageUp;           }
- (void)movePageDown:(id)sender                { keyAction = kActionPageDown;         }
- (void)pageUp:(id)sender                      { keyAction = kActionPageUp;           }
- (void)pageDown:(id)sender                    { keyAction = kActionPageDown;         }
- (void)scrollPageUp:(id)sender                { keyAction = kActionPageUp;           }
- (void)scrollPageDown:(id)sender              { keyAction = kActionPageDown;         }

- (void)moveToBeginningOfDocument:(id)sender   { keyAction = kActionMoveToBeginning;  }
- (void)moveToEndOfDocument:(id)sender         { keyAction = kActionMoveToEnd;        }
- (void)scrollToBeginningOfDocument:(id)sender { keyAction = kActionMoveToBeginning;  }
- (void)scrollToEndOfDocument:(id)sender       { keyAction = kActionMoveToEnd;        }
@end
