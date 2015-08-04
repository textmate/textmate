#import "OakChoiceMenu.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <oak/algorithm.h>

NSUInteger const OakChoiceMenuKeyUnused   = 0;
NSUInteger const OakChoiceMenuKeyReturn   = 1;
NSUInteger const OakChoiceMenuKeyTab      = 2;
NSUInteger const OakChoiceMenuKeyCancel   = 3;
NSUInteger const OakChoiceMenuKeyMovement = 4;

@interface OakChoiceMenu () <NSTableViewDelegate>
{
	NSTableView* tableView;
	NSUInteger keyAction;
	NSPoint topLeftPosition;
}
@property (nonatomic) NSWindow* window;
@end

enum action_t { kActionNop, kActionTab, kActionReturn, kActionCancel, kActionMoveUp, kActionMoveDown, kActionPageUp, kActionPageDown, kActionMoveToBeginning, kActionMoveToEnd };

@implementation OakChoiceMenu
- (id)init
{
	if((self = [super init]))
	{
		_choices = [NSArray array];
		_choiceIndex = NSNotFound;
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

	NSTextField* textField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);
	CGFloat width = 60;
	for(NSInteger i = 0; i < MIN(_choices.count, 256); ++i)
	{
		textField.stringValue = _choices[i];
		[textField sizeToFit];
		width = std::max(width, kTableViewPadding + NSWidth(textField.frame));
	}

	if([_choices count] > 10)
		width += kScrollBarWidth;

	CGFloat height = std::min<NSUInteger>([_choices count], 10) * ([tableView rowHeight]+[tableView intercellSpacing].height);
	NSRect frame   = { { NSMinX(_window.frame), NSMaxY(_window.frame) - height }, { std::min<CGFloat>(ceil(width), 400), height } };
	[_window setFrame:frame display:YES];
}

- (void)setWindow:(NSWindow*)aWindow
{
	if(aWindow == _window)
		return;

	[[_window parentWindow] removeChildWindow:_window];
	_window = aWindow;
}

- (void)viewBoundsDidChange:(NSNotification*)aNotification
{
	NSView* aView = [[aNotification object] documentView];
	[_window setFrameTopLeftPoint:[[aView window] convertRectToScreen:[aView convertRect:(NSRect){ topLeftPosition, NSZeroSize } fromView:nil]].origin];
}

- (NSString*)selectedChoice
{
	return _choiceIndex == NSNotFound ? nil : [_choices objectAtIndex:_choiceIndex];
}

- (void)setChoices:(NSArray*)newChoices
{
	if([_choices isEqualToArray:newChoices])
		return;

	id oldSelection = self.selectedChoice;
	self.choiceIndex = NSNotFound;
	_choices = newChoices;
	[tableView reloadData];
	self.choiceIndex = [_choices indexOfObject:oldSelection];

	[self sizeToFit];
}

- (void)setChoiceIndex:(NSUInteger)newIndex
{
	if(_choiceIndex != newIndex)
	{
		_choiceIndex = newIndex;
		if(_choiceIndex == NSNotFound)
		{
			[tableView deselectAll:self];
		}
		else
		{
			[tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:_choiceIndex] byExtendingSelection:NO];
			[tableView scrollRectToVisible:[tableView rectOfRow:_choiceIndex]];
		}
	}
}

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return [_choices count];
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	return [_choices objectAtIndex:rowIndex];
}

- (NSView*)tableView:(NSTableView*)aTableView viewForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)row
{
	NSString* identifier = aTableColumn.identifier;
	NSTextField* res = [aTableView makeViewWithIdentifier:identifier owner:self];
	if(!res)
	{
		res = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);
		res.identifier = identifier;
	}
	return res;
}

- (void)showAtTopLeftPoint:(NSPoint)aPoint forView:(NSView*)aView
{
	_window = [[NSPanel alloc] initWithContentRect:NSMakeRect(aPoint.x, aPoint.y, 0, 0) styleMask:NSBorderlessWindowMask backing:NSBackingStoreBuffered defer:NO];
	[_window setReleasedWhenClosed:NO];
	[_window setOpaque:NO];
	_window.alphaValue         = 0.97;
	_window.backgroundColor    = [NSColor colorWithCalibratedRed:1.00 green:0.96 blue:0.76 alpha:1];
	_window.hasShadow          = YES;
	_window.level              = NSStatusWindowLevel;
	_window.ignoresMouseEvents = YES;

	tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
	[tableView addTableColumn:[[NSTableColumn alloc] initWithIdentifier:@"mainColumn"]];
	tableView.headerView                         = nil;
	tableView.focusRingType                      = NSFocusRingTypeNone;
	tableView.autoresizingMask                   = NSViewWidthSizable|NSViewHeightSizable;
	tableView.usesAlternatingRowBackgroundColors = YES;
	tableView.allowsMultipleSelection            = YES;
	tableView.dataSource                         = self;
	tableView.delegate                           = self;
	[tableView reloadData];

	NSScrollView* scrollView         = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = NO;
	scrollView.autohidesScrollers    = YES;
	scrollView.borderType            = NSNoBorder;
	scrollView.documentView          = tableView;
	scrollView.autoresizingMask      = NSViewWidthSizable|NSViewHeightSizable;

	[_window setContentView:scrollView];

	if(_choiceIndex != NSNotFound)
		[tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:_choiceIndex] byExtendingSelection:NO];

	[self sizeToFit];

	topLeftPosition = [aView convertRect:[[aView window] convertRectFromScreen:(NSRect){ aPoint, NSZeroSize }] toView:nil].origin;
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(viewBoundsDidChange:) name:NSViewBoundsDidChangeNotification object:[[aView enclosingScrollView] contentView]];
	[[aView window] addChildWindow:_window ordered:NSWindowAbove];

	[_window orderFront:self];
}

- (BOOL)isVisible
{
	return [_window isVisible];
}

- (NSUInteger)didHandleKeyEvent:(NSEvent*)anEvent
{
	NSUInteger res = OakChoiceMenuKeyUnused;
	if(!_window)
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
		self.choiceIndex = oak::cap<NSInteger>(0, (_choiceIndex == NSNotFound ? (offset > 0 ? -1 : [_choices count]) : _choiceIndex) + offset, [_choices count] - 1);

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
