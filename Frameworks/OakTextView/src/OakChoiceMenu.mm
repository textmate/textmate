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
	NSTableView* _tableView;
	NSUInteger _keyAction;
	NSPoint _topLeftPosition;
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
		_font = [NSFont controlContentFontOfSize:0];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[_window close];
	_window = nil;
}

- (void)sizeToFit
{
	CGFloat const kTableViewPadding = 4;
	CGFloat const kScrollBarWidth   = 15;

	NSTextField* textField = OakCreateLabel(@"", self.font, NSTextAlignmentLeft, NSLineBreakByTruncatingTail);
	if(_choices.count == 0)
		[textField sizeToFit];

	CGFloat width = 60;
	for(NSInteger i = 0; i < MIN(_choices.count, 256); ++i)
	{
		textField.stringValue = _choices[i];
		[textField sizeToFit];
		width = std::max(width, kTableViewPadding + NSWidth(textField.frame));
	}

	_tableView.rowHeight = NSHeight(textField.frame);

	if([_choices count] > 10)
		width += kScrollBarWidth;

	CGFloat height = std::min<NSUInteger>([_choices count], 10) * ([_tableView rowHeight]+[_tableView intercellSpacing].height);
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
	[_window setFrameTopLeftPoint:[[aView window] convertRectToScreen:[aView convertRect:(NSRect){ _topLeftPosition, NSZeroSize } fromView:nil]].origin];
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
	[_tableView reloadData];
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
			[_tableView deselectAll:self];
		}
		else
		{
			[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:_choiceIndex] byExtendingSelection:NO];
			[_tableView scrollRectToVisible:[_tableView rectOfRow:_choiceIndex]];
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
		res = OakCreateLabel(@"", self.font, NSTextAlignmentLeft, NSLineBreakByTruncatingTail);
		res.identifier = identifier;
	}
	return res;
}

- (void)showAtTopLeftPoint:(NSPoint)aPoint forView:(NSView*)aView
{
	_window = [[NSPanel alloc] initWithContentRect:NSMakeRect(aPoint.x, aPoint.y, 0, 0) styleMask:NSWindowStyleMaskBorderless backing:NSBackingStoreBuffered defer:NO];
	_window.opaque             = NO;
	_window.alphaValue         = 0.97;
	_window.backgroundColor    = [NSColor colorWithCalibratedRed:1.00 green:0.96 blue:0.76 alpha:1];
	_window.hasShadow          = YES;
	_window.level              = NSStatusWindowLevel;
	_window.ignoresMouseEvents = YES;

	_tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
	[_tableView addTableColumn:[[NSTableColumn alloc] initWithIdentifier:@"mainColumn"]];
	_tableView.headerView                         = nil;
	_tableView.focusRingType                      = NSFocusRingTypeNone;
	_tableView.autoresizingMask                   = NSViewWidthSizable|NSViewHeightSizable;
	_tableView.usesAlternatingRowBackgroundColors = YES;
	_tableView.allowsMultipleSelection            = YES;
	_tableView.dataSource                         = self;
	_tableView.delegate                           = self;
	[_tableView reloadData];

	NSScrollView* scrollView         = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = NO;
	scrollView.autohidesScrollers    = YES;
	scrollView.borderType            = NSNoBorder;
	scrollView.documentView          = _tableView;
	scrollView.autoresizingMask      = NSViewWidthSizable|NSViewHeightSizable;

	[_window setContentView:scrollView];

	if(_choiceIndex != NSNotFound)
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:_choiceIndex] byExtendingSelection:NO];

	[self sizeToFit];

	_topLeftPosition = [aView convertRect:[[aView window] convertRectFromScreen:(NSRect){ aPoint, NSZeroSize }] toView:nil].origin;
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

	_keyAction = kActionNop;
	[self interpretKeyEvents:@[ anEvent ]];
	if(_keyAction == kActionNop)
		return res;

	NSInteger offset = 0;
	NSInteger visibleRows = floor(NSHeight([_tableView visibleRect]) / ([_tableView rowHeight]+[_tableView intercellSpacing].height)) - 1;
	res = OakChoiceMenuKeyMovement;
	switch(_keyAction)
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
	static std::map<SEL, NSUInteger> const map = {
		{ @selector(insertNewline:),                               kActionReturn          },
		{ @selector(insertNewlineIgnoringFieldEditor:),            kActionReturn          },
		{ @selector(insertTab:),                                   kActionTab             },
		{ @selector(cancelOperation:),                             kActionCancel          },
		{ @selector(moveUp:),                                      kActionMoveUp          },
		{ @selector(moveDown:),                                    kActionMoveDown        },
		{ @selector(moveUpAndModifySelection:),                    kActionMoveUp          },
		{ @selector(moveDownAndModifySelection:),                  kActionMoveDown        },
		{ @selector(pageUp:),                                      kActionPageUp          },
		{ @selector(pageDown:),                                    kActionPageDown        },
		{ @selector(pageUpAndModifySelection:),                    kActionPageUp          },
		{ @selector(pageDownAndModifySelection:),                  kActionPageDown        },
		{ @selector(moveToBeginningOfDocument:),                   kActionMoveToBeginning },
		{ @selector(moveToEndOfDocument:),                         kActionMoveToEnd       },
		{ @selector(moveToBeginningOfDocumentAndModifySelection:), kActionMoveToBeginning },
		{ @selector(moveToEndOfDocumentAndModifySelection:),       kActionMoveToEnd       },
		{ @selector(scrollPageUp:),                                kActionPageUp          },
		{ @selector(scrollPageDown:),                              kActionPageDown        },
		{ @selector(scrollToBeginningOfDocument:),                 kActionMoveToBeginning },
		{ @selector(scrollToEndOfDocument:),                       kActionMoveToEnd       },
	};

	auto it = map.find(aSelector);
	if(it != map.end())
		_keyAction = it->second;
}

- (void)insertText:(id)aString
{
}
@end
