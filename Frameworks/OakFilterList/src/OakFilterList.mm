#import "OakFilterList.h"
#import "OakFilterListView.h"
#import <oak/oak.h>

@interface OakFilterWindowController ()
- (void)sendAction:(id)sender;
- (IBAction)accept:(id)sender;
- (IBAction)cancel:(id)sender;
@property (nonatomic, retain) NSView* filterControls;
@property (nonatomic, retain) OakFilterWindowController* retainedSelf;
@end

@implementation OakFilterWindowController
- (id)init
{
	if(self = [super initWithWindowNibName:@"FilterWindow"])
	{
		self.shouldCascadeWindows = NO;
	}
	return self;
}

- (void)windowDidLoad
{
	filterView.target       = self;
	filterView.action       = @selector(singleClick:);
	filterView.doubleAction = @selector(accept:);

	self.retainedSelf = self;
}

- (void)showWindowRelativeToWindow:(NSWindow*)parentWindow
{
	if(parentWindow && ![self.window isVisible])
	{
		NSRect frame  = [self.window frame];
		NSRect parent = [parentWindow frame];

		frame.origin.x = round(NSMidX(parent) - 0.5 * NSWidth(frame));
		frame.origin.y = NSMinY(parent) + round((NSHeight(parent) - NSHeight(frame)) * 3 / 4);
		[self.window setFrame:frame display:NO];
	}

	[self.window makeKeyAndOrderFront:self];
}

- (id)dataSource
{
	return filterView.filterDataSource;
}

- (NSView*)filterControls
{
	return filterControlsView;
}

- (void)setFilterControls:(NSView*)newControlsView
{
	if(!newControlsView)
		return [filterControlsView removeFromSuperview];

	NSRect filterViewFrame                = filterView.enclosingScrollView.frame;
	filterViewFrame.size.height          += NSHeight(self.filterControls.frame) - NSHeight(newControlsView.frame);
	filterView.enclosingScrollView.frame  = filterViewFrame;

	NSRect controlsFrame             = newControlsView.frame;
	controlsFrame.size.width         = NSWidth(self.filterControls.frame);
	controlsFrame.origin.y           = NSMaxY(filterView.enclosingScrollView.frame);
	newControlsView.frame            = controlsFrame;
	newControlsView.autoresizingMask = NSViewWidthSizable|NSViewMinYMargin;

	[self.window.contentView replaceSubview:self.filterControls with:newControlsView];

	filterControlsView = newControlsView;
}

- (void)setDataSource:(id)dataSource
{
	[(NSObject*)filterView.filterDataSource removeObserver:self forKeyPath:@"title"];

	NSWindow* window = [self window]; // loads filterView from nib

	filterView.filterDataSource = dataSource;
	self.accessoryAction        = _accessoryAction; // trigger accessory button creation if necessary

	if(dataSource)
		[window setFrameAutosaveName:[NSString stringWithFormat:@"Filter Window %@ Saved Frame", [dataSource className]]];

	[dataSource addObserver:self forKeyPath:@"title" options:NSKeyValueObservingOptionInitial context:NULL];

	self.filterControls = [dataSource viewController].view;

	[window.contentView setNextResponder:[dataSource viewController]];
	[[dataSource viewController] setNextResponder:self];
	if([[dataSource viewController] respondsToSelector:@selector(setSearchFieldDelegate:)])
		[[dataSource viewController] performSelector:@selector(setSearchFieldDelegate:) withObject:self];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	self.window.title = [object title];
}

- (void)setSendActionOnSingleClick:(BOOL)newSendActionOnSingleClick
{
	_sendActionOnSingleClick = newSendActionOnSingleClick;
	[(NSPanel*)self.window setBecomesKeyOnlyIfNeeded:_sendActionOnSingleClick];
}

- (BOOL)allowsMultipleSelection
{
	return filterView.allowsMultipleSelection;
}

- (void)setAllowsMultipleSelection:(BOOL)newAllowsMultipleSelection
{
	filterView.allowsMultipleSelection = newAllowsMultipleSelection;
}

- (NSArray*)selectedItems
{
	return filterView.selectedItems;
}

- (void)setTarget:(id)newTarget
{
	if(_target != newTarget)
	{
		_target = newTarget;
		self.accessoryAction = _accessoryAction; // update accessory button target
	}
}

- (void)setAccessoryAction:(SEL)selector
{
	_accessoryAction = selector;
	if(_accessoryAction && [filterView.filterDataSource respondsToSelector:@selector(accessoryButton)])
	{
		NSButtonCell* button = [filterView.filterDataSource accessoryButton];
		[button setAction:_accessoryAction];
		[button setTarget:self.target];
		filterView.accessoryButton = button;
	}
	else
	{
		filterView.accessoryButton = nil;
	}
}

// =========================
// = Search Field Delegate =
// =========================

- (void)controlTextDidChange:(NSNotification*)aNotification
{
	NSSearchField* searchField = [aNotification object];
	[searchField sendAction:searchField.action to:searchField.target];
}

- (void)moveSelectedRowByOffset:(NSInteger)anOffset extendingSelection:(BOOL)extend
{
	if([filterView numberOfRows])
	{
		if(filterView.allowsMultipleSelection == NO)
			extend = NO;
		NSInteger row = oak::cap((NSInteger)0, [filterView selectedRow] + anOffset, [filterView numberOfRows] - 1);
		[filterView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:extend];
		[filterView scrollRowToVisible:row];
	}
}

- (int)visibleRows                                      { return (int)floorf(NSHeight([filterView visibleRect]) / ([filterView rowHeight]+[filterView intercellSpacing].height)) - 1; }

- (void)moveUp:(id)sender                               { [self moveSelectedRowByOffset:-1 extendingSelection:NO]; }
- (void)moveDown:(id)sender                             { [self moveSelectedRowByOffset:+1 extendingSelection:NO]; }
- (void)moveUpAndModifySelection:(id)sender             { [self moveSelectedRowByOffset:-1 extendingSelection:YES];}
- (void)moveDownAndModifySelection:(id)sender           { [self moveSelectedRowByOffset:+1 extendingSelection:YES];}
- (void)movePageUp:(id)sender                           { [self moveSelectedRowByOffset:-[self visibleRows] extendingSelection:NO]; }
- (void)movePageDown:(id)sender                         { [self moveSelectedRowByOffset:+[self visibleRows] extendingSelection:NO]; }
- (void)moveToBeginningOfDocument:(id)sender            { [self moveSelectedRowByOffset:-(INT_MAX >> 1) extendingSelection:NO]; }
- (void)moveToEndOfDocument:(id)sender                  { [self moveSelectedRowByOffset:+(INT_MAX >> 1) extendingSelection:NO]; }

- (void)pageUp:(id)sender                               { [self movePageUp:sender]; }
- (void)pageDown:(id)sender                             { [self movePageDown:sender]; }
- (void)scrollPageUp:(id)sender                         { [self movePageUp:sender]; }
- (void)scrollPageDown:(id)sender                       { [self movePageDown:sender]; }

- (IBAction)insertNewline:(id)sender                    { [self accept:sender]; }
- (IBAction)insertNewlineIgnoringFieldEditor:(id)sender { [self accept:sender]; }
- (IBAction)cancelOperation:(id)sender                  { [self cancel:sender]; }

- (BOOL)control:(NSControl*)aControl textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aCommand
{
	static std::set<SEL> const forward = { @selector(moveUp:), @selector(moveDown:), @selector(moveUpAndModifySelection:), @selector(moveDownAndModifySelection:), @selector(pageUp:), @selector(pageDown:), @selector(movePageUp:), @selector(movePageDown:), @selector(scrollPageUp:), @selector(scrollPageDown:), @selector(moveToBeginningOfDocument:), @selector(moveToEndOfDocument:), @selector(insertNewline:), @selector(insertNewlineIgnoringFieldEditor:), @selector(cancelOperation:) };
	if(forward.find(aCommand) != forward.end() && [self respondsToSelector:aCommand])
		return [NSApp sendAction:aCommand to:self from:aControl];
	return NO;
}

// ===========
// = Actions =
// ===========

- (void)windowWillClose:(NSNotification*)aNotification
{
	filterView.target = nil;
	filterView.accessoryButton.target = nil;
	self.dataSource = nil;
	self.retainedSelf = nil;
}

- (IBAction)cancel:(id)sender
{
	// Make the window behind the filter list key, so keystrokes sent during the fade out animation are not lost
	// Panels are not included in the orderedWindows list, so we make the first object key
	if([NSApp orderedWindows].count > 0)
		[[[NSApp orderedWindows] objectAtIndex:0] makeKeyWindow];

	NSWindow* window = [self window];
	[window setFrameAutosaveName:@""];

	CAAnimation* anim = [CABasicAnimation animation];
	[anim setDuration:([[NSApp currentEvent] modifierFlags] & NSShiftKeyMask) ? 3.0 : 0.20];
	[anim setDelegate:self];
	[window setAnimations:@{ @"alphaValue" : anim, @"frame" : anim }];

	[CATransaction begin];
	[[window animator] setFrame:NSOffsetRect(window.frame, 0, -10) display:NO animate:YES];
	[[window animator] setAlphaValue:0];
	[CATransaction commit];
}

- (void)animationDidStop:(CAAnimation*)animation finished:(BOOL)flag
{
	[[self window] setAnimations:nil];
	[[self window] close];
}

- (IBAction)accept:(id)sender
{
	OakFilterWindowController* retainedSelf = self;
	[retainedSelf.window orderOut:self];
	[retainedSelf sendAction:nil];
	[retainedSelf.window close];
}

- (IBAction)singleClick:(id)sender
{
	if(!_sendActionOnSingleClick)
		return;

	if([[[filterView.tableColumns objectAtIndex:filterView.clickedColumn] identifier] isEqualToString:@"accessoryColumn"])
		return;

	if(self.action)
		[NSApp sendAction:self.action to:self.target from:self];
}

- (void)sendAction:(id)sender
{
	[filterView waitForAllItems];
	[[self.dataSource viewController] commitEditing];
	[filterView makeSelectedItemsBestMatch];
	if(self.action)
		[NSApp sendAction:self.action to:self.target from:self];
}

- (IBAction)goToParentFolder:(id)sender
{
	if([self.target respondsToSelector:_cmd])
		[NSApp sendAction:_cmd to:self.target from:sender];
}

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	if([item action] == @selector(goToParentFolder:))
			return [self.target respondsToSelector:_cmd];
	else	return YES;
}
@end
