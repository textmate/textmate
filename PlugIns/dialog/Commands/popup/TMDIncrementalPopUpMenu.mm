//
//  TMDIncrementalPopUpMenu.mm
//
//  Created by Joachim Mårtensson on 2007-08-10.
//

#import "TMDIncrementalPopUpMenu.h"
#import "../Utilities/TextMate.h" // -insertSnippetWithOptions
#import "../../TMDCommand.h" // -writeString:
#import "../../Dialog2.h"

@interface NSTableView (MovingSelectedRow)
- (BOOL)TMDcanHandleEvent:(NSEvent*)anEvent;
@end

@implementation NSTableView (MovingSelectedRow)
- (BOOL)TMDcanHandleEvent:(NSEvent*)anEvent
{
	if([anEvent type] != NSEventTypeKeyDown || [[anEvent characters] length] != 1)
		return NO;

	int visibleRows = (int)floorf(NSHeight([self visibleRect]) / ([self rowHeight]+[self intercellSpacing].height)) - 1;
	struct { unichar key; int rows; } const keyMovements[] =
	{
		{ NSUpArrowFunctionKey,              -1 },
		{ NSDownArrowFunctionKey,            +1 },
		{ NSPageUpFunctionKey,     -visibleRows },
		{ NSPageDownFunctionKey,   +visibleRows },
		{ NSHomeFunctionKey,    -(INT_MAX >> 1) },
		{ NSEndFunctionKey,     +(INT_MAX >> 1) },
	};

	unichar keyCode = [[anEvent characters] characterAtIndex:0];
	for(auto const& keyMovement : keyMovements)
	{
		if(keyCode == keyMovement.key)
		{
			int row = std::max<NSInteger>(0, std::min([self selectedRow] + keyMovement.rows, [self numberOfRows]-1));
			[self selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
			[self scrollRowToVisible:row];

			return YES;
		}
	}

	return NO;
}
@end

@interface TMDIncrementalPopUpMenu ()
{
	NSFileHandle* outputHandle;
	NSArray* suggestions;
	NSMutableString* mutablePrefix;
	NSString* staticPrefix;
	NSArray* filtered;
	NSTableView* theTableView;
	BOOL isAbove;
	BOOL closeMe;
	BOOL caseSensitive;

	NSMutableCharacterSet* textualInputCharacters;
}

- (NSRect)rectOfMainScreen;
- (NSString*)filterString;
- (void)setupInterface;
- (void)filter;
- (void)insertCommonPrefix;
- (void)completeAndInsertSnippet;
@end

@implementation TMDIncrementalPopUpMenu
// =============================
// = Setup/tear-down functions =
// =============================
- (id)init
{
	if(self = [super initWithContentRect:NSMakeRect(0, 0, 1, 1) styleMask:NSWindowStyleMaskBorderless backing:NSBackingStoreBuffered defer:NO])
	{
		mutablePrefix = [NSMutableString new];
		textualInputCharacters = [NSMutableCharacterSet alphanumericCharacterSet];
		caseSensitive = YES;

		[self setupInterface];
	}
	return self;
}

- (id)initWithItems:(NSArray*)someSuggestions alreadyTyped:(NSString*)aUserString staticPrefix:(NSString*)aStaticPrefix additionalWordCharacters:(NSString*)someAdditionalWordCharacters caseSensitive:(BOOL)isCaseSensitive writeChoiceToFileDescriptor:(NSFileHandle*)aFileDescriptor
{
	if(self = [self init])
	{
		suggestions = someSuggestions;

		if(aUserString)
			[mutablePrefix appendString:aUserString];

		if(aStaticPrefix)
			staticPrefix = aStaticPrefix;

		if(someAdditionalWordCharacters)
			[textualInputCharacters addCharactersInString:someAdditionalWordCharacters];

		caseSensitive = isCaseSensitive;
		outputHandle = aFileDescriptor;
	}
	return self;
}

- (void)setCaretPos:(NSPoint)aPos
{
	_caretPos = aPos;
	isAbove = NO;

	NSRect mainScreen = [self rectOfMainScreen];

	CGFloat offx = (_caretPos.x/mainScreen.size.width) + 1.0;
	if((_caretPos.x + [self frame].size.width) > (mainScreen.size.width*offx))
		_caretPos.x = _caretPos.x - [self frame].size.width;

	if(_caretPos.y>=0 && _caretPos.y<[self frame].size.height)
	{
		_caretPos.y = _caretPos.y + ([self frame].size.height + [NSUserDefaults.standardUserDefaults integerForKey:@"OakTextViewNormalFontSize"]*1.5);
		isAbove = YES;
	}
	if(_caretPos.y<0 && (mainScreen.size.height-[self frame].size.height)<(_caretPos.y*-1.0))
	{
		_caretPos.y = _caretPos.y + ([self frame].size.height + [NSUserDefaults.standardUserDefaults integerForKey:@"OakTextViewNormalFontSize"]*1.5);
		isAbove = YES;
	}
	[self setFrameTopLeftPoint:_caretPos];
}

- (void)setupInterface
{
	// Since we are relying on `setReleaseWhenClosed:`, we need to ensure that we are over-retained.
	CFBridgingRetain(self);
	[self setReleasedWhenClosed:YES];
	[self setLevel:NSStatusWindowLevel];
	[self setHidesOnDeactivate:YES];
	[self setHasShadow:YES];

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	[scrollView setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
	[scrollView setAutohidesScrollers:YES];
	[scrollView setHasVerticalScroller:YES];
	[[scrollView verticalScroller] setControlSize:NSControlSizeSmall];

	theTableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
	[theTableView setFocusRingType:NSFocusRingTypeNone];
	[theTableView setAllowsEmptySelection:NO];
	[theTableView setHeaderView:nil];
	//[theTableView setBackgroundColor:[NSColor blackColor]];
	[theTableView setDoubleAction:@selector(didDoubleClickRow:)];
	[theTableView setTarget:self];

	if(@available(macos 11.0, *))
		theTableView.style = NSTableViewStylePlain;

	NSTableColumn* column = [[NSTableColumn alloc] initWithIdentifier:@"display"];
	[theTableView addTableColumn:column];

	[theTableView setDataSource:self];
	[theTableView setDelegate:self];
	[scrollView setDocumentView:theTableView];

	if(@available(macos 10.14, *))
	{
		theTableView.backgroundColor = NSColor.clearColor;
		scrollView.drawsBackground   = NO;

		NSVisualEffectView* effectView = [[NSVisualEffectView alloc] initWithFrame:NSZeroRect];
		effectView.material         = NSVisualEffectMaterialMenu;
		effectView.blendingMode     = NSVisualEffectBlendingModeBehindWindow;
		effectView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
		[effectView addSubview:scrollView positioned:NSWindowBelow relativeTo:nil];

		[self setContentView:effectView];
	}
	else
	{
		[self setContentView:scrollView];
	}
}

//- (void)tableView:(NSTableView*)aTableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex {
//	[aCell setTextColor:[NSColor blueColor]];
//}


// ========================
// = TableView DataSource =
// ========================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return [filtered count];
}

// ======================
// = TableView Delegate =
// ======================

- (NSView*)tableView:(NSTableView*)tableView viewForTableColumn:(NSTableColumn*)tableColumn row:(NSInteger)row
{
	id object = [filtered objectAtIndex:row];
	NSString* identifier = [object objectForKey:@"display"];
	NSTableCellView* cell = [tableView makeViewWithIdentifier:identifier owner:self];
	if(!cell)
	{
		cell = [[NSTableCellView alloc] initWithFrame:NSZeroRect];
		NSImageView* image = [[NSImageView alloc] initWithFrame:NSZeroRect];
		cell.imageView = image;
		[cell addSubview:image];
		NSTextField* text = [[NSTextField alloc] initWithFrame:NSMakeRect(0, 0, 1, 1)];
		text.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
		text.bordered = NO;
		text.drawsBackground = NO;
		text.editable = NO;
		text.lineBreakMode = NSLineBreakByTruncatingTail;
		text.textColor = NSColor.controlTextColor;
		cell.textField = text;
		[cell addSubview:text];
	}
	cell.textField.stringValue = identifier;
	cell.imageView.image = [NSImage imageNamed:[object objectForKey:@"image"]];
	if(cell.imageView.image)
	{
		CGFloat h = tableView.rowHeight;
		cell.imageView.frame = NSMakeRect(0, 0, h, h);
		cell.textField.frame = NSMakeRect(h + 3, 0, 1, 1);
	}
	cell.toolTip = [object objectForKey:@"tooltip"];
	return cell;
}

// ====================
// = Filter the items =
// ====================

- (void)filter
{
	NSRect mainScreen = [self rectOfMainScreen];

	NSArray* newFiltered;
	NSArray* itemsWithChildren;
	if([mutablePrefix length] > 0)
	{
		NSPredicate* matchesFilter;
		NSPredicate* hasChildren;

		if(caseSensitive)
				matchesFilter = [NSPredicate predicateWithFormat:@"match BEGINSWITH %@ OR (match == NULL AND display BEGINSWITH %@)", [self filterString], [self filterString]];
		else	matchesFilter = [NSPredicate predicateWithFormat:@"match BEGINSWITH[c] %@ OR (match == NULL AND display BEGINSWITH[c] %@)", [self filterString], [self filterString]];

		newFiltered = [suggestions filteredArrayUsingPredicate:matchesFilter];
		if([newFiltered count] == 1)
		{
			newFiltered = [newFiltered arrayByAddingObjectsFromArray:[[newFiltered lastObject] objectForKey:@"children"]];
		}
		else if([newFiltered count] == 0)
		{
			hasChildren =  [NSPredicate predicateWithFormat:@"children != NULL"];
			itemsWithChildren = [suggestions filteredArrayUsingPredicate:hasChildren];
			for(NSUInteger i = 0; i < [itemsWithChildren count]; i++)
			{
				newFiltered=[newFiltered arrayByAddingObjectsFromArray:[[[itemsWithChildren objectAtIndex:i] objectForKey:@"children"] filteredArrayUsingPredicate:matchesFilter]];
			}
		}
	}
	else
	{
		newFiltered = suggestions;
	}


	filtered = newFiltered;
	[theTableView reloadData];

	NSPoint old = NSMakePoint([self frame].origin.x, [self frame].origin.y + [self frame].size.height);

	NSUInteger displayedRows = [newFiltered count] < MAX_ROWS ? [newFiltered count] : MAX_ROWS;
	CGFloat newHeight   = ([theTableView rowHeight] + [theTableView intercellSpacing].height) * displayedRows;

	CGFloat maxWidth = 60;
	if([newFiltered count]>0)
	{
		for(NSUInteger i = 0; i < theTableView.numberOfRows; ++i)
			maxWidth = MAX(maxWidth, [self widthAtColumn:0 row:i]);
		maxWidth = MIN(maxWidth, 600);
	}
	if(_caretPos.y>=0 && (isAbove || _caretPos.y<newHeight))
	{
		isAbove = YES;
		old.y = _caretPos.y + (newHeight + [NSUserDefaults.standardUserDefaults integerForKey:@"OakTextViewNormalFontSize"]*1.5);
	}
	if(_caretPos.y<0 && (isAbove || (mainScreen.size.height-newHeight)<(_caretPos.y*-1.0)))
	{
		old.y = _caretPos.y + (newHeight + [NSUserDefaults.standardUserDefaults integerForKey:@"OakTextViewNormalFontSize"]*1.5);
	}

	// newHeight is currently the new height for theTableView, but we need to resize the whole window
	// so here we use the difference in height to find the new height for the window
	// newHeight = [[self contentView] frame].size.height + (newHeight - [theTableView frame].size.height);
	[self setFrame:NSMakeRect(old.x,old.y-newHeight,maxWidth,newHeight) display:YES];
}

// =========================
// = Convenience functions =
// =========================

- (NSString*)filterString
{
	return staticPrefix ? [staticPrefix stringByAppendingString:mutablePrefix] : mutablePrefix;
}

- (NSRect)rectOfMainScreen
{
	NSRect mainScreen = [[NSScreen mainScreen] frame];
	for(NSScreen* candidate in [NSScreen screens])
	{
		if(NSMinX([candidate frame]) == 0 && NSMinY([candidate frame]) == 0)
			mainScreen = [candidate frame];
	}
	return mainScreen;
}

- (CGFloat)widthAtColumn:(NSUInteger)column row:(NSUInteger)row
{
	CGFloat const kTableViewPadding = 6;
	CGFloat const kTableCellPadding = 3;
	NSTableCellView* cell = [theTableView viewAtColumn:column row:row makeIfNecessary:YES];
	CGFloat width = kTableViewPadding + ceil(cell.textField.attributedStringValue.size.width);
	if(cell.imageView.image)
		width += kTableCellPadding + ceil(cell.imageView.image.size.width);
	return width;
}

// =============================
// = Run the actual popup-menu =
// =============================

- (void)orderFront:(id)sender
{
	[self filter];
	[super orderFront:sender];
	[self performSelector:@selector(watchUserEvents) withObject:nil afterDelay:0.05];
}

- (void)watchUserEvents
{
	closeMe = NO;
	while(!closeMe)
	{
		NSEvent* event = [NSApp nextEventMatchingMask:NSEventMaskAny untilDate:[NSDate distantFuture] inMode:NSDefaultRunLoopMode dequeue:YES];

		if(!event)
			continue;

		NSEventType t = [event type];
		if([theTableView TMDcanHandleEvent:event])
		{
			// skip the rest
		}
		else if(t == NSEventTypeKeyDown)
		{
			unsigned int flags = [event modifierFlags];
			unichar key        = [[event characters] length] == 1 ? [[event characters] characterAtIndex:0] : 0;
			if((flags & NSEventModifierFlagControl) || (flags & NSEventModifierFlagOption) || (flags & NSEventModifierFlagCommand))
			{
				[NSApp sendEvent:event];
				break;
			}
			else if([event keyCode] == 53) // escape
			{
				break;
			}
			else if(key == NSCarriageReturnCharacter)
			{
				if([filtered count] == 0)
				{
					[NSApp sendEvent:event];
					break;
				}
				else
				{
					[self completeAndInsertSnippet];
				}
			}
			else if(key == NSBackspaceCharacter || key == NSDeleteCharacter)
			{
				[NSApp sendEvent:event];
				if([mutablePrefix length] == 0)
					break;

				[mutablePrefix deleteCharactersInRange:NSMakeRange([mutablePrefix length]-1, 1)];
				[self filter];
			}
			else if(key == NSTabCharacter)
			{
				if([filtered count] == 0)
				{
					[NSApp sendEvent:event];
					break;
				}
				else if([filtered count] == 1)
				{
					[self completeAndInsertSnippet];
				}
				else
				{
					[self insertCommonPrefix];
				}
			}
			else if([textualInputCharacters characterIsMember:key])
			{
				[NSApp sendEvent:event];
				[mutablePrefix appendString:[event characters]];
				[self filter];
			}
			else
			{
				[NSApp sendEvent:event];
				break;
			}
		}
		else if(t == NSEventTypeRightMouseDown || t == NSEventTypeLeftMouseDown)
		{
			[NSApp sendEvent:event];
			if(!NSPointInRect([NSEvent mouseLocation], [self frame]))
				break;
		}
		else if(t == NSEventTypeScrollWheel)
		{
			[self sendEvent:event];
		}
		else
		{
			[NSApp sendEvent:event];
		}
	}
	[self close];
}

- (void)didDoubleClickRow:(id)sender
{
	[self completeAndInsertSnippet];
}

// ==================
// = Action methods =
// ==================

- (void)insertCommonPrefix
{
	NSInteger row = [theTableView selectedRow];
	if(row == -1)
		return;

	id cur = [filtered objectAtIndex:row];
	NSString* curMatch = [cur objectForKey:@"match"] ?: [cur objectForKey:@"display"];
	if([[self filterString] length] + 1 < [curMatch length])
	{
		NSString* prefix = [curMatch substringToIndex:[[self filterString] length] + 1];
		NSMutableArray* candidates = [NSMutableArray array];
		for(NSUInteger i = row; i < [filtered count]; ++i)
		{
			id candidate = [filtered objectAtIndex:i];
			NSString* candidateMatch = [candidate objectForKey:@"match"] ?: [candidate objectForKey:@"display"];
			if([candidateMatch hasPrefix:prefix])
				[candidates addObject:candidateMatch];
		}

		NSString* commonPrefix = curMatch;
		for(NSString* candidateMatch in candidates)
			commonPrefix = [commonPrefix commonPrefixWithString:candidateMatch options:NSLiteralSearch];

		if([[self filterString] length] < [commonPrefix length])
		{
			NSString* toInsert = [commonPrefix substringFromIndex:[[self filterString] length]];
			[mutablePrefix appendString:toInsert];
			insert_text(toInsert);
			[self filter];
		}
	}
	else
	{
		[self completeAndInsertSnippet];
	}
}

- (void)completeAndInsertSnippet
{
	if([theTableView selectedRow] == -1)
		return;

	NSMutableDictionary* selectedItem = [[filtered objectAtIndex:[theTableView selectedRow]] mutableCopy];

	NSString* candidateMatch = [selectedItem objectForKey:@"match"] ?: [selectedItem objectForKey:@"display"];
	if([[self filterString] length] < [candidateMatch length])
		insert_text([candidateMatch substringFromIndex:[[self filterString] length]]);

	if(outputHandle)
	{
		// We want to return the index of the selected item into the array which was passed in,
		// but we can’t use the selected row index as the contents of the tablview is filtered down.
		[selectedItem setObject:[NSNumber numberWithUnsignedInteger:[suggestions indexOfObject:[filtered objectAtIndex:[theTableView selectedRow]]]] forKey:@"index"];
		[TMDCommand writePropertyList:selectedItem toFileHandle:outputHandle];
	}
	else if(NSString* toInsert = [selectedItem objectForKey:@"insert"])
	{
		insert_snippet(toInsert);
	}

	closeMe = YES;
}
@end
