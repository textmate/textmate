#import "OakPasteboardChooser.h"
#import "OakPasteboard.h"
#import "OakAppKit.h"
#import "OakScopeBarView.h"
#import "OakUIConstructionFunctions.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>

@implementation OakPasteboardEntry (DisplayString)
- (NSAttributedString*)displayString
{
	static NSAttributedString* const lineJoiner = [[NSAttributedString alloc] initWithString:@"¬" attributes:@{ NSForegroundColorAttributeName : [NSColor lightGrayColor] }];
	static NSAttributedString* const tabJoiner  = [[NSAttributedString alloc] initWithString:@"‣" attributes:@{ NSForegroundColorAttributeName : [NSColor lightGrayColor] }];
	static NSAttributedString* const ellipsis   = [[NSAttributedString alloc] initWithString:@"…" attributes:@{ NSForegroundColorAttributeName : [NSColor lightGrayColor] }];

	NSMutableAttributedString* res = [[NSMutableAttributedString alloc] init];

	__block bool firstLine = true;
	[self.string enumerateLinesUsingBlock:^(NSString* line, BOOL* stop){
		if(!std::exchange(firstLine, false))
			[res appendAttributedString:lineJoiner];

		bool firstTab = true;
		for(NSString* str in [line componentsSeparatedByString:@"\t"])
		{
			if([[res string] length] > 1024)
			{
				[res appendAttributedString:ellipsis];
				*stop = YES;
				break;
			}

			if(!std::exchange(firstTab, false))
				[res appendAttributedString:tabJoiner];
			[res appendAttributedString:[[NSAttributedString alloc] initWithString:str]];
		}
	}];

	NSMutableParagraphStyle* paragraphStyle = [[NSMutableParagraphStyle alloc] init];
	[paragraphStyle setLineBreakMode:NSLineBreakByTruncatingTail];
	[res addAttributes:@{ NSParagraphStyleAttributeName : paragraphStyle } range:NSMakeRange(0, [[res string] length])];

	return res;
}
@end

#if !defined(MAC_OS_X_VERSION_10_11) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_11)
@interface OakPasteboardChooser () <NSWindowDelegate, NSTextFieldDelegate, NSTableViewDelegate>
#else
@interface OakPasteboardChooser () <NSWindowDelegate, NSTextFieldDelegate, NSTableViewDelegate, NSSearchFieldDelegate>
#endif
@property (nonatomic) OakPasteboard*        pasteboard;
@property (nonatomic) NSArrayController*    arrayController;
@property (nonatomic) NSWindow*             window;
@property (nonatomic) NSSearchField*        searchField;
@property (nonatomic) NSScrollView*         scrollView;
@property (nonatomic) NSTableView*          tableView;
@property (nonatomic) BOOL                  didFetchTableViewData;
@end

static void* kOakPasteboardChooserSelectionBinding    = &kOakPasteboardChooserSelectionBinding;
static void* kOakPasteboardChooserCurrentEntryBinding = &kOakPasteboardChooserCurrentEntryBinding;

static NSMutableDictionary* SharedChoosers;

@implementation OakPasteboardChooser
+ (instancetype)sharedChooserForName:(NSString*)aName
{
	SharedChoosers = SharedChoosers ?: [NSMutableDictionary new];
	return [SharedChoosers objectForKey:aName] ?: [[OakPasteboardChooser alloc] initWithPasteboard:[OakPasteboard pasteboardWithName:aName]];
}

- (id)initWithPasteboard:(OakPasteboard*)aPasteboard
{
	if((self = [super init]))
	{
		_pasteboard = aPasteboard;

		NSString* windowTitle = @"Clipboard History";
		NSString* actionName  = @"Paste";
		if([_pasteboard isEqual:[OakPasteboard pasteboardWithName:NSFindPboard]])
		{
			windowTitle = @"Find History";
			actionName  = @"Find Next";
		}

		_searchField = [[NSSearchField alloc] initWithFrame:NSZeroRect];
		[_searchField.cell setScrollable:YES];
		[_searchField.cell setSendsSearchStringImmediately:YES];
		_searchField.delegate = self;

		OakScopeBarView* scopeBar = [OakScopeBarView new];
		scopeBar.labels = @[ @"All", @"Starred" ];
		[scopeBar.buttons[1] setEnabled:NO];

		NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"name"];
		tableColumn.dataCell = [[NSTextFieldCell alloc] initTextCell:@""];
		[tableColumn.dataCell setLineBreakMode:NSLineBreakByTruncatingMiddle];

		NSTableView* tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
		[tableView addTableColumn:tableColumn];
		tableView.allowsTypeSelect                   = NO;
		tableView.headerView                         = nil;
		tableView.focusRingType                      = NSFocusRingTypeNone;
		tableView.allowsEmptySelection               = NO;
		tableView.allowsMultipleSelection            = NO;
		tableView.usesAlternatingRowBackgroundColors = YES;
		tableView.doubleAction                       = @selector(accept:);
		tableView.target                             = self;
		tableView.delegate                           = self;
		_tableView                                   = tableView;

		_scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		_scrollView.hasVerticalScroller   = YES;
		_scrollView.hasHorizontalScroller = NO;
		_scrollView.autohidesScrollers    = YES;
		_scrollView.borderType            = NSNoBorder;
		_scrollView.documentView          = _tableView;

		_window = [[NSPanel alloc] initWithContentRect:NSMakeRect(600, 700, 400, 500) styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSTexturedBackgroundWindowMask) backing:NSBackingStoreBuffered defer:NO];
		[_window setAutorecalculatesContentBorderThickness:NO forEdge:NSMaxYEdge];
		[_window setContentBorderThickness:32 forEdge:NSMaxYEdge];
		[[_window standardWindowButton:NSWindowMiniaturizeButton] setHidden:YES];
		[[_window standardWindowButton:NSWindowZoomButton] setHidden:YES];
		_window.autorecalculatesKeyViewLoop = YES;
		_window.delegate                    = self;
		_window.level                       = NSFloatingWindowLevel;
		_window.releasedWhenClosed          = NO;
		_window.title                       = windowTitle;

		_arrayController = [[NSArrayController alloc] init];
		_arrayController.managedObjectContext         = aPasteboard.managedObjectContext;
		_arrayController.automaticallyPreparesContent = YES;
		_arrayController.entityName                   = @"PasteboardEntry";
		_arrayController.fetchPredicate               = [NSPredicate predicateWithFormat:@"pasteboard == %@", _pasteboard];
		_arrayController.sortDescriptors              = @[ [NSSortDescriptor sortDescriptorWithKey:@"date" ascending:NO] ];

		NSButton* deleteButton   = OakCreateButton(@"Delete", NSTexturedRoundedBezelStyle);
		NSButton* clearAllButton = OakCreateButton(@"Clear All", NSTexturedRoundedBezelStyle);
		NSButton* actionButton   = OakCreateButton(actionName, NSTexturedRoundedBezelStyle);

		deleteButton.action   = @selector(deleteForward:);
		clearAllButton.action = @selector(clearAll:);
		actionButton.action   = @selector(accept:);

		NSDictionary* views = @{
			@"searchField"        : self.searchField,
			@"aboveScopeBarDark"  : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"aboveScopeBarLight" : OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.797 alpha:1], [NSColor colorWithCalibratedWhite:0.912 alpha:1]),
			@"scopeBar"           : scopeBar,
			@"topDivider"         : OakCreateHorizontalLine([NSColor darkGrayColor], [NSColor colorWithCalibratedWhite:0.551 alpha:1]),
			@"scrollView"         : self.scrollView,
			@"bottomDivider"      : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"delete"             : deleteButton,
			@"clearAll"           : clearAllButton,
			@"action"             : actionButton,
		};

		NSView* contentView = self.window.contentView;
		OakAddAutoLayoutViewsToSuperview([views allValues], contentView);

		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[searchField(>=50)]-(8)-|"                      options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[aboveScopeBarDark(==aboveScopeBarLight)]|"          options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[scopeBar]-(>=8)-|"                             options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView(==topDivider,==bottomDivider)]|"         options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[delete]-[clearAll]-(>=20)-[action]-(8)-|"     options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(2)-[searchField]-(8)-[aboveScopeBarDark][aboveScopeBarLight]-(3)-[scopeBar]-(4)-[topDivider][scrollView(>=50)][bottomDivider]-(5)-[clearAll]-(6)-|" options:0 metrics:nil views:views]];

		_window.defaultButtonCell = actionButton.cell;

		NSResponder* nextResponder = [_tableView nextResponder];
		[_tableView setNextResponder:self];
		[self setNextResponder:nextResponder];

		[deleteButton bind:NSEnabledBinding toObject:_arrayController withKeyPath:@"canRemove" options:nil];
		[actionButton bind:NSEnabledBinding toObject:_arrayController withKeyPath:@"canRemove" options:nil];
		[tableColumn bind:NSValueBinding toObject:_arrayController withKeyPath:@"arrangedObjects.displayString" options:nil];
		[_pasteboard addObserver:self forKeyPath:@"currentEntry" options:0 context:kOakPasteboardChooserCurrentEntryBinding];
		[_arrayController addObserver:self forKeyPath:@"selection" options:0 context:kOakPasteboardChooserSelectionBinding];
	}
	return self;
}

- (void)dealloc
{
	[_arrayController removeObserver:self forKeyPath:@"selection" context:kOakPasteboardChooserSelectionBinding];
	[_pasteboard removeObserver:self forKeyPath:@"currentEntry" context:kOakPasteboardChooserCurrentEntryBinding];
	[[[_tableView tableColumns] lastObject] unbind:NSValueBinding];

	_window.delegate    = nil;
	_tableView.delegate = nil;
	_tableView.target   = nil;
}

- (void)showWindow:(id)sender
{
	[SharedChoosers setObject:self forKey:_pasteboard.name];

	[_searchField bind:NSValueBinding toObject:self withKeyPath:@"filterString" options:nil];
	[_arrayController fetch:self];
	[self performSelector:@selector(arrayControllerDidFinishInitialFetch:) withObject:nil afterDelay:0];
}

- (void)arrayControllerDidFinishInitialFetch:(id)sender
{
	if(_pasteboard.currentEntry)
	{
		_arrayController.selectedObjects = @[ _pasteboard.currentEntry ];
		[_tableView scrollRowToVisible:[_tableView selectedRow]];
	}
	[_window makeFirstResponder:_tableView];
	[_window makeKeyAndOrderFront:self];
	self.didFetchTableViewData = YES;
}

- (void)showWindowRelativeToFrame:(NSRect)parentFrame
{
	if(![_window isVisible])
	{
		[_window layoutIfNeeded];
		NSRect frame  = [_window frame];
		NSRect parent = parentFrame;

		frame.origin.x = NSMinX(parent) + round((NSWidth(parent)  - NSWidth(frame))  * 1 / 4);
		frame.origin.y = NSMinY(parent) + round((NSHeight(parent) - NSHeight(frame)) * 3 / 4);
		[_window setFrame:frame display:NO];
	}
	[self showWindow:self];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[_searchField unbind:NSValueBinding];
	[SharedChoosers performSelector:@selector(removeObjectForKey:) withObject:_pasteboard.name afterDelay:0];
}

- (void)close
{
	[_window performClose:self];
}

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)anObject change:(NSDictionary*)aDictionary context:(void*)aContext
{
	if(aContext == kOakPasteboardChooserSelectionBinding)
	{
		if(self.didFetchTableViewData)
			_pasteboard.currentEntry = [_arrayController.selectedObjects firstObject];
	}
	else if(aContext == kOakPasteboardChooserCurrentEntryBinding)
	{
		_arrayController.selectedObjects = _pasteboard.currentEntry ? @[ _pasteboard.currentEntry ] : @[ ];
		[_tableView scrollRowToVisible:[_tableView selectedRow]];
	}
	else
	{
		[super observeValueForKeyPath:aKeyPath ofObject:anObject change:aDictionary context:aContext];
	}
}

- (void)setFilterString:(NSString*)newString
{
	if(_filterString == newString || [_filterString isEqualToString:newString])
		return;

	[self willChangeValueForKey:@"filterString"];
	_filterString = newString;
	[self didChangeValueForKey:@"filterString"];

	if(OakIsEmptyString(_filterString))
			_arrayController.fetchPredicate = [NSPredicate predicateWithFormat:@"pasteboard == %@", _pasteboard];
	else	_arrayController.fetchPredicate = [NSPredicate predicateWithFormat:@"pasteboard == %@ AND string LIKE[nc] %@", _pasteboard, [NSString stringWithFormat:@"*%@*", _filterString]];
	[_arrayController fetch:self];
}

// ========================
// = NSTableView Delegate =
// ========================

- (void)tableView:(NSTableView*)aTableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if([aCell backgroundStyle] == NSBackgroundStyleDark && [[aTableColumn identifier] isEqualToString:@"name"])
	{
		id obj = [aCell objectValue];
		if([obj isKindOfClass:[NSAttributedString class]])
		{
			NSMutableAttributedString* str = [obj mutableCopy];
			[str addAttribute:NSForegroundColorAttributeName value:[NSColor alternateSelectedControlTextColor] range:NSMakeRange(0, [str length])];
			[aCell setAttributedStringValue:str];
		}
	}
}

// =================
// = Action Method =
// =================

- (IBAction)orderFrontFindPanel:(id)sender { [_window makeFirstResponder:_searchField]; }
- (IBAction)findAllInSelection:(id)sender  { [_window makeFirstResponder:_searchField]; }

- (void)accept:(id)sender
{
	[_window orderOut:self];
	if(_action)
		[NSApp sendAction:_action to:_target from:self];
	[_window close];
}

- (void)cancel:(id)sender
{
	[self close];
}

- (void)clearAll:(id)sender
{
	NSFetchRequest* request = [NSFetchRequest fetchRequestWithEntityName:@"PasteboardEntry"];
	request.predicate = [NSPredicate predicateWithFormat:@"pasteboard = %@", _pasteboard];
	NSManagedObjectContext* managedObjectContext = _pasteboard.managedObjectContext;
	for(OakPasteboardEntry* entry in [managedObjectContext executeFetchRequest:request error:nullptr])
	{
		if(entry != _pasteboard.currentEntry)
			[managedObjectContext deleteObject:entry];
	}
}

- (void)deleteForward:(id)sender
{
	[_arrayController remove:sender];
}

- (void)deleteBackward:(id)sender
{
	NSArray* entries = [_arrayController selectedObjects];
	[_arrayController selectPrevious:self];
	[_arrayController removeObjects:entries];
}

- (void)insertTab:(id)sender
{
	[_window selectNextKeyView:self];
}

- (void)insertBacktab:(id)sender
{
	[_window selectPreviousKeyView:self];
}

- (void)insertText:(id)aString
{
	self.filterString = aString;
	[_window makeFirstResponder:_searchField];
	NSText* fieldEditor = (NSText*)[_window firstResponder];
	if([fieldEditor isKindOfClass:[NSText class]])
		[fieldEditor setSelectedRange:NSMakeRange([[fieldEditor string] length], 0)];
}

- (void)keyDown:(NSEvent*)anEvent
{
	[self interpretKeyEvents:@[ anEvent ]];
}

// ========================
// = NSTextField Delegate =
// ========================

- (BOOL)control:(NSControl*)aControl textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aCommand
{
	static auto const forward = new std::set<SEL>{ @selector(moveUp:), @selector(moveDown:), @selector(moveUpAndModifySelection:), @selector(moveDownAndModifySelection:), @selector(pageUp:), @selector(pageDown:), @selector(movePageUp:), @selector(movePageDown:), @selector(scrollPageUp:), @selector(scrollPageDown:), @selector(moveToBeginningOfDocument:), @selector(moveToEndOfDocument:), @selector(scrollToBeginningOfDocument:), @selector(scrollToEndOfDocument:), @selector(insertNewline:), @selector(insertNewlineIgnoringFieldEditor:), @selector(cancelOperation:) };
	if(forward->find(aCommand) != forward->end() && [self respondsToSelector:aCommand])
		return [NSApp sendAction:aCommand to:self from:aControl];
	return NO;
}

- (void)moveSelectedRowByOffset:(NSInteger)anOffset extendingSelection:(BOOL)extend
{
	if([_tableView numberOfRows])
	{
		if(_tableView.allowsMultipleSelection == NO)
			extend = NO;
		NSInteger row = oak::cap((NSInteger)0, [_tableView selectedRow] + anOffset, [_tableView numberOfRows] - 1);
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:extend];
		[_tableView scrollRowToVisible:row];
	}
}

- (int)visibleRows                                      { return (int)floor(NSHeight([_tableView visibleRect]) / ([_tableView rowHeight]+[_tableView intercellSpacing].height)) - 1; }

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
- (void)scrollToBeginningOfDocument:(id)sender          { [self moveToBeginningOfDocument:sender]; }
- (void)scrollToEndOfDocument:(id)sender                { [self moveToEndOfDocument:sender]; }

- (IBAction)insertNewline:(id)sender                    { [NSApp sendAction:@selector(accept:) to:nil from:sender]; }
- (IBAction)insertNewlineIgnoringFieldEditor:(id)sender { [NSApp sendAction:@selector(accept:) to:nil from:sender]; }
- (IBAction)cancelOperation:(id)sender                  { [NSApp sendAction:@selector(cancel:) to:nil from:sender]; }
@end
