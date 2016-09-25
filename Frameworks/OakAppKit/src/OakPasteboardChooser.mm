#import "OakPasteboardChooser.h"
#import "OakPasteboard.h"
#import "OakAppKit.h"
#import "OakScopeBarView.h"
#import "OakUIConstructionFunctions.h"
#import "OakSyntaxFormatter.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>

@implementation OakPasteboardEntry (DisplayString)
- (NSAttributedString*)displayString
{
	static NSAttributedString* const lineJoiner = [[NSAttributedString alloc] initWithString:@"¬" attributes:@{ NSForegroundColorAttributeName : [NSColor lightGrayColor] }];
	static NSAttributedString* const tabJoiner  = [[NSAttributedString alloc] initWithString:@"‣" attributes:@{ NSForegroundColorAttributeName : [NSColor lightGrayColor] }];
	static NSAttributedString* const ellipsis   = [[NSAttributedString alloc] initWithString:@"…" attributes:@{ NSForegroundColorAttributeName : [NSColor lightGrayColor] }];

	NSMutableParagraphStyle* paragraphStyle = [[NSMutableParagraphStyle alloc] init];
	[paragraphStyle setLineBreakMode:NSLineBreakByTruncatingTail];
	NSDictionary* defaultAttributes = @{
		NSParagraphStyleAttributeName : paragraphStyle,
		NSFontAttributeName : [NSFont controlContentFontOfSize:0]
	};

	if([self.options[OakFindRegularExpressionOption] boolValue])
	{
		OakSyntaxFormatter* formatter = [[OakSyntaxFormatter alloc] initWithGrammarName:@"source.regexp.oniguruma"];
		formatter.enabled = YES;
		NSMutableAttributedString* str = [[NSMutableAttributedString alloc] initWithString:self.string attributes:defaultAttributes];
		[formatter addStylesToString:str];
		return str;
	}

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

	[res addAttributes:defaultAttributes range:NSMakeRange(0, res.string.length)];
	return res;
}
@end

@interface OakPasteboardChooser () <NSWindowDelegate, NSTextFieldDelegate, NSTableViewDelegate, NSSearchFieldDelegate>
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
		_window.title                       = windowTitle;

		_arrayController = [[NSArrayController alloc] init];
		_arrayController.managedObjectContext         = aPasteboard.managedObjectContext;
		_arrayController.automaticallyPreparesContent = YES;
		_arrayController.entityName                   = @"PasteboardEntry";
		_arrayController.fetchPredicate               = [NSPredicate predicateWithFormat:@"pasteboard == %@", _pasteboard];
		_arrayController.sortDescriptors              = @[ [NSSortDescriptor sortDescriptorWithKey:@"date" ascending:NO] ];

		NSButton* deleteButton   = OakCreateButton(@"Delete", NSTexturedRoundedBezelStyle);
		NSButton* clearAllButton = OakCreateButton(@"Clear History", NSTexturedRoundedBezelStyle);
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
			[str removeAttribute:NSBackgroundColorAttributeName range:NSMakeRange(0, [str length])];
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
	request.predicate = [NSPredicate predicateWithFormat:@"pasteboard = %@ AND SELF != %@", _pasteboard, _pasteboard.currentEntry];
	request.includesPropertyValues = NO;
	NSManagedObjectContext* managedObjectContext = _pasteboard.managedObjectContext;
	for(OakPasteboardEntry* entry in [managedObjectContext executeFetchRequest:request error:nullptr])
		[managedObjectContext deleteObject:entry];
}

- (NSArray*)selectedEntriesPreservingOne
{
	NSFetchRequest* request = [[NSFetchRequest alloc] init];
	request.entity    = [NSEntityDescription entityForName:@"PasteboardEntry" inManagedObjectContext:_pasteboard.managedObjectContext];
	request.predicate = [NSPredicate predicateWithFormat:@"pasteboard == %@", _pasteboard];
	NSUInteger countOfAllEntries = [_pasteboard.managedObjectContext countForFetchRequest:request error:nullptr];

	NSArray* entries = [_arrayController selectedObjects];
	if([entries count] == countOfAllEntries)
	{
		NSMutableArray* tmp = [entries mutableCopy];
		[tmp removeObject:_pasteboard.currentEntry];
		entries = tmp;
	}

	return entries;
}

- (void)deleteForward:(id)sender
{
	[_arrayController removeObjects:[self selectedEntriesPreservingOne]];
}

- (void)deleteBackward:(id)sender
{
	NSArray* entries = [self selectedEntriesPreservingOne];
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

- (void)doCommandBySelector:(SEL)aSelector
{
	if([self respondsToSelector:aSelector])
	{
		[super doCommandBySelector:aSelector];
	}
	else
	{
		NSUInteger res = OakPerformTableViewActionFromSelector(_tableView, aSelector);
		if(res == OakMoveAcceptReturn)
			[self accept:self];
		else if(res == OakMoveCancelReturn)
			[self cancel:self];
	}
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
	static auto const forward = new std::set<SEL>{ @selector(moveUp:), @selector(moveDown:), @selector(moveUpAndModifySelection:), @selector(moveDownAndModifySelection:), @selector(pageUp:), @selector(pageDown:), @selector(pageUpAndModifySelection:), @selector(pageDownAndModifySelection:), @selector(scrollPageUp:), @selector(scrollPageDown:), @selector(moveToBeginningOfDocument:), @selector(moveToEndOfDocument:), @selector(scrollToBeginningOfDocument:), @selector(scrollToEndOfDocument:), @selector(insertNewline:), @selector(insertNewlineIgnoringFieldEditor:), @selector(cancelOperation:) };
	if(forward->find(aCommand) == forward->end())
		return NO;

	NSUInteger res = OakPerformTableViewActionFromSelector(_tableView, aCommand);
	if(res == OakMoveAcceptReturn)
		[self accept:aControl];
	else if(res == OakMoveCancelReturn)
		[self cancel:aControl];
	return YES;
}
@end
