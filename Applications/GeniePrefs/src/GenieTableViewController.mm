#import "GenieTableViewController.h"
#import "AddAutoLayoutViews.h"

@interface GenieTableViewController () <NSTableViewDataSource>
{
	NSArray* _columnNames;
	NSUInteger  _visibleRows;
	BOOL _showHeaderView;
	NSDictionary* _prototype;
	NSButton* _removeButton;
	NSString* _dragType;
}
@end

@implementation GenieTableViewController
- (instancetype)initWithColumnNames:(NSArray*)columnNames visibleRows:(NSUInteger)visibleRows
{
	return [self initWithColumnNames:columnNames visibleRows:visibleRows showHeaderView:NO prototype:@{ }];
}

- (instancetype)initWithColumnNames:(NSArray*)columnNames visibleRows:(NSUInteger)visibleRows showHeaderView:(BOOL)showHeaderView
{
	return [self initWithColumnNames:columnNames visibleRows:visibleRows showHeaderView:showHeaderView prototype:@{ }];
}

- (instancetype)initWithColumnNames:(NSArray*)columnNames visibleRows:(NSUInteger)visibleRows showHeaderView:(BOOL)showHeaderView prototype:(NSDictionary*)prototype
{
	if(self = [super init])
	{
		_columnNames    = columnNames;
		_visibleRows    = visibleRows;
		_showHeaderView = showHeaderView;
		_prototype      = prototype;

		_arrayController = [[NSArrayController alloc] init];
		_arrayController.avoidsEmptySelection = NO;
	}
	return self;
}

- (void)insert:(id)sender
{
	NSUInteger rowIndex = _arrayController.selectionIndex;
	if(rowIndex == NSNotFound)
		rowIndex = [_arrayController.arrangedObjects count];

	[_arrayController insertObject:[_prototype mutableCopy] atArrangedObjectIndex:rowIndex];

	NSArray<NSTableColumn*>* tableColumns = _tableView.tableColumns;
	for(NSUInteger i = 0; i < tableColumns.count; ++i)
	{
		if(tableColumns[i].isEditable && [tableColumns[i].dataCell isKindOfClass:[NSTextFieldCell class]])
		{
			[_tableView editColumn:i row:rowIndex withEvent:nil select:YES];
			break;
		}
	}
}

- (void)delete:(id)sender
{
	_arrayController.avoidsEmptySelection = YES;
	[_removeButton performClick:self];
	_arrayController.avoidsEmptySelection = NO;
}

- (void)cancel:(id)sender
{
	NSResponder* firstResponder = _tableView.window.firstResponder;
	if([firstResponder isKindOfClass:[NSTextView class]] && ((NSTextView*)firstResponder).delegate == _tableView)
	{
		[_arrayController discardEditing];
		[_tableView.window makeFirstResponder:_tableView];
	}
	else
	{
		[self.nextResponder doCommandBySelector:@selector(cancel:)];
	}
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if(aMenuItem.action == @selector(delete:))
		return _removeButton.isEnabled;
	return [super respondsToSelector:@selector(validateMenuItem:)] ? [super validateMenuItem:aMenuItem] : YES;
}

- (void)loadView
{
	_tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
	_tableView.usesAlternatingRowBackgroundColors = YES;
	_tableView.dataSource = self;

	_dragType = [NSUUID UUID].UUIDString;
	[_tableView registerForDraggedTypes:@[ _dragType ]];

	if(_showHeaderView == NO)
		_tableView.headerView = nil;

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller = YES;
	scrollView.autohidesScrollers  = YES;
	scrollView.borderType          = NSBezelBorder;
	scrollView.documentView        = _tableView;

	NSButton* addButton = [NSButton buttonWithImage:[NSImage imageNamed:NSImageNameAddTemplate] target:self action:@selector(insert:)];
	_removeButton       = [NSButton buttonWithImage:[NSImage imageNamed:NSImageNameRemoveTemplate] target:_arrayController action:@selector(remove:)];
	for(NSButton* button in @[ addButton, _removeButton ])
		button.bezelStyle = NSSmallSquareBezelStyle;

	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];

	NSDictionary* views = @{
		@"scrollView": scrollView,
		@"add":        addButton,
		@"remove":     _removeButton,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, _tableView, addButton, _removeButton ]);

	CGFloat rowHeight    = [_tableView rowHeight] + [_tableView intercellSpacing].height;
	CGFloat headerHeight = _tableView.headerView ? NSHeight([_tableView.headerView headerRectOfColumn:0]) : 0;

	NSDictionary* metrics = @{
		@"kRowHeight":   @(headerHeight+2+rowHeight),
		@"kTableHeight": @(headerHeight+2+rowHeight*_visibleRows),
	};

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView]|"                                               options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[add(==21)]-(-1)-[remove(==21)]-(>=8)-|"                     options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[scrollView(==kTableHeight@251,>=kRowHeight)]-[add(==21)]|"  options:NSLayoutFormatAlignAllLeading metrics:metrics views:views]];

	[_tableView bind:NSContentBinding toObject:_arrayController withKeyPath:@"arrangedObjects" options:nil];
	[_tableView bind:NSSelectionIndexesBinding toObject:_arrayController withKeyPath:@"selectionIndexes" options:nil];

	for(NSString* columnName in _columnNames)
	{
		NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:columnName];
		if([columnName isEqualToString:@"disabled"])
		{
			NSButtonCell* checkboxCell = [[NSButtonCell alloc] init];
			checkboxCell.buttonType  = NSSwitchButton;
			checkboxCell.controlSize = NSControlSizeSmall;
			checkboxCell.title       = @"";
			tableColumn.dataCell = checkboxCell;
			tableColumn.width    = 16;
			[tableColumn bind:NSValueBinding toObject:_arrayController withKeyPath:@"arrangedObjects.disabled" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
		}
		else
		{
			[tableColumn bind:NSValueBinding toObject:_arrayController withKeyPath:[NSString stringWithFormat:@"arrangedObjects.%@", columnName] options:nil];
		}
		[_tableView addTableColumn:tableColumn];
	}

	[addButton bind:NSEnabledBinding toObject:_arrayController withKeyPath:@"canInsert" options:nil];
	[_removeButton bind:NSEnabledBinding toObject:_arrayController withKeyPath:@"canRemove" options:nil];

	[_tableView sizeLastColumnToFit];

	self.view = contentView;
}

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return 0;
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)row
{
	return nil;
}

- (BOOL)tableView:(NSTableView*)aTableView writeRowsWithIndexes:(NSIndexSet*)indexSet toPasteboard:(NSPasteboard*)pboard
{
	NSMutableArray* selectedRows = [NSMutableArray array];
	for(NSUInteger index = [indexSet firstIndex]; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
		[selectedRows addObject:@(index)];

	[pboard clearContents];
	[pboard setPropertyList:selectedRows forType:_dragType];
	return YES;
}

- (NSDragOperation)tableView:(NSTableView*)aTableView validateDrop:(id<NSDraggingInfo>)info proposedRow:(NSInteger)row proposedDropOperation:(NSTableViewDropOperation)operation
{
	BOOL optionDown = ([NSEvent modifierFlags] & NSEventModifierFlagOption) == NSEventModifierFlagOption;
	return operation == NSTableViewDropOn ? NSDragOperationNone : (optionDown ? NSDragOperationCopy : NSDragOperationMove);
}

- (BOOL)tableView:(NSTableView*)aTableView acceptDrop:(id<NSDraggingInfo>)info row:(NSInteger)row dropOperation:(NSTableViewDropOperation)operation
{
	NSPasteboard* pboard = info.draggingPasteboard;
	if(NSArray* draggedRows = [pboard propertyListForType:_dragType])
	{
		NSDragOperation op = [info draggingSourceOperationMask];
		BOOL move = (op & NSDragOperationMove) == NSDragOperationMove;

		NSInteger rowOffset = 0;
		id arrangedObjects = _arrayController.arrangedObjects;

		NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
		NSMutableArray* draggedObjects = [NSMutableArray array];

		for(NSNumber* draggedRow in draggedRows)
		{
			NSInteger index = [draggedRow intValue];
			[indexSet addIndex:index];
			id object = [arrangedObjects objectAtIndex:index];
			[draggedObjects addObject:move ? object : [object mutableCopy]];

			if(move && index < row)
				++rowOffset;
		}

		if(move)
			[_arrayController removeObjectsAtArrangedObjectIndexes:indexSet];

		NSIndexSet* destinationRows = [NSIndexSet indexSetWithIndexesInRange:NSMakeRange(row - rowOffset, draggedObjects.count)];
		[_arrayController insertObjects:draggedObjects atArrangedObjectIndexes:destinationRows];

		return YES;
	}
	return NO;
}
@end
