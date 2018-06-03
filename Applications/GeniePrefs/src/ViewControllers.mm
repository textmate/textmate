#import "ViewControllers.h"
#import "AddAutoLayoutViews.h"
#import <GenieManager/GenieManager.h>
#import <GenieManager/GenieItem.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <MenuBuilder/MenuBuilder.h>

@interface GeneralViewController ()
{
	BOOL _didLoadView;
}
@end

@implementation GeneralViewController
- (void)loadView
{
	if(!_didLoadView)
	{
		NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];
		contentView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
		if([self respondsToSelector:@selector(makeView:)])
			[self performSelector:@selector(makeView:) withObject:contentView];
		self.view = contentView;
	}
	_didLoadView = YES;
}
@end

@interface TreeViewController ()
@property (nonatomic) NSTreeController* treeController;
@end

@implementation TreeViewController
- (instancetype)initWithTreeController:(NSTreeController*)aTreeController
{
	if(self = [super init])
		_treeController = aTreeController;
	return self;
}
@end

@interface TableViewController () <NSTableViewDataSource>
{
	NSArray* _columnNames;
	NSUInteger  _visibleRows;
	BOOL _showHeaderView;
	NSDictionary* _prototype;
	NSButton* _removeButton;
	NSString* _dragType;
}
@end

@implementation TableViewController
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

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if(aMenuItem.action == @selector(delete:))
		return _removeButton.isEnabled;
	return [super validateMenuItem:aMenuItem];
}

- (void)makeView:(NSView*)contentView
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

	NSDictionary* views = @{
		@"scrollView": scrollView,
		@"add":        addButton,
		@"remove":     _removeButton,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

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

@interface CatalogViewController () <NSOutlineViewDataSource>
{
	NSString* _dragType;
	NSArray* _draggedNodes;
}
@end

@implementation CatalogViewController
- (void)viewWillAppear
{
	if(!_outlineView.window.initialFirstResponder)
		_outlineView.window.initialFirstResponder = _outlineView;
	[super viewWillAppear];
}

- (void)viewDidAppear
{
	NSInteger selectedRow = _outlineView.selectedRow;
	if(selectedRow > 0)
		[_outlineView scrollRowToVisible:selectedRow];
	[super viewDidAppear];
}

- (void)delete:(id)sender
{
	[_removeButton performClick:self];
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if(aMenuItem.action == @selector(delete:))
		return _removeButton.isEnabled;
	return [super validateMenuItem:aMenuItem];
}

- (void)makeView:(NSView*)contentView
{
	NSOutlineView* outlineView = [[NSOutlineView alloc] initWithFrame:NSZeroRect];
	_outlineView = outlineView;

	NSTableColumn* spaceColumn = [[NSTableColumn alloc] initWithIdentifier:@"space"];
	spaceColumn.width    = 0;
	spaceColumn.editable = NO;
	[outlineView addTableColumn:spaceColumn];

	NSTableColumn* enabledColumn = [[NSTableColumn alloc] initWithIdentifier:@"enabled"];
	NSButtonCell* checkboxCell = [[NSButtonCell alloc] init];
	checkboxCell.buttonType = NSSwitchButton;
	checkboxCell.title      = @"";
	enabledColumn.dataCell = checkboxCell;
	enabledColumn.width    = 16;
	[outlineView addTableColumn:enabledColumn];

	NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"title"];
	[outlineView addTableColumn:tableColumn];
	outlineView.intercellSpacing = NSMakeSize(3, 12);
	outlineView.outlineTableColumn = tableColumn;
	outlineView.headerView = nil;
	outlineView.usesAlternatingRowBackgroundColors = YES;
	outlineView.indentationMarkerFollowsCell = YES;
	outlineView.indentationPerLevel = 16;
	outlineView.autoresizesOutlineColumn = NO;
	outlineView.dataSource = self;

	_dragType = [NSUUID UUID].UUIDString;
	[outlineView registerForDraggedTypes:@[ _dragType ]];

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller = YES;
	scrollView.autohidesScrollers  = YES;
	scrollView.borderType          = NSBezelBorder;
	scrollView.documentView        = outlineView;

	_addButton    = [NSButton buttonWithImage:[NSImage imageNamed:NSImageNameAddTemplate] target:self.treeController action:@selector(insert:)];
	_removeButton = [NSButton buttonWithImage:[NSImage imageNamed:NSImageNameRemoveTemplate] target:self.treeController action:@selector(remove:)];
	for(NSButton* button in @[ _addButton, _removeButton ])
		button.bezelStyle = NSSmallSquareBezelStyle;

	NSDictionary* views = @{
		@"scrollView":   scrollView,
		@"add":          _addButton,
		@"remove":       _removeButton,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scrollView]|"                           options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[add(==21)]-(-1)-[remove(==21)]-(>=0)-|" options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[scrollView]-[add(==21)]-|"              options:0 metrics:nil views:views]];

	[outlineView   bind:NSContentBinding             toObject:self.treeController withKeyPath:@"arrangedObjects" options:nil];
	[outlineView   bind:NSSelectionIndexPathsBinding toObject:self.treeController withKeyPath:NSSelectionIndexPathsBinding options:nil];
	[enabledColumn bind:NSValueBinding               toObject:self.treeController withKeyPath:@"arrangedObjects.disabled" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[tableColumn   bind:NSValueBinding               toObject:self.treeController withKeyPath:@"arrangedObjects.displayName" options:nil];
	[tableColumn   bind:NSEditableBinding            toObject:self.treeController withKeyPath:@"arrangedObjects.canEditDisplayName" options:nil];

	[_addButton     bind:NSEnabledBinding toObject:self.treeController withKeyPath:@"canInsert" options:nil];
	[_removeButton  bind:NSEnabledBinding toObject:self.treeController withKeyPath:@"canRemove" options:nil];
}

- (NSInteger)outlineView:(NSOutlineView*)anOutlineView numberOfChildrenOfItem:(id)item                                 { return 0; }
- (BOOL)outlineView:(NSOutlineView*)anOutlineView isItemExpandable:(id)item                                            { return NO; }
- (id)outlineView:(NSOutlineView*)anOutlineView child:(NSInteger)index ofItem:(id)item                                 { return nil; }
- (id)outlineView:(NSOutlineView*)anOutlineView objectValueForTableColumn:(NSTableColumn*)aTableColumn byItem:(id)item { return nil; }

- (void)outlineView:(NSOutlineView*)outlineView draggingSession:(NSDraggingSession*)session willBeginAtPoint:(NSPoint)screenPoint forItems:(NSArray*)draggedItems
{
	_draggedNodes = draggedItems;
	[session.draggingPasteboard setData:[NSData data] forType:_dragType];
}
 
- (void)outlineView:(NSOutlineView*)outlineView draggingSession:(NSDraggingSession*)session endedAtPoint:(NSPoint)screenPoint operation:(NSDragOperation)operation
{
	if(operation == NSDragOperationDelete)
		NSLog(@"%s trash items", sel_getName(_cmd));
	_draggedNodes = nil;
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView writeItems:(NSArray*)items toPasteboard:(NSPasteboard*)pboard
{
	NSMutableArray* itemUids = [NSMutableArray array];
	for(NSTreeNode* item in items)
		[itemUids addObject:[item.representedObject identifier]];

	[pboard clearContents];
	[pboard setPropertyList:itemUids forType:_dragType];
	return YES;
}

- (NSDragOperation)outlineView:(NSOutlineView*)anOutlineView validateDrop:(id<NSDraggingInfo>)info proposedItem:(id)item proposedChildIndex:(NSInteger)childIndex
{
	BOOL optionDown = ([NSEvent modifierFlags] & NSEventModifierFlagOption) == NSEventModifierFlagOption;
	return optionDown ? NSDragOperationCopy : NSDragOperationMove;
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView acceptDrop:(id<NSDraggingInfo>)info item:(id)item childIndex:(NSInteger)childIndex
{
	// TODO Duplicate
	// TODO Check if drop destination is valid (not dragging item into itself)
	// TODO Drag to trash?

	if(_draggedNodes)
	{
		if(childIndex == NSOutlineViewDropOnItemIndex)
			childIndex = 0;

		NSIndexPath* indexPath = item ? [((NSTreeNode*)item).indexPath indexPathByAddingIndex:childIndex] : [NSIndexPath indexPathWithIndex:childIndex];
		NSLog(@"%s move %@ to %@", sel_getName(_cmd), _draggedNodes, indexPath);
		[self.treeController moveNodes:_draggedNodes toIndexPath:indexPath];
		return YES;
	}

	// NSPasteboard* pboard = info.draggingPasteboard;
	// if(NSArray* draggedUids = [pboard propertyListForType:_dragType])
	// {
	// 	NSDragOperation op = [info draggingSourceOperationMask];
	// 	BOOL move = (op & NSDragOperationMove) == NSDragOperationMove;
	// }
	return NO;
}
@end

@interface ChangeIconViewController : GeneralViewController
{
	NSPopUpButton* _popUpButton;
	NSTextField* _textField;
	NSDictionary* _currentItem;
}
@property (nonatomic) NSObjectController* objectController;
@property (nonatomic) SEL action;
@property (nonatomic, weak) id target;
@end

@implementation ChangeIconViewController
- (instancetype)initWithContent:(NSDictionary*)iconInfo
{
	if(self = [super init])
		_objectController = [[NSObjectController alloc] initWithContent:[(iconInfo ?: @{ }) mutableCopy]];
	return self;
}

- (void)popUpButtonDidChange:(NSMenuItem*)sender
{
	[_objectController commitEditing];

	if(_currentItem)
		[_textField unbind:NSValueBinding];

	if(_currentItem = sender.representedObject)
	{
		_textField.placeholderString = _currentItem[@"placeholder"];
		[_textField bind:NSValueBinding toObject:_objectController withKeyPath:[NSString stringWithFormat:@"content.%@", _currentItem[@"property"]] options:@{ NSNullPlaceholderBindingOption: _currentItem[@"placeholder"] }];
	}
}

- (void)makeView:(NSView*)contentView
{
	NSArray* popUpItems = @[
		@{ @"title": @"Image Path",     @"placeholder": @"Image file path",           @"property": @"image"       },
		@{ @"title": @"File Type",      @"placeholder": @"Universal type identifier", @"property": @"fileType"    },
		@{ @"title": @"Application",    @"placeholder": @"Bundle identifier",         @"property": @"application" },
		@{ @"title": @"Text",           @"placeholder": @"String",                    @"property": @"text"        },
		@{ @"title": @"Image Named",    @"placeholder": @"Named system image",        @"property": @"name"        },
		@{ @"title": @"Icon from File", @"placeholder": @"File path",                 @"property": @"file"        },
		@{ @"title": @"Favorite Icon",  @"placeholder": @"URL",                       @"property": @"url"         },
	];

	NSPopUpButton* popUpButton = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	for(NSDictionary* item in popUpItems)
		[popUpButton.menu addItemWithTitle:item[@"title"] action:@selector(popUpButtonDidChange:) keyEquivalent:@""].representedObject = item;
	NSTextField* textField = [NSTextField textFieldWithString:@""];

	_popUpButton = popUpButton;
	_textField   = textField;

	NSMenuItem* selectItem = _popUpButton.itemArray.firstObject;
	for(NSMenuItem* menuItem in _popUpButton.itemArray)
	{
		if(id value = [_objectController valueForKeyPath:[NSString stringWithFormat:@"content.%@", menuItem.representedObject[@"property"]]])
		{
			if([value isKindOfClass:[NSString class]] && OakNotEmptyString(value))
			{
				selectItem = menuItem;
				break;
			}
		}
	}

	[_popUpButton selectItem:selectItem];
	[self popUpButtonDidChange:selectItem];

	NSButton* acceptButton = [NSButton buttonWithTitle:@"OK" target:self action:@selector(acceptChangedIcon:)];
	NSButton* cancelButton = [NSButton buttonWithTitle:@"Cancel" target:self action:@selector(cancelChangedIcon:)];

	[acceptButton setKeyEquivalent:@"\r"];
	[cancelButton setKeyEquivalent:@"\e"];

	NSDictionary* views = @{
		@"popUp":         popUpButton,
		@"textField":     textField,
		@"ok":            acceptButton,
		@"cancel":        cancelButton,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[popUp]-[textField(>=200)]-|"    options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(>=8)-[cancel]-[ok(==cancel)]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[textField]-(20)-[ok]-|"         options:0 metrics:nil views:views]];
}

- (void)acceptChangedIcon:(id)sender
{
	[self.objectController commitEditing];
	[self.presentingViewController dismissViewController:self];

	NSDictionary* dict = @{ };
	if(NSString* key = _currentItem[@"property"])
	{
		NSString* value = [_objectController.content valueForKey:key];
		if(OakNotEmptyString(value))
			dict = @{ key: value };
	}
	[_objectController setContent:[dict mutableCopy]];

	[NSApp sendAction:self.action to:self.target from:self];
}

- (void)cancelChangedIcon:(id)sender
{
	[self.presentingViewController dismissViewController:self];
}
@end

@implementation BasicProperties
{
	NSTextField* _matchTextField;
	ChangeIconViewController* _changeIconViewController;
}

- (void)changeImage:(id)sender
{
	if(GenieItem* selectedItem = self.treeController.selectedObjects.firstObject)
	{
		_changeIconViewController = [[ChangeIconViewController alloc] initWithContent:selectedItem.iconDescription];
		_changeIconViewController.action = @selector(acceptChangedIcon:);
		_changeIconViewController.target = self;
		[self presentViewControllerAsSheet:_changeIconViewController];
	}
}

- (void)acceptChangedIcon:(id)sender
{
	if(GenieItem* selectedItem = self.treeController.selectedObjects.firstObject)
	{
		NSDictionary* updatedIconInfo = _changeIconViewController.objectController.content;
		selectedItem.iconDescription = updatedIconInfo.count ? updatedIconInfo : nil;
	}
}

- (void)realMakeView:(NSView*)contentView
{
	NSTextField* titleLabel    = [NSTextField labelWithString:@"Title:"];
	NSTextField* subtitleLabel = [NSTextField labelWithString:@"Subtitle:"];
	NSTextField* matchLabel    = [NSTextField labelWithString:@"Match:"];

	NSTextField* titleTextField    = [NSTextField textFieldWithString:@""];
	NSTextField* subtitleTextField = [NSTextField textFieldWithString:@""];
	NSTextField* matchTextField    = [NSTextField textFieldWithString:@""];
	_matchTextField = matchTextField;

	NSImageView* imageView = [NSImageView imageViewWithImage:[NSImage imageNamed:NSImageNameApplicationIcon]];
	imageView.imageFrameStyle = NSImageFrameGrayBezel;

	_changeImageButton = [NSButton buttonWithTitle:@"Change…" target:self action:@selector(changeImage:)];

	[imageView setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationVertical];
	[imageView setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationVertical];

	[_changeImageButton setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];
	[_changeImageButton setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

	NSDictionary* views = @{
		@"titleLabel":      titleLabel,
		@"subtitleLabel":   subtitleLabel,
		@"matchLabel":      matchLabel,
		@"title":           titleTextField,
		@"subtitle":        subtitleTextField,
		@"match":           matchTextField,
		@"imageView":       imageView,
		@"changeImage":     _changeImageButton,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]"                              options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[subtitleLabel]-[subtitle]"                        options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[matchLabel]-[match]"                              options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[title]-[imageView]-|"                               options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[subtitle]-[imageView]-|"                            options:NSLayoutFormatAlignAllBottom metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[match]-[changeImage]-|"                             options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[subtitle]-[match]"                        options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	NSLayoutConstraint* imageViewWidth = [NSLayoutConstraint constraintWithItem:imageView attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationGreaterThanOrEqual toItem:imageView attribute:NSLayoutAttributeHeight multiplier:1 constant:0];
	[contentView addConstraint:imageViewWidth];

	[titleTextField    bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveTitle"    options:nil];
	[subtitleTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveSubtitle" options:nil];
	[matchTextField    bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveMatch"    options:@{ NSNullPlaceholderBindingOption: @"Optional" }];
	[imageView         bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.iconImage"         options:nil];
}

- (void)makeView:(NSView*)contentView
{
	[self realMakeView:contentView];
}

- (void)makeView:(NSView*)contentView aboveView:(NSView*)aView
{
	[self realMakeView:contentView];

	NSDictionary* views = @{
		@"match":    _matchTextField,
		@"nextView": aView,
	};

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[match]-[nextView]" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];
}
@end

@interface Properties ()
@property (nonatomic) NSUInteger countOfAdvancedKeys;
@end

@implementation Properties
- (void)makeView:(NSView*)contentView
{
	MBMenu const items = {
		{ @"Group Item",            .tag = kGenieItemKindGroup,                       },
		{ @"Action",                @selector(nop:)                                   },
		{ @"Go to Web Address",     .tag = kGenieItemKindWebAddress,      .indent = 1 },
		{ @"Run Script",            .tag = kGenieItemKindRunScript,       .indent = 1 },
		{ @"Open File",             .tag = kGenieItemKindOpenFile,        .indent = 1 },
		{ @"Data Source",           @selector(nop:)                                   },
		{ @"Spotlight Search",      .tag = kGenieItemKindSpotlight,       .indent = 1 },
		{ @"Sqlite Search",         .tag = kGenieItemKindSqlite,          .indent = 1 },
		{ @"Items from Script",     .tag = kGenieItemKindCommandResult,   .indent = 1 },
		{ @"Recent Documents",      .tag = kGenieItemKindRecentDocuments, .indent = 1 },
		{ @"Child Actions",         @selector(nop:)                                   },
		{ @"Predicate Group",       .tag = kGenieItemKindPredicateGroup,  .indent = 1 },
	};

	NSTextField* actionLabel   = [NSTextField labelWithString:@"Item Type:"];
	NSPopUpButton* popUpButton = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	popUpButton.menu = MBCreateMenu(items);

	NSBox* leftSeparator  = [[NSBox alloc] initWithFrame:NSZeroRect];
	NSBox* rightSeparator = [[NSBox alloc] initWithFrame:NSZeroRect];

	for(NSBox* separator in @[ leftSeparator, rightSeparator ])
	{
		separator.boxType = NSBoxSeparator;
		[separator setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	}

	_containerView  = [[NSView alloc] initWithFrame:NSZeroRect];
	_advancedButton = [NSButton buttonWithTitle:@"Advanced…" target:nil action:@selector(showAdvancedSettings:)];

	NSDictionary* views = @{
		@"actionLabel":     actionLabel,
		@"action":          popUpButton,
		@"leftSeparator":   leftSeparator,
		@"rightSeparator":  rightSeparator,
		@"container":       _containerView,
		@"advanced":        _advancedButton,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[leftSeparator(>=10)]-[actionLabel]"               options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[actionLabel]-[action]"                              options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[action]-[rightSeparator(==leftSeparator)]-|"        options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[container]|"                                       options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(>=20)-[advanced]-|"                               options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[action]-[container][advanced]-|"                  options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[leftSeparator(==rightSeparator,==1)]"               options:0 metrics:nil views:views]];

	NSLayoutConstraint* separatorConstraint = [NSLayoutConstraint constraintWithItem:leftSeparator attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:rightSeparator attribute:NSLayoutAttributeTop multiplier:1 constant:0];
	[contentView addConstraint:separatorConstraint];

	[popUpButton bind:NSSelectedTagBinding toObject:self.treeController withKeyPath:@"selection.kind" options:nil];

	[self bind:@"countOfAdvancedKeys" toObject:self.treeController withKeyPath:@"selection.countOfAdvancedKeys" options:nil];
}

- (void)setCountOfAdvancedKeys:(NSUInteger)newCountOfAdvancedKeys
{
	_countOfAdvancedKeys = newCountOfAdvancedKeys;
	_advancedButton.title = newCountOfAdvancedKeys ? [NSString stringWithFormat:@"Advanced (%lu)…", _countOfAdvancedKeys] : @"Advanced…";
}
@end

@implementation URLProperties
- (void)makeView:(NSView*)contentView
{
	NSTextField* urlLabel     = [NSTextField labelWithString:@"URL:"];
	NSTextField* urlTextField = [NSTextField textFieldWithString:@""];

	NSDictionary* views = @{
		@"urlLabel":  urlLabel,
		@"url":       urlTextField,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[urlLabel]-[url]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[url]-(>=20)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	[urlTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveUrl" options:nil];

	[super makeView:contentView aboveView:urlTextField];
}
@end

@implementation FileProperties
- (void)makeView:(NSView*)contentView
{
	NSTextField* fileLabel     = [NSTextField labelWithString:@"File:"];
	NSTextField* fileTextField = [NSTextField textFieldWithString:@""];

	NSDictionary* views = @{
		@"fileLabel":  fileLabel,
		@"file":       fileTextField,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[fileLabel]-[file]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[file]-(>=20)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	[fileTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveFile" options:nil];

	[super makeView:contentView aboveView:fileTextField];
}
@end

@implementation ShellProperties
{
	TableViewController* _argvTable;
}

- (void)makeView:(NSView*)contentView
{
	_argvTable = [[TableViewController alloc] initWithColumnNames:@[ @"value" ] visibleRows:3 showHeaderView:NO prototype:@{ @"value": @"argument" }];
	[_argvTable.arrayController bind:NSContentArrayBinding toObject:self.treeController withKeyPath:@"selection.mutableScriptArguments" options:nil];

	NSTextField* scriptLabel = [NSTextField labelWithString:@"Script:"];
	NSTextField* argvLabel   = [NSTextField labelWithString:@"Arguments:"];

	NSScrollView* scriptScrollView = GenieCreateTextView();
	NSTextView* scriptTextView     = scriptScrollView.documentView;

	NSDictionary* views = @{
		@"scriptLabel": scriptLabel,
		@"argvLabel":   argvLabel,
		@"script":      scriptScrollView,
		@"argv":        _argvTable.view,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scriptLabel]-[script]-|"              options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[argvLabel]-[argv]-|"                  options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[script]-[argv]-|"                       options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];

	[scriptTextView bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.script" options:nil];

	[super makeView:contentView aboveView:scriptScrollView];
}
@end

@implementation SpotlightProperties
{
	TableViewController* scope;
}

- (void)makeView:(NSView*)contentView
{
	static NSArray<NSString*>* const metaDataFields = @[
		NSMetadataItemCFBundleIdentifierKey, NSMetadataItemContentCreationDateKey, NSMetadataItemContentModificationDateKey, NSMetadataItemContentTypeKey, NSMetadataItemDateAddedKey, NSMetadataItemDisplayNameKey, NSMetadataItemFSContentChangeDateKey, NSMetadataItemFSNameKey, NSMetadataItemKindKey, NSMetadataItemLastUsedDateKey, NSMetadataItemPathKey, NSMetadataItemURLKey, NSMetadataItemVersionKey
	];

	NSTextField* titleLabel     = [NSTextField labelWithString:@"Title:"];
	NSTextField* queryLabel     = [NSTextField labelWithString:@"Query:"];
	NSTextField* sortByLabel    = [NSTextField labelWithString:@"Sort By:"];
	NSTextField* scopeLabel     = [NSTextField labelWithString:@"Folders:"];

	NSTextField* titleTextField = [NSTextField textFieldWithString:@""];
	NSTextField* queryTextField = [NSTextField textFieldWithString:@""];

	NSPopUpButton* sortByPopUp = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	for(NSString* field in metaDataFields)
		[sortByPopUp.menu addItemWithTitle:field action:NULL keyEquivalent:@""];

	NSButton* ascendingCheckBox = [NSButton checkboxWithTitle:@"Ascending" target:nil action:NULL];

	scope = [[TableViewController alloc] initWithColumnNames:@[ @"disabled", @"path" ] visibleRows:3 showHeaderView:NO prototype:@{ @"path": @"/some/path" }];
	[scope.arrayController bind:NSContentArrayBinding toObject:self.treeController withKeyPath:@"selection.mdScope" options:nil];

	NSDictionary* views = @{
		@"titleLabel":   titleLabel,
		@"queryLabel":   queryLabel,
		@"sortByLabel":  sortByLabel,
		@"scopeLabel":   scopeLabel,
		@"title":        titleTextField,
		@"query":        queryTextField,
		@"sortBy":       sortByPopUp,
		@"ascending":    ascendingCheckBox,
		@"scope":        scope.view,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]-|"            options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[queryLabel]-[query]-|"            options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[sortByLabel]-[sortBy]-(>=20)-|"   options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[ascending]-(>=20)-|"                options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scopeLabel]-[scope]-|"            options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[query]-(20)-[sortBy]-[ascending]-(20)-[scope]-(>=20)-|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];

	[titleTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveTitle" options:nil];
	[queryTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.mdQuery" options:nil];
	[sortByPopUp bind:NSSelectedValueBinding toObject:self.treeController withKeyPath:@"selection.sortBy" options:@{ NSNullPlaceholderBindingOption: NSMetadataItemLastUsedDateKey }];
	[ascendingCheckBox bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.descending" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
}
@end

@implementation SqliteProperties
- (void)makeView:(NSView*)contentView
{
	NSTextField* titleLabel        = [NSTextField labelWithString:@"Title:"];
	NSTextField* databaseLabel     = [NSTextField labelWithString:@"Database:"];
	NSTextField* queryLabel        = [NSTextField labelWithString:@"Query:"];
	NSTextField* titleTextField    = [NSTextField textFieldWithString:@""];
	NSTextField* databaseTextField = [NSTextField textFieldWithString:@""];
	NSScrollView* queryScrollView  = GenieCreateTextView();
	NSTextView* queryTextView      = queryScrollView.documentView;

	NSDictionary* views = @{
		@"titleLabel":     titleLabel,
		@"databaseLabel":  databaseLabel,
		@"queryLabel":     queryLabel,
		@"title":          titleTextField,
		@"database":       databaseTextField,
		@"query":          queryScrollView,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]-|"               options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[databaseLabel]-[database]-|"         options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[queryLabel]-[query]-|"               options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[database]-[query(>=100)]-|"  options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	[titleTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveTitle" options:nil];
	[databaseTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveSqlDatabase" options:nil];
	[queryTextView bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.sqlQuery" options:nil];
}
@end

@implementation RecentDocumentsProperties
- (void)makeView:(NSView*)contentView
{
	NSTextField* titleLabel                = [NSTextField labelWithString:@"Title:"];
	NSTextField* bundleIdentifierLabel     = [NSTextField labelWithString:@"Bundle Identifier:"];

	NSTextField* titleTextField            = [NSTextField textFieldWithString:@""];
	NSTextField* bundleIdentifierTextField = [NSTextField textFieldWithString:@""];

	NSDictionary* views = @{
		@"titleLabel":             titleLabel,
		@"bundleIdentifierLabel":  bundleIdentifierLabel,
		@"title":                  titleTextField,
		@"bundleIdentifier":       bundleIdentifierTextField,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]-|"                       options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[bundleIdentifierLabel]-[bundleIdentifier]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[bundleIdentifier]-(>=20)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	[titleTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveTitle" options:nil];
	[bundleIdentifierTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveBundleIdentifier" options:nil];
}
@end

@implementation PredicateGroupProperties
- (void)makeView:(NSView*)contentView
{
	NSTextField* titleLabel         = [NSTextField labelWithString:@"Title:"];
	NSTextField* predicateLabel     = [NSTextField labelWithString:@"Predicate:"];

	NSTextField* titleTextField     = [NSTextField textFieldWithString:@""];
	NSTextField* predicateTextField = [NSTextField textFieldWithString:@""];

	NSDictionary* views = @{
		@"titleLabel":      titleLabel,
		@"predicateLabel":  predicateLabel,
		@"title":           titleTextField,
		@"predicate":       predicateTextField,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]-|"         options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[predicateLabel]-[predicate]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[predicate]-(>=20)-|"   options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	[titleTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.title" options:nil];
	[predicateTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.predicate" options:nil];
}
@end

@implementation ExecDataSourceProperties
{
	TableViewController* _argvTable;
}

- (void)makeView:(NSView*)contentView
{
	_argvTable = [[TableViewController alloc] initWithColumnNames:@[ @"value" ] visibleRows:3 showHeaderView:NO prototype:@{ @"value": @"argument" }];

	NSTextField* titleLabel  = [NSTextField labelWithString:@"Title:"];
	NSTextField* scriptLabel = [NSTextField labelWithString:@"Script:"];
	NSTextField* argvLabel   = [NSTextField labelWithString:@"Arguments:"];

	NSTextField* titleTextField    = [NSTextField textFieldWithString:@""];
	NSScrollView* scriptScrollView = GenieCreateTextView();
	NSTextView* scriptTextView     = scriptScrollView.documentView;

	NSDictionary* views = @{
		@"titleLabel":   titleLabel,
		@"scriptLabel":  scriptLabel,
		@"argvLabel":    argvLabel,
		@"title":        titleTextField,
		@"script":       scriptScrollView,
		@"argv":         _argvTable.view,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]-|"                options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scriptLabel]-[script]-|"              options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[argvLabel]-[argv]-|"                  options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[script]-[argv]-|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];

	[titleTextField             bind:NSValueBinding        toObject:self.treeController withKeyPath:@"selection.primitiveTitle" options:nil];
	[scriptTextView             bind:NSValueBinding        toObject:self.treeController withKeyPath:@"selection.script" options:nil];
	[_argvTable.arrayController bind:NSContentArrayBinding toObject:self.treeController withKeyPath:@"selection.mutableScriptArguments" options:nil];
}
@end
