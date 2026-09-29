#import "VariablesPreferences.h"
#import "Keys.h"
#import <OakAppKit/src/NSImage Additions.h>
#import <OakAppKit/src/OakUIConstructionFunctions.h>
#import <ns/src/ns.h>

static NSString* const kVariableKeyEnabled = @"enabled";
static NSString* const kVariableKeyName    = @"name";
static NSString* const kVariableKeyValue   = @"value";

@interface VariablesPreferences () <NSTableViewDelegate, NSTableViewDataSource>
{
	NSTableView* variablesTableView;
}
@property (nonatomic) NSMutableArray* variables;
@property (nonatomic) BOOL canRemove;
@end

@implementation VariablesPreferences
- (NSImage*)toolbarItemImage { return [NSImage imageNamed:@"Variables" inSameBundleAsClass:[self class]]; }

- (id)init
{
	if(self = [self initWithNibName:nil bundle:nil])
	{
		self.identifier = @"Variables";
		self.title      = @"Variables";
		self.variables  = [NSMutableArray arrayWithArray:[NSUserDefaults.standardUserDefaults arrayForKey:kUserDefaultsEnvironmentVariablesKey]];
	}
	return self;
}

- (IBAction)addVariable:(id)sender
{
	NSDictionary* entry = @{
		kVariableKeyEnabled: @YES,
		kVariableKeyName:    @"VARIABLE_NAME",
		kVariableKeyValue:   @"variable value",
	};

	NSInteger pos = [variablesTableView selectedRow] != -1 ? [variablesTableView selectedRow] : [_variables count];
	[_variables insertObject:entry atIndex:pos];
	[NSUserDefaults.standardUserDefaults setObject:[_variables copy] forKey:kUserDefaultsEnvironmentVariablesKey];
	[variablesTableView reloadData];
	[variablesTableView selectRowIndexes:[NSIndexSet indexSetWithIndex:pos] byExtendingSelection:NO];
	[variablesTableView editColumn:1 row:pos withEvent:nil select:YES];
}

- (IBAction)delete:(id)sender
{
	NSInteger row = [variablesTableView selectedRow];
	if(row != -1)
	{
		if([variablesTableView editedColumn] != -1)
		{
			[variablesTableView abortEditing];
			[[[self view] window] makeFirstResponder:variablesTableView];
		}

		[_variables removeObjectAtIndex:row];
		[NSUserDefaults.standardUserDefaults setObject:[_variables copy] forKey:kUserDefaultsEnvironmentVariablesKey];
		[variablesTableView reloadData];
		if(row > 0)
			--row;

		if(row < [_variables count])
		{
			[variablesTableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
			[variablesTableView scrollRowToVisible:row];
		}
	}
}

- (BOOL)commitEditing
{
	id firstResponder = [[[self view] window] firstResponder];
	if([firstResponder isKindOfClass:[NSTextView class]] && [(NSTextView*)firstResponder delegate] == variablesTableView)
		[[[self view] window] makeFirstResponder:variablesTableView];
	return YES;
}

- (NSTableColumn*)columnWithIdentifier:(NSUserInterfaceItemIdentifier)identifier title:(NSString*)title editable:(BOOL)editable width:(CGFloat)width resizingMask:(NSTableColumnResizingOptions)resizingMask
{
	NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:identifier];

	tableColumn.title        = title;
	tableColumn.editable     = editable;
	tableColumn.width        = width;
	tableColumn.resizingMask = resizingMask;

	if(resizingMask == NSTableColumnNoResizing)
	{
		tableColumn.minWidth = width;
		tableColumn.maxWidth = width;
	}

	return tableColumn;
}

- (void)loadView
{
	NSTableColumn* enabledTableColumn = [self columnWithIdentifier:kVariableKeyEnabled title:@""              editable:YES width:16  resizingMask:NSTableColumnNoResizing];
	NSTableColumn* nameTableColumn    = [self columnWithIdentifier:kVariableKeyName    title:@"Variable Name" editable:YES width:140 resizingMask:NSTableColumnUserResizingMask];
	NSTableColumn* valueTableColumn   = [self columnWithIdentifier:kVariableKeyValue   title:@"Value"         editable:YES width:200 resizingMask:NSTableColumnAutoresizingMask];

	NSButtonCell* enabledCell = [[NSButtonCell alloc] init];
	enabledCell.buttonType  = NSButtonTypeSwitch;
	enabledCell.controlSize = NSControlSizeSmall;
	enabledCell.title       = @"";
	enabledTableColumn.dataCell = enabledCell;

	variablesTableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
	variablesTableView.allowsColumnReordering  = NO;
	variablesTableView.columnAutoresizingStyle = NSTableViewLastColumnOnlyAutoresizingStyle;
	variablesTableView.delegate                = self;
	variablesTableView.dataSource              = self;

	for(NSTableColumn* tableColumn in @[ enabledTableColumn, nameTableColumn, valueTableColumn ])
		[variablesTableView addTableColumn:tableColumn];

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = NO;
	scrollView.autohidesScrollers    = YES;
	scrollView.borderType            = NSBezelBorder;
	scrollView.documentView          = variablesTableView;

	NSButton* addButton    = [NSButton buttonWithImage:[NSImage imageNamed:NSImageNameAddTemplate] target:self action:@selector(addVariable:)];
	NSButton* removeButton = [NSButton buttonWithImage:[NSImage imageNamed:NSImageNameRemoveTemplate] target:self action:@selector(delete:)];
	for(NSButton* button in @[ addButton, removeButton ])
		button.bezelStyle = NSBezelStyleSmallSquare;

	NSView* view = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 622, 454)];

	NSDictionary* views = @{
		@"scrollView": scrollView,
		@"add":        addButton,
		@"remove":     removeButton,
	};

	OakAddAutoLayoutViewsToSuperview(views.allValues, view);

	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scrollView(>=50)]-|" options:0 metrics:nil views:views]];
	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[add(==20)]-(-1)-[remove(==add)]-(>=20)-|" options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:views]];
	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[scrollView(>=50)]-8-[add(==19)]-|" options:0 metrics:nil views:views]];

	[removeButton bind:NSEnabledBinding toObject:self withKeyPath:@"canRemove" options:nil];

	self.view = view;
}

// ========================
// = NSTableView Delegate =
// ========================

- (void)tableViewSelectionDidChange:(NSNotification*)aNotification
{
	self.canRemove = [variablesTableView selectedRow] != -1 && [_variables count] != 0;
}

// ==========================
// = NSTableView DataSource =
// ==========================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return [_variables count];
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	return [_variables[rowIndex] objectForKey:[aTableColumn identifier]];
}

- (void)tableView:(NSTableView*)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	NSMutableDictionary* newValue = [NSMutableDictionary dictionaryWithDictionary:_variables[rowIndex]];
	newValue[aTableColumn.identifier] = anObject;
	if(![aTableColumn.identifier isEqualToString:kVariableKeyEnabled] && ![newValue[kVariableKeyEnabled] boolValue])
		newValue[kVariableKeyEnabled] = @YES;
	[_variables replaceObjectAtIndex:rowIndex withObject:newValue];
	[NSUserDefaults.standardUserDefaults setObject:[_variables copy] forKey:kUserDefaultsEnvironmentVariablesKey];
}
@end
