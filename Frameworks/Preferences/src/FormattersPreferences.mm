#import "FormattersPreferences.h"
#import "FormatterRegistry.h"
#import "Keys.h"
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>

static NSString* const kColumnEnabled   = @"enabled";
static NSString* const kColumnGlob      = @"glob";
static NSString* const kColumnName      = @"name";
static NSString* const kColumnCommand   = @"command";
static NSString* const kColumnStatus    = @"status";

@interface FormattersPreferences () <NSTableViewDelegate, NSTableViewDataSource>
{
	NSTableView* _tableView;
}
@end

@implementation FormattersPreferences
- (NSImage*)toolbarItemImage { return PreferencesToolbarImage(@"hammer", @"Formatters", nil); }

- (id)init
{
	if(self = [self initWithNibName:nil bundle:nil])
	{
		self.identifier = @"Formatters";
		self.title      = @"Formatters";
	}
	return self;
}

- (NSTableColumn*)columnWithIdentifier:(NSUserInterfaceItemIdentifier)identifier title:(NSString*)title editable:(BOOL)editable width:(CGFloat)width resizingMask:(NSTableColumnResizingOptions)resizingMask
{
	NSTableColumn* col = [[NSTableColumn alloc] initWithIdentifier:identifier];
	col.title        = title;
	col.editable     = editable;
	col.width        = width;
	col.resizingMask = resizingMask;
	if(resizingMask == NSTableColumnNoResizing)
	{
		col.minWidth = width;
		col.maxWidth = width;
	}
	return col;
}

- (void)loadView
{
	NSTableColumn* enabledCol = [self columnWithIdentifier:kColumnEnabled title:@""          editable:YES width:20  resizingMask:NSTableColumnNoResizing];
	NSTableColumn* globCol    = [self columnWithIdentifier:kColumnGlob    title:@"File Type" editable:NO  width:120 resizingMask:NSTableColumnUserResizingMask];
	NSTableColumn* nameCol    = [self columnWithIdentifier:kColumnName    title:@"Formatter" editable:NO  width:100 resizingMask:NSTableColumnUserResizingMask];
	NSTableColumn* commandCol = [self columnWithIdentifier:kColumnCommand title:@"Command"   editable:YES width:260 resizingMask:NSTableColumnAutoresizingMask];
	NSTableColumn* statusCol  = [self columnWithIdentifier:kColumnStatus  title:@"Status"    editable:NO  width:60  resizingMask:NSTableColumnNoResizing];

	NSButtonCell* checkboxCell = [[NSButtonCell alloc] init];
	checkboxCell.buttonType  = NSButtonTypeSwitch;
	checkboxCell.controlSize = NSControlSizeSmall;
	checkboxCell.title       = @"";
	enabledCol.dataCell = checkboxCell;

	_tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
	_tableView.allowsColumnReordering  = NO;
	_tableView.columnAutoresizingStyle = NSTableViewLastColumnOnlyAutoresizingStyle;
	_tableView.delegate                = self;
	_tableView.dataSource              = self;
	_tableView.usesAlternatingRowBackgroundColors = YES;

	for(NSTableColumn* col in @[ enabledCol, globCol, nameCol, commandCol, statusCol ])
		[_tableView addTableColumn:col];

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = NO;
	scrollView.autohidesScrollers    = YES;
	scrollView.borderType            = NSBezelBorder;
	scrollView.documentView          = _tableView;

	NSButton* detectButton = OakCreateButton(@"Re-detect");
	detectButton.target = self;
	detectButton.action = @selector(redetect:);

	NSTextField* infoLabel = OakCreateLabel(@"Formatters are used when no formatCommand is set in .tm_properties. Configure formatCommand there to override.");
	infoLabel.textColor = NSColor.secondaryLabelColor;
	[infoLabel setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];

	NSView* view = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 622, 454)];

	NSDictionary* views = @{
		@"scrollView": scrollView,
		@"detect":     detectButton,
		@"info":       infoLabel,
	};

	OakAddAutoLayoutViewsToSuperview(views.allValues, view);

	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scrollView(>=50)]-|" options:0 metrics:nil views:views]];
	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[info]-[detect]-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[scrollView(>=50)]-8-[detect]-|" options:0 metrics:nil views:views]];

	self.view = view;
}

- (IBAction)redetect:(id)sender
{
	[[FormatterRegistry sharedInstance] detectAll];
	[_tableView reloadData];
}

// ========================
// = NSTableView Delegate =
// ========================

- (void)tableView:(NSTableView*)tableView willDisplayCell:(id)cell forTableColumn:(NSTableColumn*)col row:(NSInteger)row
{
	FormatterEntry* entry = [FormatterRegistry sharedInstance].allEntries[row];
	if([col.identifier isEqualToString:kColumnStatus])
	{
		NSTextFieldCell* textCell = cell;
		textCell.textColor = entry.detectedPath ? NSColor.systemGreenColor : NSColor.secondaryLabelColor;
	}
}

// ==========================
// = NSTableView DataSource =
// ==========================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)tableView
{
	return [FormatterRegistry sharedInstance].allEntries.count;
}

- (id)tableView:(NSTableView*)tableView objectValueForTableColumn:(NSTableColumn*)col row:(NSInteger)row
{
	FormatterEntry* entry = [FormatterRegistry sharedInstance].allEntries[row];
	if([col.identifier isEqualToString:kColumnEnabled])
		return @(entry.enabled);
	else if([col.identifier isEqualToString:kColumnGlob])
		return entry.fileTypeGlob;
	else if([col.identifier isEqualToString:kColumnName])
		return entry.formatterName;
	else if([col.identifier isEqualToString:kColumnCommand])
		return entry.command;
	else if([col.identifier isEqualToString:kColumnStatus])
		return entry.detectedPath ? @"Found" : @"Not found";
	return nil;
}

- (void)tableView:(NSTableView*)tableView setObjectValue:(id)value forTableColumn:(NSTableColumn*)col row:(NSInteger)row
{
	if([col.identifier isEqualToString:kColumnEnabled])
		[[FormatterRegistry sharedInstance] setEnabled:[value boolValue] forEntryAtIndex:row];
	else if([col.identifier isEqualToString:kColumnCommand])
		[[FormatterRegistry sharedInstance] setCommand:value forEntryAtIndex:row];
}
@end
