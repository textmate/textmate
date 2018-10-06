#import "VariablesPreferences.h"
#import "Keys.h"
#import <OakAppKit/NSImage Additions.h>
#import <ns/ns.h>

@interface VariablesPreferences ()
@property (nonatomic) NSMutableArray* variables;
@end

@implementation VariablesPreferences
- (NSString*)viewIdentifier        { return @"Variables"; }
- (NSImage*)toolbarItemImage       { return [NSImage imageNamed:@"Variables" inSameBundleAsClass:[self class]]; }
- (NSString*)toolbarItemLabel      { return @"Variables"; }

- (id)init
{
	if(self = [super initWithNibName:@"VariablesPreferences" bundle:[NSBundle bundleForClass:[self class]]])
	{
		self.variables = [NSMutableArray arrayWithArray:[[NSUserDefaults standardUserDefaults] arrayForKey:kUserDefaultsEnvironmentVariablesKey]];
	}
	return self;
}

- (IBAction)addVariable:(id)sender
{
	NSDictionary* entry = @{
		@"enabled": @YES,
		@"name":    @"VARIABLE_NAME",
		@"value":   @"variable value",
	};

	NSInteger pos = [variablesTableView selectedRow] != -1 ? [variablesTableView selectedRow] : [_variables count];
	[_variables insertObject:entry atIndex:pos];
	[[NSUserDefaults standardUserDefaults] setObject:[_variables copy] forKey:kUserDefaultsEnvironmentVariablesKey];
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
		[[NSUserDefaults standardUserDefaults] setObject:[_variables copy] forKey:kUserDefaultsEnvironmentVariablesKey];
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
	[newValue setObject:anObject forKey:[aTableColumn identifier]];
	if(![[aTableColumn identifier] isEqualToString:@"enabled"] && ![[newValue objectForKey:@"enabled"] boolValue])
		[newValue setObject:@YES forKey:@"enabled"];
	[_variables replaceObjectAtIndex:rowIndex withObject:newValue];
	[[NSUserDefaults standardUserDefaults] setObject:[_variables copy] forKey:kUserDefaultsEnvironmentVariablesKey];
}
@end
