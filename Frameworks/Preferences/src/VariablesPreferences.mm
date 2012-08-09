#import "VariablesPreferences.h"
#import "Keys.h"
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <plist/ascii.h>
#import <ns/ns.h>

@implementation VariablesPreferences
@synthesize canRemove;

- (NSString*)identifier            { return @"Variables"; }
- (NSImage*)toolbarItemImage       { return [NSImage imageNamed:@"Variables" inSameBundleAsClass:[self class]]; }
- (NSString*)toolbarItemLabel      { return @"Variables"; }

- (id)init
{
	if(self = [super initWithNibName:@"VariablesPreferences" bundle:[NSBundle bundleForClass:[self class]]])
	{
		variables = [[NSMutableArray alloc] initWithArray:[[NSUserDefaults standardUserDefaults] arrayForKey:kUserDefaultsEnvironmentVariablesKey]];
	}
	return self;
}

- (void)dealloc
{
	[variables release];
	[super dealloc];
}

- (IBAction)addVariable:(id)sender
{
	NSDictionary* entry = [NSDictionary dictionaryWithObjectsAndKeys:
		YES_obj,           @"enabled",
		@"VARIABLE_NAME",  @"name",
		@"variable value", @"value",
		nil];

	NSInteger pos = [variablesTableView selectedRow] != -1 ? [variablesTableView selectedRow] : [variables count];
	[variables insertObject:entry atIndex:pos];
	[[NSUserDefaults standardUserDefaults] setObject:[[variables copy] autorelease] forKey:kUserDefaultsEnvironmentVariablesKey];
	[variablesTableView reloadData];
	[variablesTableView editColumn:1 row:pos withEvent:nil select:YES];
}

- (IBAction)delete:(id)sender
{
	NSInteger row = [variablesTableView selectedRow];
	if(row != -1)
	{
		[variables removeObjectAtIndex:row];
		[[NSUserDefaults standardUserDefaults] setObject:[[variables copy] autorelease] forKey:kUserDefaultsEnvironmentVariablesKey];
		[variablesTableView reloadData];
		if(row > 0)
			--row;

		if(row < [variables count])
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
	self.canRemove = [variablesTableView selectedRow] != -1;
}

// ==========================
// = NSTableView DataSource =
// ==========================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return [variables count];
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	return [[variables objectAtIndex:rowIndex] objectForKey:[aTableColumn identifier]];
}

- (void)tableView:(NSTableView*)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	NSMutableDictionary* newValue = [NSMutableDictionary dictionaryWithDictionary:[variables objectAtIndex:rowIndex]];
	[newValue setObject:anObject forKey:[aTableColumn identifier]];
	[variables replaceObjectAtIndex:rowIndex withObject:newValue];
	[[NSUserDefaults standardUserDefaults] setObject:[[variables copy] autorelease] forKey:kUserDefaultsEnvironmentVariablesKey];
}
@end
