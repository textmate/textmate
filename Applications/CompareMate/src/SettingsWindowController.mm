#import "SettingsWindowController.h"
#import "WindowController.h"

@interface SettingsWindowController () <NSTableViewDataSource>
@property (nonatomic) IBOutlet NSTableView* tableView;
@property (nonatomic) NSMutableArray<NSString*>* ignoredFileNames;
@end

@implementation SettingsWindowController
- (instancetype)init
{
	if(self = [super initWithWindowNibName:@"SettingsWindow"])
		_ignoredFileNames = [[NSUserDefaults.standardUserDefaults stringArrayForKey:CompareMateIgnoredFileNamesDefaultsKey] mutableCopy] ?: [NSMutableArray array];
	return self;
}

- (void)windowDidLoad
{
	[super windowDidLoad];
	self.window.identifier = @"CompareMate.Settings";
	[self.window setFrameAutosaveName:self.window.identifier];
	if(![self.window setFrameUsingName:self.window.identifier])
		[self.window center];
	[self.tableView reloadData];
}

- (NSInteger)numberOfRowsInTableView:(NSTableView*)tableView
{
	return self.ignoredFileNames.count;
}

- (id)tableView:(NSTableView*)tableView objectValueForTableColumn:(NSTableColumn*)tableColumn row:(NSInteger)row
{
	return self.ignoredFileNames[row];
}

- (void)tableView:(NSTableView*)tableView setObjectValue:(id)object forTableColumn:(NSTableColumn*)tableColumn row:(NSInteger)row
{
	if(row >= 0 && row < (NSInteger)self.ignoredFileNames.count)
		self.ignoredFileNames[row] = [object isKindOfClass:NSString.class] ? object : @"";
	[self saveIgnoredFileNames];
}

- (void)saveIgnoredFileNames
{
	NSMutableArray<NSString*>* normalizedNames = [NSMutableArray array];
	NSMutableSet<NSString*>* seenNames = [NSMutableSet set];
	for(NSString* name in self.ignoredFileNames)
	{
		NSString* normalizedName = [name stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
		if(normalizedName.length && ![seenNames containsObject:normalizedName])
		{
			[normalizedNames addObject:normalizedName];
			[seenNames addObject:normalizedName];
		}
	}

	NSArray<NSString*>* previousNames = [NSUserDefaults.standardUserDefaults stringArrayForKey:CompareMateIgnoredFileNamesDefaultsKey] ?: @[];
	self.ignoredFileNames = normalizedNames;
	[self.tableView reloadData];
	if(![previousNames isEqualToArray:normalizedNames])
	{
		[NSUserDefaults.standardUserDefaults setObject:normalizedNames forKey:CompareMateIgnoredFileNamesDefaultsKey];
		[NSNotificationCenter.defaultCenter postNotificationName:CompareMateIgnoredFileNamesDidChangeNotification object:self];
	}
}

- (IBAction)addIgnoredFileName:(id)sender
{
	[self.window makeFirstResponder:nil];
	[self.ignoredFileNames addObject:@""];
	NSInteger const row = self.ignoredFileNames.count - 1;
	[self.tableView reloadData];
	[self.tableView scrollRowToVisible:row];
	[self.tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
	[self.tableView editColumn:0 row:row withEvent:nil select:YES];
}

- (IBAction)removeIgnoredFileNames:(id)sender
{
	NSIndexSet* selectedRows = self.tableView.selectedRowIndexes;
	if(selectedRows.count == 0)
	{
		NSBeep();
		return;
	}
	[self.ignoredFileNames removeObjectsAtIndexes:selectedRows];
	[self saveIgnoredFileNames];
}

- (IBAction)restoreDefaults:(id)sender
{
	self.ignoredFileNames = [@[ @".DS_Store", @".git" ] mutableCopy];
	[self saveIgnoredFileNames];
}
@end
