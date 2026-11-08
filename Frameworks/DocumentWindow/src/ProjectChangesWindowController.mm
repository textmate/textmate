#import "ProjectChangesWindowController.h"

@interface ProjectChangesWindowController () <NSTableViewDataSource, NSTableViewDelegate, NSWindowDelegate>
@property (copy) NSDictionary* (^bufferProvider)(void);
@property (copy) void (^openFileHandler)(NSString*, NSUInteger);
@property NSDictionary* snapshot;
@property NSDictionary* buffers;
@property NSArray<NSDictionary*>* files;
@property NSDictionary* displayedDiff;
@property NSString* displayedPath;
@property NSTableView* table;
@property NSTextView* diffView;
@property NSTextField* summaryLabel;
@property NSTextField* repositoryLabel;
@property NSTextField* fileLabel;
@property NSTextField* statusLabel;
@property NSTextField* positionLabel;
@property NSButton* refreshButton;
@property NSButton* openButton;
@property NSButton* previousButton;
@property NSButton* nextButton;
@property NSInteger hunkIndex;
@property NSInteger pendingHunkIndex;
@property NSUInteger refreshGeneration;
@property NSUInteger diffGeneration;
@property BOOL refreshing;
@property BOOL loadingDiff;
@property NSTimer* refreshTimer;
@end

@implementation ProjectChangesWindowController

static NSTextField* Label(NSString* text, CGFloat size, NSFontWeight weight)
{
	NSTextField* label = [NSTextField labelWithString:text];
	label.font = [NSFont systemFontOfSize:size weight:weight];
	label.lineBreakMode = NSLineBreakByTruncatingMiddle;
	return label;
}

- (instancetype)initWithDirectory:(NSString*)directory buffers:(NSDictionary<NSString*, ProjectChangesBuffer*>* (^)(void))buffers openFile:(void (^)(NSString*, NSUInteger))openFile
{
	NSWindow* window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 1100, 730)
		styleMask:NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskMiniaturizable
		backing:NSBackingStoreBuffered defer:NO];
	if((self = [super initWithWindow:window])) {
		_directory = [directory copy];
		_bufferProvider = [buffers copy];
		_openFileHandler = [openFile copy];
		_files = @[];
		_hunkIndex = -1;
		_pendingHunkIndex = 0;
		window.title = @"Project Changes";
		window.minSize = NSMakeSize(790, 480);
		window.releasedWhenClosed = NO;
		window.delegate = self;
		[window center];
		[self buildView];
	}
	return self;
}

- (NSButton*)button:(NSString*)title action:(SEL)action
{
	return [NSButton buttonWithTitle:title target:self action:action];
}

- (void)buildView
{
	NSView* content = self.window.contentView;
	NSView* header = [[NSView alloc] init];
	NSView* footer = [[NSView alloc] init];
	NSSplitView* split = [[NSSplitView alloc] init];
	split.vertical = YES;
	split.dividerStyle = NSSplitViewDividerStyleThin;
	for(NSView* view in @[header, split, footer]) { view.translatesAutoresizingMaskIntoConstraints = NO; [content addSubview:view]; }
	[NSLayoutConstraint activateConstraints:@[
		[header.topAnchor constraintEqualToAnchor:content.topAnchor], [header.leadingAnchor constraintEqualToAnchor:content.leadingAnchor],
		[header.trailingAnchor constraintEqualToAnchor:content.trailingAnchor], [header.heightAnchor constraintEqualToConstant:82],
		[footer.bottomAnchor constraintEqualToAnchor:content.bottomAnchor], [footer.leadingAnchor constraintEqualToAnchor:content.leadingAnchor],
		[footer.trailingAnchor constraintEqualToAnchor:content.trailingAnchor], [footer.heightAnchor constraintEqualToConstant:54],
		[split.topAnchor constraintEqualToAnchor:header.bottomAnchor], [split.bottomAnchor constraintEqualToAnchor:footer.topAnchor],
		[split.leadingAnchor constraintEqualToAnchor:content.leadingAnchor], [split.trailingAnchor constraintEqualToAnchor:content.trailingAnchor]
	]];
	self.summaryLabel = Label(@"Project Changes", 20, NSFontWeightSemibold);
	self.repositoryLabel = Label(@"Loading repository…", 12, NSFontWeightRegular);
	self.repositoryLabel.textColor = NSColor.secondaryLabelColor;
	self.refreshButton = [self button:@"Refresh" action:@selector(refresh:)];
	for(NSView* view in @[self.summaryLabel, self.repositoryLabel, self.refreshButton]) { view.translatesAutoresizingMaskIntoConstraints = NO; [header addSubview:view]; }
	[NSLayoutConstraint activateConstraints:@[
		[self.summaryLabel.leadingAnchor constraintEqualToAnchor:header.leadingAnchor constant:20], [self.summaryLabel.topAnchor constraintEqualToAnchor:header.topAnchor constant:15],
		[self.summaryLabel.trailingAnchor constraintLessThanOrEqualToAnchor:self.refreshButton.leadingAnchor constant:-20],
		[self.repositoryLabel.leadingAnchor constraintEqualToAnchor:self.summaryLabel.leadingAnchor], [self.repositoryLabel.topAnchor constraintEqualToAnchor:self.summaryLabel.bottomAnchor constant:6],
		[self.repositoryLabel.trailingAnchor constraintLessThanOrEqualToAnchor:self.refreshButton.leadingAnchor constant:-20],
		[self.refreshButton.trailingAnchor constraintEqualToAnchor:header.trailingAnchor constant:-20], [self.refreshButton.centerYAnchor constraintEqualToAnchor:header.centerYAnchor]
	]];

	NSScrollView* sidebar = [[NSScrollView alloc] initWithFrame:NSMakeRect(0, 0, 310, 594)];
	sidebar.hasVerticalScroller = YES;
	sidebar.borderType = NSNoBorder;
	self.table = [[NSTableView alloc] initWithFrame:sidebar.bounds];
	NSTableColumn* column = [[NSTableColumn alloc] initWithIdentifier:@"file"];
	column.width = 300;
	[self.table addTableColumn:column];
	self.table.headerView = nil;
	self.table.rowHeight = 60;
	self.table.style = NSTableViewStyleSourceList;
	self.table.columnAutoresizingStyle = NSTableViewUniformColumnAutoresizingStyle;
	self.table.delegate = self;
	self.table.dataSource = self;
	self.table.target = self;
	self.table.doubleAction = @selector(openSelectedFile:);
	self.table.accessibilityLabel = @"Changed project files";
	sidebar.documentView = self.table;
	[split addSubview:sidebar];
	NSView* detail = [[NSView alloc] initWithFrame:NSMakeRect(311, 0, 789, 594)];
	[split addSubview:detail];
	[sidebar.widthAnchor constraintGreaterThanOrEqualToConstant:220].active = YES;
	[detail.widthAnchor constraintGreaterThanOrEqualToConstant:450].active = YES;
	[split setHoldingPriority:NSLayoutPriorityDefaultHigh forSubviewAtIndex:0];
	self.fileLabel = Label(@"Choose a changed file", 15, NSFontWeightSemibold);
	self.statusLabel = Label(@"Last commit → current files, including unsaved edits", 12, NSFontWeightRegular);
	self.statusLabel.textColor = NSColor.secondaryLabelColor;
	NSScrollView* scroll = [[NSScrollView alloc] init];
	scroll.hasVerticalScroller = YES;
	scroll.hasHorizontalScroller = YES;
	scroll.borderType = NSNoBorder;
	self.diffView = [[NSTextView alloc] initWithFrame:NSMakeRect(0, 0, 750, 500)];
	self.diffView.editable = NO;
	self.diffView.richText = NO;
	self.diffView.font = [NSFont monospacedSystemFontOfSize:12 weight:NSFontWeightRegular];
	self.diffView.textColor = NSColor.textColor;
	self.diffView.backgroundColor = NSColor.textBackgroundColor;
	self.diffView.textContainerInset = NSMakeSize(18, 16);
	self.diffView.horizontallyResizable = YES;
	self.diffView.verticallyResizable = YES;
	self.diffView.textContainer.containerSize = NSMakeSize(CGFLOAT_MAX, CGFLOAT_MAX);
	self.diffView.textContainer.widthTracksTextView = NO;
	self.diffView.accessibilityLabel = @"File diff. Minus lines are removed; plus lines are added.";
	scroll.documentView = self.diffView;
	for(NSView* view in @[self.fileLabel, self.statusLabel, scroll]) { view.translatesAutoresizingMaskIntoConstraints = NO; [detail addSubview:view]; }
	[NSLayoutConstraint activateConstraints:@[
		[self.fileLabel.leadingAnchor constraintEqualToAnchor:detail.leadingAnchor constant:18], [self.fileLabel.topAnchor constraintEqualToAnchor:detail.topAnchor constant:14],
		[self.fileLabel.trailingAnchor constraintEqualToAnchor:detail.trailingAnchor constant:-18],
		[self.statusLabel.leadingAnchor constraintEqualToAnchor:self.fileLabel.leadingAnchor], [self.statusLabel.topAnchor constraintEqualToAnchor:self.fileLabel.bottomAnchor constant:5],
		[self.statusLabel.trailingAnchor constraintEqualToAnchor:self.fileLabel.trailingAnchor],
		[scroll.topAnchor constraintEqualToAnchor:self.statusLabel.bottomAnchor constant:14], [scroll.bottomAnchor constraintEqualToAnchor:detail.bottomAnchor],
		[scroll.leadingAnchor constraintEqualToAnchor:detail.leadingAnchor], [scroll.trailingAnchor constraintEqualToAnchor:detail.trailingAnchor]
	]];
	self.previousButton = [self button:@"Previous Change" action:@selector(previousChange:)];
	self.nextButton = [self button:@"Next Change" action:@selector(nextChange:)];
	self.openButton = [self button:@"Open File" action:@selector(openSelectedFile:)];
	self.positionLabel = Label(@"", 12, NSFontWeightRegular);
	self.positionLabel.textColor = NSColor.secondaryLabelColor;
	for(NSView* view in @[self.previousButton, self.nextButton, self.openButton, self.positionLabel]) { view.translatesAutoresizingMaskIntoConstraints = NO; [footer addSubview:view]; }
	[NSLayoutConstraint activateConstraints:@[
		[self.previousButton.leadingAnchor constraintEqualToAnchor:footer.leadingAnchor constant:16], [self.previousButton.centerYAnchor constraintEqualToAnchor:footer.centerYAnchor],
		[self.nextButton.leadingAnchor constraintEqualToAnchor:self.previousButton.trailingAnchor constant:8], [self.nextButton.centerYAnchor constraintEqualToAnchor:footer.centerYAnchor],
		[self.positionLabel.leadingAnchor constraintEqualToAnchor:self.nextButton.trailingAnchor constant:16], [self.positionLabel.centerYAnchor constraintEqualToAnchor:footer.centerYAnchor],
		[self.positionLabel.trailingAnchor constraintLessThanOrEqualToAnchor:self.openButton.leadingAnchor constant:-16],
		[self.openButton.trailingAnchor constraintEqualToAnchor:footer.trailingAnchor constant:-16], [self.openButton.centerYAnchor constraintEqualToAnchor:footer.centerYAnchor]
	]];
	[self updateControls];
}

- (void)showWindow:(id)sender
{
	[super showWindow:sender];
	[self refresh:sender];
	if(!self.refreshTimer) {
		__weak ProjectChangesWindowController* weakSelf = self;
		self.refreshTimer = [NSTimer scheduledTimerWithTimeInterval:5 repeats:YES block:^(NSTimer*){
			if(weakSelf.window.isVisible && NSApp.isActive) [weakSelf refresh:nil];
		}];
	}
}
- (void)windowWillClose:(NSNotification*)notification
{
	[self.refreshTimer invalidate];
	self.refreshTimer = nil;
	++self.refreshGeneration;
	++self.diffGeneration;
	self.refreshing = NO;
	self.loadingDiff = NO;
}
- (void)dealloc { [_refreshTimer invalidate]; }

- (IBAction)refresh:(id)sender
{
	if(self.refreshing) return;
	self.refreshing = YES;
	self.refreshButton.enabled = NO;
	NSUInteger generation = ++self.refreshGeneration;
	NSDictionary* buffers = self.bufferProvider ? self.bufferProvider() : @{};
	NSString* directory = self.directory;
	__weak ProjectChangesWindowController* weakSelf = self;
	dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
		NSDictionary* snapshot = ProjectChangesSnapshot(directory, buffers);
		dispatch_async(dispatch_get_main_queue(), ^{
			ProjectChangesWindowController* controller = weakSelf;
			if(!controller || generation != controller.refreshGeneration) return;
			controller.refreshing = NO;
			controller.refreshButton.enabled = YES;
			NSString* selected = controller.selectedFile[@"path"];
			controller.snapshot = snapshot;
			controller.buffers = buffers;
			controller.files = snapshot[@"files"] ?: @[];
			controller.summaryLabel.stringValue = [NSString stringWithFormat:@"%lu changed %@", (unsigned long)controller.files.count, controller.files.count == 1 ? @"file" : @"files"];
			controller.repositoryLabel.stringValue = snapshot[@"error"] ?: [NSString stringWithFormat:@"%@ · %@ · Last commit → current files + unsaved edits", [snapshot[@"root"] lastPathComponent], snapshot[@"branch"]];
			[controller.table reloadData];
			NSUInteger row = 0;
			if(selected) for(NSUInteger i = 0; i < controller.files.count; ++i) if([controller.files[i][@"path"] isEqual:selected]) { row = i; break; }
			if(controller.files.count) {
				[controller.table selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
				// Reloading the same selected row need not send a selection event.
				[controller loadSelectedDiff];
			} else {
				++controller.diffGeneration;
				controller.loadingDiff = NO;
				controller.displayedDiff = nil;
				controller.displayedPath = nil;
				controller.fileLabel.stringValue = snapshot[@"error"] ? @"Changes unavailable" : @"No changes to review";
				controller.statusLabel.stringValue = @"Last commit → current files, including unsaved edits";
				controller.diffView.string = snapshot[@"error"] ?: @"Your project matches its last commit.";
				[controller updateControls];
			}
		});
	});
}

- (NSDictionary*)selectedFile
{
	NSInteger row = self.table.selectedRow;
	return row >= 0 && row < self.files.count ? self.files[row] : nil;
}
- (NSInteger)numberOfRowsInTableView:(NSTableView*)tableView { return self.files.count; }
- (NSView*)tableView:(NSTableView*)tableView viewForTableColumn:(NSTableColumn*)column row:(NSInteger)row
{
	NSTableCellView* cell = [tableView makeViewWithIdentifier:@"change" owner:self];
	if(!cell) {
		cell = [[NSTableCellView alloc] init];
		cell.identifier = @"change";
		NSTextField* name = Label(@"", 13, NSFontWeightMedium);
		NSTextField* status = Label(@"", 11, NSFontWeightRegular);
		name.tag = 1; status.tag = 2;
		for(NSView* view in @[name, status]) { view.translatesAutoresizingMaskIntoConstraints = NO; [cell addSubview:view]; }
		[NSLayoutConstraint activateConstraints:@[
			[name.leadingAnchor constraintEqualToAnchor:cell.leadingAnchor constant:10], [name.trailingAnchor constraintEqualToAnchor:cell.trailingAnchor constant:-10],
			[name.topAnchor constraintEqualToAnchor:cell.topAnchor constant:10],
			[status.leadingAnchor constraintEqualToAnchor:name.leadingAnchor], [status.trailingAnchor constraintEqualToAnchor:name.trailingAnchor],
			[status.topAnchor constraintEqualToAnchor:name.bottomAnchor constant:5]
		]];
	}
	NSDictionary* file = self.files[row];
	((NSTextField*)[cell viewWithTag:1]).stringValue = file[@"path"];
	NSTextField* status = [cell viewWithTag:2];
	status.stringValue = file[@"status"];
	status.textColor = [file[@"status"] hasPrefix:@"New"] ? NSColor.systemGreenColor : [file[@"status"] hasPrefix:@"Deleted"] ? NSColor.systemRedColor : NSColor.secondaryLabelColor;
	cell.toolTip = file[@"path"];
	return cell;
}
- (void)tableViewSelectionDidChange:(NSNotification*)notification { self.pendingHunkIndex = 0; [self loadSelectedDiff]; }

- (void)loadSelectedDiff
{
	NSDictionary* file = self.selectedFile;
	if(!file) return;
	NSUInteger generation = ++self.diffGeneration;
	BOOL sameFile = [self.displayedPath isEqual:file[@"path"]];
	NSInteger target = sameFile ? self.hunkIndex : self.pendingHunkIndex;
	self.loadingDiff = YES;
	self.fileLabel.stringValue = file[@"path"];
	self.statusLabel.stringValue = [file[@"unsaved"] boolValue] ? @"Comparing last commit with unsaved editor buffer…" : @"Comparing last commit with saved file…";
	[self updateControls];
	NSDictionary* snapshot = self.snapshot;
	NSDictionary* buffers = self.buffers;
	__weak ProjectChangesWindowController* weakSelf = self;
	dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
		NSDictionary* diff = ProjectChangesDiff(snapshot, file, buffers);
		dispatch_async(dispatch_get_main_queue(), ^{
			ProjectChangesWindowController* controller = weakSelf;
			if(!controller || generation != controller.diffGeneration) return;
			controller.loadingDiff = NO;
			NSString* text = diff[@"text"] ?: diff[@"error"];
			BOOL unchanged = sameFile && [controller.diffView.string isEqual:text];
			controller.displayedDiff = diff;
			controller.displayedPath = file[@"path"];
			NSUInteger count = [diff[@"hunks"] count];
			controller.hunkIndex = count ? (target < 0 ? (NSInteger)count-1 : MIN(target, (NSInteger)count-1)) : -1;
			controller.statusLabel.stringValue = diff[@"error"] ?: [NSString stringWithFormat:@"%@ · +%@ / −%@ lines · %@", file[@"status"], diff[@"added"], diff[@"removed"], [file[@"unsaved"] boolValue] ? @"Unsaved editor buffer" : @"Saved file"];
			if(!unchanged) { controller.diffView.string = text; [controller styleDiff]; [controller revealHunk]; }
			[controller updateControls];
		});
	});
}

- (void)styleDiff
{
	NSTextStorage* storage = self.diffView.textStorage;
	[storage beginEditing];
	[storage setAttributes:@{ NSFontAttributeName: [NSFont monospacedSystemFontOfSize:12 weight:NSFontWeightRegular], NSForegroundColorAttributeName: NSColor.textColor } range:NSMakeRange(0, storage.length)];
	NSUInteger offset = 0;
	for(NSString* line in [self.diffView.string componentsSeparatedByString:@"\n"]) {
		NSColor* color = [line hasPrefix:@"+"] ? NSColor.systemGreenColor : [line hasPrefix:@"-"] ? NSColor.systemRedColor : [line hasPrefix:@"@@"] ? NSColor.secondaryLabelColor : nil;
		if(color) {
			NSRange range = NSMakeRange(offset, line.length);
			[storage addAttribute:NSForegroundColorAttributeName value:color range:range];
			if([line hasPrefix:@"+"] || [line hasPrefix:@"-"]) [storage addAttribute:NSBackgroundColorAttributeName value:[color colorWithAlphaComponent:0.07] range:range];
		}
		offset += line.length+1;
	}
	[storage endEditing];
}
- (void)revealHunk
{
	if(self.hunkIndex < 0) return;
	NSDictionary* hunk = self.displayedDiff[@"hunks"][self.hunkIndex];
	NSRange range = NSMakeRange([hunk[@"offset"] unsignedIntegerValue], [hunk[@"length"] unsignedIntegerValue]);
	[self.diffView setSelectedRange:NSMakeRange(range.location, 0)];
	[self.diffView scrollRangeToVisible:NSMakeRange(range.location, MIN(range.length, (NSUInteger)100))];
}
- (void)updateControls
{
	BOOL file = self.selectedFile != nil;
	self.openButton.enabled = file && !self.loadingDiff && (![self.selectedFile[@"flags"] containsString:@"D"] || [self.selectedFile[@"unsaved"] boolValue]);
	self.previousButton.enabled = file && !self.loadingDiff;
	self.nextButton.enabled = file && !self.loadingDiff;
	NSUInteger count = [self.displayedDiff[@"hunks"] count];
	self.positionLabel.stringValue = file ? [NSString stringWithFormat:@"File %ld of %lu%@", (long)self.table.selectedRow+1, (unsigned long)self.files.count,
		count && !self.loadingDiff ? [NSString stringWithFormat:@" · Change %ld of %lu", (long)self.hunkIndex+1, (unsigned long)count] : @""] : @"";
}
- (void)moveChange:(NSInteger)direction
{
	if(self.loadingDiff || !self.files.count) return;
	NSInteger count = [self.displayedDiff[@"hunks"] count];
	NSInteger next = self.hunkIndex+direction;
	if(next >= 0 && next < count) { self.hunkIndex = next; [self revealHunk]; [self updateControls]; return; }
	NSInteger row = (self.table.selectedRow+direction+(NSInteger)self.files.count) % self.files.count;
	[self.table selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
	self.pendingHunkIndex = direction > 0 ? 0 : -1;
	// Explicitly load for the one-file case, and after setting the backwards
	// target (selection notifications are synchronous).
	self.displayedPath = nil;
	[self loadSelectedDiff];
	[self.table scrollRowToVisible:row];
}
- (IBAction)previousChange:(id)sender { [self moveChange:-1]; }
- (IBAction)nextChange:(id)sender { [self moveChange:1]; }
- (IBAction)openSelectedFile:(id)sender
{
	if(!self.openButton.enabled || !self.openFileHandler) return;
	NSUInteger line = self.hunkIndex >= 0 ? [self.displayedDiff[@"hunks"][self.hunkIndex][@"line"] unsignedIntegerValue] : 1;
	self.openFileHandler(self.selectedFile[@"absolutePath"], line);
}
@end
