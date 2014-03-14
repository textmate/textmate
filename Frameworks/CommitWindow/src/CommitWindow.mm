#import "CommitWindow.h"
#import <OakAppKit/OakUIConstructionFunctions.h>

@interface OakCommitWindow : NSWindowController <NSWindowDelegate, NSTableViewDataSource>
@property (nonatomic) NSArray*          paths;
@property (nonatomic) NSString*         clientPortName;
@property (nonatomic) NSScrollView*     scrollView;
@property (nonatomic) NSTableView*      tableView;
@property (nonatomic) OakCommitWindow*  retainedSelf;
@end

@implementation OakCommitWindow
- (id)init
{
	if((self = [super init]))
	{
		_paths = @[
			@{ @"path" : @"/path/to/foo" },
			@{ @"path" : @"/path/to/bar" },
		];

		NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"path"];
		tableColumn.editable = NO;
		tableColumn.dataCell = [[NSTextFieldCell alloc] initTextCell:@""];
		[tableColumn.dataCell setLineBreakMode:NSLineBreakByTruncatingMiddle];

		NSTableView* tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
		[tableView addTableColumn:tableColumn];
		tableView.headerView                         = nil;
		tableView.focusRingType                      = NSFocusRingTypeNone;
		tableView.usesAlternatingRowBackgroundColors = YES;
		tableView.doubleAction                       = @selector(didDoubleClickTableView:);
		tableView.target                             = self;
		tableView.dataSource                         = self;
		_tableView                                   = tableView;

		_scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		_scrollView.hasVerticalScroller   = YES;
		_scrollView.hasHorizontalScroller = NO;
		_scrollView.autohidesScrollers    = YES;
		_scrollView.borderType            = NSNoBorder;
		_scrollView.documentView          = _tableView;

		self.window = [[NSWindow alloc] initWithContentRect:NSMakeRect(600, 700, 400, 500) styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask|NSTexturedBackgroundWindowMask) backing:NSBackingStoreBuffered defer:NO];
		self.window.delegate           = self;
		self.window.level              = NSFloatingWindowLevel;
		self.window.releasedWhenClosed = NO;
		self.window.title              = @"Commit";

		NSButton* commitButton = OakCreateButton(@"Commit", NSTexturedRoundedBezelStyle);
		NSButton* cancelButton = OakCreateButton(@"Cancel", NSTexturedRoundedBezelStyle);

		commitButton.action = @selector(performCommit:);
		cancelButton.action = @selector(cancel:);

		NSDictionary* views = @{
			@"scrollView"         : self.scrollView,
			@"bottomDivider"      : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"cancel"             : cancelButton,
			@"commit"             : commitButton,
		};

		NSView* contentView = self.window.contentView;
		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[contentView addSubview:view];
		}

		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView(==bottomDivider)]|" options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[cancel]-[commit]-(8)-|"         options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[scrollView(>=50)][bottomDivider]-(5)-[commit]-(6)-|" options:0 metrics:nil views:views]];

		self.window.defaultButtonCell = commitButton.cell;
	}
	return self;
}

- (void)showWindow:(id)sender
{
	[self.window recalculateKeyViewLoop];
	[super showWindow:sender];

	self.retainedSelf = self;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self sendCommitMessageToClient:YES];
}

- (void)sendCommitMessageToClient:(BOOL)success
{
	if(!self.clientPortName) // Reply already sent
		return;

	if(id proxy = [NSConnection rootProxyForConnectionWithRegisteredName:self.clientPortName host:nil])
	{
		[proxy setProtocolForProxy:@protocol(OakCommitWindowClientProtocol)];

		if(success)
		{
			[proxy connectFromServerWithOptions:@{
				kOakCommitWindowStandardOutput : @"Hello world",
				kOakCommitWindowStandardError  : @"",
				kOakCommitWindowReturnCode     : @0,
			}];
		}
		else
		{
			[proxy connectFromServerWithOptions:@{
				kOakCommitWindowReturnCode     : @1,
			}];
		}

		self.clientPortName = nil;
	}

	[self performSelector:@selector(setRetainedSelf:) withObject:nil afterDelay:0];
}

// ==================
// = Action Methods =
// ==================

- (void)didDoubleClickTableView:(id)sender
{
	if(_tableView.clickedRow == -1)
		return;

	NSDictionary* row = _paths[_tableView.clickedRow];
	NSLog(@"%s show diff for %@", sel_getName(_cmd), row);
}

- (void)performCommit:(id)sender
{
	[self sendCommitMessageToClient:YES];
	[self close];
}

- (void)cancel:(id)sender
{
	[self sendCommitMessageToClient:NO];
	[self close];
}

// =========================
// = NSTableViewDataSource =
// =========================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return [_paths count];
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	NSDictionary* row = _paths[rowIndex];
	return row[aTableColumn.identifier];
}
@end

@interface OakCommitWindowServer ()
@property (nonatomic) NSConnection* connection;
@end

@implementation OakCommitWindowServer
+ (instancetype)sharedInstance
{
	static OakCommitWindowServer* sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if(self = [super init])
	{
		_connection = [NSConnection new];
		[_connection setRootObject:self];
		if([_connection registerName:kOakCommitWindowServerConnectionName] == NO)
			NSLog(@"failed to setup connection ‘%@’", kOakCommitWindowServerConnectionName);
	}
	return self;
}

- (void)connectFromClientWithOptions:(NSDictionary*)someOptions
{
	OakCommitWindow* commitWindow = [[OakCommitWindow alloc] init];
	commitWindow.clientPortName = someOptions[kOakCommitWindowClientPortName];
	[commitWindow showWindow:self];
}
@end
