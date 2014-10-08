#import "CommitWindow.h"
#import "CWItem.h"
#import "CWStatusStringTransformer.h"
#import "CWTableCellView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakTextView/OakDocumentView.h>
#import <bundles/bundles.h>
#import <document/document.h>
#import <io/io.h>
#import <text/trim.h>
#import <text/tokenize.h>
#import <text/parse.h>
#import <ns/ns.h>
#import <oak/oak.h>
#import <plist/uuid.h>

@interface actionCommandObj : NSObject
@property (nonatomic, readonly) NSString* name;
@property (nonatomic, readonly) NSSet* targetStatuses;
@property (nonatomic, readonly) NSArray* command;
+ (actionCommandObj*)actionCommandWithString:(NSString*)aString;
@end

@implementation actionCommandObj
- (id)initWithName:(NSString*)aName command:(NSArray*)aCommand andTargetStatuses:(NSSet*)theTargetStatuses
{
	if((self = [super init]))
	{
		_name = aName;
		_command = aCommand;
		_targetStatuses = theTargetStatuses;
	}
	return self;
}

+ (actionCommandObj*)actionCommandWithString:(NSString*)aString
{
	NSRange range = [aString rangeOfString:@":"];
	NSArray* commandComponents = [[aString substringFromIndex:NSMaxRange(range)] componentsSeparatedByString:@","];
	NSString* statuses = [aString substringToIndex:range.location];
	NSArray* command = [commandComponents subarrayWithRange:NSMakeRange(1, [commandComponents count] - 1)];
	return [[actionCommandObj alloc] initWithName:[commandComponents objectAtIndex:0] command:command andTargetStatuses:[NSSet setWithArray:[statuses componentsSeparatedByString:@","]]];
}
@end

static std::map<std::string, std::string> convert (NSDictionary* dict)
{
	std::map<std::string, std::string> res;
	for(NSString* key in dict)
		res[[key UTF8String]] = [[dict objectForKey:key] UTF8String];
	return res;
}

static std::string absolute_path_for_tool (std::string const& tool, std::map<std::string, std::string> const& env)
{
	if(!path::is_executable(tool))
	{
		std::map<std::string, std::string>::const_iterator pathList = env.find("PATH");
		if(pathList != env.end())
		{
			for(auto const& dir : text::tokenize(pathList->second.begin(), pathList->second.end(), ':'))
			{
				if(path::is_executable(path::join(dir, tool)))
					return path::join(dir, tool);
			}
		}
	}
	return tool;
}

static NSString* const kOakCommitWindowShowFileList = @"showFileListInCommitWindow";
static NSString* const kOakCommitWindowCommitMessages = @"commitMessages";

static NSUInteger const kOakCommitWindowCommitMessagesTitleLength = 30;
static NSUInteger const kOakCommitWindowCommitMessagesMax = 5;

static void* kOakCommitWindowIncludeItemBinding = &kOakCommitWindowIncludeItemBinding;

@interface OakCommitWindow : NSWindowController <NSWindowDelegate, NSTableViewDelegate, NSMenuDelegate, OakTextViewDelegate>
@property (nonatomic) NSMutableDictionary*               options;
@property (nonatomic) NSMutableArray*                    parameters;
@property (nonatomic) std::map<std::string, std::string> environment;
@property (nonatomic) NSArrayController*                 arrayController;
@property (nonatomic) NSString*                          clientPortName;

@property (nonatomic) NSPopUpButton*                     previousCommitMessagesPopUpButton;
@property (nonatomic) OakDocumentView*                   documentView;
@property (nonatomic) NSScrollView*                      scrollView;
@property (nonatomic) NSTableView*                       tableView;

@property (nonatomic) NSView*                            topDivider;
@property (nonatomic) NSView*                            middleDivider;
@property (nonatomic) NSView*                            bottomDivider;

@property (nonatomic) NSButton*                          showTableButton;
@property (nonatomic) NSButton*                          commitButton;
@property (nonatomic) NSButton*                          cancelButton;

@property (nonatomic) BOOL                               showsTableView;
@property (nonatomic) OakCommitWindow*                   retainedSelf;
@end

@implementation OakCommitWindow
- (id)initWithOptions:(NSDictionary*)someOptions
{
	if((self = [super init]))
	{
		self.clientPortName = someOptions[kOakCommitWindowClientPortName];
		[self parseArguments:someOptions[kOakCommitWindowArguments]];
		self.environment = convert(someOptions[kOakCommitWindowEnvironment]);

		// send all diffs to a separate window
		_environment["TM_PROJECT_UUID"] = to_s(oak::uuid_t().generate());

		self.documentView = [[OakDocumentView alloc] initWithFrame:NSZeroRect];
		self.documentView.hideStatusBar     = YES;
		self.documentView.textView.delegate = self;

		_arrayController = [[NSArrayController alloc] init];
		_arrayController.objectClass = [CWItem class];

		[CWStatusStringTransformer register];

		NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"path"];
		tableColumn.editable = NO;

		NSTableView* tableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
		[tableView addTableColumn:tableColumn];
		tableView.headerView                         = nil;
		tableView.focusRingType                      = NSFocusRingTypeNone;
		tableView.usesAlternatingRowBackgroundColors = YES;
		tableView.doubleAction                       = @selector(didDoubleClickTableView:);
		tableView.target                             = self;
		tableView.delegate                           = self;
		tableView.menu                               = [NSMenu new];
		tableView.menu.delegate                      = self;
		_tableView                                   = tableView;

		[self populateTableView];
		[_tableView bind:NSContentBinding toObject:_arrayController withKeyPath:@"arrangedObjects" options:0];

		_scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		_scrollView.hasVerticalScroller   = YES;
		_scrollView.hasHorizontalScroller = NO;
		_scrollView.autohidesScrollers    = YES;
		_scrollView.borderType            = NSNoBorder;
		_scrollView.documentView          = _tableView;

		self.window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 600, 350) styleMask:NSTitledWindowMask|NSResizableWindowMask backing:NSBackingStoreBuffered defer:NO];
		self.window.delegate           = self;
		self.window.releasedWhenClosed = NO;
		self.window.frameAutosaveName  = @"Commit Window";

		_commitButton = OakCreateButton(@"Commit", NSRoundedBezelStyle);
		_commitButton.action                    = @selector(performCommit:);
		_commitButton.keyEquivalent             = @"\r";
		_commitButton.keyEquivalentModifierMask = NSCommandKeyMask;
		_commitButton.target                    = self;

		_cancelButton = OakCreateButton(@"Cancel", NSRoundedBezelStyle);
		_cancelButton.action                    = @selector(cancel:);
		_cancelButton.keyEquivalent             = @".";
		_cancelButton.keyEquivalentModifierMask = NSCommandKeyMask;
		_cancelButton.target                    = self;

		_showTableButton = [[NSButton alloc] initWithFrame:NSZeroRect];
		_showTableButton.buttonType = NSOnOffButton;
		_showTableButton.bezelStyle = NSRoundedDisclosureBezelStyle;
		_showTableButton.title = @"";
		[_showTableButton bind:NSValueBinding toObject:self withKeyPath:@"showsTableView" options:nil];

		_previousCommitMessagesPopUpButton = [NSPopUpButton new];
		_previousCommitMessagesPopUpButton.bordered   = YES;
		_previousCommitMessagesPopUpButton.pullsDown  = YES;
		_previousCommitMessagesPopUpButton.bezelStyle = NSTexturedRoundedBezelStyle;
		[self setupPreviousCommitMessagesMenu];

		_topDivider     = OakCreateHorizontalLine([NSColor grayColor]);
		_middleDivider  = OakCreateHorizontalLine([NSColor grayColor]);
		_bottomDivider  = OakCreateHorizontalLine([NSColor grayColor]);

		NSView* contentView = self.window.contentView;
		for(NSView* view in [self.allViews allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[contentView addSubview:view];
		}

		[self updateConstraints];

		[_arrayController addObserver:self forKeyPath:@"arrangedObjects.commit" options:NSKeyValueObservingOptionInitial|NSKeyValueObservingOptionNew context:kOakCommitWindowIncludeItemBinding];
	}
	return self;
}

- (void)dealloc
{
	[self.arrayController removeObserver:self forKeyPath:@"arrangedObjects.commit"];
}

- (NSDictionary*)allViews
{
	NSDictionary* views = @{
		@"previousMessages"   : self.previousCommitMessagesPopUpButton,
		@"topDivider"         : self.topDivider,
		@"documentView"       : self.documentView,
		@"middleDivider"      : self.middleDivider,
		@"scrollView"         : self.scrollView,
		@"bottomDivider"      : self.bottomDivider,
		@"showTableButton"    : self.showTableButton,
		@"cancel"             : self.cancelButton,
		@"commit"             : self.commitButton,
	};

	return views;
}

- (void)updateConstraints
{
	NSView* contentView = self.window.contentView;
	[contentView removeConstraints:[contentView constraints]];

	NSDictionary* views = self.allViews;

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[previousMessages(>=200)]-(20)-|" options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[documentView(>=400,==topDivider,==bottomDivider)]|" options:0 metrics:nil views:views]];

	if(self.showsTableView)
	{
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView(==documentView,==middleDivider)]|" options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(12)-[previousMessages]-(12)-[topDivider][documentView(>=195)][middleDivider][scrollView(==190)][bottomDivider]-(12)-[commit]-(12)-|" options:0 metrics:nil views:views]];
	}
	else
	{
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(12)-[previousMessages]-(12)-[topDivider][documentView(>=195)][bottomDivider]-(12)-[commit]-(12)-|" options:0 metrics:nil views:views]];
	}

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(20)-[showTableButton]" options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[cancel]-[commit]-(20)-|" options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[bottomDivider]-(12)-[showTableButton]-(12)-|" options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[bottomDivider]-(12)-[cancel]-(12)-|" options:0 metrics:nil views:views]];
}

- (void)parseArguments:(NSArray*)args
{
	NSArray* optionKeys = @[@"--log", @"--diff-cmd", @"--action-cmd", @"--status"];
	args = [args subarrayWithRange:NSMakeRange(1, [args count]-1)];
	self.options    = [NSMutableDictionary dictionary];
	self.parameters = [NSMutableArray array];

	NSEnumerator* enumerator = [args objectEnumerator];
	NSString* arg;

	NSMutableArray* actions = [NSMutableArray array];

	if([args count] < 2)
		[self cancel:nil];

	while(arg = [enumerator nextObject])
	{
		if([optionKeys containsObject:arg])
		{
			if(NSString* value = [enumerator nextObject])
			{
				if([arg isEqualToString:@"--action-cmd"])
						[actions addObject:[actionCommandObj actionCommandWithString:value]];
				else	[self.options addEntriesFromDictionary:@{arg : value}];
			}
			else
			{
				[self cancel:nil];
			}
		}
		else
		{
			[self.parameters addObject:arg];
		}
	}

	if(actions != nil | [actions count] != 0)
		[self.options setObject:actions forKey:@"--action-cmd"];
}

- (void)populateTableView
{
	auto selectedFiles = _environment.find("TM_SELECTED_FILES");
	BOOL didSelectFiles = selectedFiles != _environment.end();

	NSArray* statuses = [[self.options objectForKey:@"--status"] componentsSeparatedByString:@":"];
	for(NSUInteger i = 0; i < [statuses count]; i++)
	{
		NSString* status = [statuses objectAtIndex:i];
		CWItem* item = [CWItem itemWithPath:[self.parameters objectAtIndex:i] andSCMStatus:status commit:([status hasPrefix:@"X"] || ([status hasPrefix:@"?"] && !didSelectFiles)) ? NO : YES];
		[self.arrayController addObject:item];
	}
}

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)anObject change:(NSDictionary*)someChange context:(void*)context
{
	if(context == kOakCommitWindowIncludeItemBinding)
	{
		NSUInteger totalFilesToCommit = 0;
		for(CWItem* item in [_arrayController arrangedObjects])
		{
			if(item.commit)
				totalFilesToCommit += 1;
		}
		self.commitButton.title = totalFilesToCommit == 1 ? [NSString stringWithFormat:@"Commit %lu File", totalFilesToCommit] : [NSString stringWithFormat:@"Commit %lu Files", totalFilesToCommit];
	}
}

- (void)setShowsTableView:(BOOL)flag
{
	if(_showsTableView == flag)
		return;

	_showsTableView = flag;

	if(!_showsTableView)
		[self.window makeFirstResponder:self.documentView.textView];

	for(NSView* view in @[ self.middleDivider, self.scrollView ])
	{
		if(_showsTableView)
				[self.window.contentView addSubview:view];
		else	[view removeFromSuperview];
	}

	[[NSUserDefaults standardUserDefaults] setBool:_showsTableView forKey:kOakCommitWindowShowFileList];

	[self updateConstraints];
}

- (void)sheetDidEnd:(id)sheetOrAlert returnCode:(NSInteger)returnCode contextInfo:(void*)unused
{
	if(self.documentView.document)
	{
		NSString* commitMessage = [NSString stringWithCxxString:self.documentView.document->content()];
		if([[commitMessage stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]] length] > 0)
			[self saveCommitMessage:commitMessage];
	}

	[self.documentView setDocument:document::document_ptr()];
	[self sendCommitMessageToClient:NO];
	[self.window orderOut:self];
}

- (void)beginSheetModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(NSInteger returnCode))aCompletionHandler
{
	std::string fileType = "text.plain";
	auto scmName = _environment.find("TM_SCM_NAME");
	if(scmName != _environment.end())
	{
		std::string fileGrammar = "text." + scmName->second + "-commit";
		for(auto item : bundles::query(bundles::kFieldGrammarScope, fileGrammar, scope::wildcard, bundles::kItemTypeGrammar))
			fileType = item->value_for_field(bundles::kFieldGrammarScope);
	}

	document::document_ptr commitMessage = document::from_content("", fileType);
	commitMessage->set_custom_name("Commit Message"); // release ‘untitled’ token
	commitMessage->set_virtual_path(path::join(_environment["TM_PROJECT_DIRECTORY"], "commit-message.txt"));

	if(NSString* logArgument = [self.options objectForKey:@"--log"])
		commitMessage->set_content(to_s(logArgument));

	[self.documentView setDocument:commitMessage];

	[self.window recalculateKeyViewLoop];
	[self.window makeFirstResponder:self.documentView];

	self.showsTableView = [[NSUserDefaults standardUserDefaults] boolForKey:kOakCommitWindowShowFileList];

	self.retainedSelf = self;

	[NSApp beginSheet:self.window modalForWindow:aWindow modalDelegate:self didEndSelector:@selector(sheetDidEnd:returnCode:contextInfo:) contextInfo:NULL];
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
			NSString* commitMessage = [NSString stringWithCxxString:self.documentView.document->content()];

			NSMutableArray* outputArray = [NSMutableArray array];
			[outputArray addObject:[NSString stringWithFormat:@" -m '%@' ", [commitMessage stringByReplacingOccurrencesOfString:@"'" withString:@"'\"'\"'"]]];
			for(CWItem* item in [_arrayController arrangedObjects])
			{
				if(item.commit)
					[outputArray addObject:[NSString stringWithCxxString:path::escape([item.path UTF8String])]];
			}
			[outputArray addObject:@"\n"];
			[proxy connectFromServerWithOptions:@{
				kOakCommitWindowStandardOutput : [outputArray componentsJoinedByString:@" "],
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

- (void)chooseAllItems:(BOOL)aState
{
	for(CWItem* item in [_arrayController arrangedObjects])
		item.commit = aState;
}

- (void)saveCommitMessage:(NSString*)aCommitMessage
{
	NSUserDefaults* defaults = [NSUserDefaults standardUserDefaults];
	NSMutableArray* messages = [[defaults arrayForKey:kOakCommitWindowCommitMessages] mutableCopy];
	if(messages)
	{
		NSUInteger currentIndex = [messages indexOfObject:aCommitMessage];
		if(currentIndex != NSNotFound)
			[messages removeObjectAtIndex:currentIndex];

		[messages addObject:aCommitMessage];
		if([messages count] > kOakCommitWindowCommitMessagesMax)
			[messages removeObjectAtIndex:0];
	}
	else
	{
		messages = [NSMutableArray arrayWithObject:aCommitMessage];
	}

	[defaults setObject:messages forKey:kOakCommitWindowCommitMessages];
	[defaults synchronize];
}

- (void)setupPreviousCommitMessagesMenu
{
	NSMenu* menu = [self.previousCommitMessagesPopUpButton menu];
	[menu removeAllItems];
	[menu addItemWithTitle:@"Previous Commit Messages" action:NULL keyEquivalent:@""];

	if(NSArray* commitMessages = [[NSUserDefaults standardUserDefaults] arrayForKey:kOakCommitWindowCommitMessages])
	{
		[commitMessages enumerateObjectsWithOptions:NSEnumerationReverse usingBlock:^(NSString* message, NSUInteger idx, BOOL* stop){
			NSString* title = message;
			if([title length] > kOakCommitWindowCommitMessagesTitleLength)
				title = [[title substringToIndex:kOakCommitWindowCommitMessagesTitleLength] stringByAppendingString:@"…"];

			NSMenuItem* item = [menu addItemWithTitle:title action:@selector(restorePreviousCommitMessage:) keyEquivalent:@""];
			[item setToolTip:message];
			[item setTarget:self];
			[item setRepresentedObject:message];
		}];

		[menu addItem:[NSMenuItem separatorItem]];
		NSMenuItem* item = [menu addItemWithTitle:@"Clear Menu" action:@selector(clearPreviousCommitMessages:) keyEquivalent:@""];
		[item setTarget:self];
	}
	else
	{
		[self.previousCommitMessagesPopUpButton setEnabled:NO];
	}
}

- (void)restorePreviousCommitMessage:(id)sender
{
	NSString* message = [sender representedObject];
	document::document_ptr commitMessage = document::from_content([message UTF8String], self.documentView.document->file_type());
	[self.documentView setDocument:commitMessage];
}

- (void)clearPreviousCommitMessages:(id)sender
{
	[[NSUserDefaults standardUserDefaults] removeObjectForKey:kOakCommitWindowCommitMessages];
	[self setupPreviousCommitMessagesMenu];
}

- (void)performBundleItem:(bundles::item_ptr const&)anItem
{
	if(anItem->kind() == bundles::kItemTypeTheme)
	{
		[self.documentView setThemeWithUUID:[NSString stringWithCxxString:anItem->uuid()]];
	}
	else
	{
		[self showWindow:self];
		[self.window makeFirstResponder:self.documentView.textView];
		[self.documentView.textView performBundleItem:anItem];
	}
}

// ==================
// = Action Methods =
// ==================

- (void)didDoubleClickTableView:(id)sender
{
	if(_tableView.clickedRow == -1 && [_tableView rowForView:sender] == -1)
		return;

	if(NSString* diffCommand = [self.options objectForKey:@"--diff-cmd"])
	{
		std::vector<std::string> diffCmd = text::split(to_s(diffCommand), ",");
		diffCmd.front() = absolute_path_for_tool(diffCmd.front(), _environment);

		NSInteger row = [_tableView rowForView:sender] == -1 ? _tableView.clickedRow : [_tableView rowForView:sender];
		NSString* filePath = [[[_arrayController arrangedObjects] objectAtIndex:row] path];
		diffCmd.push_back(to_s(filePath));

		std::transform(diffCmd.begin(), diffCmd.end(), diffCmd.begin(), &path::escape);

		std::string const cmdString = text::format("cd \"${TM_PROJECT_DIRECTORY}\" && %s|\"$TM_MATE\" --no-wait --name \"---/+++ %s\"", text::join(diffCmd, " ").c_str(), path::display_name(to_s(filePath)).c_str());
		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			bool success = io::exec(_environment, "/bin/sh", "-c", cmdString.c_str(), NULL) != NULL_STR;
			if(!success)
			{
				dispatch_async(dispatch_get_main_queue(), ^{
					NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Failed running diff command." informativeText:[NSString stringWithCxxString:cmdString] buttons:@"OK", nil];
					OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){});
				});
			}
		});
	}
}

- (void)performCommit:(id)sender
{
	[self sendCommitMessageToClient:YES];
	[NSApp endSheet:self.window returnCode:NSRunStoppedResponse];
}

- (void)cancel:(id)sender
{
	[self sendCommitMessageToClient:NO];
	[NSApp endSheet:self.window returnCode:NSRunAbortedResponse];
}

- (void)checkAll:(id)sender
{
	[self chooseAllItems:YES];
}

- (void)uncheckAll:(id)sender
{
	[self chooseAllItems:NO];
}

- (void)performActionCommand:(id)sender
{
	std::vector<std::string> command;

	actionCommandObj* obj = [sender representedObject];
	for(NSString* str in obj.command)
		command.push_back(to_s(str));
	command.front() = absolute_path_for_tool(command.front(), _environment);

	NSInteger row = [_tableView clickedColumn] == -1 ? [_tableView selectedRow] : [_tableView clickedRow];
	NSString* filePath = [[[_arrayController arrangedObjects] objectAtIndex:row] path];
	command.push_back(to_s(filePath));

	std::transform(command.begin(), command.end(), command.begin(), &path::escape);
	std::string const cmdString = "cd \"${TM_PROJECT_DIRECTORY}\" && " + text::join(command, " ");

	std::string res = io::exec(_environment, "/bin/sh", "-c", cmdString.c_str(), NULL);
	if(res != NULL_STR)
	{
		NSString* outputStatus = [NSString stringWithCxxString:res];
		NSRange rangeOfStatus = [outputStatus rangeOfCharacterFromSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];

		if(rangeOfStatus.location == NSNotFound)
		{
			NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Cannot understand output from command" informativeText:[NSString stringWithCxxString:cmdString] buttons:@"OK", nil];
			OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){});
		}
		NSString* newStatus = [outputStatus substringToIndex:rangeOfStatus.location];
		CWItem* item = [[_arrayController arrangedObjects] objectAtIndex:row];
		item.scmStatus = newStatus;
		item.commit    = NO;
	}
	else
	{
		NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Failed running command" informativeText:[NSString stringWithCxxString:cmdString] buttons:@"OK", nil];
		OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){});
	}
}

// ===============
// = Action menu =
// ===============

- (void)menuNeedsUpdate:(NSMenu*)menu
{
	[menu removeAllItems];

	NSInteger row = [_tableView clickedRow];
	if(row == -1 && [_tableView selectedRow] == -1)
	{
		[menu addItemWithTitle:@"Check All" action:@selector(checkAll:) keyEquivalent:@""];
		[menu addItemWithTitle:@"Uncheck All" action:@selector(uncheckAll:) keyEquivalent:@""];
	}
	else
	{
		if(NSArray* commands = [self.options objectForKey:@"--action-cmd"])
		{
			for(actionCommandObj* cmd in commands)
			{
				NSMenuItem* item = [[NSMenuItem alloc] initWithTitle:cmd.name action:@selector(performActionCommand:) keyEquivalent:@""];
				[item setRepresentedObject:cmd];
				[menu addItem:item];
			}
			[menu addItem:[NSMenuItem separatorItem]];
		}
		[menu addItemWithTitle:@"Check All" action:@selector(checkAll:) keyEquivalent:@""];
		[menu addItemWithTitle:@"Uncheck All" action:@selector(uncheckAll:) keyEquivalent:@""];
	}
}

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	BOOL active = YES;
	NSInteger row = [_tableView clickedColumn] == -1 || [_tableView clickedRow] == -1 ? [_tableView selectedRow] : [_tableView clickedRow];
	if([menuItem action] == @selector(performActionCommand:))
	{
		if(row == -1)
			return NO;

		CWItem* cwItem = [[_arrayController arrangedObjects] objectAtIndex:row];
		actionCommandObj* cmd = [menuItem representedObject];

		std::map<std::string, std::string> variables;
		if(active = [cmd.targetStatuses containsObject:cwItem.scmStatus])
			variables = { { "TM_DISPLAYNAME", path::display_name(to_s(cwItem.path)) } };

		menuItem.title = [NSString stringWithCxxString:format_string::expand(to_s(cmd.name), variables)];
	}
	return active;
}

// ========================
// = OakTextView Delegate =
// ========================

- (std::map<std::string, std::string>)variables
{
	std::map<std::string, std::string> res;
	auto projectDirectory = _environment.find("TM_PROJECT_DIRECTORY");
	if(projectDirectory != _environment.end())
		res["TM_PROJECT_DIRECTORY"] = projectDirectory->second;
	return res;
}

// ========================
// = NSTableView Delegate =
// ========================

- (NSView*)tableView:(NSTableView*)aTableView viewForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)row
{
	CWTableCellView* cellView = [aTableView makeViewWithIdentifier:aTableColumn.identifier owner:self];
	if(!cellView)
	{
		NSViewController* viewController = [[NSViewController alloc] initWithNibName:@"CWTableCellView" bundle:[NSBundle bundleForClass:[self class]]];
		cellView = (CWTableCellView*)viewController.view;
		cellView.identifier = aTableColumn.identifier;
	}

	[cellView.textField bind:NSValueBinding toObject:cellView withKeyPath:@"objectValue.path" options:0];
	[cellView.commitCheckBox bind:NSValueBinding toObject:cellView withKeyPath:@"objectValue.commit" options:0];
	[cellView.statusTextField bind:NSValueBinding toObject:cellView withKeyPath:@"objectValue.scmStatus" options:@{ NSValueTransformerNameBindingOption : @"CWStatusStringTransformer" }];

	[cellView.diffButton setAction:@selector(didDoubleClickTableView:)];
	[cellView.diffButton setTarget:self];
	if(![self.options objectForKey:@"--diff-cmd"])
		[cellView.diffButton setHidden:YES];

	return cellView;
}
@end

@protocol OakProjectIdentifier
- (NSString*)identifier;
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
	NSWindow* projectWindow = [NSApp mainWindow];
	if(NSString* identifier = [someOptions valueForKeyPath:@"environment.TM_PROJECT_UUID"])
	{
		for(NSWindow* window in [NSApp orderedWindows])
		{
			if([window.delegate respondsToSelector:@selector(identifier)])
			{
				if([identifier isEqualToString:[id <OakProjectIdentifier>(window.delegate) identifier]])
				{
					projectWindow = window;
					break;
				}
			}
		}
	}

	OakCommitWindow* commitWindow = [[OakCommitWindow alloc] initWithOptions:someOptions];
	[commitWindow beginSheetModalForWindow:projectWindow completionHandler:^(NSInteger returnCode){ }];
}
@end
