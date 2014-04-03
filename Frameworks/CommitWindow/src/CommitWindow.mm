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
#import <oak/oak.h>

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

static std::map<std::string, std::string> convert(NSDictionary* dict)
{
	std::map<std::string, std::string> res;
	for(NSString* key in dict)
		res[[key UTF8String]] = [[dict objectForKey:key] UTF8String];
	return res;
}

static NSString* const kOakCommitWindowCommitMessages = @"commitMessages";

static NSUInteger const kOakCommitWindowCommitMessagesTitleLength = 30;
static NSUInteger const kOakCommitWindowCommitMessagesMax = 5;

@interface OakCommitWindow () <NSWindowDelegate, NSTableViewDelegate, NSMenuDelegate, OakTextViewDelegate>
@property (nonatomic) NSMutableDictionary*               options;
@property (nonatomic) NSMutableArray*                    parameters;
@property (nonatomic) std::map<std::string, std::string> environment;
@property (nonatomic) NSArrayController*                 arrayController;
@property (nonatomic) NSString*                          clientPortName;
@property (nonatomic) OakDocumentView*                   documentView;
@property (nonatomic) NSScrollView*                      scrollView;
@property (nonatomic) NSTableView*                       tableView;
@property (nonatomic) NSPopUpButton*                     actionPopUpButton;
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

		self.documentView = [[OakDocumentView alloc] initWithFrame:NSZeroRect];
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

		[_tableView registerNib:[[NSNib alloc] initWithNibNamed:@"CWTableCellView" bundle:[NSBundle bundleForClass:[self class]]] forIdentifier:@"path"];
		[self populateTableView];
		[_tableView bind:NSContentBinding toObject:_arrayController withKeyPath:@"arrangedObjects" options:0];

		_scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		_scrollView.hasVerticalScroller   = YES;
		_scrollView.hasHorizontalScroller = NO;
		_scrollView.autohidesScrollers    = YES;
		_scrollView.borderType            = NSNoBorder;
		_scrollView.documentView          = _tableView;

		self.window = [[NSWindow alloc] initWithContentRect:NSMakeRect(600, 700, 400, 500) styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSTexturedBackgroundWindowMask) backing:NSBackingStoreBuffered defer:NO];
		self.window.delegate           = self;
		self.window.level              = NSFloatingWindowLevel;
		self.window.releasedWhenClosed = NO;
		self.window.title              = @"Commit";
		[self.window setAutorecalculatesContentBorderThickness:NO forEdge:NSMaxYEdge];
		[self.window setContentBorderThickness:29 forEdge:NSMaxYEdge];

		NSButton* commitButton = OakCreateButton(@"Commit", NSTexturedRoundedBezelStyle);
		NSButton* cancelButton = OakCreateButton(@"Cancel", NSTexturedRoundedBezelStyle);

		commitButton.action = @selector(performCommit:);
		[commitButton setKeyEquivalent:@"s"];
		[commitButton setKeyEquivalentModifierMask:NSCommandKeyMask];

		cancelButton.action = @selector(cancel:);
		cancelButton.target = self;
		[cancelButton setKeyEquivalent:@"w"];
		[cancelButton setKeyEquivalentModifierMask:NSCommandKeyMask];

		_actionPopUpButton = OakCreateActionPopUpButton(YES);
		_actionPopUpButton.bezelStyle = NSTexturedRoundedBezelStyle;
		_actionPopUpButton.menu.delegate = self;

		NSPopUpButton* previousCommitMessagesPopUpButton = [NSPopUpButton new];
		previousCommitMessagesPopUpButton.bordered   = YES;
		previousCommitMessagesPopUpButton.pullsDown  = NO;
		previousCommitMessagesPopUpButton.bezelStyle = NSTexturedRoundedBezelStyle;
		NSMenuItem* placeholder = [NSMenuItem new];
		placeholder.title = @"Previous Commit Messages";
		[[previousCommitMessagesPopUpButton cell] setUsesItemFromMenu:NO];
		[[previousCommitMessagesPopUpButton cell] setMenuItem:placeholder];

		// ========================================
		// = Create previous commit messages menu =
		// ========================================

		NSMenu* aMenu = [previousCommitMessagesPopUpButton menu];
		NSUserDefaults* defaults = [NSUserDefaults standardUserDefaults];
		NSArray* commitMessages = [defaults arrayForKey:kOakCommitWindowCommitMessages];
		if(commitMessages == nil)
		{
			[previousCommitMessagesPopUpButton setEnabled:NO];
		}
		else
		{
			[commitMessages enumerateObjectsWithOptions:NSEnumerationReverse usingBlock:^(NSString* message, NSUInteger idx, BOOL* stop){
				NSString* title = message;
				if([title length] > kOakCommitWindowCommitMessagesTitleLength)
					title = [[title substringToIndex:kOakCommitWindowCommitMessagesTitleLength] stringByAppendingString:@"…"];

				NSMenuItem* item = [aMenu addItemWithTitle:title action:@selector(restorePreviousCommitMessage:) keyEquivalent:@""];
				[item setToolTip:message];
				[item setTarget:self];
				[item setRepresentedObject:message];
			}];
			[previousCommitMessagesPopUpButton setMenu:aMenu];
		}

		// ===============
		// = Constraints =
		// ===============

		NSDictionary* views = @{
			@"previousMessages"   : previousCommitMessagesPopUpButton,
			@"topDivider"         : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"documentView"       : self.documentView,
			@"middleDivider"      : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"scrollView"         : self.scrollView,
			@"bottomDivider"      : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"action"             : self.actionPopUpButton,
			@"cancel"             : cancelButton,
			@"commit"             : commitButton,
		};

		NSView* contentView = self.window.contentView;
		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[contentView addSubview:view];
		}

		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[topDivider]|" options:NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[documentView(==middleDivider)]|" options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView(==bottomDivider)]|" options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[cancel]-[commit]-(8)-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[action(==36)]" options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[previousMessages(>=200)]-(8)-|" options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(3)-[previousMessages]-(3)-[topDivider][documentView(>=100)][middleDivider][scrollView(>=200)][bottomDivider]-(5)-[commit]-(6)-|" options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[bottomDivider]-(5)-[action]-(6)-|" options:0 metrics:nil views:views]];
	}
	return self;
}

- (void)parseArguments:(NSArray*)args
{
	NSArray* optionKeys = @[@"--ask", @"--log", @"--diff-cmd", @"--action-cmd", @"--status"];
	args = [args subarrayWithRange:NSMakeRange(1, [args count]-1)];
	self.options    = [NSMutableDictionary dictionary];
	self.parameters = [NSMutableArray array];

	NSEnumerator* enumerator = [args objectEnumerator];
	NSString* arg;

	NSMutableArray* actions = [NSMutableArray array];

	if([args count] < 2)
		[self cancel:nil];

	while (arg = [enumerator nextObject])
	{
		if([optionKeys containsObject:arg])
		{
			if(NSString* value = [enumerator nextObject])
			{
				if([arg isEqualToString:@"--action-cmd"])
					[actions addObject:[actionCommandObj actionCommandWithString:value]];
				else [self.options addEntriesFromDictionary:@{arg : value}];
			}
			else [self cancel:nil];
		}
		else [self.parameters addObject:arg];
	}

	if(actions != nil | [actions count] != 0)
		[self.options setObject:actions forKey:@"--action-cmd"];
}

- (void)populateTableView
{
	auto selected_files = _environment.find("TM_SELECTED_FILES");
	BOOL didSelectFiles = selected_files != _environment.end();

	NSArray* statuses = [[self.options objectForKey:@"--status"] componentsSeparatedByString:@":"];
	for(NSUInteger i = 0; i < [statuses count]; i++)
	{
		NSString* status = [statuses objectAtIndex:i];
		CWItem* item = [CWItem itemWithPath:[self.parameters objectAtIndex:i] andSCMStatus:status commit:([status hasPrefix:@"X"] || ([status hasPrefix:@"?"] && !didSelectFiles)) ? NO : YES];
		[self.arrayController addObject:item];
	}
}

- (void)showWindow:(id)sender
{
	std::string file_type = "text.plain";
	auto scm_name = _environment.find("TM_SCM_NAME");
	if(scm_name != _environment.end())
	{
		std::string file_grammar = "text." + scm_name->second + "-commit";
		for(auto item : bundles::query(bundles::kFieldGrammarScope, file_grammar, scope::wildcard, bundles::kItemTypeGrammar))
		{
			if(item)
				file_type = item->value_for_field(bundles::kFieldGrammarScope);
		}
	}

	document::document_ptr commitMessage = document::from_content("", file_type);
	[self.documentView setDocument:commitMessage];

	std::string title = text::format("Commit (%s %s: %s)", path::display_name(_environment["TM_PROJECT_DIRECTORY"]).c_str(), scm_name->second.c_str(), _environment["TM_SCM_BRANCH"].c_str());
	[self.window setTitle:[NSString stringWithCxxString:title]];

	[self.window recalculateKeyViewLoop];
	[self.window makeFirstResponder:self.documentView];
	[super showWindow:sender];

	self.retainedSelf = self;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self.documentView setDocument:document::document_ptr()];
	[self sendCommitMessageToClient:NO];
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

			if([[commitMessage stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]] length] > 0)
				[self saveCommitMessage:commitMessage];

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

- (void)chooseAllItems:(BOOL)aState
{
	for(CWItem* item in [_arrayController arrangedObjects])
			item.commit = aState;
}

- (NSString*)absolutePathForPath:(NSString*)path
{
	NSString* newPath;
	std::string const oldPath = [path UTF8String];

	std::string res = io::exec("/usr/bin/which", oldPath.c_str(), NULL);
	if(res != NULL_STR)
		newPath = [NSString stringWithCxxString:path::escape(text::trim(res))];
	else newPath = [NSString stringWithCxxString:path::escape(res)];

	return newPath;
}

- (void)saveCommitMessage:(NSString*)aCommitMessage
{
	NSUserDefaults* defaults = [NSUserDefaults standardUserDefaults];
	NSMutableArray* messages = [[defaults arrayForKey:kOakCommitWindowCommitMessages] mutableCopy];
	if(messages)
	{
		if([messages containsObject:aCommitMessage])
		{
			NSUInteger currentIndex = [messages indexOfObject:aCommitMessage];
			[messages exchangeObjectAtIndex:currentIndex withObjectAtIndex:[messages count] -1];
		}
		else
		{
			[messages addObject:aCommitMessage];
			if([messages count] > kOakCommitWindowCommitMessagesMax)
				[messages removeObjectAtIndex:0];
		}
	}
	else
	{
		messages = [NSMutableArray arrayWithObject:aCommitMessage];
	}

	[defaults setObject:messages forKey:kOakCommitWindowCommitMessages];
	[defaults synchronize];
}

- (void)restorePreviousCommitMessage:(id)sender
{
	NSString* message = [sender representedObject];
	document::document_ptr commitMessage = document::from_content([message UTF8String], self.documentView.document->file_type());
	[self.documentView setDocument:commitMessage];
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
		NSInteger row = [_tableView rowForView:sender] == -1 ? _tableView.clickedRow : [_tableView rowForView:sender];

		std::string const pwd = _environment["PWD"];
		std::string const tm_mate = _environment["TM_MATE"];
		NSMutableArray* arguments = [[diffCommand componentsSeparatedByString:@","] mutableCopy];
		NSString* filePath = [[[_arrayController arrangedObjects] objectAtIndex:row] path];
		[arguments replaceObjectAtIndex:0 withObject:[self absolutePathForPath:[arguments objectAtIndex:0]]];
		NSString* joinedArguments = [arguments componentsJoinedByString:@" "];

		std::string const cmd_string = text::format("cd %s && %s %s|%s --async", path::escape(pwd).c_str(), [joinedArguments UTF8String], path::escape([filePath UTF8String]).c_str(), path::escape(tm_mate).c_str());
		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			bool success = io::exec(_environment, "/bin/sh", "-c", cmd_string.c_str(), NULL) != NULL_STR;
			if(!success)
			{
				dispatch_async(dispatch_get_main_queue(), ^{
					NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Failed running diff command." informativeText:[NSString stringWithCxxString:cmd_string] buttons:@"OK", nil];
					OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){});
				});
			}
		});
	}
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
	actionCommandObj* cmd = [sender representedObject];
	NSMutableArray* arguments = [cmd.command mutableCopy];
	NSInteger row             = [_tableView clickedColumn] == -1 ? [_tableView selectedRow] : [_tableView clickedRow];

	std::string const filePath = [[[[_arrayController arrangedObjects] objectAtIndex:row] path] UTF8String];
	NSString* pathToCommand = [self absolutePathForPath:[arguments objectAtIndex:0]];
	[arguments replaceObjectAtIndex:0 withObject:pathToCommand];

	std::string const cmd_string = text::format("%s %s", [[arguments componentsJoinedByString:@" "] UTF8String], path::escape(filePath).c_str());
	std::string res = io::exec(_environment, "/bin/sh", "-c", cmd_string.c_str(), NULL);
	if(res != NULL_STR)
	{
		NSString* outputStatus = [NSString stringWithCxxString:res];
		NSRange rangeOfStatus = [outputStatus rangeOfCharacterFromSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];

		if(rangeOfStatus.location == NSNotFound)
		{
			NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Cannot understand output from command" informativeText:[NSString stringWithCxxString:cmd_string] buttons:@"OK", nil];
			OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){});
		}
		NSString* newStatus = [outputStatus substringToIndex:rangeOfStatus.location];
		CWItem* item = [[_arrayController arrangedObjects] objectAtIndex:row];
		item.scmStatus = newStatus;
		item.commit    = NO;
	}
	else
	{
		NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Failed running command" informativeText:[NSString stringWithCxxString:cmd_string] buttons:@"OK", nil];
		OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){});
	}

}

// ===========================
// = Action menu =
// ===========================

- (void)menuNeedsUpdate:(NSMenu*)menu
{
	[menu removeAllItems];
	if([_tableView clickedColumn] == -1)
		[menu addItemWithTitle:@"Dummy" action:@selector(nop:) keyEquivalent:@""];

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
		CWItem* cwItem = [[_arrayController arrangedObjects] objectAtIndex:row];
		actionCommandObj* cmd = [menuItem representedObject];
		if(![cmd.targetStatuses containsObject:cwItem.scmStatus])
			active = NO;
	}
	return active;
}

// ========================
// = OakTextView Delegate =
// ========================

- (std::map<std::string, std::string>)variables
{
	std::map<std::string, std::string> res;
	auto project_directory = _environment.find("TM_PROJECT_DIRECTORY");
	if(project_directory != _environment.end())
		res["TM_PROJECT_DIRECTORY"] = project_directory->second;
	return res;
}

// ========================
// = NSTableView Delegate =
// ========================

- (NSView*)tableView:(NSTableView*)aTableView viewForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)row
{
	CWTableCellView* cellView = [aTableView makeViewWithIdentifier:@"path" owner:self];

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
	OakCommitWindow* commitWindow = [[OakCommitWindow alloc] initWithOptions:someOptions];
	[commitWindow showWindow:self];
}
@end
