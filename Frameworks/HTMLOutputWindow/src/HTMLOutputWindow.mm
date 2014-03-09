#import "HTMLOutputWindow.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakWindowFrameHelper.h>
#import <OakFoundation/NSString Additions.h>
#import <OakSystem/process.h>
#import <command/runner.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(HTMLOutputWindow);

static std::multimap<oak::uuid_t, HTMLOutputWindowController*> Windows;

@interface HTMLOutputWindowController ()
{
	OBJC_WATCH_LEAKS(HTMLOutputWindowController);
}
@property (nonatomic) OakHTMLOutputView* htmlOutputView;
@end

@implementation HTMLOutputWindowController
- (id)init
{
	if(self = [super init])
	{
		self.window         = [[NSWindow alloc] initWithContentRect:NSMakeRect(100, 100, 100, 100) styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask) backing:NSBackingStoreBuffered defer:NO];
		self.htmlOutputView = [[OakHTMLOutputView alloc] init];

		[self.window setFrameAutosaveName:@"Command Output (HTML)"];
		[self.window bind:NSTitleBinding toObject:self.htmlOutputView.webView withKeyPath:@"mainFrameTitle" options:nil];
		[self.window bind:NSDocumentEditedBinding toObject:self.htmlOutputView withKeyPath:@"runningCommand" options:nil];
		[self.window setContentView:self.htmlOutputView];
		[self.window setDelegate:self];
		[self.window setReleasedWhenClosed:NO];
		[self.window setAutorecalculatesContentBorderThickness:NO forEdge:NSMinYEdge];
		[self.window setContentBorderThickness:25 forEdge:NSMinYEdge];
		[self.window setCollectionBehavior:NSWindowCollectionBehaviorMoveToActiveSpace|NSWindowCollectionBehaviorFullScreenAuxiliary];

		[OakWindowFrameHelper windowFrameHelperWithWindow:self.window];
	}
	return self;
}

- (id)initWithRunner:(command::runner_ptr const&)aRunner
{
	D(DBF_HTMLOutputWindow, bug("\n"););
	if(self = [self init])
	{
		[self setCommandRunner:aRunner];
	}
	return self;
}

+ (HTMLOutputWindowController*)HTMLOutputWindowWithRunner:(command::runner_ptr const&)aRunner
{
	D(DBF_HTMLOutputWindow, bug("%s\n", to_s(aRunner->uuid()).c_str()););
	foreach(it, Windows.lower_bound(aRunner->uuid()), Windows.upper_bound(aRunner->uuid()))
	{
		HTMLOutputWindowController* controller = it->second;
		if(![controller running])
		{
			D(DBF_HTMLOutputWindow, bug("found existing controller\n"););
			return [controller setCommandRunner:aRunner], controller;
		}
	}

	for(NSWindow* window in [NSApp orderedWindows])
	{
		HTMLOutputWindowController* delegate = [window delegate];
		if(![window isMiniaturized] && [window isVisible] && [delegate isKindOfClass:[HTMLOutputWindowController class]])
		{
			D(DBF_HTMLOutputWindow, bug("found existing window\n"););
			return [delegate setCommandRunner:aRunner], delegate;
		}
	}

	return [[self alloc] initWithRunner:aRunner];
}

- (void)setCommandRunner:(command::runner_ptr)aRunner
{
	if(_commandRunner)
		Windows.erase(_commandRunner->uuid());

	_commandRunner = aRunner;
	Windows.emplace(_commandRunner->uuid(), self);

	self.window.title = [NSString stringWithCxxString:_commandRunner->name()];
	[self.htmlOutputView loadRequest:URLRequestForCommandRunner(_commandRunner) environment:_commandRunner->environment() autoScrolls:_commandRunner->auto_scroll_output()];
	[self.window makeKeyAndOrderFront:nil];

}

- (BOOL)running
{
	return _commandRunner->running();
}

- (BOOL)needsNewWebView
{
	return _htmlOutputView.needsNewWebView;
}

- (void)tearDown
{
	foreach(it, Windows.lower_bound(_commandRunner->uuid()), Windows.upper_bound(_commandRunner->uuid()))
	{
		if(it->second == self)
		{
			Windows.erase(it);
			break;
		}
	}
}

- (BOOL)windowShouldClose:(id)aWindow
{
	D(DBF_HTMLOutputWindow, bug("\n"););
	if(!_commandRunner->running())
		return [self tearDown], YES;

	NSAlert* alert = [NSAlert alertWithMessageText:@"Stop task before closing?" defaultButton:@"Stop Task" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"The job that the task is performing will not be completed."];
	OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){
		D(DBF_HTMLOutputWindow, bug("close %s\n", BSTR(returnCode == NSAlertDefaultReturn)););
		if(returnCode == NSAlertDefaultReturn) /* "Stop" */
		{
			oak::kill_process_group_in_background(_commandRunner->process_id());
			[self.window close];
			[self tearDown];
		}
	});
	return NO;
}

- (void)dealloc
{
	D(DBF_HTMLOutputWindow, bug("\n"););
	self.window.delegate = nil;
}

- (IBAction)toggleHTMLOutput:(id)sender
{
	[self.window performClose:self];
}

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	if([menuItem action] == @selector(toggleHTMLOutput:))
		[menuItem setTitle:@"Hide HTML Output"];
	return YES;
}
@end
