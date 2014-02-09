#import "HTMLOutputWindow.h"
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
	command::runner_ptr runner;
}
@property (nonatomic, retain) OakHTMLOutputView* htmlOutputView;
@property (nonatomic, readonly) BOOL running;
@end

@implementation HTMLOutputWindowController
- (id)initWithRunner:(command::runner_ptr const&)aRunner
{
	D(DBF_HTMLOutputWindow, bug("\n"););
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

- (BOOL)setCommandRunner:(command::runner_ptr const&)aRunner
{
	if(runner)
		Windows.erase(runner->uuid());

	runner = aRunner;
	Windows.emplace(runner->uuid(), self);

	self.window.title = [NSString stringWithCxxString:runner->name()];

	[self.htmlOutputView setEnvironment:runner->environment()];
	[self.htmlOutputView loadRequest:URLRequestForCommandRunner(runner) autoScrolls:runner->auto_scroll_output()];

	[self.window makeKeyAndOrderFront:nil];

	return YES;
}

- (BOOL)running
{
	return runner->running();
}

- (BOOL)needsNewWebView
{
	return _htmlOutputView.needsNewWebView;
}

- (void)tearDown
{
	foreach(it, Windows.lower_bound(runner->uuid()), Windows.upper_bound(runner->uuid()))
	{
		if(it->second == self)
		{
			Windows.erase(it);
			break;
		}
	}
}

- (void)closeWarningDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(void*)stack
{
	D(DBF_HTMLOutputWindow, bug("close %s\n", BSTR(returnCode == NSAlertDefaultReturn)););
	if(returnCode == NSAlertDefaultReturn) /* "Stop" */
	{
		oak::kill_process_group_in_background(runner->process_id());
		[self.window close];
		[self tearDown];
	}
}

- (BOOL)windowShouldClose:(id)aWindow
{
	D(DBF_HTMLOutputWindow, bug("\n"););
	if(!runner->running())
		return [self tearDown], YES;

	NSAlert* alert = [NSAlert alertWithMessageText:@"Stop task before closing?" defaultButton:@"Stop Task" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"The job that the task is performing will not be completed."];
	[alert beginSheetModalForWindow:self.window modalDelegate:self didEndSelector:@selector(closeWarningDidEnd:returnCode:contextInfo:) contextInfo:NULL];
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
