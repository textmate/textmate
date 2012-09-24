#import "HTMLOutputWindow.h"
#import <OakAppKit/OakWindowFrameHelper.h>
#import <command/runner.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(HTMLOutputWindow);

static std::multimap<oak::uuid_t, HTMLOutputWindowController*> Windows;

@interface HTMLOutputWindowController ()
@property (nonatomic, retain) NSWindow* window;
@property (nonatomic, retain) OakHTMLOutputView* htmlOutputView;
@property (nonatomic, readonly) BOOL running;
@end

@implementation HTMLOutputWindowController
{
	command::runner_ptr runner;
}

- (id)initWithRunner:(command::runner_ptr const&)aRunner
{
	if(self = [super init])
	{
		self.window         = [[NSWindow alloc] initWithContentRect:NSMakeRect(100, 100, 100, 100) styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask) backing:NSBackingStoreBuffered defer:NO];
		self.htmlOutputView = [[OakHTMLOutputView alloc] init];

		[self.window setFrameAutosaveName:@"Command Output (HTML)"];
		[self.window bind:@"title" toObject:self.htmlOutputView.webView withKeyPath:@"mainFrameTitle" options:nil];
		[self.window bind:@"documentEdited" toObject:self.htmlOutputView withKeyPath:@"runningCommand" options:nil];
		[self.window setContentView:self.htmlOutputView];
		[self.window setDelegate:self];
		[self.window setReleasedWhenClosed:NO];

		[OakWindowFrameHelper windowFrameHelperWithWindow:self.window];

		[self setCommandRunner:aRunner];
	}
	return self;
}

+ (HTMLOutputWindowController*)HTMLOutputWindowWithRunner:(command::runner_ptr const&)aRunner
{
	foreach(it, Windows.lower_bound(aRunner->uuid()), Windows.upper_bound(aRunner->uuid()))
	{
		HTMLOutputWindowController* controller = it->second;
		if(![controller running])
			return [controller setCommandRunner:aRunner], controller;
	}

	for(NSWindow* window in [NSApp orderedWindows])
	{
		HTMLOutputWindowController* delegate = [window delegate];
		if(![window isMiniaturized] && [delegate isKindOfClass:[HTMLOutputWindowController class]])
			return [delegate setCommandRunner:aRunner], delegate;
	}

	return [[self alloc] initWithRunner:aRunner];
}

- (BOOL)setCommandRunner:(command::runner_ptr const&)aRunner
{
	if(runner)
		Windows.erase(runner->uuid());

	runner = aRunner;
	Windows.insert(std::make_pair(runner->uuid(), self));

	[self.htmlOutputView setEnvironment:runner->environment()];
	[self.htmlOutputView loadRequest:URLRequestForCommandRunner(runner) autoScrolls:runner->auto_scroll_output()];

	[self.window makeKeyAndOrderFront:nil];

	return YES;
}

- (BOOL)running
{
	return runner->running();
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
@end
