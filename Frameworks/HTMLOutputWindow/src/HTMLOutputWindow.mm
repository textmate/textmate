#import "HTMLOutputWindow.h"
#import <OakAppKit/OakWindowFrameHelper.h>

OAK_DEBUG_VAR(HTMLOutputWindow);

static std::multimap<oak::uuid_t, HTMLOutputWindowController*> Windows;

@interface HTMLOutputWindowController ()
@property (nonatomic, readonly) BOOL running;
@end

@implementation HTMLOutputWindowController
- (id)initWithRunner:(command::runner_ptr const&)aRunner
{
	if(self = [[super init] retain])
	{
		window         = [[NSWindow alloc] initWithContentRect:NSMakeRect(100, 100, 100, 100) styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask) backing:NSBackingStoreBuffered defer:NO];
		htmlOutputView = [[OakHTMLOutputView alloc] init];

		[window setFrameAutosaveName:@"Command Output (HTML)"];
		[window bind:@"title" toObject:htmlOutputView.webView withKeyPath:@"mainFrameTitle" options:nil];
		[window bind:@"documentEdited" toObject:htmlOutputView withKeyPath:@"runningCommand" options:nil];
		[window setContentView:htmlOutputView];
		[window setDelegate:self];

		[OakWindowFrameHelper windowFrameHelperWithWindow:window];

		[self setCommandRunner:aRunner];
	}
	return self;
}

+ (HTMLOutputWindowController*)HTMLOutputWindowWithRunner:(command::runner_ptr const&)aRunner
{
	foreach(it, Windows.lower_bound(aRunner->uuid()), Windows.upper_bound(aRunner->uuid()))
	{
		if(![it->second running])
			return [it->second setCommandRunner:aRunner], it->second;
	}

	for(NSWindow* window in [NSApp orderedWindows])
	{
		HTMLOutputWindowController* delegate = [window delegate];
		if(![window isMiniaturized] && [delegate isKindOfClass:[HTMLOutputWindowController class]])
			return [delegate setCommandRunner:aRunner], delegate;
	}

	return [[[self alloc] initWithRunner:aRunner] autorelease];
}

- (void)dealloc
{
	D(DBF_HTMLOutputWindow, bug("\n"););
	foreach(it, Windows.lower_bound(runner->uuid()), Windows.upper_bound(runner->uuid()))
	{
		if(it->second == self)
		{
			Windows.erase(it);
			break;
		}
	}

	[htmlOutputView release];
	[super dealloc];
}

- (BOOL)setCommandRunner:(command::runner_ptr const&)aRunner
{
	if(runner)
		Windows.erase(runner->uuid());

	runner = aRunner;
	Windows.insert(std::make_pair(runner->uuid(), self));

	[htmlOutputView setEnvironment:runner->environment()];
	[htmlOutputView loadRequest:URLRequestForCommandRunner(runner) autoScrolls:runner->auto_scroll_output()];

	[window makeKeyAndOrderFront:nil];

	return YES;
}

- (BOOL)running
{
	return runner->running();
}

- (void)tearDown
{
	[window setDelegate:nil];
	window = nil; // window will ‘releaseOnClose’
	[self release];
}

- (void)closeWarningDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(void*)stack
{
	D(DBF_HTMLOutputWindow, bug("close %s\n", BSTR(returnCode == NSAlertDefaultReturn)););
	if(returnCode == NSAlertDefaultReturn) /* "Stop" */
	{
		[window close];
		[self tearDown];
	}
	[alert release];
}

- (BOOL)windowShouldClose:(id)aWindow
{
	D(DBF_HTMLOutputWindow, bug("\n"););

	if(!runner->running())
		return [self tearDown], YES;

	NSAlert* alert = [[NSAlert alertWithMessageText:@"Stop task before closing?" defaultButton:@"Stop Task" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"The job that the task is performing will not be completed."] retain];
	[alert beginSheetModalForWindow:window modalDelegate:self didEndSelector:@selector(closeWarningDidEnd:returnCode:contextInfo:) contextInfo:NULL];
	return NO;
}
@end
