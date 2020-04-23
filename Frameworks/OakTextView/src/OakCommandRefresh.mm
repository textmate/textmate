#import "OakCommandRefresh.h"
#import <OakCommand/OakCommand.h>
#import <document/OakDocument.h>
#import <HTMLOutput/HTMLOutput.h>
#import <settings/settings.h>
#import <ns/ns.h>

@interface OakCommandRefresher ()
{
	OakCommandRefresherOptions _options;
	std::map<std::string, std::string> _variables;
	NSTimer* _idleTimer;
}
@property (nonatomic, readwrite) OakCommand* command;
@property (nonatomic) OakDocument* document;
@property (nonatomic, weak) NSWindow* window;
@property (nonatomic) BOOL running;
@property (nonatomic) BOOL shouldRun;
@end

static NSTimeInterval kDocumentIdleDelay = 0.6;
static NSMutableSet<OakCommandRefresher*>* CommandRefreshers = [NSMutableSet set];

@implementation OakCommandRefresher
+ (OakCommandRefresher*)scheduleRefreshForCommand:(OakCommand*)aCommand document:(OakDocument*)document window:(NSWindow*)window options:(OakCommandRefresherOptions)options variables:(std::map<std::string, std::string> const&)variables
{
	OakCommandRefresher* refresher = [[OakCommandRefresher alloc] initWithCommand:aCommand document:document window:window options:options variables:variables];
	[CommandRefreshers addObject:refresher];
	return refresher;
}

+ (OakCommandRefresher*)findRefresherForCommandUUID:(NSUUID*)anIdentifier document:(OakDocument*)document window:(NSWindow*)window
{
	for(OakCommandRefresher* refresher in CommandRefreshers)
	{
		if([refresher.identifier isEqual:anIdentifier] && (!document || [document isEqual:refresher.document]) && (!window || [window isEqual:refresher.window]))
			return refresher;
	}
	return nil;
}

- (id)initWithCommand:(OakCommand*)aCommand document:(OakDocument*)document window:(NSWindow*)window options:(OakCommandRefresherOptions)options variables:(std::map<std::string, std::string> const&)variables
{
	if((self = [super init]))
	{
		_command   = aCommand;
		_options   = options;
		_document  = document;
		_window    = window;
		_variables = variables;

		_command.updateHTMLViewAtomically = YES;
		_command.htmlOutputView.reusable  = NO;

		__weak OakCommandRefresher* weakSelf = self;
		_command.terminationHandler = ^(OakCommand* command, BOOL normalExit){
			if(OakCommandRefresher* refresher = weakSelf)
			{
				refresher.running = NO;
				if(refresher.shouldRun)
				{
					refresher.shouldRun = NO;
					if(normalExit)
						[refresher execute];
				}
			}
		};

		[_command.htmlOutputView addObserver:self forKeyPath:@"visible" options:0 context:nullptr];
		if(_options & OakCommandRefresherDocumentDidChange)
			[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(contentDidChange:) name:OakDocumentContentDidChangeNotification object:_document];

		if(_options & (OakCommandRefresherDocumentDidChange|OakCommandRefresherDocumentDidClose))
			[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentWillClose:) name:OakDocumentWillCloseNotification object:_document];

		if(_options & OakCommandRefresherDocumentDidSave)
		{
			[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentDidSave:) name:OakDocumentDidSaveNotification object:nil];
			[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(windowWillClose:) name:NSWindowWillCloseNotification object:window];
		}
	}
	return self;
}

- (void)dealloc
{
	_command.htmlOutputView.reusable = YES;
	[NSNotificationCenter.defaultCenter removeObserver:self];
	[_command.htmlOutputView removeObserver:self forKeyPath:@"visible"];
}

- (NSUUID*)identifier
{
	return _command.identifier;
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)anObject change:(NSDictionary*)someChanges context:(void*)context
{
	if([keyPath isEqualToString:@"visible"])
		return [self teardown];
	[super observeValueForKeyPath:keyPath ofObject:anObject change:someChanges context:context];
}

- (void)executeForAction:(NSString*)action afterDelay:(BOOL)flag
{
	_variables["TM_REFRESH"] = to_s(action);

	[_idleTimer invalidate];
	if(flag)
			_idleTimer = [NSTimer scheduledTimerWithTimeInterval:kDocumentIdleDelay target:self selector:@selector(idleTimerDidFire:) userInfo:nil repeats:NO];
	else	[self idleTimerDidFire:nil];
}

- (void)idleTimerDidFire:(NSTimer*)aTimer
{
	_idleTimer = nil;
	[self execute];
}

- (void)contentDidChange:(NSNotification*)aNotification
{
	[self executeForAction:@"DocumentChanged" afterDelay:YES];
}

- (void)documentDidSave:(NSNotification*)aNotification
{
	[self executeForAction:@"DocumentSaved" afterDelay:YES];
}

- (void)updateEnvironment:(std::map<std::string, std::string>&)res forCommand:(OakCommand*)aCommand
{
	res << _document.variables;
	res = bundles::scope_variables(res); // Bundle items with a shellVariables setting
	res = variables_for_path(res, to_s(_document.path)); // .tm_properties
}

- (void)documentWillClose:(NSNotification*)aNotification
{
	if(_options & OakCommandRefresherDocumentDidClose)
	{
		_options &= ~OakCommandRefresherDocumentAsInput;
		_command.firstResponder = self;
		[self executeForAction:@"DocumentClosed" afterDelay:NO];
	}

	[_command closeHTMLOutputView];
	[self teardown];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self teardown];
}

- (void)bringHTMLOutputToFront:(id)sender
{
	[[_command.htmlOutputView window] makeKeyAndOrderFront:sender];
}

- (void)teardown
{
	[_idleTimer invalidate];
	_idleTimer = nil;
	[CommandRefreshers removeObject:self];
}

- (void)execute
{
	if(_shouldRun = (_running == YES))
		return;

	NSFileHandle* stdinFH;
	if(_options & OakCommandRefresherDocumentAsInput)
	{
		NSMutableData* data = [NSMutableData data];
		[_document enumerateByteRangesUsingBlock:^(char const* bytes, NSRange byteRange, BOOL* stop){
			[data appendBytes:bytes length:byteRange.length];
		}];

		int stdinRead, stdinWrite;
		std::tie(stdinRead, stdinWrite) = io::create_pipe();

		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			if(write(stdinWrite, data.bytes, data.length) == -1)
				perror("command_info_t: write");
			close(stdinWrite);
		});

		stdinFH = [[NSFileHandle alloc] initWithFileDescriptor:stdinRead closeOnDealloc:YES];
	}
	[_command executeWithInput:stdinFH variables:_variables outputHandler:nil];
}
@end
