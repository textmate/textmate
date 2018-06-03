#import <os/log.h>
#import "GenieTask.h"
#import "hash.h"

@interface GenieTask ()
{
	NSArray*  _command;
	NSString* _directory;
}
@property (nonatomic) NSTask* task;
@end

@implementation GenieTask
- (instancetype)initWithCommand:(NSArray*)command directory:(NSString*)directory
{
	if(self = [super init])
	{
		_command   = command;
		_directory = directory ?: NSTemporaryDirectory();

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self name:NSApplicationWillTerminateNotification object:NSApp];
	[self terminate];
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	if(_task.isRunning)
		[self terminate];
}

- (BOOL)isRunning
{
	return _task != nil;
}

- (void)launch:(void(^)(int, NSData*, NSData*))callback
{
	[self terminate];

	_task = [[NSTask alloc] init];
	_task.launchPath = _command.firstObject;
	_task.arguments  = [_command subarrayWithRange:NSMakeRange(1, _command.count-1)];

	NSPipe* stdinPipe  = [NSPipe pipe];
	NSPipe* stdoutPipe = [NSPipe pipe];
	NSPipe* stderrPipe = [NSPipe pipe];

	_task.standardInput        = stdinPipe;
	_task.standardOutput       = stdoutPipe;
	_task.standardError        = stderrPipe;
	_task.currentDirectoryPath = _directory;
	if(_environment)
		_task.environment = _environment;

   dispatch_group_t group = dispatch_group_create();

	dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		NSFileHandle* fh = [stdinPipe fileHandleForWriting];
		@try {
			if(NSString* input = self.standardInputString)
				[fh writeData:[input dataUsingEncoding:NSUTF8StringEncoding]];
			[fh closeFile];
		}
		@catch (NSException* e) {
			os_log_error(OS_LOG_DEFAULT, "exception writing to task’s stdin: %@", e);
		}
	});

	__block NSData* stdoutData;
	dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		stdoutData = [[stdoutPipe fileHandleForReading] readDataToEndOfFile];
	});

	__block NSData* stderrData;
	dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		stderrData = [[stderrPipe fileHandleForReading] readDataToEndOfFile];
	});

	NSTask* temp = _task;
	temp.terminationHandler = ^(NSTask* theTask){
	   dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
		dispatch_async(dispatch_get_main_queue(), ^{
			self.task = nil;
			callback(theTask.terminationStatus, stdoutData, stderrData);
		});
	};

	@try {
		[_task launch];
		if(_timeOut > 0)
		{
			if(dispatch_source_t timer = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0)))
			{
				__block int state = 0;
				dispatch_source_set_timer(timer, dispatch_time(DISPATCH_TIME_NOW, _timeOut * NSEC_PER_SEC), 1 * NSEC_PER_SEC, NSEC_PER_SEC / 5);
				dispatch_source_set_event_handler(timer, ^{
					switch(state = temp.isRunning ? state+1 : 4)
					{
						case 1: NSLog(@"*** timeout: send SIGINT to %@",  temp.launchPath); [temp interrupt]; break;
						case 2: NSLog(@"*** timeout: send SIGTERM to %@", temp.launchPath); [temp terminate]; break;
						case 3: NSLog(@"*** timeout: send SIGKILL to %@", temp.launchPath); kill(temp.processIdentifier, SIGKILL);
						case 4: dispatch_source_cancel(timer); break;
					}
				});
				dispatch_resume(timer);
			}
		}
	}
	@catch (NSException* e) {
		os_log_error(OS_LOG_DEFAULT, "exception launching task: %@", e);
		[[stdinPipe fileHandleForReading] closeFile];
		[[stdoutPipe fileHandleForWriting] closeFile];
		[[stderrPipe fileHandleForWriting] closeFile];
		self.task = nil;
	}
}

- (void)terminate
{
	for(int state = 0; true; usleep(100000))
	{
		switch(state = _task.isRunning ? state+1 : 4)
		{
			case 1: NSLog(@"*** timeout: send SIGINT to %@",  _task.launchPath); [_task interrupt]; break;
			case 2: NSLog(@"*** timeout: send SIGTERM to %@", _task.launchPath); [_task terminate]; break;
			case 3: NSLog(@"*** timeout: send SIGKILL to %@", _task.launchPath); kill(_task.processIdentifier, SIGKILL);
			case 4:
				self.task = nil;
				return;
			break;
		}
	}
}
@end
