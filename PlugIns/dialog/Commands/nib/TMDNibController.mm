//
//  TMDNibController.mm
//  Dialog2
//

#import <string>
#import <map>
#import "../../Dialog2.h"
#import "../../TMDCommand.h"
#import "TMDNibController.h"

static NSMutableDictionary* NibControllers = [NSMutableDictionary new];
static NSInteger NibTokenCount = 0;

@interface TMDNibController ()
{
	NSMutableArray* clientFileHandles;
	BOOL runningModal;
}
@property (nonatomic, readwrite) NSString* token;
@property (nonatomic) NSArray* topLevelObjects;
@property (nonatomic) NSMutableDictionary* parameters;
@end

@implementation TMDNibController
+ (TMDNibController*)controllerForToken:(NSString*)aToken
{
	return [NibControllers objectForKey:aToken];
}

+ (NSArray*)controllers
{
	return [NibControllers allValues];
}

- (id)init
{
	if(self = [super init])
	{
		_parameters = [NSMutableDictionary new];
		[_parameters setObject:self forKey:@"controller"];

		clientFileHandles = [NSMutableArray new];

		_token = [NSString stringWithFormat:@"%ld", ++NibTokenCount];
		[NibControllers setObject:self forKey:_token];
	}
	return self;
}

- (id)initWithNibPath:(NSString*)aPath
{
	if(self = [self init])
	{
		NSData* nibData;
		NSString* keyedObjectsNibPath = [aPath stringByAppendingPathComponent:@"keyedobjects.nib"];
		if([NSFileManager.defaultManager fileExistsAtPath:keyedObjectsNibPath])
			nibData = [NSData dataWithContentsOfFile:keyedObjectsNibPath];
		else	nibData = [NSData dataWithContentsOfFile:aPath];

		if(NSNib* nib = [[NSNib alloc] initWithNibData:nibData bundle:nil])
		{
			BOOL didInstantiate = NO;
			NSArray* objects;

			didInstantiate = [nib instantiateWithOwner:self topLevelObjects:&objects];

			if(didInstantiate)
			{
				_topLevelObjects = objects;
				for(id object in _topLevelObjects)
				{
					if([object isKindOfClass:[NSWindow class]])
						[self setWindow:object];
				}

				if(_window)
					return self;

				NSLog(@"%s failed to find window in nib: %@", sel_getName(_cmd), aPath);
			}
		}
		else
		{
			NSLog(@"%s failed loading nib: %@", sel_getName(_cmd), aPath);
		}
	}
	return nil;
}

- (void)dealloc
{
	[self setWindow:nil];
}

- (void)setWindow:(NSWindow*)aWindow
{
	if(_window != aWindow)
	{
		[_window setDelegate:nil];
		[_window orderOut:self];

		_window = aWindow;
		[_window setDelegate:self];
		[_window setReleasedWhenClosed:NO]; // incase this was set wrong in IB
	}
}

- (void)updateParametersWith:(id)plist
{
	for(id key in [plist allKeys])
		[self.parameters setValue:[plist valueForKey:key] forKey:key];
}

- (void)showWindowAndCenter:(BOOL)shouldCenter
{
	if(shouldCenter)
	{
		if(NSWindow* keyWindow = [NSApp keyWindow])
		{
			NSRect frame = [_window frame], parentFrame = [keyWindow frame];
			[_window setFrame:NSMakeRect(NSMidX(parentFrame) - 0.5 * NSWidth(frame), NSMidY(parentFrame) - 0.5 * NSHeight(frame), NSWidth(frame), NSHeight(frame)) display:NO];
		}
		else
		{
			[_window center];
		}
	}
	[_window makeKeyAndOrderFront:self];
}

- (void)makeControllersCommitEditing
{
	for(id object in self.topLevelObjects)
	{
		if([object respondsToSelector:@selector(commitEditing)])
			[object commitEditing];
	}

	[NSUserDefaults.standardUserDefaults synchronize];
}

- (void)tearDown
{
	[self.parameters removeObjectForKey:@"controller"];

	// if we do not manually unbind, the object in the nib will keep us retained, and thus we will never reach dealloc
	for(id object in self.topLevelObjects)
	{
		if([object isKindOfClass:[NSObjectController class]])
			[object unbind:@"contentObject"];
	}

	// It isn’t always safe to release our window in windowWillClose: (at least on 10.9) which
	// is why we schedule the (implicit) release to run after current event loop cycle.
	[NibControllers performSelector:@selector(removeObjectForKey:) withObject:_token afterDelay:0];
}

// ==================================
// = Getting stuff from this window =
// ==================================
- (void)addClientFileHandle:(NSFileHandle*)aFileHandle modal:(BOOL)flag
{
	[clientFileHandles addObject:aFileHandle];
	if(!runningModal && (runningModal = flag))
		[NSApp runModalForWindow:_window];
}

- (void)return:(NSDictionary*)eventInfo
{
	[self makeControllersCommitEditing];

	id model = [self.parameters mutableCopy];
	[model removeObjectForKey:@"controller"];

	NSDictionary* res = @{ @"model": model, @"eventInfo": eventInfo };

	for(NSFileHandle* fileHandle in clientFileHandles)
		[TMDCommand writePropertyList:res toFileHandle:fileHandle];

	[clientFileHandles removeAllObjects];

	if(runningModal)
	{
		[NSApp stopModal];
		runningModal = NO;
	}
}

// ================================================
// = Events which return data to clients waiting  =
// ================================================
- (void)windowWillClose:(NSNotification*)aNotification
{
	if([clientFileHandles count])
			[self return:@{ @"type": @"closeWindow" }];
	else	[self tearDown];
}

// ================================================
// = Faking a returnArgument:[…:]* implementation =
// ================================================
// returnArgument: implementation. See <http://lists.macromates.com/textmate/2006-November/015321.html>
- (NSMethodSignature*)methodSignatureForSelector:(SEL)aSelector
{
	NSString* str = NSStringFromSelector(aSelector);
	if([str hasPrefix:@"returnArgument:"])
	{
		std::string types;
		types += @encode(void);
		types += @encode(id);
		types += @encode(SEL);

		NSUInteger numberOfArgs = [[str componentsSeparatedByString:@":"] count];
		while(numberOfArgs-- > 1)
			types += @encode(id);

		return [NSMethodSignature signatureWithObjCTypes:types.c_str()];
	}
	return [super methodSignatureForSelector:aSelector];
}

- (void)forwardInvocation:(NSInvocation*)invocation
{
	NSString* str = NSStringFromSelector([invocation selector]);
	if([str hasPrefix:@"returnArgument:"])
	{
		NSArray* argNames = [str componentsSeparatedByString:@":"];

		NSMutableDictionary* res = [NSMutableDictionary dictionary];
		[res setObject:@"bindingAction" forKey:@"type"];

		for(NSUInteger i = 2; i < [[invocation methodSignature] numberOfArguments]; ++i)
		{
			__unsafe_unretained id arg = nil;
			[invocation getArgument:&arg atIndex:i];
			[res setObject:(arg ?: @"") forKey:[argNames objectAtIndex:i - 2]];
		}

		[self return:res];
	}
	else
	{
		[super forwardInvocation:invocation];
	}
}

// ===============================
// = The old performButtonClicl: =
// ===============================
- (IBAction)performButtonClick:(id)sender
{
	NSMutableDictionary* res = [NSMutableDictionary dictionary];
	res[@"type"] = @"buttonClick";

	if([sender respondsToSelector:@selector(title)])
		res[@"title"] = [sender title];
	if([sender respondsToSelector:@selector(tag)])
		res[@"tag"] = @([sender tag]);

	[self return:res];
}
@end
