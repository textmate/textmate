#import <string>
#import <sys/stat.h>
#import "Dialog.h"
#import "TMDSemaphore.h"
#import "TMDChameleon.h"

@interface TMDWindowController : NSObject <NSWindowDelegate>
{
	NSWindow* window;
	BOOL isModal;
	BOOL center;
	BOOL async;
	BOOL didCleanup;
	NSInteger token;
}
@property (nonatomic) TMDWindowController* retainedSelf;

// Fetch an existing controller
+ (TMDWindowController*)windowControllerForToken:(NSInteger)token;
+ (NSArray*)nibDescriptions;
- (void)cleanupAndRelease:(id)sender;

// return unique ID for this TMDWindowController instance
- (NSInteger)token;
- (NSString*)windowTitle;
- (void)setWindow:(NSWindow*)aWindow;
- (BOOL)isAsync;
- (void)wakeClient;
- (NSMutableDictionary*)returnResult;
@end

@implementation TMDWindowController

static NSMutableArray* sWindowControllers    = nil;
static NSUInteger sNextWindowControllerToken = 1;

+ (NSArray*)nibDescriptions
{
	NSMutableArray* outNibArray = [NSMutableArray array];

	for(TMDWindowController* windowController in sWindowControllers)
	{
		NSMutableDictionary* nibDict = [NSMutableDictionary dictionary];
		NSString* nibTitle           = [windowController windowTitle];

		[nibDict setObject:@([windowController token]) forKey:@"token"];

		if(nibTitle != nil)
			[nibDict setObject:nibTitle forKey:@"windowTitle"];

		[outNibArray addObject:nibDict];
	}

	return outNibArray;
}

+ (TMDWindowController*)windowControllerForToken:(NSInteger)token
{
	TMDWindowController* outLoader = nil;

	for(TMDWindowController* loader in sWindowControllers)
	{
		if([loader token] == token)
		{
			outLoader = loader;
			break;
		}
	}

	return outLoader;
}

- (id)init
{
	if(self = [super init])
	{
		if(sWindowControllers == nil)
			sWindowControllers = [[NSMutableArray alloc] init];

		token = sNextWindowControllerToken;
		sNextWindowControllerToken += 1;

		[sWindowControllers addObject:self];
		self.retainedSelf = self;
	}
	return self;
}

// Return the result; if there is no result, return the parameters
- (NSMutableDictionary*)returnResult
{
	// override me
	return nil;
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self];
}

- (BOOL)isAsync
{
	return async;
}

- (NSInteger)token
{
	return token;
}

- (NSString*)windowTitle
{
	return [window title];
}

- (void)wakeClient
{
	if(isModal)
		[NSApp stopModal];

	// Post dummy event; the event system sometimes stalls unless we do this after stopModal. See also connectionDidDie: in this file.
	[NSApp postEvent:[NSEvent otherEventWithType:NSEventTypeApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:nil subtype:0 data1:0 data2:0] atStart:NO];

	TMDSemaphore* semaphore = [TMDSemaphore semaphoreForTokenInt:token];
	[semaphore stopWaiting];
}

- (void)setWindow:(NSWindow*)aWindow
{
	if(window != aWindow)
	{
		[window setDelegate:nil];
		window = aWindow;
		[window setDelegate:self];

		// We own the window, and we will release it. This prevents a potential crash later on.
		if([window isReleasedWhenClosed])
		{
			NSLog(@"warning: Window (%@) should not have released-when-closed bit set. I will clear it for you, but this it crash earlier versions of TextMate.", [window title]);
			[window setReleasedWhenClosed:NO];
		}
	}
}

- (void)cleanupAndRelease:(id)sender
{
	if(didCleanup)
		return;
	didCleanup = YES;

	[sWindowControllers removeObject:self];

	[NSNotificationCenter.defaultCenter removeObserver:self];
	[self setWindow:nil];

	[self wakeClient];
	[self performSelector:@selector(setRetainedSelf:) withObject:nil afterDelay:0];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self wakeClient];
}


- (void)connectionDidDie:(NSNotification*)aNotification
{
	[window orderOut:self];
	[self cleanupAndRelease:self];

	// post dummy event, since the system has a tendency to stall the next event, after replying to a DO message where the receiver has disappeared, posting this dummy event seems to solve it
	[NSApp postEvent:[NSEvent otherEventWithType:NSEventTypeApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:nil subtype:0 data1:0 data2:0] atStart:NO];
}
@end

@interface TMDNibWindowController : TMDWindowController
@property (nonatomic) NSMutableDictionary* parameters;
@property (nonatomic) NSMutableArray* topLevelObjects;

- (id)initWithParameters:(NSMutableDictionary*)someParameters modal:(BOOL)flag center:(BOOL)shouldCenter aysnc:(BOOL)inAsync;
- (NSDictionary*)instantiateNib:(NSNib*)aNib;
- (void)updateParameters:(NSMutableDictionary*)params;

- (void)wakeClient;
- (void)makeControllersCommitEditing;
@end

@implementation TMDNibWindowController
- (id)initWithParameters:(NSMutableDictionary*)someParameters modal:(BOOL)flag center:(BOOL)shouldCenter aysnc:(BOOL)inAsync
{
	if(self = [super init])
	{
		_parameters = someParameters;
		_parameters[@"controller"] = self;
		isModal = flag;
		center  = shouldCenter;
		async   = inAsync;
	}
	return self;
}

// Return the result; if there is no result, return the parameters
- (NSMutableDictionary*)returnResult
{
	id result = nil;

	if(async)
	{
		// Async dialogs return just the results
		result = self.parameters[@"result"];

		[self.parameters removeObjectForKey:@"result"];

		if(result == nil)
			result = [self.parameters mutableCopy];
	}
	else
	{
		// Other dialogs return everything
		result = [self.parameters mutableCopy];
	}

	[result removeObjectForKey:@"controller"];

	return result;
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

- (void)cleanupAndRelease:(id)sender
{
	if(didCleanup)
		return;

	[self.parameters removeObjectForKey:@"controller"];
	[self makeControllersCommitEditing];

	// if we do not manually unbind, the object in the nib will keep us retained, and thus we will never reach dealloc
	for(id object in self.topLevelObjects)
	{
		if([object isKindOfClass:[NSObjectController class]])
			[object unbind:@"contentObject"];
	}

	[super cleanupAndRelease:sender];
}

- (void)performButtonClick:(id)sender
{
	if([sender respondsToSelector:@selector(title)])
		self.parameters[@"returnButton"] = [sender title];
	if([sender respondsToSelector:@selector(tag)])
		self.parameters[@"returnCode"] = @([sender tag]);

	[self wakeClient];
}

// returnArgument: implementation. See <http://lists.macromates.com/pipermail/textmate/2006-November/015321.html>
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

// returnArgument: implementation. See <http://lists.macromates.com/pipermail/textmate/2006-November/015321.html>
- (void)forwardInvocation:(NSInvocation*)invocation
{
	NSString* str = NSStringFromSelector([invocation selector]);
	if([str hasPrefix:@"returnArgument:"])
	{
		NSArray* argNames = [str componentsSeparatedByString:@":"];

		NSMutableDictionary* dict = [NSMutableDictionary dictionary];
		for(NSUInteger i = 2; i < [[invocation methodSignature] numberOfArguments]; ++i)
		{
			__unsafe_unretained id arg = nil;
			[invocation getArgument:&arg atIndex:i];
			dict[[argNames objectAtIndex:i - 2]] = arg ?: @"";
		}
		self.parameters[@"result"] = dict;

		// unblock the connection thread
		[self wakeClient];
	}
	else
	{
		[super forwardInvocation:invocation];
	}
}

- (NSDictionary*)instantiateNib:(NSNib*)aNib
{
	if(!async)
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(connectionDidDie:) name:NSPortDidBecomeInvalidNotification object:nil];

	BOOL didInstantiate = NO;
	NSMutableArray* objects;

	didInstantiate = [aNib instantiateWithOwner:self topLevelObjects:&objects];
	if(!didInstantiate)
	{
		NSLog(@"%s failed to instantiate nib.", sel_getName(_cmd));
		[self cleanupAndRelease:self];
		return self.parameters;
	}


	self.topLevelObjects = objects;
	for(id object in self.topLevelObjects)
	{
		if([object isKindOfClass:[NSWindow class]])
			[self setWindow:object];
	}

	if(window)
	{
		if(center)
		{
			if(NSWindow* keyWindow = [NSApp keyWindow])
			{
				NSRect frame = [window frame], parentFrame = [keyWindow frame];
				[window setFrame:NSMakeRect(NSMidX(parentFrame) - 0.5 * NSWidth(frame), NSMidY(parentFrame) - 0.5 * NSHeight(frame), NSWidth(frame), NSHeight(frame)) display:NO];
			}
			else
			{
				[window center];
			}
		}

		// Show the window
		[window makeKeyAndOrderFront:self];

		// TODO: When TextMate is capable of running script I/O in it's own thread(s), modal blocking
		// can go away altogether.
		if(isModal)
			[NSApp runModalForWindow:window];
	}
	else
	{
		NSLog(@"%s didn't find a window in nib", sel_getName(_cmd));
		[self cleanupAndRelease:self];
	}

	return self.parameters;
}

// Async param updates
- (void)updateParameters:(NSMutableDictionary*)updatedParams
{
	NSArray* keys = [updatedParams allKeys];

	for(id key in keys)
		[self.parameters setValue:[updatedParams valueForKey:key] forKey:key];
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self];
}

- (void)wakeClient
{
	// makeControllersCommitEditing can only be in this (sub) class, since it needs access to topLevelObjects, but wakeClient is the logical place for committing the UI values, yet that is defined in our super class
	[self makeControllersCommitEditing];
	[super wakeClient];
}
@end

@interface NSObject (OakTextView)
- (NSPoint)positionForWindowUnderCaret;
@end

@interface LegacyDialogPopupMenuTarget : NSObject
{
	NSInteger selectedIndex;
}
@property NSInteger selectedIndex;
@end

@implementation LegacyDialogPopupMenuTarget
@synthesize selectedIndex;
- (id)init
{
	if(self = [super init])
		self.selectedIndex = NSNotFound;
	return self;
}

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	return [menuItem action] == @selector(takeSelectedItemIndexFrom:);
}

- (void)takeSelectedItemIndexFrom:(id)sender
{
	NSAssert([sender isKindOfClass:[NSMenuItem class]], @"Unexpected sender for menu target");
	self.selectedIndex = [(NSMenuItem*)sender tag];
}
@end

@interface Dialog : NSObject <TextMateDialogServerProtocol>
@property (nonatomic) NSConnection* connection;
- (id)initWithPlugInController:(id <TMPlugInController>)aController;
@end

@implementation Dialog
- (id)initWithPlugInController:(id <TMPlugInController>)aController
{
	NSApp = NSApplication.sharedApplication;
	if(self = [super init])
	{
		_connection = [NSConnection new];
		[_connection setRootObject:self];

		NSString* portName = [NSString stringWithFormat:@"%@.%d", @"com.macromates.dialog_1", getpid()];
		if([_connection registerName:portName] == NO)
			NSLog(@"couldn't setup port: %@", portName), NSBeep();
		setenv("DIALOG_1_PORT_NAME", [portName UTF8String], 1);

		if(NSString* path = [[NSBundle bundleForClass:[self class]] pathForResource:@"tm_dialog" ofType:nil])
			setenv(getenv("DIALOG") ? "DIALOG_1" : "DIALOG", [path UTF8String], 1);
	}
	return self;
}

- (NSInteger)textMateDialogServerProtocolVersion
{
	return TextMateDialogServerProtocolVersion;
}

// filePath: find the window with this path, and create a sheet on it. If we can't find one, may go app-modal.
- (id)showAlertForPath:(NSString*)filePath withParameters:(NSDictionary*)parameters modal:(BOOL)modal
{
	NSAlertStyle   alertStyle = NSAlertStyleInformational;
	NSAlert*       alert;
	NSDictionary*  resultDict = nil;
	NSArray*       buttonTitles = [parameters objectForKey:@"buttonTitles"];
	NSString*      alertStyleString = [parameters objectForKey:@"alertStyle"];

	alert = [NSAlert new];

	if([alertStyleString isEqualToString:@"warning"])
		alertStyle = NSAlertStyleWarning;
	else if([alertStyleString isEqualToString:@"critical"])
		alertStyle = NSAlertStyleCritical;
	else if([alertStyleString isEqualToString:@"informational"])
		alertStyle = NSAlertStyleInformational;

	[alert setAlertStyle:alertStyle];
	[alert setMessageText:[parameters objectForKey:@"messageTitle"]];
	[alert setInformativeText:[parameters objectForKey:@"informativeText"]];

	// Setup buttons
	if(buttonTitles != nil && [buttonTitles count] > 0)
	{
		NSUInteger buttonCount = [buttonTitles count];

		// NSAlert always preallocates the OK button.
		// No -- docs are not entirely correct.
//		[[[alert buttons] objectAtIndex:0] setTitle:[buttonTitles objectAtIndex:0]];

		for(NSUInteger index = 0; index < buttonCount; index += 1)
		{
			NSString* buttonTitle = [buttonTitles objectAtIndex:index];
			[alert addButtonWithTitle:buttonTitle];
		}
	}

	// Show alert
	NSInteger alertResult = ([alert runModal] - NSAlertFirstButtonReturn);
	resultDict = [NSDictionary dictionaryWithObject:[NSNumber numberWithInteger:alertResult] forKey:@"buttonClicked"];

	return resultDict;
}

- (id)showNib:(NSString*)aNibPath withParameters:(id)someParameters andInitialValues:(NSDictionary*)initialValues dynamicClasses:(NSDictionary*)dynamicClasses modal:(BOOL)modal center:(BOOL)shouldCenter async:(BOOL)async
{
	for(id key in [dynamicClasses allKeys])
		[TMDChameleon createSubclassNamed:key withValues:[dynamicClasses objectForKey:key]];

	if(![NSFileManager.defaultManager fileExistsAtPath:aNibPath])
	{
		NSLog(@"%s nib file not found: %@", sel_getName(_cmd), aNibPath);
		return nil;
	}

	if(initialValues && [initialValues count])
		[NSUserDefaults.standardUserDefaults registerDefaults:initialValues];

	NSData* nibData;
	NSString* keyedObjectsNibPath = [aNibPath stringByAppendingPathComponent:@"keyedobjects.nib"];
	if([NSFileManager.defaultManager fileExistsAtPath:keyedObjectsNibPath])
		nibData = [NSData dataWithContentsOfFile:keyedObjectsNibPath];
	else	nibData = [NSData dataWithContentsOfFile:aNibPath];

	NSNib* nib = [[NSNib alloc] initWithNibData:nibData bundle:nil];

	if(!nib)
	{
		NSLog(@"%s failed loading nib: %@", sel_getName(_cmd), aNibPath);
		return nil;
	}

	TMDNibWindowController* nibOwner = [[TMDNibWindowController alloc] initWithParameters:someParameters modal:modal center:shouldCenter aysnc:async];
	if(!nibOwner)
		NSLog(@"%s couldn't create nib loader", sel_getName(_cmd));
	[nibOwner instantiateNib:nib];

	return @{ @"token": @([nibOwner token]), @"returnCode": @0 };
}

// Async updates of parameters
- (id)updateNib:(id)token withParameters:(id)someParameters
{
	TMDWindowController* windowController = [TMDWindowController windowControllerForToken:[token intValue]];
	NSInteger resultCode = -43;

	if((windowController != nil) && [windowController isAsync] && [windowController isKindOfClass:[TMDNibWindowController class]])
	{
		[((TMDNibWindowController*)windowController) updateParameters:someParameters];
		resultCode = 0;
	}

	return @{ @"returnCode": @(resultCode) };
}

// Async close
- (id)closeNib:(id)token
{
	TMDWindowController* windowController = [TMDWindowController windowControllerForToken:[token intValue]];
	NSInteger resultCode = -43;

	if(windowController != nil)
	{
		[windowController connectionDidDie:nil];
		resultCode = 0;
	}

	return @{ @"returnCode": @(resultCode) };
}

// Async get results
- (id)retrieveNibResults:(id)token
{
	TMDWindowController* windowController = [TMDWindowController windowControllerForToken:[token intValue]];
	NSInteger resultCode = -43;
	id results;

	if(windowController != nil)
		results = [windowController returnResult];
	else	results = @{ @"returnCode": @(resultCode) };

	return results;
}

// Async list
- (id)listNibTokens
{
	NSArray* outNibArray = [TMDWindowController nibDescriptions];
	return @{ @"nibs": outNibArray, @"returnCode": @0 };
}


- (id)showMenuWithOptions:(NSDictionary*)someOptions
{
	NSMenu* menu = [NSMenu new];
	[menu setFont:[NSFont menuFontOfSize:([NSUserDefaults.standardUserDefaults integerForKey:@"OakBundleManagerDisambiguateMenuFontSize"] ?: 11)]];
	LegacyDialogPopupMenuTarget* menuTarget = [[LegacyDialogPopupMenuTarget alloc] init];

	NSInteger itemId = 0;
	char key = '0';
	NSArray* menuItems = [someOptions objectForKey:@"menuItems"];
	for(NSDictionary* menuItem in menuItems)
	{
		if([[menuItem objectForKey:@"separator"] intValue])
		{
			[menu addItem:[NSMenuItem separatorItem]];
		}
		else
		{
			NSMenuItem* theItem = [menu addItemWithTitle:[menuItem objectForKey:@"title"] action:@selector(takeSelectedItemIndexFrom:) keyEquivalent:key++ < '9' ? [NSString stringWithFormat:@"%c", key] : @""];
			[theItem setKeyEquivalentModifierMask:0];
			[theItem setTarget:menuTarget];
			[theItem setTag:itemId];
		}
		++itemId;
	}

	NSPoint pos = [NSEvent mouseLocation];
	if(id textView = [NSApp targetForAction:@selector(positionForWindowUnderCaret)])
		pos = [textView positionForWindowUnderCaret];

	NSMutableDictionary* selectedItem = [NSMutableDictionary dictionary];

	if([menu popUpMenuPositioningItem:nil atLocation:pos inView:nil] && menuTarget.selectedIndex != NSNotFound)
	{
		selectedItem[@"selectedIndex"]    = @(menuTarget.selectedIndex);
		selectedItem[@"selectedMenuItem"] = [menuItems objectAtIndex:menuTarget.selectedIndex];
	}

	return selectedItem;
}
@end
