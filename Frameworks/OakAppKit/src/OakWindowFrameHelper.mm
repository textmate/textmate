#import "OakWindowFrameHelper.h"
#import "NSScreen Additions.h"

OAK_DEBUG_VAR(WindowFrameHelper);

@interface OakWindowFrameHelper ()
{
	OBJC_WATCH_LEAKS(OakWindowFrameHelper);
	Class windowDelegateClass;
}
- (id)initWithWindow:(NSWindow*)aWindow;
- (void)placeAndSizeWindow:(NSWindow*)aWindow;
- (void)snapshotWindowFrame;
@property (nonatomic) NSWindow* window;
@property (nonatomic) NSString* autosaveName;
@property (nonatomic) OakWindowFrameHelper* retainedSelf;
@end

@implementation OakWindowFrameHelper
+ (OakWindowFrameHelper*)windowFrameHelperWithWindow:(NSWindow*)aWindow
{
	return [[self alloc] initWithWindow:aWindow];
}

- (id)initWithWindow:(NSWindow*)aWindow
{
	if((self = [super init]))
	{
		windowDelegateClass = [[aWindow delegate] class];
		self.autosaveName   = [NSString stringWithFormat:@"%@WindowFrame", NSStringFromClass(windowDelegateClass)];

		D(DBF_WindowFrameHelper, bug("Auto-save name: %s\n", [self.autosaveName UTF8String]););

		if([[aWindow delegate] isKindOfClass:[NSWindowController class]])
			[(NSWindowController*)[aWindow delegate] setShouldCascadeWindows:NO];
		[self placeAndSizeWindow:aWindow];

		self.window = aWindow;
	}
	return self;
}

- (void)setWindow:(NSWindow*)newWindow
{
	if(_window == newWindow)
		return;

	if(_window)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidMoveNotification object:_window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidResizeNotification object:_window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowWillCloseNotification object:_window];
	}

	if(_window = newWindow)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidMoveOrResize:) name:NSWindowDidMoveNotification object:_window];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidMoveOrResize:) name:NSWindowDidResizeNotification object:_window];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowWillClose:) name:NSWindowWillCloseNotification object:_window];
	}

	self.retainedSelf = _window ? self : nil;
}

- (void)windowDidMoveOrResize:(NSNotification*)aNotification
{
	D(DBF_WindowFrameHelper, bug("\n"););
	[self snapshotWindowFrame];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	D(DBF_WindowFrameHelper, bug("\n"););
	[self snapshotWindowFrame];
	self.window = nil;
}

// ====================
// = The Actual Logic =
// ====================

- (NSRect)windowFrame:(NSWindow*)aWindow
{
	if([[aWindow delegate] conformsToProtocol:@protocol(OakWindowFrameHelperDelegate)])
		return [(id <OakWindowFrameHelperDelegate>)[aWindow delegate] savableWindowFrame];
	return [aWindow frame];
}

- (void)snapshotWindowFrame
{
	if((([self.window styleMask] & NSFullScreenWindowMask) != NSFullScreenWindowMask))
	{
		NSString* zoomedKey = [self.autosaveName stringByAppendingString:@"Zoomed"];
		if(self.window.isZoomed)
		{
			[[NSUserDefaults standardUserDefaults] setBool:YES forKey:zoomedKey];
		}
		else
		{
			[[NSUserDefaults standardUserDefaults] removeObjectForKey:zoomedKey];
			[[NSUserDefaults standardUserDefaults] setObject:NSStringFromRect([self windowFrame:self.window]) forKey:self.autosaveName];
		}
	}
}

- (BOOL)ignoreWindow:(NSWindow*)aWindow
{
	if(![aWindow isVisible] || ![aWindow isOnActiveSpace])
		return YES;
	if(![[aWindow delegate] isKindOfClass:windowDelegateClass])
		return YES;
	if(([aWindow styleMask] & NSFullScreenWindowMask) == NSFullScreenWindowMask)
		return YES;

	return NO;
}

- (void)placeAndSizeWindow:(NSWindow*)aWindow
{
	NSString* rectStr = [[NSUserDefaults standardUserDefaults] stringForKey:self.autosaveName];
	NSRect r = rectStr ? NSRectFromString(rectStr) : NSZeroRect;
	BOOL zoom = NO;

	NSWindow* lowerWin = nil;
	CGFloat winYPos = CGFLOAT_MAX;
	for(NSWindow* win in [NSApp windows])
	{
		if([self ignoreWindow:win])
			continue;

		D(DBF_WindowFrameHelper, bug("window @ %s (%s)\n", [NSStringFromRect([self windowFrame:win]) UTF8String], [[[[win delegate] class] description] UTF8String]););
		if(NSMaxY([self windowFrame:win]) < winYPos)
		{
			winYPos = NSMaxY([self windowFrame:win]);
			lowerWin = win;
		}
	}

	if(lowerWin)
	{
		D(DBF_WindowFrameHelper, bug("cascade window\n"););
		r = [self windowFrame:lowerWin];
		[aWindow setFrame:r display:NO];
		r.origin = [aWindow cascadeTopLeftFromPoint:NSMakePoint(NSMinX(r), NSMaxY(r))];
		r.origin.y -= r.size.height;

		NSRect scrRect = [[NSScreen mainScreen] visibleFrame];
		if(!NSContainsRect(scrRect, r))
		{
			r.origin.x = 61;
			r.origin.y = NSMaxY(scrRect) - NSHeight(r);

			bool alreadyHasWrappedWindow = false;
			for(NSWindow* win in [NSApp windows])
			{
				if([self ignoreWindow:win])
					continue;

				if(NSEqualPoints([self windowFrame:win].origin, r.origin))
					alreadyHasWrappedWindow = true;
			}

			if(alreadyHasWrappedWindow)
			{
				NSWindow* win = [NSApp mainWindow];
				if([[win delegate] isKindOfClass:windowDelegateClass])
				{
					r = [self windowFrame:win];
					[aWindow setFrame:r display:NO];
					r.origin = [aWindow cascadeTopLeftFromPoint:NSMakePoint(NSMinX(r), NSMaxY(r))];
					r.origin.y -= r.size.height;
				}
			}
		}
	}
	else if(NSEqualRects(r, NSZeroRect))
	{
		D(DBF_WindowFrameHelper, bug("center window\n"););
		r = [[NSScreen mainScreen] visibleFrame];
		r = NSIntegralRect(NSInsetRect(r, NSWidth(r) / 3, NSHeight(r) / 5));
	}
	else if([[NSUserDefaults standardUserDefaults] boolForKey:[self.autosaveName stringByAppendingString:@"Zoomed"]])
	{
		zoom = YES;
	}

	[aWindow setFrame:[[NSScreen screenWithFrame:r] restrainFrameToVisibleScreen:r] display:NO];

	if(zoom)
		[aWindow zoom:self];
}
@end
