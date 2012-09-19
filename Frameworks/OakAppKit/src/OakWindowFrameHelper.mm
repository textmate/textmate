#import "OakWindowFrameHelper.h"
#import "NSScreen Additions.h"

OAK_DEBUG_VAR(WindowFrameHelper);

@interface OakWindowFrameHelper ()
- (id)initWithWindow:(NSWindow*)aWindow;
- (void)placeAndSizeWindow:(NSWindow*)aWindow;
- (void)snapshotWindowFrame;
@property (nonatomic, retain) NSWindow* window;
@property (nonatomic, retain) NSString* autosaveName;
@end

@implementation OakWindowFrameHelper
@synthesize window, autosaveName;

+ (OakWindowFrameHelper*)windowFrameHelperWithWindow:(NSWindow*)aWindow
{
	return [[[self alloc] initWithWindow:aWindow] autorelease];
}

- (id)initWithWindow:(NSWindow*)aWindow
{
	if((self = [super init]))
	{
		windowDelegateClass = [[aWindow delegate] class];
		self.autosaveName   = [NSString stringWithFormat:@"%@WindowFrame", NSStringFromClass(windowDelegateClass)];

		D(DBF_WindowFrameHelper, bug("Auto-save name: %s\n", [autosaveName UTF8String]););

		if([[aWindow delegate] isKindOfClass:[NSWindowController class]])
			[(NSWindowController*)[aWindow delegate] setShouldCascadeWindows:NO];
		[self placeAndSizeWindow:aWindow];

		self.window = aWindow;
	}
	return self;
}

- (void)dealloc
{
	self.window       = nil;
	self.autosaveName = nil;
	[super dealloc];
}

- (void)setWindow:(NSWindow*)newWindow
{
	NSWindow* oldWindow = window;
	if(oldWindow == newWindow)
		return;

	if(window = [newWindow retain])
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidMoveOrResize:) name:NSWindowDidMoveNotification object:window];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidMoveOrResize:) name:NSWindowDidResizeNotification object:window];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowWillClose:) name:NSWindowWillCloseNotification object:window];
		[self retain]; // as long as we are observing the window, we want to be retained
	}

	if(oldWindow)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidMoveNotification object:oldWindow];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidResizeNotification object:oldWindow];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowWillCloseNotification object:oldWindow];
		[oldWindow release];
		[self release];
	}
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

- (void)snapshotWindowFrame
{
	[[NSUserDefaults standardUserDefaults] setObject:NSStringFromRect([window frame]) forKey:self.autosaveName];
}

- (BOOL)ignoreWindow:(NSWindow*)aWindow
{
	return !([aWindow isVisible] && [[aWindow delegate] isKindOfClass:windowDelegateClass]);
}

- (void)placeAndSizeWindow:(NSWindow*)aWindow
{
	NSString* rectStr = [[NSUserDefaults standardUserDefaults] stringForKey:self.autosaveName];
	NSRect r = rectStr ? NSRectFromString(rectStr) : NSZeroRect;

	NSWindow* lowerWin = nil;
	CGFloat winYPos = CGFLOAT_MAX;
	for(NSWindow* win in [NSApp windows])
	{
		if([self ignoreWindow:win])
			continue;

		D(DBF_WindowFrameHelper, bug("window @ %s (%s)\n", [NSStringFromRect([win frame]) UTF8String], [[[[win delegate] class] description] UTF8String]););
		if(NSMaxY([win frame]) < winYPos)
		{
			winYPos = NSMaxY([win frame]);
			lowerWin = win;
		}
	}

	if(lowerWin)
	{
		D(DBF_WindowFrameHelper, bug("cascade window\n"););
		r = [lowerWin frame];
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

				if(NSEqualPoints([win frame].origin, r.origin))
					alreadyHasWrappedWindow = true;
			}

			if(alreadyHasWrappedWindow)
			{
				NSWindow* win = [NSApp mainWindow];
				if([[win delegate] isKindOfClass:windowDelegateClass])
				{
					r = [win frame];
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
	[aWindow setFrame:[[NSScreen screenWithFrame:r] restrainFrameToVisibleScreen:r] display:NO];
}
@end
