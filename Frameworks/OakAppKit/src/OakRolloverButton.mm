#import "OakRolloverButton.h"

NSNotificationName const OakRolloverButtonMouseDidEnterNotification = @"OakRolloverButtonMouseDidEnterNotification";
NSNotificationName const OakRolloverButtonMouseDidLeaveNotification = @"OakRolloverButtonMouseDidLeaveNotification";

typedef NS_ENUM(NSUInteger, OakImageState) {
	OakImageStateRegular = 0,
	OakImageStatePressed,
	OakImageStateRollover,
	OakImageStateInactiveRegular,
	OakImageStateInactivePressed,
	OakImageStateInactiveRollover,
	OakImageStateCount
};

@interface OakRolloverButton ()
{
	NSImage* _images[OakImageStateCount];
}
@property (nonatomic) BOOL active;
@property (nonatomic) BOOL mouseInside;
@property (nonatomic) NSTrackingArea* trackingArea;
@end

@implementation OakRolloverButton
- (id)initWithFrame:(NSRect)aFrame
{
	if((self = [super initWithFrame:aFrame]))
	{
		self.buttonType = NSButtonTypeMomentaryChange;
		self.bordered   = NO;

		[self setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationVertical];
		[self setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationVertical];
	}
	return self;
}

- (BOOL)shouldDelayWindowOrderingForEvent:(NSEvent*)anEvent
{
	return _disableWindowOrderingForFirstMouse;
}

- (NSMenu*)menuForEvent:(NSEvent*)anEvent
{
	// Control-clicks are not sent to superview <rdar://20200363>
	return [[self superview] menuForEvent:anEvent];
}

- (void)mouseDown:(NSEvent*)anEvent
{
	if(_disableWindowOrderingForFirstMouse)
		[NSApp preventWindowOrdering];
	[super mouseDown:anEvent];
}

- (void)setHidden:(BOOL)flag
{
	[super setHidden:flag];
	self.mouseInside = !flag && NSMouseInRect([self convertPoint:[self.window mouseLocationOutsideOfEventStream] fromView:nil], [self visibleRect], [self isFlipped]);
}

- (void)setImage:(NSImage*)anImage forState:(OakImageState)imageState
{
	if(_images[imageState] == anImage)
		return;
	_images[imageState] = anImage;
	[self updateImage];
}

- (NSImage*)regularImage          { return _images[OakImageStateRegular];          }
- (NSImage*)pressedImage          { return _images[OakImageStatePressed];          }
- (NSImage*)rolloverImage         { return _images[OakImageStateRollover];         }
- (NSImage*)inactiveRegularImage  { return _images[OakImageStateInactiveRegular];  }
- (NSImage*)inactivePressedImage  { return _images[OakImageStateInactivePressed];  }
- (NSImage*)inactiveRolloverImage { return _images[OakImageStateInactiveRollover]; }

- (void)setRegularImage:(NSImage*)anImage          { [self setImage:anImage forState:OakImageStateRegular];          }
- (void)setPressedImage:(NSImage*)anImage          { [self setImage:anImage forState:OakImageStatePressed];          }
- (void)setRolloverImage:(NSImage*)anImage         { [self setImage:anImage forState:OakImageStateRollover];         }
- (void)setInactiveRegularImage:(NSImage*)anImage  { [self setImage:anImage forState:OakImageStateInactiveRegular];  }
- (void)setInactivePressedImage:(NSImage*)anImage  { [self setImage:anImage forState:OakImageStateInactivePressed];  }
- (void)setInactiveRolloverImage:(NSImage*)anImage { [self setImage:anImage forState:OakImageStateInactiveRollover]; }

- (void)updateImage
{
	NSImage* image    = _images[OakImageStateRegular];
	NSImage* altImage = _images[OakImageStatePressed];

	if(_mouseInside)
	{
		image = _images[OakImageStateRollover] ?: image;
		if(!_active)
			image = _images[OakImageStateInactiveRollover] ?: image;
	}
	else if(!_active)
	{
		image    = _images[OakImageStateInactiveRegular] ?: image;
		altImage = _images[OakImageStateInactivePressed] ?: image;
	}

	self.image          = image;
	self.alternateImage = altImage;
}

- (void)viewWillMoveToWindow:(NSWindow*)newWindow
{
	if(self.window)
	{
		[NSNotificationCenter.defaultCenter removeObserver:self name:NSWindowDidBecomeMainNotification object:self.window];
		[NSNotificationCenter.defaultCenter removeObserver:self name:NSWindowDidResignMainNotification object:self.window];
		[NSNotificationCenter.defaultCenter removeObserver:self name:NSWindowDidBecomeKeyNotification object:self.window];
		[NSNotificationCenter.defaultCenter removeObserver:self name:NSWindowDidResignKeyNotification object:self.window];
	}

	if(newWindow)
	{
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidBecomeMainNotification object:newWindow];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidResignMainNotification object:newWindow];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidBecomeKeyNotification object:newWindow];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidResignKeyNotification object:newWindow];
	}

	self.active = ([newWindow styleMask] & NSWindowStyleMaskFullScreen) || [newWindow isMainWindow] || [newWindow isKeyWindow];
}

- (void)windowDidChangeMainOrKey:(NSNotification*)aNotification
{
	self.active = ([self.window styleMask] & NSWindowStyleMaskFullScreen) || [self.window isMainWindow] || [self.window isKeyWindow];
}

- (void)updateTrackingAreas
{
	[super updateTrackingAreas];

	self.mouseInside = NSMouseInRect([self convertPoint:[self.window mouseLocationOutsideOfEventStream] fromView:nil], [self visibleRect], [self isFlipped]);

	if(_trackingArea)
		[self removeTrackingArea:_trackingArea];
	NSTrackingAreaOptions options = NSTrackingMouseEnteredAndExited|NSTrackingActiveAlways|(_mouseInside ? NSTrackingAssumeInside : 0);
	_trackingArea = [[NSTrackingArea alloc] initWithRect:[self bounds] options:options owner:self userInfo:nil];
	[self addTrackingArea:_trackingArea];
}

- (void)mouseEntered:(NSEvent*)anEvent
{
	self.mouseInside = YES;
}

- (void)mouseExited:(NSEvent*)anEvent
{
	self.mouseInside = NO;
}

- (void)setActive:(BOOL)flag
{
	if(_active == flag)
		return;
	_active = flag;
	[self updateImage];
}

- (void)setMouseInside:(BOOL)flag
{
	if(_mouseInside == flag)
		return;

	_mouseInside = flag;
	[self updateImage];

	NSNotificationName notification = _mouseInside ? OakRolloverButtonMouseDidEnterNotification : OakRolloverButtonMouseDidLeaveNotification;
	[NSNotificationCenter.defaultCenter postNotificationName:notification object:self];
}
@end
