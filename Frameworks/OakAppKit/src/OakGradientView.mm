#import "OakGradientView.h"

@interface OakGradientView ()
@property (nonatomic) NSGradient* activeGradient;
@property (nonatomic) NSGradient* inactiveGradient;
@property (nonatomic) BOOL renderInactive;
@end

@implementation OakGradientView
- (id)initWithGradient:(NSGradient*)activeGradient inactiveGradient:(NSGradient*)inactiveGradient
{
	if(self = [super initWithFrame:NSZeroRect])
	{
		self.activeGradient   = activeGradient;
		self.inactiveGradient = inactiveGradient;
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)viewWillMoveToWindow:(NSWindow*)newWindow
{
	if(self.window)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidBecomeMainNotification object:self.window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidResignMainNotification object:self.window];
	}

	if(newWindow)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(tmWindowDidChangeStatus:) name:NSWindowDidBecomeMainNotification object:newWindow];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(tmWindowDidChangeStatus:) name:NSWindowDidResignMainNotification object:newWindow];
	}

	[super viewWillMoveToWindow:newWindow];
}

- (void)viewDidMoveToWindow
{
	[self tmWindowDidChangeStatus:nil];
	[super viewDidMoveToWindow];
}

- (void)tmWindowDidChangeStatus:(NSNotification*)aNotification
{
	self.renderInactive = ![self.window isKeyWindow] && ![self.window isMainWindow];
}

- (void)setRenderInactive:(BOOL)flag
{
	if(_renderInactive != flag)
	{
		_renderInactive = flag;
		[self setNeedsDisplay:YES];
	}
}

- (void)drawRect:(NSRect)aRect
{
	// Although we draw the full background this view, nor any of its parents, are allowed to return YES from isOpaque <rdar://13161778>
	NSGradient* gradient = self.renderInactive ? self.inactiveGradient : self.activeGradient;
	[gradient drawInRect:self.bounds angle:270];
}
@end
