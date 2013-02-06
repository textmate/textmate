#import "OakGradientView.h"

@interface OakGradientView ()
@property (nonatomic, retain) NSColor* activeTopColor;
@property (nonatomic, retain) NSColor* activeBottomColor;
@property (nonatomic, retain) NSColor* inactiveTopColor;
@property (nonatomic, retain) NSColor* inactiveBottomColor;
@property (nonatomic) BOOL renderInactive;
@end

@implementation OakGradientView
- (id)initWithTopColor:(NSColor*)topColor bottomColor:(NSColor*)bottomColor inactiveTopColor:(NSColor*)inactiveTopColor inactiveBottomColor:(NSColor*)inactiveBottomColor
{
	if(self = [self initWithFrame:NSZeroRect])
	{
		self.activeTopColor      = topColor;
		self.activeBottomColor   = bottomColor;
		self.inactiveTopColor    = inactiveTopColor;
		self.inactiveBottomColor = inactiveBottomColor;
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[super dealloc];
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
	NSColor* topColor    = self.renderInactive ? self.inactiveTopColor    : self.activeTopColor;
	NSColor* bottomColor = self.renderInactive ? self.inactiveBottomColor : self.activeBottomColor;
	[[[NSGradient alloc] initWithStartingColor:bottomColor endingColor:topColor] drawInRect:self.bounds angle:90];
}
@end
