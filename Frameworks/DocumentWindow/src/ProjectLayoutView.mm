#import "ProjectLayoutView.h"
#import <Preferences/Keys.h>
#import <oak/misc.h>

NSString* const kUserDefaultsFileBrowserWidthKey = @"fileBrowserWidth";
NSString* const kUserDefaultsHTMLOutputHeightKey = @"htmlOutputHeight";

@interface ProjectLayoutView ()
@property (nonatomic, retain) NSLayoutConstraint* fileBrowserWidthConstraint;
@property (nonatomic, retain) NSLayoutConstraint* htmlOutputHeightConstraint;
@end

@implementation ProjectLayoutView
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsFileBrowserWidthKey : @250,
		kUserDefaultsHTMLOutputHeightKey : @200
	}];
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		fileBrowserWidth   = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsFileBrowserWidthKey];
		fileBrowserOnRight = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsFileBrowserPlacementKey] isEqualToString:@"right"];

		htmlOutputHeight   = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsHTMLOutputHeightKey];
		htmlOutputOnRight  = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"right"];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];

	self.tabBarView      = nil;
	self.documentView    = nil;
	self.fileBrowserView = nil;
	self.htmlOutputView  = nil;

	self.fileBrowserWidthConstraint = nil;
	self.htmlOutputHeightConstraint = nil;

	[super dealloc];
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.fileBrowserOnRight = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsFileBrowserPlacementKey] isEqualToString:@"right"];
	self.htmlOutputOnRight  = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"right"];
}

- (void)replaceView:(NSView**)oldView withView:(NSView*)newView
{
	if(newView == *oldView)
		return;

	if(NSView* view = *oldView)
	{
		[view removeFromSuperview];
		[view release];
		*oldView = nil;
	}

	if(newView)
	{
		[newView setTranslatesAutoresizingMaskIntoConstraints:NO];
		[self addSubview:newView];
		*oldView = [newView retain];
	}

	[self setNeedsUpdateConstraints:YES];
	[[self window] invalidateCursorRectsForView:self];
}

- (void)setTabBarView:(NSView*)aTabBarView           { [self replaceView:&tabBarView      withView:aTabBarView];      }
- (void)setDocumentView:(NSView*)aDocumentView       { [self replaceView:&documentView    withView:aDocumentView];    }
- (void)setHtmlOutputView:(NSView*)aHtmlOutputView   { [self replaceView:&htmlOutputView  withView:aHtmlOutputView];  }

- (void)setFileBrowserView:(NSView*)aFileBrowserView
{
	[self replaceView:&fileBrowserView withView:aFileBrowserView];
}

- (void)setFileBrowserOnRight:(BOOL)flag
{
	if(fileBrowserOnRight != flag)
	{
		fileBrowserOnRight = flag;
		if(fileBrowserView)
			[self setNeedsUpdateConstraints:YES];
	}
}

- (void)setHtmlOutputOnRight:(BOOL)flag
{
	if(htmlOutputOnRight != flag)
	{
		htmlOutputOnRight = flag;
		if(htmlOutputView)
			[self setNeedsUpdateConstraints:YES];
	}
}

- (void)updateConstraints
{
	[self removeConstraints:[self constraints]];
	[super updateConstraints];

	NSDictionary* views = @{
		@"tabBarView"       : tabBarView,
		@"documentView"     : documentView,
		@"fileBrowserView"  : fileBrowserView ?: [NSNull null],
		@"htmlOutputView"   : htmlOutputView  ?: [NSNull null]
	};

	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[tabBarView]|" options:0 metrics:nil views:views]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[tabBarView][documentView(>=40)]" options:0 metrics:nil views:views]];

	if(fileBrowserView)
	{
		NSString* fileBrowserLayout = fileBrowserOnRight ? @"H:|[documentView]-(1)-[fileBrowserView]|" : @"H:|[fileBrowserView]-(1)-[documentView]|";

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:fileBrowserLayout options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[tabBarView][fileBrowserView(==documentView)]" options:0 metrics:nil views:views]];

		self.fileBrowserWidthConstraint = [NSLayoutConstraint constraintWithItem:fileBrowserView attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:fileBrowserWidth];
		self.fileBrowserWidthConstraint.priority = NSLayoutPriorityRequired-1;
		[self addConstraint:self.fileBrowserWidthConstraint];
	}
	else
	{
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[documentView]|" options:0 metrics:nil views:views]];
	}

	if(htmlOutputView)
	{
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[htmlOutputView]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[documentView]-(1)-[htmlOutputView]|" options:0 metrics:nil views:views]];

		self.htmlOutputHeightConstraint = [NSLayoutConstraint constraintWithItem:htmlOutputView attribute:NSLayoutAttributeHeight relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:htmlOutputHeight];
		self.htmlOutputHeightConstraint.priority = NSLayoutPriorityRequired-1;
		[self addConstraint:self.htmlOutputHeightConstraint];
	}
	else
	{
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[documentView]|" options:0 metrics:nil views:views]];
	}
}

- (NSRect)fileBrowserResizeRect
{
	if(!fileBrowserView)
		return NSZeroRect;
	NSRect r = fileBrowserView.frame;
	return NSMakeRect(fileBrowserOnRight ? NSMinX(r)-3 : NSMaxX(r)-4, NSMinY(r), 10, NSHeight(r));
}

- (NSRect)htmlOutputResizeRect
{
	if(!htmlOutputView)
		return NSZeroRect;
	NSRect r = htmlOutputView.frame;
	return NSMakeRect(NSMinX(r), NSMaxY(r)-4, NSWidth(r), 10);
}

- (void)resetCursorRects
{
	[self addCursorRect:[self fileBrowserResizeRect] cursor:[NSCursor resizeLeftRightCursor]];
	[self addCursorRect:[self htmlOutputResizeRect]  cursor:[NSCursor resizeUpDownCursor]];
}

- (NSView*)hitTest:(NSPoint)aPoint
{
	if(NSMouseInRect([self convertPoint:aPoint fromView:[self superview]], [self fileBrowserResizeRect], [self isFlipped]))
		return self;
	if(NSMouseInRect([self convertPoint:aPoint fromView:[self superview]], [self htmlOutputResizeRect], [self isFlipped]))
		return self;
	return [super hitTest:aPoint];
}

- (void)mouseDown:(NSEvent*)anEvent
{
	NSView* view = nil;
	NSPoint mouseDownPos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	if(NSMouseInRect(mouseDownPos, [self fileBrowserResizeRect], [self isFlipped]))
		view = fileBrowserView;
	else if(NSMouseInRect(mouseDownPos, [self htmlOutputResizeRect], [self isFlipped]))
		view = htmlOutputView;

	if(!view || [anEvent type] != NSLeftMouseDown)
		return [super mouseDown:anEvent];

	NSEvent* mouseDownEvent = [anEvent retain];
	NSRect initialFrame = view.frame;

	BOOL didDrag = NO;
	while([anEvent type] != NSLeftMouseUp)
	{
		anEvent = [NSApp nextEventMatchingMask:(NSLeftMouseDraggedMask|NSLeftMouseUpMask) untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES];
		if([anEvent type] != NSLeftMouseDragged)
			break;

		NSPoint mouseCurrentPos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
		if(!didDrag && SQ(fabs(mouseDownPos.x - mouseCurrentPos.x)) + SQ(fabs(mouseDownPos.y - mouseCurrentPos.y)) < SQ(1))
			continue; // we didn't even drag a pixel

		if(view == htmlOutputView)
		{
			CGFloat height = NSHeight(initialFrame) + (mouseCurrentPos.y - mouseDownPos.y);
			htmlOutputHeight = std::max<CGFloat>(50, round(height));
			self.htmlOutputHeightConstraint.constant = htmlOutputHeight;

			[[NSUserDefaults standardUserDefaults] setInteger:htmlOutputHeight forKey:kUserDefaultsHTMLOutputHeightKey];
		}
		else if(view == fileBrowserView)
		{
			CGFloat width = NSWidth(initialFrame) + (mouseCurrentPos.x - mouseDownPos.x) * (fileBrowserOnRight ? -1 : +1);
			fileBrowserWidth = std::max<CGFloat>(50, round(width));
			self.fileBrowserWidthConstraint.constant = fileBrowserWidth;

			[[NSUserDefaults standardUserDefaults] setInteger:fileBrowserWidth forKey:kUserDefaultsFileBrowserWidthKey];
		}

		didDrag = YES;
	}

	if(!didDrag)
	{
		NSView* view = [super hitTest:[[self superview] convertPoint:[mouseDownEvent locationInWindow] fromView:nil]];
		if(view && view != self)
		{
			[NSApp postEvent:anEvent atStart:NO];
			[view mouseDown:mouseDownEvent];
		}
	}

	[mouseDownEvent release];
}

- (BOOL)isOpaque
{
	return YES;
}

- (void)drawRect:(NSRect)aRect
{
	[[NSColor lightGrayColor] set];
	for(NSRect dividerRect : { [self htmlOutputResizeRect], [self fileBrowserResizeRect] })
		NSRectFill(NSIntersectionRect(dividerRect, aRect));
}

- (void)performClose:(id)sender
{
	NSView* view = (NSView*)[[self window] firstResponder];
	if([view isKindOfClass:[NSView class]] && [view isDescendantOf:htmlOutputView])
		[NSApp sendAction:@selector(performCloseSplit:) to:nil from:htmlOutputView];
	else if(tabBarView)
		[tabBarView tryToPerform:_cmd with:sender];
}
@end
