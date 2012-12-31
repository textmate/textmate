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
		_fileBrowserWidth   = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsFileBrowserWidthKey];
		_fileBrowserOnRight = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsFileBrowserPlacementKey] isEqualToString:@"right"];

		_htmlOutputHeight   = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsHTMLOutputHeightKey];
		_htmlOutputOnRight  = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"right"];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.fileBrowserOnRight = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsFileBrowserPlacementKey] isEqualToString:@"right"];
	self.htmlOutputOnRight  = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"right"];
}

- (NSView*)replaceView:(NSView*)oldView withView:(NSView*)newView
{
	if(newView == oldView)
		return oldView;

	[oldView removeFromSuperview];

	if(newView)
	{
		[newView setTranslatesAutoresizingMaskIntoConstraints:NO];
		[self addSubview:newView];
	}

	[self setNeedsUpdateConstraints:YES];
	[[self window] invalidateCursorRectsForView:self];

	return newView;
}

- (void)setTabBarView:(NSView*)aTabBarView           { _tabBarView     = [self replaceView:_tabBarView      withView:aTabBarView];      }
- (void)setDocumentView:(NSView*)aDocumentView       { _documentView   = [self replaceView:_documentView    withView:aDocumentView];    }
- (void)setHtmlOutputView:(NSView*)aHtmlOutputView   { _htmlOutputView = [self replaceView:_htmlOutputView  withView:aHtmlOutputView];  }

- (void)setFileBrowserView:(NSView*)aFileBrowserView
{
	_fileBrowserView = [self replaceView:_fileBrowserView withView:aFileBrowserView];
}

- (void)setFileBrowserOnRight:(BOOL)flag
{
	if(_fileBrowserOnRight != flag)
	{
		_fileBrowserOnRight = flag;
		if(_fileBrowserView)
			[self setNeedsUpdateConstraints:YES];
	}
}

- (void)setHtmlOutputOnRight:(BOOL)flag
{
	if(_htmlOutputOnRight != flag)
	{
		_htmlOutputOnRight = flag;
		if(_htmlOutputView)
			[self setNeedsUpdateConstraints:YES];
	}
}

- (void)updateConstraints
{
	[self removeConstraints:[self constraints]];
	[super updateConstraints];

	NSDictionary* views = @{
		@"tabBarView"       : _tabBarView,
		@"documentView"     : _documentView,
		@"fileBrowserView"  : _fileBrowserView ?: [NSNull null],
		@"htmlOutputView"   : _htmlOutputView  ?: [NSNull null]
	};

	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[tabBarView]|" options:0 metrics:nil views:views]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[tabBarView][documentView(>=40)]" options:0 metrics:nil views:views]];

	if(_fileBrowserView)
	{
		NSString* fileBrowserLayout = _fileBrowserOnRight ? @"H:|[documentView]-(1)-[fileBrowserView]|" : @"H:|[fileBrowserView]-(1)-[documentView]|";

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:fileBrowserLayout options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[tabBarView][fileBrowserView(==documentView)]" options:0 metrics:nil views:views]];

		self.fileBrowserWidthConstraint = [NSLayoutConstraint constraintWithItem:_fileBrowserView attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:_fileBrowserWidth];
		self.fileBrowserWidthConstraint.priority = NSLayoutPriorityRequired-1;
		[self addConstraint:self.fileBrowserWidthConstraint];
	}
	else
	{
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[documentView]|" options:0 metrics:nil views:views]];
	}

	if(_htmlOutputView)
	{
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[htmlOutputView]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[documentView]-(1)-[htmlOutputView]|" options:0 metrics:nil views:views]];

		self.htmlOutputHeightConstraint = [NSLayoutConstraint constraintWithItem:_htmlOutputView attribute:NSLayoutAttributeHeight relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:_htmlOutputHeight];
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
	if(!_fileBrowserView)
		return NSZeroRect;
	NSRect r = _fileBrowserView.frame;
	return NSMakeRect(_fileBrowserOnRight ? NSMinX(r)-3 : NSMaxX(r)-4, NSMinY(r), 10, NSHeight(r));
}

- (NSRect)htmlOutputResizeRect
{
	if(!_htmlOutputView)
		return NSZeroRect;
	NSRect r = _htmlOutputView.frame;
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
		view = _fileBrowserView;
	else if(NSMouseInRect(mouseDownPos, [self htmlOutputResizeRect], [self isFlipped]))
		view = _htmlOutputView;

	if(!view || [anEvent type] != NSLeftMouseDown)
		return [super mouseDown:anEvent];

	NSEvent* mouseDownEvent = anEvent;
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

		if(view == _htmlOutputView)
		{
			CGFloat height = NSHeight(initialFrame) + (mouseCurrentPos.y - mouseDownPos.y);
			_htmlOutputHeight = std::max<CGFloat>(50, round(height));
			self.htmlOutputHeightConstraint.constant = _htmlOutputHeight;

			[[NSUserDefaults standardUserDefaults] setInteger:_htmlOutputHeight forKey:kUserDefaultsHTMLOutputHeightKey];
		}
		else if(view == _fileBrowserView)
		{
			CGFloat width = NSWidth(initialFrame) + (mouseCurrentPos.x - mouseDownPos.x) * (_fileBrowserOnRight ? -1 : +1);
			_fileBrowserWidth = std::max<CGFloat>(50, round(width));
			self.fileBrowserWidthConstraint.constant = _fileBrowserWidth;

			[[NSUserDefaults standardUserDefaults] setInteger:_fileBrowserWidth forKey:kUserDefaultsFileBrowserWidthKey];
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
	if([view isKindOfClass:[NSView class]] && [view isDescendantOf:_htmlOutputView])
		[NSApp sendAction:@selector(performCloseSplit:) to:nil from:_htmlOutputView];
	else if(_tabBarView)
		[_tabBarView tryToPerform:_cmd with:sender];
}
@end
