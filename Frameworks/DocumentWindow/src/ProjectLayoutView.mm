#import "ProjectLayoutView.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/OakFoundation.h>
#import <Preferences/Keys.h>
#import <oak/misc.h>
#import <oak/debug.h>

NSString* const kUserDefaultsFileBrowserWidthKey = @"fileBrowserWidth";
NSString* const kUserDefaultsHTMLOutputSizeKey   = @"htmlOutputSize";

@interface ProjectLayoutView () <OakUserDefaultsObserver>
@property (nonatomic) NSView* fileBrowserDivider;
@property (nonatomic) NSView* htmlOutputDivider;
@property (nonatomic) NSLayoutConstraint* fileBrowserWidthConstraint;
@property (nonatomic) NSLayoutConstraint* htmlOutputSizeConstraint;
@property (nonatomic) NSMutableArray* myConstraints;
@property (nonatomic) BOOL mouseDownRecursionGuard;
@end

@implementation ProjectLayoutView
+ (void)initialize
{
	[NSUserDefaults.standardUserDefaults registerDefaults:@{
		kUserDefaultsFileBrowserWidthKey: @250,
		kUserDefaultsHTMLOutputSizeKey:   NSStringFromSize(NSMakeSize(200, 200))
	}];
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		_myConstraints    = [NSMutableArray array];
		_fileBrowserWidth = [NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsFileBrowserWidthKey];
		_htmlOutputSize   = NSSizeFromString([NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsHTMLOutputSizeKey]);

		[self userDefaultsDidChange:nil];
		OakObserveUserDefaults(self);
	}
	return self;
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self];
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.htmlOutputOnRight = [[NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"right"];
}

- (NSView*)replaceView:(NSView*)oldView withView:(NSView*)newView
{
	if(newView == oldView)
		return oldView;

	[oldView removeFromSuperview];
	if(newView)
		OakAddAutoLayoutViewsToSuperview(@[ newView ], self);

	[self setNeedsUpdateConstraints:YES];
	return newView;
}

- (void)updateKeyViewLoop
{
	NSMutableArray<NSView*>* views = [NSMutableArray array];
	for(NSView* view : { _documentView, _htmlOutputView, _fileBrowserView })
	{
		if(view)
			[views addObject:view];
	}
	OakSetupKeyViewLoop(views);
}

- (void)setDocumentView:(NSView*)aDocumentView       { _documentView = [self replaceView:_documentView withView:aDocumentView]; [self updateKeyViewLoop]; }

- (NSView*)createDividerAlongYAxis:(BOOL)flag
{
	NSView* res = OakCreateNSBoxSeparator();
	res.translatesAutoresizingMaskIntoConstraints = NO;
	[res addConstraint:[NSLayoutConstraint constraintWithItem:res attribute:(flag ? NSLayoutAttributeWidth : NSLayoutAttributeHeight) relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:1]];
	[res addConstraint:[NSLayoutConstraint constraintWithItem:res attribute:(flag ? NSLayoutAttributeHeight : NSLayoutAttributeWidth) relatedBy:NSLayoutRelationGreaterThanOrEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:2]];
	return res;
}

- (void)setHtmlOutputView:(NSView*)aHtmlOutputView
{
	_htmlOutputDivider = [self replaceView:_htmlOutputDivider withView:(aHtmlOutputView ? [self createDividerAlongYAxis:_htmlOutputOnRight] : nil)];
	_htmlOutputView    = [self replaceView:_htmlOutputView withView:aHtmlOutputView];
	[self updateKeyViewLoop];
}

- (void)setFileBrowserView:(NSView*)aFileBrowserView
{
	_fileBrowserDivider = [self replaceView:_fileBrowserDivider withView:aFileBrowserView ? [self createDividerAlongYAxis:YES] : nil];
	_fileBrowserView    = [self replaceView:_fileBrowserView withView:aFileBrowserView];
	[self updateKeyViewLoop];
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
		self.htmlOutputView = _htmlOutputView; // recreate divider line, required due to <rdar://13093498>
	}
}

#ifndef CONSTRAINT
#define CONSTRAINT(str, align) [_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:str options:align metrics:nil views:views]]
#endif

- (void)updateConstraints
{
	[self removeConstraints:_myConstraints];
	[_myConstraints removeAllObjects];
	[super updateConstraints];

	NSDictionary* views = @{
		@"documentView":       _documentView,
		@"fileBrowserView":    _fileBrowserView    ?: [NSNull null],
		@"fileBrowserDivider": _fileBrowserDivider ?: [NSNull null],
		@"htmlOutputView":     _htmlOutputView     ?: [NSNull null],
		@"htmlOutputDivider":  _htmlOutputDivider  ?: [NSNull null],
	};

	// ========================
	// = Anchor Document View =
	// ========================

	// top
	CONSTRAINT(@"V:|[documentView]", 0);

	// bottom
	if(_htmlOutputView && !_htmlOutputOnRight)
		CONSTRAINT(@"V:[documentView][htmlOutputDivider]", 0);
	else
		CONSTRAINT(@"V:[documentView]|", 0);

	// left
	if(_fileBrowserView && !_fileBrowserOnRight)
		CONSTRAINT(@"H:[fileBrowserDivider][documentView]", 0);
	else
		CONSTRAINT(@"H:|[documentView]", 0);

	// right
	if(_htmlOutputView && _htmlOutputOnRight)
		CONSTRAINT(@"H:[documentView][htmlOutputDivider]", 0);
	else if(_fileBrowserView && _fileBrowserOnRight)
		CONSTRAINT(@"H:[documentView][fileBrowserDivider]", 0);
	else
		CONSTRAINT(@"H:[documentView]|", 0);

	// =======================
	// = Anchor File Browser =
	// =======================

	if(_fileBrowserView)
	{
		// width
		self.fileBrowserWidthConstraint = [NSLayoutConstraint constraintWithItem:_fileBrowserView attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:_fileBrowserWidth];
		self.fileBrowserWidthConstraint.priority = NSLayoutPriorityDragThatCannotResizeWindow;
		[_myConstraints addObject:self.fileBrowserWidthConstraint];

		// top
		CONSTRAINT(@"V:|[fileBrowserDivider]", 0);
		CONSTRAINT(@"V:|[fileBrowserView]", 0);

		// bottom
		if(_htmlOutputView && !_htmlOutputOnRight)
		{
			CONSTRAINT(@"V:[fileBrowserView][htmlOutputDivider]", 0);
			CONSTRAINT(@"V:[fileBrowserDivider][htmlOutputDivider]", 0);
		}
		else
		{
			CONSTRAINT(@"V:[fileBrowserView]|", 0);
			CONSTRAINT(@"V:[fileBrowserDivider]|", 0);
		}

		// left
		if(_fileBrowserOnRight && _htmlOutputView && _htmlOutputOnRight)
			CONSTRAINT(@"H:[htmlOutputView][fileBrowserDivider][fileBrowserView]", 0);
		else if(_fileBrowserOnRight)
			CONSTRAINT(@"H:[documentView][fileBrowserDivider][fileBrowserView]", 0);
		else
			CONSTRAINT(@"H:|[fileBrowserView][fileBrowserDivider]", 0);

		// right
		if(_fileBrowserOnRight)
			CONSTRAINT(@"H:[fileBrowserView]|", 0);
		else
			CONSTRAINT(@"H:[fileBrowserDivider][documentView]", 0);
	}

	// ===========================
	// = Anchor HTML Output View =
	// ===========================

	if(_htmlOutputView)
	{
		// size (either width or height)
		self.htmlOutputSizeConstraint = _htmlOutputOnRight ? [NSLayoutConstraint constraintWithItem:_htmlOutputView attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:_htmlOutputSize.width] : [NSLayoutConstraint constraintWithItem:_htmlOutputView attribute:NSLayoutAttributeHeight relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:_htmlOutputSize.height];
		self.htmlOutputSizeConstraint.priority = NSLayoutPriorityDragThatCannotResizeWindow-1;
		[_myConstraints addObject:self.htmlOutputSizeConstraint];

		if(_htmlOutputOnRight)
		{
			// top + bottom
			CONSTRAINT(@"V:|[htmlOutputView]|", 0);
			CONSTRAINT(@"V:|[htmlOutputDivider]|", 0);

			// left + right
			if(_fileBrowserView && _fileBrowserOnRight)
				CONSTRAINT(@"H:[documentView][htmlOutputDivider][htmlOutputView][fileBrowserDivider]", 0);
			else
				CONSTRAINT(@"H:[documentView][htmlOutputDivider][htmlOutputView]|", 0);
		}
		else
		{
			// top + bottom
			CONSTRAINT(@"V:[documentView][htmlOutputDivider][htmlOutputView]|", 0);

			// left + right
			CONSTRAINT(@"H:|[htmlOutputView]|", 0);
			CONSTRAINT(@"H:|[htmlOutputDivider]|", 0);
		}
	}

	[self addConstraints:_myConstraints];
	[[self window] invalidateCursorRectsForView:self];
}

#undef CONSTRAINT

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
	return _htmlOutputOnRight ? NSMakeRect(NSMinX(r)-3, NSMinY(r), 10, NSHeight(r)) : NSMakeRect(NSMinX(r), NSMaxY(r)-4, NSWidth(r), 10);
}

- (void)resetCursorRects
{
	[self addCursorRect:[self fileBrowserResizeRect] cursor:[NSCursor resizeLeftRightCursor]];
	[self addCursorRect:[self htmlOutputResizeRect]  cursor:_htmlOutputOnRight ? [NSCursor resizeLeftRightCursor] : [NSCursor resizeUpDownCursor]];
}

- (BOOL)mouseDownCanMoveWindow
{
	return NO;
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
	if(_mouseDownRecursionGuard)
		return;
	_mouseDownRecursionGuard = YES;

	NSView* view = nil;
	NSPoint mouseDownPos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	if(NSMouseInRect(mouseDownPos, [self fileBrowserResizeRect], [self isFlipped]))
		view = _fileBrowserView;
	else if(NSMouseInRect(mouseDownPos, [self htmlOutputResizeRect], [self isFlipped]))
		view = _htmlOutputView;

	if(!view || [anEvent type] != NSEventTypeLeftMouseDown)
	{
		[super mouseDown:anEvent];
	}
	else
	{
		if(_fileBrowserView)
		{
			self.fileBrowserWidthConstraint.constant = NSWidth(_fileBrowserView.frame);
			self.fileBrowserWidthConstraint.priority = NSLayoutPriorityDragThatCannotResizeWindow;
		}

		if(_htmlOutputView)
		{
			if(_htmlOutputOnRight)
					self.htmlOutputSizeConstraint.constant = NSWidth(_htmlOutputView.frame);
			else	self.htmlOutputSizeConstraint.constant = NSHeight(_htmlOutputView.frame);
			self.htmlOutputSizeConstraint.priority = NSLayoutPriorityDragThatCannotResizeWindow;
		}

		NSEvent* mouseDownEvent = anEvent;
		NSRect initialFrame = view.frame;

		BOOL didDrag = NO;
		while([anEvent type] != NSEventTypeLeftMouseUp)
		{
			anEvent = [NSApp nextEventMatchingMask:(NSEventMaskLeftMouseDragged|NSEventMaskLeftMouseDown|NSEventMaskLeftMouseUp) untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES];
			if([anEvent type] != NSEventTypeLeftMouseDragged)
				break;

			NSPoint mouseCurrentPos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
			if(!didDrag && hypot(mouseDownPos.x - mouseCurrentPos.x, mouseDownPos.y - mouseCurrentPos.y) < 2.5)
				continue;

			if(view == _htmlOutputView)
			{
				if(_htmlOutputOnRight)
				{
					CGFloat width = NSWidth(initialFrame) + (mouseCurrentPos.x - mouseDownPos.x) * (_htmlOutputOnRight ? -1 : +1);
					_htmlOutputSize.width = std::max<CGFloat>(50, round(width));
					self.htmlOutputSizeConstraint.constant = width;
				}
				else
				{
					CGFloat height = NSHeight(initialFrame) + (mouseCurrentPos.y - mouseDownPos.y);
					_htmlOutputSize.height = std::max<CGFloat>(50, round(height));
					self.htmlOutputSizeConstraint.constant = height;
				}
				self.htmlOutputSizeConstraint.priority   = NSLayoutPriorityDragThatCannotResizeWindow-1;

				[NSUserDefaults.standardUserDefaults setObject:NSStringFromSize(_htmlOutputSize) forKey:kUserDefaultsHTMLOutputSizeKey];
			}
			else if(view == _fileBrowserView)
			{
				CGFloat width = NSWidth(initialFrame) + (mouseCurrentPos.x - mouseDownPos.x) * (_fileBrowserOnRight ? -1 : +1);
				_fileBrowserWidth = std::max<CGFloat>(50, round(width));
				self.fileBrowserWidthConstraint.constant = _fileBrowserWidth;
				self.fileBrowserWidthConstraint.priority = NSLayoutPriorityDragThatCannotResizeWindow-1;

				[NSUserDefaults.standardUserDefaults setInteger:_fileBrowserWidth forKey:kUserDefaultsFileBrowserWidthKey];
			}

			[[self window] invalidateCursorRectsForView:self];
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

		self.fileBrowserWidthConstraint.priority = NSLayoutPriorityDragThatCannotResizeWindow;
		self.htmlOutputSizeConstraint.priority   = NSLayoutPriorityDragThatCannotResizeWindow-1;
	}

	_mouseDownRecursionGuard = NO;
}

- (void)performClose:(id)sender
{
	NSView* view = (NSView*)[[self window] firstResponder];
	if([view isKindOfClass:[NSView class]] && [view isDescendantOf:_htmlOutputView])
		[NSApp sendAction:@selector(performCloseSplit:) to:nil from:_htmlOutputView];
	else if([self.window.delegate respondsToSelector:@selector(performClose:)])
		[self.window.delegate performSelector:@selector(performClose:) withObject:sender];
	else
		NSBeep();
}
@end
