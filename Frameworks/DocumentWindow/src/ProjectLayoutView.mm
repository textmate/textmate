#import "ProjectLayoutView.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSColor Additions.h>
#import <Preferences/Keys.h>
#import <oak/misc.h>
#import <oak/debug.h>

NSString* const kUserDefaultsFileBrowserWidthKey = @"fileBrowserWidth";
NSString* const kUserDefaultsHTMLOutputSizeKey   = @"htmlOutputSize";

@interface ProjectLayoutView ()
@property (nonatomic) NSView* fileBrowserDivider;
@property (nonatomic) NSView* htmlOutputDivider;
@property (nonatomic) NSLayoutConstraint* fileBrowserWidthConstraint;
@property (nonatomic) NSLayoutConstraint* htmlOutputSizeConstraint;
@property (nonatomic) NSMutableArray* myConstraints;
@property (nonatomic) BOOL mouseDownRecursionGuard;
@end

@implementation ProjectLayoutView { OBJC_WATCH_LEAKS(ProjectLayoutView); }
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsFileBrowserWidthKey : @250,
		kUserDefaultsHTMLOutputSizeKey   : NSStringFromSize(NSMakeSize(200, 200))
	}];
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		_myConstraints    = [NSMutableArray array];
		_fileBrowserWidth = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsFileBrowserWidthKey];
		_htmlOutputSize   = NSSizeFromString([[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsHTMLOutputSizeKey]);

		[self userDefaultsDidChange:nil];
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
	self.htmlOutputOnRight  = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"right"];
	self.tabsAboveDocument  = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsTabsAboveDocumentKey];
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
	return newView;
}

- (void)setTabBarView:(NSView*)aTabBarView           { _tabBarView     = [self replaceView:_tabBarView      withView:aTabBarView];      }
- (void)setDocumentView:(NSView*)aDocumentView       { _documentView   = [self replaceView:_documentView    withView:aDocumentView];    }

- (void)setHtmlOutputView:(NSView*)aHtmlOutputView
{
	_htmlOutputDivider = [self replaceView:_htmlOutputDivider withView:(aHtmlOutputView ? (_htmlOutputOnRight ? OakCreateVerticalLine([NSColor controlShadowColor]) : OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1])) : nil)];
	_htmlOutputView    = [self replaceView:_htmlOutputView withView:aHtmlOutputView];
}

- (void)setFileBrowserView:(NSView*)aFileBrowserView
{
	_fileBrowserDivider = [self replaceView:_fileBrowserDivider withView:aFileBrowserView ? OakCreateVerticalLine([NSColor controlShadowColor]) : nil];
	_fileBrowserView    = [self replaceView:_fileBrowserView withView:aFileBrowserView];
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

- (void)setTabsAboveDocument:(BOOL)flag
{
	if(_tabsAboveDocument != flag)
	{
		_tabsAboveDocument = flag;
		[self setNeedsUpdateConstraints:YES];
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
		@"tabBarView"                 : _tabBarView,
		@"documentView"               : _documentView,
		@"fileBrowserView"            : _fileBrowserView            ?: [NSNull null],
		@"fileBrowserDivider"         : _fileBrowserDivider         ?: [NSNull null],
		@"htmlOutputView"             : _htmlOutputView             ?: [NSNull null],
		@"htmlOutputDivider"          : _htmlOutputDivider          ?: [NSNull null],
	};

	// Tab bar and document
	CONSTRAINT(@"H:|-(==0@400)-[tabBarView]-(==0@400)-|", 0);
	CONSTRAINT(@"H:|-(==0@400)-[documentView]-(==0@400)-|", 0);
	CONSTRAINT(@"V:|[tabBarView][documentView]-(==0@400)-|", 0);

	if(_fileBrowserView)
	{
		// width
		self.fileBrowserWidthConstraint = [NSLayoutConstraint constraintWithItem:_fileBrowserView attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:_fileBrowserWidth];
		self.fileBrowserWidthConstraint.priority = NSLayoutPriorityDragThatCannotResizeWindow;
		[_myConstraints addObject:self.fileBrowserWidthConstraint];

		if(_fileBrowserOnRight)
				CONSTRAINT(@"H:[documentView]-(==0@400)-[fileBrowserDivider][fileBrowserView]|", NSLayoutFormatAlignAllBottom);
		else	CONSTRAINT(@"H:|[fileBrowserView][fileBrowserDivider][documentView]", NSLayoutFormatAlignAllBottom);

		CONSTRAINT(@"V:|[tabBarView][fileBrowserDivider]", 0);
		if(_tabsAboveDocument)
		{
			CONSTRAINT(@"V:|[fileBrowserView]", 0);

			if(_fileBrowserOnRight)
					CONSTRAINT(@"H:|[tabBarView]-(-1)-[fileBrowserDivider]", 0);
			else	CONSTRAINT(@"H:[fileBrowserDivider]-(-1)-[tabBarView]|", 0);
		}
		else
		{
			CONSTRAINT(@"V:|[tabBarView][fileBrowserView]", 0);
		}
	}

	if(_htmlOutputView)
	{
		// size (either width or height)
		self.htmlOutputSizeConstraint = _htmlOutputOnRight ? [NSLayoutConstraint constraintWithItem:_htmlOutputView attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:_htmlOutputSize.width] : [NSLayoutConstraint constraintWithItem:_htmlOutputView attribute:NSLayoutAttributeHeight relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:_htmlOutputSize.height];
		self.htmlOutputSizeConstraint.priority = NSLayoutPriorityDragThatCannotResizeWindow-1;
		[_myConstraints addObject:self.htmlOutputSizeConstraint];

		if(_htmlOutputOnRight)
		{
			CONSTRAINT(@"H:[documentView][htmlOutputDivider][htmlOutputView]-(==0@400)-|", NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom);
		}
		else
		{
			CONSTRAINT(@"H:|[htmlOutputView(==htmlOutputDivider)]|", 0);
			CONSTRAINT(@"V:[documentView][htmlOutputDivider][htmlOutputView]|", 0);
		}

		if(_htmlOutputOnRight && _fileBrowserView && _fileBrowserOnRight)
			CONSTRAINT(@"H:[htmlOutputView][fileBrowserDivider]", 0);
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

	if(!view || [anEvent type] != NSLeftMouseDown)
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
		while([anEvent type] != NSLeftMouseUp)
		{
			anEvent = [NSApp nextEventMatchingMask:(NSLeftMouseDraggedMask|NSLeftMouseDown|NSLeftMouseUpMask) untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES];
			if([anEvent type] != NSLeftMouseDragged)
				break;

			NSPoint mouseCurrentPos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
			if(!didDrag && SQ(fabs(mouseDownPos.x - mouseCurrentPos.x)) + SQ(fabs(mouseDownPos.y - mouseCurrentPos.y)) < SQ(1))
				continue; // we didn't even drag a pixel

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

				[[NSUserDefaults standardUserDefaults] setObject:NSStringFromSize(_htmlOutputSize) forKey:kUserDefaultsHTMLOutputSizeKey];
			}
			else if(view == _fileBrowserView)
			{
				CGFloat width = NSWidth(initialFrame) + (mouseCurrentPos.x - mouseDownPos.x) * (_fileBrowserOnRight ? -1 : +1);
				_fileBrowserWidth = std::max<CGFloat>(50, round(width));
				self.fileBrowserWidthConstraint.constant = _fileBrowserWidth;
				self.fileBrowserWidthConstraint.priority = NSLayoutPriorityDragThatCannotResizeWindow-1;

				[[NSUserDefaults standardUserDefaults] setInteger:_fileBrowserWidth forKey:kUserDefaultsFileBrowserWidthKey];
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
	else if(_tabBarView)
		[_tabBarView tryToPerform:_cmd with:sender];
}
@end
