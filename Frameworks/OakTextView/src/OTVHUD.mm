#import "OTVHUD.h"
#import <OakAppKit/src/OakUIConstructionFunctions.h>

@interface OTVHUDView : NSView
@end

@implementation OTVHUDView
- (void)drawRect:(NSRect)aRect
{
	[[NSColor colorWithCalibratedWhite:0.5 alpha:0.5] set];
	[[NSBezierPath bezierPathWithRoundedRect:[self bounds] xRadius:6 yRadius:6] fill];
}
@end

@interface OTVHUD ()
{
	NSTextField* _textField;
	NSUInteger _requestID;
}
@property (nonatomic, weak) NSView* lastView;
@end

@implementation OTVHUD
- (instancetype)initWithView:(NSView*)aView
{
	CGFloat const kWidth  = 100;
	CGFloat const kHeight = 30;

	NSRect aRect = [aView.window convertRectToScreen:[aView convertRect:[aView visibleRect] toView:nil]];
	aRect = NSInsetRect(aRect, 10, 10);
	aRect = NSMakeRect(NSMaxX(aRect) - kWidth, NSMaxY(aRect) - kHeight, kWidth, kHeight);

	NSWindow* window = [[NSWindow alloc] initWithContentRect:aRect styleMask:NSWindowStyleMaskBorderless backing:NSBackingStoreBuffered defer:NO];
	if(!window)
		return nil;

	if(self = [super initWithWindow:window])
	{
		_lastView = aView;

		window.ignoresMouseEvents = YES;
		window.backgroundColor    = [NSColor clearColor];
		window.opaque             = NO;
		window.level              = NSPopUpMenuWindowLevel;

		OTVHUDView* contentView = [[OTVHUDView alloc] initWithFrame:aRect];
		window.contentView = contentView;

		_textField = OakCreateLabel(@"", [NSFont systemFontOfSize:20]);
		self.stringValue = @"88888";

		[_textField sizeToFit];
		CGFloat textHeight = NSHeight(_textField.frame);
		[_textField setFrame:NSMakeRect(0, round((kHeight - textHeight) / 2), kWidth, textHeight)];

		[contentView addSubview:_textField];
	}
	return self;
}

- (void)setStringValue:(NSString*)someText
{
	NSMutableParagraphStyle* pStyle = [NSMutableParagraphStyle new];
	[pStyle setAlignment:NSTextAlignmentCenter];

	NSShadow* shadow = [NSShadow new];
	[shadow setShadowColor:[NSColor darkGrayColor]];
	[shadow setShadowOffset:NSMakeSize(1, -1)];
	[shadow setShadowBlurRadius:1.2];

	_textField.objectValue = [[NSMutableAttributedString alloc] initWithString:someText attributes:@{
		NSParagraphStyleAttributeName:  pStyle,
		NSForegroundColorAttributeName: [NSColor whiteColor],
		NSShadowAttributeName:          shadow
	}];
}

- (void)fadeOut:(id)sender
{
	NSUInteger requestID = _requestID;

	[NSAnimationContext beginGrouping];
	[NSAnimationContext currentContext].completionHandler = ^{
		if(requestID == _requestID)
			[self close];
	};
	[self.window.animator setAlphaValue:0];
	[NSAnimationContext endGrouping];
}

- (void)showWindow:(id)sender
{
	++_requestID;
	[NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(fadeOut:) object:nil];

	[NSAnimationContext beginGrouping];
	[NSAnimationContext currentContext].duration = 0;
	[self.window.animator setAlphaValue:1];
	[NSAnimationContext endGrouping];

	[super showWindow:sender];

	[self performSelector:@selector(fadeOut:) withObject:nil afterDelay:1];
}

+ (OTVHUD*)showHudForView:(NSView*)aView withText:(NSString*)someText
{
	static __weak OTVHUD* LastHUD;

	OTVHUD* res = LastHUD;
	if(!res || res.lastView != aView)
		LastHUD = res = [[OTVHUD alloc] initWithView:aView];

	res.stringValue = someText;
	[res showWindow:self];
	return res;
}
@end
