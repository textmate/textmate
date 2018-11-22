#import "OakToolTip.h"
#import <oak/debug.h>
#import <oak/oak.h>

OAK_DEBUG_VAR(OakToolTip);

@interface OakToolTip : NSPanel
{
	OBJC_WATCH_LEAKS(OakToolTip);

	NSTextField* field;

	NSDate* didOpenAtDate; // ignore mouse moves for the next second
	NSPoint mousePositionWhenOpened;
}
@property (nonatomic) BOOL enforceMouseThreshold;
- (void)setFont:(NSFont*)aFont;
- (void)setStringValue:(NSString*)aString;
- (void)showAtLocation:(NSPoint)aPoint forScreen:(NSScreen*)aScreen;
@end

static __weak OakToolTip* LastToolTip;

@implementation OakToolTip
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		@"OakToolTipMouseMoveIgnorePeriod":  @1,
		@"OakToolTipMouseDistanceThreshold": @5,
	}];
}

- (id)init
{
	D(DBF_OakToolTip, bug("\n"););
	if(self = [super initWithContentRect:NSZeroRect styleMask:NSWindowStyleMaskBorderless backing:NSBackingStoreBuffered defer:NO])
	{
		NSFont* defaultFont = [NSFont toolTipsFontOfSize:0];
		NSFontDescriptor* descriptor = [defaultFont.fontDescriptor fontDescriptorByAddingAttributes:@{
			NSFontFeatureSettingsAttribute: @[ @{ NSFontFeatureTypeIdentifierKey: @(kNumberSpacingType), NSFontFeatureSelectorIdentifierKey: @(kMonospacedNumbersSelector) } ]
		}];
		defaultFont = [NSFont fontWithDescriptor:descriptor size:0];

		[self setHasShadow:YES];
		[self setLevel:NSStatusWindowLevel];

		field = [[NSTextField alloc] initWithFrame:self.contentView.bounds];
		[field setEditable:NO];
		[field setSelectable:NO];
		[field setBezeled:NO];
		[field setBordered:NO];
		[field setDrawsBackground:NO];
		[field setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
		[field setFont:defaultFont];
		[field setStringValue:@"This is a nice little code block"];

		if(@available(macos 10.14, *))
		{
			NSVisualEffectView* effectView = [[NSVisualEffectView alloc] initWithFrame:self.contentView.frame];
			effectView.material = NSVisualEffectMaterialToolTip;
			effectView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
			[self.contentView addSubview:effectView];
			[field setTextColor:NSColor.labelColor];

			NSAppearanceName appearanceName = [NSApp.effectiveAppearance bestMatchFromAppearancesWithNames:@[ NSAppearanceNameAqua, NSAppearanceNameDarkAqua ]];
			if([appearanceName isEqualToString:NSAppearanceNameDarkAqua])
				[field setBordered:YES];
		}
		else
		{
			[self setAlphaValue:0.97];
			[self setOpaque:NO];
			[self setBackgroundColor:[NSColor colorWithCalibratedRed:1.0 green:0.96 blue:0.76 alpha:1]];
			[field setTextColor:NSColor.blackColor];
		}

		[self.contentView addSubview:field positioned:NSWindowAbove relativeTo:nil];
		[self setFrame:[self frameRectForContentRect:[field frame]] display:NO];

		self.enforceMouseThreshold = YES;
	}
	return self;
}

- (void)setFont:(NSFont*)aFont
{
	ASSERT(aFont != nil);
	[field setFont:aFont];
}

- (void)setStringValue:(NSString*)aString
{
	D(DBF_OakToolTip, bug("%s\n", [aString UTF8String]););
	ASSERT(aString != nil);
	[field setStringValue:aString];
}

- (BOOL)shouldCloseForMousePosition:(NSPoint)aPoint
{
	if(!_enforceMouseThreshold)
		return YES;

	CGFloat ignorePeriod = [[NSUserDefaults standardUserDefaults] floatForKey:@"OakToolTipMouseMoveIgnorePeriod"];
	if([[NSDate date] timeIntervalSinceDate:didOpenAtDate] < ignorePeriod)
		return NO;

	if(NSEqualPoints(mousePositionWhenOpened, NSZeroPoint))
	{
		mousePositionWhenOpened = aPoint;
		return NO;
	}

	if(NSPointInRect(aPoint, NSInsetRect([self frame], 5, 5)))
		return NO;

	NSPoint const& p = mousePositionWhenOpened;
	CGFloat dist = hypot(p.x - aPoint.x, p.y - aPoint.y);

	CGFloat moveThreshold = [[NSUserDefaults standardUserDefaults] floatForKey:@"OakToolTipMouseDistanceThreshold"];
	return dist > moveThreshold;
}

- (void)showUntilUserActivity
{
	[self orderFront:self];

	didOpenAtDate = [NSDate date];
	mousePositionWhenOpened = NSZeroPoint;

	__weak __block id eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:(NSEventMaskLeftMouseDown|NSEventMaskRightMouseDown|NSEventMaskMouseMoved|NSEventMaskKeyDown|NSEventMaskScrollWheel|NSEventMaskOtherMouseDown) handler:^NSEvent*(NSEvent* event){
		if([event type] == NSEventTypeMouseMoved && ![self shouldCloseForMousePosition:[NSEvent mouseLocation]])
			return event;

		[self fadeOutSlowly:[event type] == NSEventTypeMouseMoved];
		[NSEvent removeMonitor:eventMonitor];
		return event;
	}];
}

- (void)showAtLocation:(NSPoint)aPoint forScreen:(NSScreen*)aScreen
{
	D(DBF_OakToolTip, bug("%s\n", [NSStringFromPoint(aPoint) UTF8String]););
	aScreen = aScreen ?: [NSScreen mainScreen];

	NSRect r = [aScreen visibleFrame];
	NSRect frame = [self frameRectForContentRect:{ NSZeroPoint, field.fittingSize }];
	frame.size.width = std::min(NSWidth(frame), NSWidth(r));
	frame.size.height = std::min(NSHeight(frame), NSHeight(r));
	[self setFrame:frame display:NO];

	aPoint.x = std::max(NSMinX(r), std::min(aPoint.x, NSMaxX(r)-NSWidth(frame)));
	aPoint.y = std::min(std::max(NSMinY(r)+NSHeight(frame), aPoint.y), NSMaxY(r));

	[self setFrameTopLeftPoint:aPoint];
	[self showUntilUserActivity];
}

- (void)fadeOutSlowly:(BOOL)slowly
{
	[NSAnimationContext beginGrouping];

	[NSAnimationContext currentContext].duration = slowly ? 0.5 : 0.25;
	[NSAnimationContext currentContext].completionHandler = ^{
		[self orderOut:self];
	};

	[self.animator setAlphaValue:0];

	[NSAnimationContext endGrouping];
}
@end

// ==============
// = Public API =
// ==============

void OakShowToolTip (NSString* msg, NSPoint location)
{
	if(msg)
	{
		OakToolTip* toolTip = [OakToolTip new];
		[toolTip setStringValue:msg];

		// Find the screen which we are displaying on
		NSScreen* screen = nil;
		for(NSScreen* candidate in [NSScreen screens])
		{
			if(NSPointInRect(location, [candidate frame]))
			{
				screen = candidate;
				break;
			}
		}

		[toolTip showAtLocation:location forScreen:screen];
		[LastToolTip close];
		LastToolTip = toolTip;
	}
}
