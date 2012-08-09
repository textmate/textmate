#import "OakKeyEquivalentView.h"
#import "NSImage Additions.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <text/utf8.h>

static NSString* const kBindingInfoControllerKey   = @"controller";
static NSString* const kBindingInfoBindingKey      = @"binding";
static NSString* const kBindingInfoKeyPathKey      = @"keyPath";

static NSString* const kRecordingPlaceholderString = @"…";

@interface OakKeyEquivalentView ()
@property (nonatomic, retain) NSString* displayString;
@property (nonatomic, assign) BOOL showClearButton;
@property (nonatomic, assign) BOOL mouseInClearButton;
@end

@implementation OakKeyEquivalentView
@synthesize eventString, disableGlobalHotkeys, displayString, showClearButton, mouseInClearButton, recording;

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
		disableGlobalHotkeys = YES;
	return self;
}

- (void)dealloc
{
	for(NSDictionary* info in observers)
		[[info objectForKey:kBindingInfoControllerKey] removeObserver:self forKeyPath:[info objectForKey:kBindingInfoKeyPathKey]];

	[eventString release];
	[displayString release];
	[observers release];
	[super dealloc];
}

- (void)setEventString:(NSString*)aString
{
	if(aString == eventString || [aString isEqualToString:eventString])
		return;
	[eventString release];
	eventString = [aString retain];

	self.showClearButton = NSNotEmptyString(eventString) && !recording;
	self.displayString = recording ? kRecordingPlaceholderString : [NSString stringWithCxxString:ns::glyphs_for_event_string(to_s(eventString))];

	for(NSDictionary* info in observers)
	{
		if([[info objectForKey:kBindingInfoBindingKey] isEqualToString:@"value"])
		{
			id controller = [info objectForKey:kBindingInfoControllerKey];
			NSString* keyPath = [info objectForKey:kBindingInfoKeyPathKey];
			NSString* oldValue = [controller valueForKeyPath:keyPath];
			if(!oldValue || ![oldValue isEqualToString:eventString])
				[controller setValue:eventString forKeyPath:keyPath];
		}
	}
}

- (void)setDisplayString:(NSString*)aString
{
	if(aString == displayString || [aString isEqualToString:displayString])
		return;
	[displayString release];
	displayString = [aString retain];
	[self setNeedsDisplay:YES];
}

- (void)setShowClearButton:(BOOL)flag
{
	if(flag == showClearButton)
		return;
	showClearButton = flag;

	if(flag)
	{
		Class cl = NSClassFromString(@"OFBPathInfoCell");
		NSImage* imgNormal = [NSImage imageNamed:@"CloseFile" inSameBundleAsClass:cl];

		NSSize imgSize = imgNormal.size;
		CGFloat imgMargin = floor((NSHeight([self bounds]) - imgSize.height) / 2);
		clearButtonRect = NSMakeRect(NSWidth([self bounds]) - imgSize.width - imgMargin, imgMargin, imgSize.width, imgSize.height);
		[self setNeedsDisplayInRect:clearButtonRect];

		NSTrackingArea* trackingArea = [[[NSTrackingArea alloc] initWithRect:clearButtonRect options:NSTrackingMouseEnteredAndExited|NSTrackingActiveAlways owner:self userInfo:nil] autorelease];
		[self addTrackingArea:trackingArea];
	}
	else
	{
		for(NSTrackingArea* trackingArea in self.trackingAreas)
			[self removeTrackingArea:trackingArea];

		self.mouseInClearButton = NO;
		[self setNeedsDisplayInRect:clearButtonRect];
		clearButtonRect = NSZeroRect;
	}
}

- (void)setRecording:(BOOL)flag
{
	if(flag == recording)
		return;

	recording = flag;
	self.showClearButton = NSNotEmptyString(eventString) && !recording;
	self.displayString = recording ? kRecordingPlaceholderString : [NSString stringWithCxxString:ns::glyphs_for_event_string(to_s(eventString))];

	if(disableGlobalHotkeys)
	{
		if(recording)
				hotkeyToken = PushSymbolicHotKeyMode(kHIHotKeyModeAllDisabled);
		else	PopSymbolicHotKeyMode(hotkeyToken);
	}
}

- (void)setMouseInClearButton:(BOOL)flag
{
	if(flag == mouseInClearButton)
		return;
	mouseInClearButton = flag;
	[self setNeedsDisplayInRect:clearButtonRect];
}

- (void)setKeyState:(NSUInteger)newState
{
	NSUInteger oldState = self.keyState;
	[super setKeyState:newState];

	BOOL didHaveFocus = (oldState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	BOOL doesHaveFocus = (newState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	if(didHaveFocus != doesHaveFocus)
		[self setKeyboardFocusRingNeedsDisplayInRect:[self bounds]];

	BOOL doesHaveResponder = (newState & (OakViewViewIsFirstResponderMask)) == (OakViewViewIsFirstResponderMask);
	if(!doesHaveResponder)
		self.recording = NO;

	if(!doesHaveFocus && self.recording)
		self.displayString = kRecordingPlaceholderString; // reset potential display string from flagsChanged:
}

- (void)clearKeyEquivalent:(id)sender
{
	self.eventString = nil;
}

- (BOOL)isMouseDownInCloseButton:(NSEvent*)anEvent
{
	return showClearButton && [anEvent type] == NSLeftMouseDown && NSMouseInRect([self convertPoint:[anEvent locationInWindow] fromView:nil], clearButtonRect, [self isFlipped]);
}

- (BOOL)acceptsFirstMouse:(NSEvent*)anEvent
{
	return YES;
}

- (BOOL)shouldDelayWindowOrderingForEvent:(NSEvent*)anEvent
{
	return [self isMouseDownInCloseButton:anEvent];
}

- (BOOL)acceptsFirstResponder
{
	return ![self isMouseDownInCloseButton:[NSApp currentEvent]];
}

- (void)mouseDown:(NSEvent*)anEvent
{
	if([self isMouseDownInCloseButton:anEvent])
	{
		[NSApp preventWindowOrdering];

		mouseDown = YES;
		[self setNeedsDisplayInRect:clearButtonRect];

		while(true)
		{
			NSPoint mousePos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
			self.mouseInClearButton = NSMouseInRect(mousePos, clearButtonRect, [self isFlipped]);
			if([anEvent type] == NSLeftMouseUp)
				break;
			anEvent = [NSApp nextEventMatchingMask:(NSLeftMouseUpMask|NSLeftMouseDraggedMask|NSRightMouseDownMask) untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES];
		}

		if(mouseInClearButton)
			[self clearKeyEquivalent:self];

		mouseDown = NO;
		[self setNeedsDisplayInRect:clearButtonRect];
	}
	else
	{
		if(self == [[self window] firstResponder])
			self.recording = YES;
	}
}

- (void)mouseEntered:(NSEvent*)anEvent
{
	self.mouseInClearButton = NSMouseInRect([self convertPoint:[anEvent locationInWindow] fromView:nil], clearButtonRect, [self isFlipped]);
}

- (void)mouseExited:(NSEvent*)anEvent
{
	self.mouseInClearButton = NSMouseInRect([self convertPoint:[anEvent locationInWindow] fromView:nil], clearButtonRect, [self isFlipped]);
}

- (void)flagsChanged:(NSEvent*)anEvent
{
	if(recording)
	{
		std::string const str = ns::glyphs_for_flags([anEvent modifierFlags]);
		self.displayString = str == "" ? kRecordingPlaceholderString : [NSString stringWithCxxString:str];
	}
}

- (BOOL)performKeyEquivalent:(NSEvent*)anEvent
{
	if(!recording)
		return NO;

	self.eventString = [NSString stringWithCxxString:to_s(anEvent)];
	self.recording = NO;
	return YES;
}

- (void)keyDown:(NSEvent*)anEvent
{
	if(recording)
	{
		[self performKeyEquivalent:anEvent];
	}
	else
	{
		static std::string const ClearKeys[] = { utf8::to_s(NSDeleteCharacter), utf8::to_s(NSDeleteFunctionKey) };
		static std::string const RecordingKeys[] = { " ", "\n", "\r" };
		std::string const keyString = to_s(anEvent);
		if(oak::contains(beginof(ClearKeys), endof(ClearKeys), keyString))
			[self clearKeyEquivalent:self];
		else if(oak::contains(beginof(RecordingKeys), endof(RecordingKeys), keyString))
			self.recording = YES;
		else
			[self interpretKeyEvents:@[ anEvent ]];
	}
}

- (void)insertTab:(id)sender
{
	if([[self window] firstResponder] == self)
		[[self window] selectNextKeyView:self];
}

- (void)insertBacktab:(id)sender
{
	if([[self window] firstResponder] == self)
		[[self window] selectPreviousKeyView:self];
}

- (void)drawRect:(NSRect)aRect
{
	NSRect frame = [self bounds];

	[[NSColor grayColor] set];
	NSFrameRect(frame);
	NSEraseRect(NSIntersectionRect(aRect, NSInsetRect(frame, 1, 1)));

	NSDictionary* stringAttributes = [NSDictionary dictionaryWithObjectsAndKeys:
		recording ? [NSColor grayColor] : [NSColor blackColor],                 NSForegroundColorAttributeName,
		[NSFont userFixedPitchFontOfSize:12], NSFontAttributeName,
		nil];

	NSSize size = [displayString sizeWithAttributes:stringAttributes];
	[displayString drawAtPoint:NSMakePoint(NSMidX([self visibleRect]) - size.width / 2, NSMidY([self visibleRect]) - size.height /2 ) withAttributes:stringAttributes];

	if(showClearButton)
	{
		Class cl = NSClassFromString(@"OFBPathInfoCell");
		NSImage* imgNormal = [NSImage imageNamed:@"CloseFile"        inSameBundleAsClass:cl];
		NSImage* imgHover  = [NSImage imageNamed:@"CloseFileOver"    inSameBundleAsClass:cl];
		NSImage* imgDown   = [NSImage imageNamed:@"CloseFilePressed" inSameBundleAsClass:cl];
		NSImage* image = mouseInClearButton ? (mouseDown ? imgDown : imgHover) : imgNormal;
		[image drawAdjustedInRect:clearButtonRect fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1.0];
	}

	BOOL doesHaveFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	if(doesHaveFocus)
	{
		[NSGraphicsContext saveGraphicsState];
		NSSetFocusRingStyle(NSFocusRingOnly);
		NSRectFill(frame);
		[NSGraphicsContext restoreGraphicsState];
	}
}

// ============
// = Bindings =
// ============

- (void)bind:(NSString*)aBinding toObject:(id)observableController withKeyPath:(NSString*)aKeyPath options:(NSDictionary*)someOptions
{
	observers = observers ?: [NSMutableArray new];
	[observers addObject:[NSDictionary dictionaryWithObjectsAndKeys:
		aBinding,             kBindingInfoBindingKey,
		observableController, kBindingInfoControllerKey,
		aKeyPath,             kBindingInfoKeyPathKey,
		nil]];

	[observableController addObserver:self forKeyPath:aKeyPath options:NSKeyValueObservingOptionInitial context:NULL];
}

- (void)unbind:(NSString*)aBinding
{
	for(NSUInteger i = [observers count]; i > 0; --i)
	{
		NSDictionary* info = [observers objectAtIndex:i-1];
		if([aBinding isEqualToString:[info objectForKey:kBindingInfoBindingKey]])
		{
			[[info objectForKey:kBindingInfoControllerKey] removeObserver:self forKeyPath:[info objectForKey:kBindingInfoKeyPathKey]];
			[observers removeObjectAtIndex:i-i];
		}
	}
}

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)observableController change:(NSDictionary*)changeDictionary context:(void*)userData
{
	for(NSDictionary* info in observers)
	{
		if(observableController == [info objectForKey:kBindingInfoControllerKey] && [aKeyPath isEqualToString:[info objectForKey:kBindingInfoKeyPathKey]])
		{
			if([[info objectForKey:kBindingInfoBindingKey] isEqualToString:@"value"])
				self.eventString = [observableController valueForKeyPath:aKeyPath];
		}
	}
}
@end
