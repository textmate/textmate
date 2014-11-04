#import "OakKeyEquivalentView.h"
#import "OakUIConstructionFunctions.h"
#import "NSImage Additions.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <text/utf8.h>

static NSString* const kRecordingPlaceholderString = @"…";

@interface OakKeyEquivalentView ()
{
	NSRect _clearButtonRect;
	id _eventMonitor;
	void* _hotkeyToken;
	BOOL _mouseDown;
}
@property (nonatomic) NSString* displayString;
@property (nonatomic) BOOL showClearButton;
@property (nonatomic) BOOL mouseInClearButton;
@end

@implementation OakKeyEquivalentView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
		self.disableGlobalHotkeys = YES;
	return self;
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, 22);
}

- (void)setEventString:(NSString*)aString
{
	if(_eventString == aString || [_eventString isEqualToString:aString])
		return;

	_eventString = aString;

	self.showClearButton = OakNotEmptyString(self.eventString) && !self.recording;
	self.displayString = self.recording ? kRecordingPlaceholderString : [NSString stringWithCxxString:ns::glyphs_for_event_string(to_s(_eventString))];

	if(NSDictionary* info = [self infoForBinding:NSValueBinding])
	{
		id controller     = info[NSObservedObjectKey];
		NSString* keyPath = info[NSObservedKeyPathKey];
		if(controller && controller != [NSNull null] && keyPath && (id)keyPath != [NSNull null])
		{
			id oldValue = [controller valueForKeyPath:keyPath];
			if(!oldValue || ![oldValue isEqualTo:_eventString])
				[controller setValue:_eventString forKeyPath:keyPath];
		}
	}
	NSAccessibilityPostNotification(self, NSAccessibilityValueChangedNotification);
}

- (id)value                   { return self.eventString; }
- (void)setValue:(id)newValue { self.eventString = newValue; }

- (void)setDisplayString:(NSString*)aString
{
	if(_displayString == aString || [_displayString isEqualToString:aString])
		return;
	_displayString = aString;
	[self setNeedsDisplay:YES];
}

- (void)setShowClearButton:(BOOL)flag
{
	if(_showClearButton == flag)
		return;

	if(_showClearButton = flag)
	{
		Class cl = NSClassFromString(@"OFBPathInfoCell");
		NSImage* imgNormal = [NSImage imageNamed:@"CloseTemplate" inSameBundleAsClass:cl];

		NSSize imgSize = imgNormal.size;
		CGFloat imgMargin = floor((NSHeight([self bounds]) - imgSize.height) / 2);
		_clearButtonRect = NSMakeRect(NSWidth([self bounds]) - imgSize.width - imgMargin, imgMargin, imgSize.width, imgSize.height);
		[self setNeedsDisplayInRect:_clearButtonRect];

		[self addTrackingArea:[[NSTrackingArea alloc] initWithRect:_clearButtonRect options:NSTrackingMouseEnteredAndExited|NSTrackingActiveAlways owner:self userInfo:nil]];
	}
	else
	{
		for(NSTrackingArea* trackingArea in self.trackingAreas)
			[self removeTrackingArea:trackingArea];

		self.mouseInClearButton = NO;
		[self setNeedsDisplayInRect:_clearButtonRect];
		_clearButtonRect = NSZeroRect;
	}
}

- (void)setRecording:(BOOL)flag
{
	if(_recording == flag)
		return;

	_recording = flag;
	self.showClearButton = OakNotEmptyString(self.eventString) && !self.recording;
	self.displayString = _recording ? kRecordingPlaceholderString : [NSString stringWithCxxString:ns::glyphs_for_event_string(to_s(self.eventString))];

	if(self.recording)
	{
		if(self.disableGlobalHotkeys)
			_hotkeyToken = PushSymbolicHotKeyMode(kHIHotKeyModeAllDisabled);

		_eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:NSFlagsChangedMask|NSKeyDownMask handler:^NSEvent*(NSEvent* event){
			if([event type] == NSFlagsChanged)
			{
				std::string const str = ns::glyphs_for_flags([event modifierFlags]);
				self.displayString = str == "" ? kRecordingPlaceholderString : [NSString stringWithCxxString:str];
			}
			else if([event type] == NSKeyDown)
			{
				self.eventString = [NSString stringWithCxxString:to_s(event)];
				self.recording = NO;
			}
			return nil;
		}];
	}
	else
	{
		[NSEvent removeMonitor:_eventMonitor];
		_eventMonitor = nil;

		if(self.disableGlobalHotkeys)
		{
			PopSymbolicHotKeyMode(_hotkeyToken);
			_hotkeyToken  = nullptr;
		}
	}
}

- (void)setMouseInClearButton:(BOOL)flag
{
	if(_mouseInClearButton == flag)
		return;
	_mouseInClearButton = flag;
	[self setNeedsDisplayInRect:_clearButtonRect];
}

- (void)setKeyState:(NSUInteger)newState
{
	[super setKeyState:newState];

	BOOL doesHaveResponder = (newState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask);
	if(!doesHaveResponder)
		self.recording = NO;

	BOOL doesHaveFocus = (newState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	if(!doesHaveFocus && self.recording)
		self.displayString = kRecordingPlaceholderString; // reset potential display string from flagsChanged:
}

- (void)clearKeyEquivalent:(id)sender
{
	self.eventString = nil;
}

- (BOOL)isMouseDownInCloseButton:(NSEvent*)anEvent
{
	return self.showClearButton && [anEvent type] == NSLeftMouseDown && NSMouseInRect([self convertPoint:[anEvent locationInWindow] fromView:nil], _clearButtonRect, [self isFlipped]);
}

- (BOOL)isOpaque
{
	return YES;
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

		_mouseDown = YES;
		[self setNeedsDisplayInRect:_clearButtonRect];

		while(true)
		{
			NSPoint mousePos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
			self.mouseInClearButton = NSMouseInRect(mousePos, _clearButtonRect, [self isFlipped]);
			if([anEvent type] == NSLeftMouseUp)
				break;
			anEvent = [NSApp nextEventMatchingMask:(NSLeftMouseUpMask|NSLeftMouseDraggedMask|NSRightMouseDownMask) untilDate:[NSDate distantFuture] inMode:NSEventTrackingRunLoopMode dequeue:YES];
		}

		if(self.mouseInClearButton)
			[self clearKeyEquivalent:self];

		_mouseDown = NO;
		[self setNeedsDisplayInRect:_clearButtonRect];
	}
	else
	{
		if(self == [[self window] firstResponder])
			self.recording = YES;
	}
}

- (void)mouseEntered:(NSEvent*)anEvent
{
	self.mouseInClearButton = NSMouseInRect([self convertPoint:[anEvent locationInWindow] fromView:nil], _clearButtonRect, [self isFlipped]);
}

- (void)mouseExited:(NSEvent*)anEvent
{
	self.mouseInClearButton = NSMouseInRect([self convertPoint:[anEvent locationInWindow] fromView:nil], _clearButtonRect, [self isFlipped]);
}

- (void)keyDown:(NSEvent*)anEvent
{
	static std::set<std::string> const ClearKeys     = { utf8::to_s(NSDeleteCharacter), utf8::to_s(NSDeleteFunctionKey), "\e" };
	static std::set<std::string> const RecordingKeys = { " " };
	std::string const keyString = to_s(anEvent);
	if(ClearKeys.find(keyString) != ClearKeys.end() && !OakIsEmptyString(self.eventString))
		[self clearKeyEquivalent:self];
	else if(RecordingKeys.find(keyString) != RecordingKeys.end())
		self.recording = YES;
	else
		[super keyDown:anEvent];
}

- (void)drawRect:(NSRect)aRect
{
	NSRect frame = [self bounds];

	[[NSColor grayColor] set];
	NSFrameRect(frame);
	NSEraseRect(NSIntersectionRect(aRect, NSInsetRect(frame, 1, 1)));

	NSDictionary* stringAttributes = @{
		NSForegroundColorAttributeName : self.recording ? [NSColor grayColor] : [NSColor blackColor],
		NSFontAttributeName            : OakControlFont()
	};

	NSSize size = [self.displayString sizeWithAttributes:stringAttributes];
	[self.displayString drawAtPoint:NSMakePoint(NSMidX([self visibleRect]) - size.width / 2, NSMidY([self visibleRect]) - size.height /2 ) withAttributes:stringAttributes];

	if(self.showClearButton)
	{
		Class cl = NSClassFromString(@"OFBPathInfoCell");
		NSImage* imgNormal = [NSImage imageNamed:@"CloseTemplate"         inSameBundleAsClass:cl];
		NSImage* imgHover  = [NSImage imageNamed:@"CloseRolloverTemplate" inSameBundleAsClass:cl];
		NSImage* imgDown   = [NSImage imageNamed:@"ClosePressedTemplate"  inSameBundleAsClass:cl];
		NSImage* image = self.mouseInClearButton ? (_mouseDown ? imgDown : imgHover) : imgNormal;
		[image drawAdjustedInRect:_clearButtonRect fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1];
	}
}

- (void)drawFocusRingMask
{
	NSRectFill([self bounds]);
}

- (NSRect)focusRingMaskBounds
{
	return [self bounds];
}

- (BOOL)accessibilityIsIgnored
{
	return NO;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* myAttributes = @[
		NSAccessibilityValueAttribute,
		NSAccessibilityNumberOfCharactersAttribute,
		NSAccessibilityDescriptionAttribute,
		NSAccessibilitySelectedTextAttribute,
		NSAccessibilitySelectedTextRangeAttribute,
		NSAccessibilityVisibleCharacterRangeAttribute,
	];
	static NSArray* attributes = [[[NSSet setWithArray:myAttributes] setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
	return attributes;
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	id value = nil;
	BOOL isEmptyRecording = [self.displayString isEqualToString:kRecordingPlaceholderString];
	if([attribute isEqualToString:NSAccessibilityRoleAttribute])
		value = NSAccessibilityTextFieldRole;
	else if([attribute isEqualToString:NSAccessibilityValueAttribute])
		value = isEmptyRecording ? @"" : self.displayString;
	else if([attribute isEqualToString:NSAccessibilityNumberOfCharactersAttribute])
		value = @(isEmptyRecording ? 0 : self.displayString.length);
	else if(
			  [attribute isEqualToString:NSAccessibilitySelectedTextAttribute]
			||[attribute isEqualToString:NSAccessibilitySelectedTextRangeAttribute]
			||[attribute isEqualToString:NSAccessibilityVisibleCharacterRangeAttribute]
				)
		value = nil;
	else if([attribute isEqualToString:NSAccessibilityDescriptionAttribute])
		value = @"Key Equivalent";
	else
		value = [super accessibilityAttributeValue:attribute];
	return value;
}
@end
