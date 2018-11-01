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
	OakRolloverButton* _clearButton;
	id _eventMonitor;
	void* _hotkeyToken;
}
@property (nonatomic) NSString* displayString;
@property (nonatomic) BOOL showClearButton;
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

- (CGFloat)baselineOffsetFromBottom
{
	return 5;
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
			if(!oldValue || ![oldValue isEqual:_eventString])
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
		if(!_clearButton)
		{
			_clearButton = OakCreateCloseButton(@"Remove key equivalent");
			_clearButton.refusesFirstResponder = YES;
			_clearButton.disableWindowOrderingForFirstMouse = YES;
			_clearButton.target = self;
			_clearButton.action = @selector(clearKeyEquivalent:);

			NSDictionary* views = @{ @"clear": _clearButton };
			OakAddAutoLayoutViewsToSuperview([views allValues], self);
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(>=4)-[clear]-(4)-|" options:0 metrics:nil views:views]];
			[self addConstraint:[NSLayoutConstraint constraintWithItem:_clearButton attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];
		}
		_clearButton.hidden = NO;
		[_clearButton updateTrackingAreas];
	}
	else
	{
		_clearButton.hidden = YES;
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

		_eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskFlagsChanged|NSEventMaskKeyDown handler:^NSEvent*(NSEvent* event){
			if([event type] == NSEventTypeFlagsChanged)
			{
				std::string const str = ns::glyphs_for_flags([event modifierFlags]);
				self.displayString = str == "" ? kRecordingPlaceholderString : [NSString stringWithCxxString:str];
			}
			else if([event type] == NSEventTypeKeyDown)
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

- (BOOL)isOpaque
{
	return YES;
}

- (BOOL)acceptsFirstMouse:(NSEvent*)anEvent
{
	return YES;
}

- (BOOL)acceptsFirstResponder
{
	return YES;
}

- (void)mouseDown:(NSEvent*)anEvent
{
	if(self.window.isKeyWindow)
	{
		if(self != self.window.firstResponder)
			[self.window makeFirstResponder:self];
		self.recording = YES;
	}
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

	[[NSColor lightGrayColor] set];
	NSFrameRect(frame);

	if(@available(macos 10.14, *))
		[[NSColor controlColor] set];
	else	[[NSColor whiteColor] set];
	NSRectFill(NSIntersectionRect(aRect, NSInsetRect(frame, 1, 1)));

	NSDictionary* stringAttributes = @{
		NSForegroundColorAttributeName: self.recording ? [NSColor secondaryLabelColor] : [NSColor labelColor],
		NSFontAttributeName:            OakControlFont()
	};

	NSSize size = [self.displayString sizeWithAttributes:stringAttributes];
	[self.displayString drawAtPoint:NSMakePoint(NSMidX([self visibleRect]) - size.width / 2, NSMidY([self visibleRect]) - size.height / 2) withAttributes:stringAttributes];
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
