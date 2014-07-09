#import "OakKeyEquivalentView.h"
#import "OakUIConstructionFunctions.h"
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
{
	NSMutableArray* _observers;
	NSRect _clearButtonRect;
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

- (void)dealloc
{
	for(NSDictionary* info in _observers)
		[info[kBindingInfoControllerKey] removeObserver:self forKeyPath:info[kBindingInfoKeyPathKey]];
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

	for(NSDictionary* info in _observers)
	{
		if([info[kBindingInfoBindingKey] isEqualToString:NSValueBinding])
		{
			id controller = info[kBindingInfoControllerKey];
			NSString* keyPath = info[kBindingInfoKeyPathKey];
			NSString* oldValue = [controller valueForKeyPath:keyPath];
			if(!oldValue || ![oldValue isEqualToString:_eventString])
				[controller setValue:_eventString forKeyPath:keyPath];
		}
	}
}

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
		NSImage* imgNormal = [NSImage imageNamed:@"CloseFile" inSameBundleAsClass:cl];

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

	if(self.disableGlobalHotkeys)
	{
		if(self.recording)
				_hotkeyToken = PushSymbolicHotKeyMode(kHIHotKeyModeAllDisabled);
		else	PopSymbolicHotKeyMode(_hotkeyToken);
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

	BOOL doesHaveResponder = (newState & (OakViewViewIsFirstResponderMask)) == (OakViewViewIsFirstResponderMask);
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

- (void)flagsChanged:(NSEvent*)anEvent
{
	if(self.recording)
	{
		std::string const str = ns::glyphs_for_flags([anEvent modifierFlags]);
		self.displayString = str == "" ? kRecordingPlaceholderString : [NSString stringWithCxxString:str];
	}
}

- (BOOL)performKeyEquivalent:(NSEvent*)anEvent
{
	if(!self.recording)
		return NO;

	self.eventString = [NSString stringWithCxxString:to_s(anEvent)];
	self.recording = NO;
	return YES;
}

- (void)keyDown:(NSEvent*)anEvent
{
	if(self.recording)
	{
		[self performKeyEquivalent:anEvent];
	}
	else
	{
		static std::set<std::string> const ClearKeys     = { utf8::to_s(NSDeleteCharacter), utf8::to_s(NSDeleteFunctionKey) };
		static std::set<std::string> const RecordingKeys = { " ", "\n", "\r" };
		std::string const keyString = to_s(anEvent);
		if(ClearKeys.find(keyString) != ClearKeys.end())
			[self clearKeyEquivalent:self];
		else if(RecordingKeys.find(keyString) != RecordingKeys.end())
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

	NSDictionary* stringAttributes = @{
		NSForegroundColorAttributeName : self.recording ? [NSColor grayColor] : [NSColor blackColor],
		NSFontAttributeName            : OakControlFont()
	};

	NSSize size = [self.displayString sizeWithAttributes:stringAttributes];
	[self.displayString drawAtPoint:NSMakePoint(NSMidX([self visibleRect]) - size.width / 2, NSMidY([self visibleRect]) - size.height /2 ) withAttributes:stringAttributes];

	if(self.showClearButton)
	{
		Class cl = NSClassFromString(@"OFBPathInfoCell");
		NSImage* imgNormal = [NSImage imageNamed:@"CloseFile"        inSameBundleAsClass:cl];
		NSImage* imgHover  = [NSImage imageNamed:@"CloseFileOver"    inSameBundleAsClass:cl];
		NSImage* imgDown   = [NSImage imageNamed:@"CloseFilePressed" inSameBundleAsClass:cl];
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

// ============
// = Bindings =
// ============

- (void)bind:(NSString*)aBinding toObject:(id)observableController withKeyPath:(NSString*)aKeyPath options:(NSDictionary*)someOptions
{
	_observers = _observers ?: [NSMutableArray new];
	[_observers addObject:@{
		kBindingInfoBindingKey    : aBinding,
		kBindingInfoControllerKey : observableController,
		kBindingInfoKeyPathKey    : aKeyPath,
	}];
	[observableController addObserver:self forKeyPath:aKeyPath options:NSKeyValueObservingOptionInitial context:NULL];
}

- (void)unbind:(NSString*)aBinding
{
	for(NSUInteger i = [_observers count]; i > 0; --i)
	{
		NSDictionary* info = _observers[i-1];
		if([aBinding isEqualToString:info[kBindingInfoBindingKey]])
		{
			[info[kBindingInfoControllerKey] removeObserver:self forKeyPath:info[kBindingInfoKeyPathKey]];
			[_observers removeObjectAtIndex:i-i];
		}
	}
}

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)observableController change:(NSDictionary*)changeDictionary context:(void*)userData
{
	for(NSDictionary* info in _observers)
	{
		if(observableController == info[kBindingInfoControllerKey] && [aKeyPath isEqualToString:info[kBindingInfoKeyPathKey]])
		{
			if([info[kBindingInfoBindingKey] isEqualToString:NSValueBinding])
				self.eventString = [observableController valueForKeyPath:aKeyPath];
		}
	}
}
@end
