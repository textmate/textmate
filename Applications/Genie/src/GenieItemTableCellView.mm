#import "GenieItemTableCellView.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <regexp/format_string.h>
#import <ns/ns.h>

@interface GenieRemoveNewlinesValueTransformer : NSValueTransformer
@end

@implementation GenieRemoveNewlinesValueTransformer
+ (Class)transformedValueClass         { return [NSString class]; }
+ (BOOL)allowsReverseTransformation    { return NO; }

- (NSString*)transformedValue:(NSString*)value
{
	if(value)
	{
		std::string str = to_s(value);
		str = format_string::replace(str, "(\\t)|(\\n)", "${1:+‣ }${2:+¬ }");
		value = to_ns(str);
	}
	return value;
}
@end

@interface GenieInactiveTableRowView ()
{
	BOOL _effectiveDrawAsHighlighted;
}
@end

static NSTextField* CreateTextField (NSString* label, NSFont* font)
{
	NSTextField* res = [NSTextField labelWithString:label];
	res.font      = font;
	res.textColor = NSColor.controlTextColor;
	[res.cell setLineBreakMode:NSLineBreakByTruncatingMiddle];
	return res;
}

@implementation GenieInactiveTableRowView
- (void)updateDrawAsHighlighted
{
	BOOL flag = self.selected && _drawAsHighlighted && self.window.isKeyWindow;
	if(_effectiveDrawAsHighlighted == flag)
		return;
	_effectiveDrawAsHighlighted = flag;

	for(NSView* view in [self subviews])
	{
		if([view respondsToSelector:@selector(setBackgroundStyle:)])
			[(NSTableCellView*)view setBackgroundStyle:self.interiorBackgroundStyle];
		if([view respondsToSelector:@selector(cell)])
			[[(NSControl*)view cell] setBackgroundStyle:self.interiorBackgroundStyle];
	}

	[self setNeedsDisplay:YES];
}

- (void)setDrawAsHighlighted:(BOOL)flag
{
	_drawAsHighlighted = flag;
	[self updateDrawAsHighlighted];
}

- (void)setSelected:(BOOL)flag
{
	[super setSelected:flag];
	[self updateDrawAsHighlighted];
}

- (void)setEmphasized:(BOOL)flag
{
	[super setEmphasized:flag];
	[self updateDrawAsHighlighted];
}

- (NSBackgroundStyle)interiorBackgroundStyle
{
	return _effectiveDrawAsHighlighted ? NSBackgroundStyleDark : [super interiorBackgroundStyle];
}

- (void)drawSelectionInRect:(NSRect)dirtyRect
{
	if(!_effectiveDrawAsHighlighted)
		return [super drawSelectionInRect:dirtyRect];

	[[NSColor alternateSelectedControlColor] set];
	NSRectFill(NSIntersectionRect(NSOffsetRect(NSInsetRect(self.bounds, 0, 0.5), 0, -0.5), dirtyRect));
}
@end

@interface GenieItemTableCellView ()
@property (nonatomic) NSTextField* subtitleTextField;
@property (nonatomic) BOOL optionDown;
@property (nonatomic) id eventMonitor;
@end

@implementation GenieItemTableCellView
- (instancetype)init
{
	if(self = [super init])
	{
		NSFont* controlContentFont = [NSFont controlContentFontOfSize:0];
		NSFontDescriptor* descriptor = [controlContentFont.fontDescriptor fontDescriptorByAddingAttributes:@{
			NSFontFeatureSettingsAttribute: @[ @{ NSFontFeatureTypeIdentifierKey : @(kNumberSpacingType), NSFontFeatureSelectorIdentifierKey : @(kMonospacedNumbersSelector) } ]
		}];

		NSImageView* imageView         = [NSImageView new];
		NSTextField* titleTextField    = CreateTextField(@"Title", [NSFont fontWithDescriptor:descriptor size:13]);
		NSTextField* subtitleTextField = CreateTextField(@"Subtitle", [NSFont fontWithDescriptor:descriptor size:10]);
		subtitleTextField.textColor = [NSColor secondaryLabelColor];

		NSDictionary* views = @{ @"icon": imageView, @"title": titleTextField, @"subtitle": subtitleTextField };
		OakAddAutoLayoutViewsToSuperview(views.allValues, self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(4)-[icon(==32)]-(4)-[title]-(8)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[title]-(2)-[subtitle]-(5)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:imageView attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];

		[imageView         bind:NSValueBinding toObject:self withKeyPath:@"objectValue.iconImage" options:nil];
		[titleTextField    bind:NSValueBinding toObject:self withKeyPath:@"objectValue.title"     options:@{ NSValueTransformerNameBindingOption: @"GenieRemoveNewlinesValueTransformer" }];
		[subtitleTextField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.subtitle"  options:@{ NSValueTransformerNameBindingOption: @"GenieRemoveNewlinesValueTransformer" }];

		self.imageView         = imageView;
		self.textField         = titleTextField;
		self.subtitleTextField = subtitleTextField;
	}
	return self;
}

- (void)viewDidMoveToSuperview
{
	if(!!self.superview == !!_eventMonitor)
		return;

	if(self.superview)
	{
		__weak id weakSelf = self;
		_eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskFlagsChanged handler:^NSEvent*(NSEvent* event){
			[weakSelf setOptionDown:event.modifierFlags & NSEventModifierFlagOption ? YES : NO];
			return event;
		}];
	}
	else
	{
		[NSEvent removeMonitor:_eventMonitor];
		_eventMonitor = nil;
	}
}

- (void)setOptionDown:(BOOL)flag
{
	if(_optionDown != flag)
	{
		_optionDown = flag;

		NSLog(@"[%@ setOptionDown:%s]", [self class], BSTR(flag));
#if 0
		NSString* keyPath = _optionDown ? @"objectValue.alternateTitle" : @"objectValue.title";
		[self.textField unbind:NSValueBinding];
		[self.textField bind:NSValueBinding toObject:self withKeyPath:keyPath options:@{ NSValueTransformerNameBindingOption: @"GenieRemoveNewlinesValueTransformer" }];
#endif
	}
}

- (void)setBackgroundStyle:(NSBackgroundStyle)backgroundStyle
{
	[super setBackgroundStyle:backgroundStyle];
	if(backgroundStyle == NSBackgroundStyleDark)
			_subtitleTextField.textColor = [NSColor colorWithCalibratedWhite:0.9 alpha:1];
	else	_subtitleTextField.textColor = [NSColor secondaryLabelColor];
}

- (void)dealloc
{
	for(NSView* view in @[ self.imageView, self.textField, _subtitleTextField ])
		[view unbind:NSValueBinding];
}
@end
