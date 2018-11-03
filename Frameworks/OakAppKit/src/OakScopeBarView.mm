#import "OakScopeBarView.h"
#import "OakUIConstructionFunctions.h"

static NSButton* OakCreateScopeButton (NSString* label, NSUInteger tag, SEL action, id target)
{
	NSButton* res = [NSButton new];
	res.accessibilityRole               = NSAccessibilityRadioButtonRole;
	res.bezelStyle                      = NSRecessedBezelStyle;
	res.buttonType                      = NSPushOnPushOffButton;
	res.title                           = label;
	res.tag                             = tag;
	res.action                          = action;
	res.target                          = target;
	res.showsBorderOnlyWhileMouseInside = YES;

	return res;
}

@interface OakScopeBarView () <NSAccessibilityGroup>
@property (nonatomic) NSArray* buttons;
@property (nonatomic) NSMutableArray* myConstraints;
@end

@implementation OakScopeBarView
- (instancetype)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.accessibilityRole = NSAccessibilityRadioGroupRole;
	}
	return self;
}

- (void)setLabels:(NSArray*)anArray
{
	if(_labels == anArray || [_labels isEqualToArray:anArray])
		return;
	_labels = anArray;

	_selectedIndex = NSNotFound;
	for(NSView* button in _buttons)
		[button removeFromSuperview];

	NSMutableArray* buttons = [NSMutableArray new];
	for(NSInteger i = 0; i < anArray.count; ++i)
		[buttons addObject:OakCreateScopeButton(anArray[i], i, @selector(takeSelectedIndexFrom:), self)];
	_buttons = buttons;

	OakAddAutoLayoutViewsToSuperview(_buttons, self);
	OakSetupKeyViewLoop([@[ self ] arrayByAddingObjectsFromArray:_buttons], NO);
	if(_buttons.count)
		self.selectedIndex = 0;

	[self setNeedsUpdateConstraints:YES];
}

- (void)updateConstraints
{
	if(_myConstraints)
		[self removeConstraints:_myConstraints];
	_myConstraints = [NSMutableArray array];
	[super updateConstraints];

	if(_buttons.count)
	{
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[view]|" options:0 metrics:nil views:@{ @"view": _buttons[0] }]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[view]" options:0 metrics:nil views:@{ @"view": _buttons[0] }]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[view]|" options:0 metrics:nil views:@{ @"view": _buttons[_buttons.count-1] }]];
		for(size_t i = 0; i < [_buttons count]-1; ++i)
		{
			[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:_buttons[i] attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:_buttons[i+1] attribute:NSLayoutAttributeLeft multiplier:1 constant:0]];
			[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:_buttons[i] attribute:NSLayoutAttributeBaseline relatedBy:NSLayoutRelationEqual toItem:_buttons[i+1] attribute:NSLayoutAttributeBaseline multiplier:1 constant:0]];
		}
	}

	[self addConstraints:_myConstraints];
}

- (void)takeSelectedIndexFrom:(id)sender
{
	if([sender respondsToSelector:@selector(tag)])
		self.selectedIndex = [sender tag];
}

- (void)setSelectedIndex:(NSInteger)newSelectedIndex
{
	for(NSButton* button in _buttons)
		[button setState:[button tag] == newSelectedIndex ? NSOnState : NSOffState];
	if(_selectedIndex == newSelectedIndex)
		return;

	_selectedIndex = newSelectedIndex;

	if(NSDictionary* info = [self infoForBinding:NSValueBinding])
	{
		id controller     = info[NSObservedObjectKey];
		NSString* keyPath = info[NSObservedKeyPathKey];
		if(controller && controller != [NSNull null] && keyPath && (id)keyPath != [NSNull null])
		{
			id newValue = @(_selectedIndex);
			id oldValue = [controller valueForKeyPath:keyPath];
			if(!oldValue || ![oldValue isEqual:newValue])
				[controller setValue:newValue forKeyPath:keyPath];
		}
	}
}

- (id)value                   { return @(self.selectedIndex); }
- (void)setValue:(id)newValue { self.selectedIndex = [newValue intValue]; }
@end
