#import "OakScopeBarView.h"

static NSButton* OakCreateScopeButton (NSString* label, NSUInteger tag, SEL action, id target)
{
	NSButton* res = [NSButton new];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[[res cell] setControlSize:NSSmallControlSize];
	NSString* accessibilityRole = NSAccessibilityRadioButtonRole;
	[[res cell] accessibilitySetOverrideValue:accessibilityRole forAttribute:NSAccessibilityRoleAttribute];
	[[res cell] accessibilitySetOverrideValue:NSAccessibilityRoleDescription(accessibilityRole, nil) forAttribute:NSAccessibilityRoleDescriptionAttribute];
	res.bezelStyle                      = NSRecessedBezelStyle;
	res.buttonType                      = NSPushOnPushOffButton;
	res.title                           = label;
	res.tag                             = tag;
	res.action                          = action;
	res.target                          = target;
	res.showsBorderOnlyWhileMouseInside = YES;

	return res;
}

@interface OakScopeBarView ()
@property (nonatomic) NSArray* buttons;
@property (nonatomic) NSMutableArray* myConstraints;
@end

@implementation OakScopeBarView
- (void)setLabels:(NSArray*)anArray
{
	_selectedIndex = NSNotFound;
	for(NSView* button in _buttons)
		[button removeFromSuperview];

	NSMutableArray* buttons = [NSMutableArray new];
	_buttons = buttons;

	for(NSInteger i = 0; i < anArray.count; ++i)
	{
		NSView* button = OakCreateScopeButton(anArray[i], i, @selector(takeSelectedIndexFrom:), self);
		[buttons addObject:button];
		[button setTranslatesAutoresizingMaskIntoConstraints:NO];
		[self addSubview:button];
	}

	if(_buttons.count)
		self.selectedIndex = 0;

	[self setNeedsLayout:YES];
}

- (void)updateConstraints
{
	if(_myConstraints)
		[self removeConstraints:_myConstraints];
	_myConstraints = [NSMutableArray array];
	[super updateConstraints];

	if(_buttons.count)
	{
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[view]|" options:0 metrics:nil views:@{ @"view" : _buttons[0] }]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[view]" options:0 metrics:nil views:@{ @"view" : _buttons[0] }]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[view]|" options:0 metrics:nil views:@{ @"view" : _buttons[_buttons.count-1] }]];
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
	if(_selectedIndex == newSelectedIndex)
		return;

	_selectedIndex = newSelectedIndex;
	for(NSButton* button in _buttons)
		[button setState:[button tag] == _selectedIndex ? NSOnState : NSOffState];

	if(NSDictionary* info = [self infoForBinding:NSValueBinding])
	{
		id controller     = info[NSObservedObjectKey];
		NSString* keyPath = info[NSObservedKeyPathKey];
		if(controller && controller != [NSNull null] && keyPath && (id)keyPath != [NSNull null])
		{
			id newValue = @(_selectedIndex);
			id oldValue = [controller valueForKeyPath:keyPath];
			if(!oldValue || ![oldValue isEqualTo:newValue])
				[controller setValue:newValue forKeyPath:keyPath];
		}
	}
}

- (id)value                   { return @(self.selectedIndex); }
- (void)setValue:(id)newValue { self.selectedIndex = [newValue intValue]; }

// =================
// = Accessibility =
// =================

- (BOOL)accessibilityIsIgnored
{
	return NO;
}

- (NSSet*)myAccessibilityAttributeNames
{
	static NSSet* set = [NSSet setWithArray:@[
		NSAccessibilityRoleAttribute,
	]];
	return set;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = [[[self myAccessibilityAttributeNames] setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
	return attributes;
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	if([[self myAccessibilityAttributeNames] containsObject:attribute])
		return NO;
	return [super accessibilityIsAttributeSettable:attribute];
}

- (id)accessibilityAttributeValue:(NSString *)attribute
{
	if([attribute isEqualToString:NSAccessibilityRoleAttribute])
		return NSAccessibilityRadioGroupRole;
	else
		return [super accessibilityAttributeValue:attribute];
}
@end
