#import "OakScopeBarView.h"
#import "OakUIConstructionFunctions.h"

static NSButton* OakCreateScopeButton (NSString* label, NSUInteger tag, SEL action, id target, NSControlSize controlSize)
{
	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	res.accessibilityRole               = NSAccessibilityRadioButtonRole;
	res.bezelStyle                      = NSBezelStyleRecessed;
	res.buttonType                      = NSButtonTypePushOnPushOff;
	res.controlSize                     = controlSize;
	res.font                            = [NSFont messageFontOfSize:[NSFont systemFontSizeForControlSize:controlSize]];
	res.title                           = label;
	res.tag                             = tag;
	res.action                          = action;
	res.target                          = target;
	res.showsBorderOnlyWhileMouseInside = YES;

	return res;
}

@interface OakScopeBarViewController ()
@property (nonatomic) NSArray<NSButton*>* buttons;
@property (nonatomic) NSMutableArray<NSLayoutConstraint*>* viewConstraints;
@end

@implementation OakScopeBarViewController
- (void)loadView
{
	NSView* view = [[NSView alloc] initWithFrame:NSZeroRect];
	view.accessibilityRole = NSAccessibilityRadioGroupRole;
	self.view = view;
}

- (void)viewWillAppear
{
	if(_buttons.count != _labels.count)
		[self updateButtons];
}

- (void)updateButtons
{
	for(NSView* button in _buttons)
		[button removeFromSuperview];

	NSMutableArray<NSButton*>* buttons = [NSMutableArray array];
	for(NSUInteger i = 0; i < _labels.count; ++i)
	{
		NSButton* button = OakCreateScopeButton(_labels[i], i, @selector(takeSelectedIndexFrom:), self, _controlSize);
		button.state = i == _selectedIndex ? NSControlStateValueOn : NSControlStateValueOff;
		[buttons addObject:button];
	}
	_buttons = buttons;

	OakAddAutoLayoutViewsToSuperview(_buttons, self.view);
	OakSetupKeyViewLoop([@[ self.view ] arrayByAddingObjectsFromArray:_buttons], NO);
	[self.view setNeedsUpdateConstraints:YES];
}

- (void)setControlSize:(NSControlSize)newControlSize
{
	if(_controlSize == newControlSize)
		return;
	_controlSize = newControlSize;

	if(_buttons.count)
	{
		for(NSButton* button in _buttons)
			button.controlSize = _controlSize;
		[self.view setNeedsUpdateConstraints:YES];
	}
}

- (void)setLabels:(NSArray*)anArray
{
	if(_labels == anArray || [_labels isEqualToArray:anArray])
		return;
	_labels = anArray;
	[self updateButtons];
}

- (void)updateViewConstraints
{
	if(_viewConstraints)
		[self.view removeConstraints:_viewConstraints];
	_viewConstraints = [NSMutableArray array];

	[super updateViewConstraints];

	if(_buttons.count)
	{
		[_viewConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[view]|" options:0 metrics:nil views:@{ @"view": _buttons[0] }]];
		[_viewConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[view]" options:0 metrics:nil views:@{ @"view": _buttons[0] }]];
		[_viewConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[view]|" options:0 metrics:nil views:@{ @"view": _buttons[_buttons.count-1] }]];
		for(NSUInteger i = 0; i < _buttons.count-1; ++i)
		{
			[_viewConstraints addObject:[NSLayoutConstraint constraintWithItem:_buttons[i] attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:_buttons[i+1] attribute:NSLayoutAttributeLeft multiplier:1 constant:0]];
			[_viewConstraints addObject:[NSLayoutConstraint constraintWithItem:_buttons[i] attribute:NSLayoutAttributeBaseline relatedBy:NSLayoutRelationEqual toItem:_buttons[i+1] attribute:NSLayoutAttributeBaseline multiplier:1 constant:0]];
		}
	}

	[self.view addConstraints:_viewConstraints];
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(self.view.window.isKeyWindow)
	{
		for(int i = 0; i < _labels.count; ++i)
		{
			NSMenuItem* item = [aMenu addItemWithTitle:_labels[i] action:@selector(takeSelectedIndexFrom:) keyEquivalent:i < 9 ? [NSString stringWithFormat:@"%c", '1' + i] : @""];
			item.tag = i;
			item.target = self;
		}
	}
	else
	{
		[aMenu addItemWithTitle:@"No Sources" action:@selector(nop:) keyEquivalent:@""];
	}
}

- (void)selectNextButton:(id)sender
{
	self.selectedIndex = (self.selectedIndex + 1) % _buttons.count;
}

- (void)selectPreviousButton:(id)sender
{
	self.selectedIndex = (self.selectedIndex + _buttons.count - 1) % _buttons.count;
}

- (void)takeSelectedIndexFrom:(id)sender
{
	if([sender respondsToSelector:@selector(tag)])
	{
		NSUInteger newSelectedIndex = [sender tag];
		if([sender isKindOfClass:[NSButton class]])
		{
			NSButton* button = sender;
			if(button.state == NSControlStateValueOff && _allowsEmptySelection)
				newSelectedIndex = NSNotFound;
		}
		self.selectedIndex = newSelectedIndex;
	}
}

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	if(item.action == @selector(takeSelectedIndexFrom:))
		item.state = [item tag] == self.selectedIndex ? NSControlStateValueOn : NSControlStateValueOff;
	return YES;
}

- (void)setSelectedIndex:(NSUInteger)newSelectedIndex
{
	BOOL notifyObservers = _selectedIndex != newSelectedIndex;

	_selectedIndex = newSelectedIndex;
	for(NSButton* button in _buttons)
		button.state = button.tag == _selectedIndex ? NSControlStateValueOn : NSControlStateValueOff;

	if(notifyObservers)
	{
		if(NSDictionary* info = [self infoForBinding:NSValueBinding])
		{
			id controller     = info[NSObservedObjectKey];
			NSString* keyPath = info[NSObservedKeyPathKey];
			if(controller && controller != [NSNull null] && keyPath && (id)keyPath != [NSNull null])
			{
				id newValue = [NSNumber numberWithUnsignedInteger:_selectedIndex];
				id oldValue = [controller valueForKeyPath:keyPath];
				if(!oldValue || ![oldValue isEqual:newValue])
					[controller setValue:newValue forKeyPath:keyPath];
			}
		}
	}
}

- (id)value                   { return @(self.selectedIndex); }
- (void)setValue:(id)newValue { self.selectedIndex = [newValue intValue]; }
@end
