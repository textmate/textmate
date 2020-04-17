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
@property (nonatomic) NSStackView* stackView;
@end

@implementation OakScopeBarViewController
- (void)loadView
{
	_stackView = [[NSStackView alloc] initWithFrame:NSZeroRect];
	_stackView.accessibilityRole = NSAccessibilityRadioGroupRole;
	[self updateButtons];
	self.view = _stackView;
}

- (void)updateButtons
{
	if(!_stackView)
		return;

	for(NSView* button in _stackView.arrangedSubviews)
		[_stackView removeArrangedSubview:button];

	switch(_controlSize)
	{
		case NSControlSizeRegular: _stackView.spacing = 4; break;
		case NSControlSizeSmall:   _stackView.spacing = 2; break;
		case NSControlSizeMini:    _stackView.spacing = 2; break;
	}

	for(NSUInteger i = 0; i < _labels.count; ++i)
	{
		NSButton* button = OakCreateScopeButton(_labels[i], i, @selector(takeSelectedIndexFrom:), self, _controlSize);
		button.state = i == _selectedIndex ? NSControlStateValueOn : NSControlStateValueOff;
		[_stackView addArrangedSubview:button];
	}

	OakSetupKeyViewLoop([@[ _stackView ] arrayByAddingObjectsFromArray:_stackView.arrangedSubviews], NO);
}

- (void)setControlSize:(NSControlSize)newControlSize
{
	if(_controlSize == newControlSize)
		return;
	_controlSize = newControlSize;
	[self updateButtons];
}

- (void)setLabels:(NSArray*)anArray
{
	if(_labels == anArray || [_labels isEqualToArray:anArray])
		return;
	_labels = anArray;
	[self updateButtons];
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
	self.selectedIndex = (_selectedIndex + 1) % _labels.count;
}

- (void)selectPreviousButton:(id)sender
{
	self.selectedIndex = (_selectedIndex + _labels.count - 1) % _labels.count;
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
	for(NSButton* button in _stackView.arrangedSubviews)
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
