#import "LiveSearchView.h"
#import <OakAppKit/src/OakUIConstructionFunctions.h>

@interface LiveSearchView ()
@property (nonatomic) NSView* divider;
@end

@implementation LiveSearchView
+ (void)initialize
{
	[NSUserDefaults.standardUserDefaults registerDefaults:@{
		@"incrementalSearchIgnoreCase": @YES,
		@"incrementalSearchWrapAround": @NO,
	}];
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.style   = OakBackgroundFillViewStyleHeader;
		self.divider = OakCreateNSBoxSeparator();

		self.textField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		self.textField.focusRingType = NSFocusRingTypeNone;

		self.ignoreCaseCheckBox = OakCreateCheckBox(@"Ignore Case");
		self.wrapAroundCheckBox = OakCreateCheckBox(@"Wrap Around");

		NSDictionary* views = @{
			@"divider":    self.divider,
			@"textField":  self.textField,
			@"ignoreCase": self.ignoreCaseCheckBox,
			@"wrapAround": self.wrapAroundCheckBox,
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[divider]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[textField]-[ignoreCase]-[wrapAround]-(8)-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[divider(==1)]-(8)-[textField]-(8)-|" options:0 metrics:nil views:views]];

		[self.ignoreCaseCheckBox bind:NSValueBinding toObject:NSUserDefaultsController.sharedUserDefaultsController withKeyPath:@"values.incrementalSearchIgnoreCase" options:nil];
		[self.wrapAroundCheckBox bind:NSValueBinding toObject:NSUserDefaultsController.sharedUserDefaultsController withKeyPath:@"values.incrementalSearchWrapAround" options:nil];
	}
	return self;
}
@end
