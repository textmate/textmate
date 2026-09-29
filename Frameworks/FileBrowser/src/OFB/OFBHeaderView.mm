#import "OFBHeaderView.h"
#import <OakAppKit/src/OakAppKit.h>
#import <OakAppKit/src/OakUIConstructionFunctions.h>

static NSButton* OakCreateImageButton (NSString* imageName)
{
	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	[res setButtonType:NSButtonTypeMomentaryChange];
	[res setBordered:NO];
	[res setImage:[NSImage imageNamed:imageName]];
	[res setImagePosition:NSImageOnly];
	return res;
}

static NSPopUpButton* OakCreateFolderPopUpButton ()
{
	NSPopUpButton* res = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:YES];
	[res setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentHuggingPriority:NSLayoutPriorityFittingSizeCompression forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationVertical];
	[res setBordered:NO];
	return res;
}

@interface OFBHeaderView ()
@property (nonatomic) NSView* bottomDivider;
@end

@implementation OFBHeaderView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.folderPopUpButton       = OakCreateFolderPopUpButton();
		self.goBackButton            = OakCreateImageButton(NSImageNameGoLeftTemplate);
		self.goBackButton.toolTip    = @"Go Back";
		self.goForwardButton         = OakCreateImageButton(NSImageNameGoRightTemplate);
		self.goForwardButton.toolTip = @"Go Forward";

		self.folderPopUpButton.accessibilityLabel           = @"Current folder";
		self.goBackButton.image.accessibilityDescription    = self.goBackButton.toolTip;
		self.goForwardButton.image.accessibilityDescription = self.goForwardButton.toolTip;

		_bottomDivider = OakCreateNSBoxSeparator();

		NSDictionary* views = @{
			@"folder":        self.folderPopUpButton,
			@"divider":       OakCreateNSBoxSeparator(),
			@"back":          self.goBackButton,
			@"forward":       self.goForwardButton,
			@"bottomDivider": _bottomDivider,
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], self);
		OakSetupKeyViewLoop(@[ self, _folderPopUpButton, _goBackButton, _goForwardButton ]);
		
		CGFloat topPadding = 4;
		CGFloat bottomPadding = 4;
		if(@available(macOS 27, *)) {
			topPadding = 6;
			bottomPadding = 6;
		}
		else if(@available(macOS 26, *)) {}
		else {
			topPadding = 6;
			bottomPadding = 5;
		}
		NSDictionary* metrics = @{ @"topPadding": @(topPadding), @"bottomPadding": @(bottomPadding) };
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(3)-[folder(>=75)]-(3)-[divider(==1)]-(2)-[back(==22)]-(2)-[forward(==back)]-(3)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[bottomDivider]|"                                                                     options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(topPadding)-[divider(==15)]-(bottomPadding)-[bottomDivider(==1)]|" options:0 metrics:metrics views:views]];
	}
	return self;
}
@end
