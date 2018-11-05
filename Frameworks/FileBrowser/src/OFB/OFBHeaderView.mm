#import "OFBHeaderView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakTabBarView/OakTabBarStyle.h>
#import <Preferences/Keys.h>

static NSButton* OakCreateImageButton (NSString* imageName)
{
	NSButton* res = [NSButton new];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBordered:NO];
	[res setImage:[NSImage imageNamed:imageName]];
	[res setImagePosition:NSImageOnly];
	return res;
}

static NSPopUpButton* OakCreateFolderPopUpButton ()
{
	NSPopUpButton* res = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:YES];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentHuggingPriority:NSLayoutPriorityFittingSizeCompression forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationVertical];
	[res setBordered:NO];
	return res;
}

@interface OFBHeaderView ()
@property (nonatomic) BOOL    inTabBar;
@property (nonatomic) NSView* bottomDivider;
@end

@implementation OFBHeaderView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.style = OakBackgroundFillViewStyleHeader;

		self.folderPopUpButton       = OakCreateFolderPopUpButton();
		self.goBackButton            = OakCreateImageButton(NSImageNameGoLeftTemplate);
		self.goBackButton.toolTip    = @"Go Back";
		self.goForwardButton         = OakCreateImageButton(NSImageNameGoRightTemplate);
		self.goForwardButton.toolTip = @"Go Forward";

		self.folderPopUpButton.accessibilityLabel           = @"Current folder";
		self.goBackButton.image.accessibilityDescription    = self.goBackButton.toolTip;
		self.goForwardButton.image.accessibilityDescription = self.goForwardButton.toolTip;

		_bottomDivider = OakCreateHorizontalLine(OakBackgroundFillViewStyleDivider);

		NSDictionary* views = @{
			@"folder":        self.folderPopUpButton,
			@"divider":       OakCreateDividerImageView(),
			@"back":          self.goBackButton,
			@"forward":       self.goForwardButton,
			@"bottomDivider": _bottomDivider,
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], self);
		OakSetupKeyViewLoop(@[ self, _folderPopUpButton, _goBackButton, _goForwardButton ], NO);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(-3)-[folder(>=75)]-(3)-[divider]-(2)-[back(==22)]-(2)-[forward(==back)]-(3)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[bottomDivider]|"                                                                options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[folder(==divider,==back,==forward)][bottomDivider]|"                            options:0 metrics:nil views:views]];

		[self userDefaultsDidChange:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.inTabBar = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsTabsAboveDocumentKey];
}

- (void)setInTabBar:(BOOL)flag
{
	if(_inTabBar != flag)
	{
		_inTabBar = flag;
		_bottomDivider.hidden = flag;
	}

	if(flag)
	{
		self.style = OakBackgroundFillViewStyleNone;
		[[OakTabBarStyle sharedInstance] setupTabBarView:self];
	}
	else
	{
		self.style = OakBackgroundFillViewStyleHeader;
	}
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, 24);
}

- (void)layout
{
	if(_inTabBar)
		[[OakTabBarStyle sharedInstance] setupTabBarView:self];
	[super layout];
}
@end
