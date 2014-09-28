#import "OFBHeaderView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakTabItemView.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <Preferences/Keys.h>

static NSButton* OakCreateImageButton (NSString* imageName)
{
	NSButton* res = [NSButton new];

	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBezelStyle:NSRecessedBezelStyle];
	[res setBordered:NO];

	NSImage* image = [[NSImage imageNamed:imageName] copy];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];

	return res;
}

@interface OFBHeaderViewPopupButtonCell : NSPopUpButtonCell
@property (nonatomic, strong) NSDictionary* activeAttrs;
@property (nonatomic, strong) NSDictionary* inactiveAttrs;
@end

static NSPopUpButton* OakCreateFolderPopUpButton ()
{
	NSPopUpButton* res = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:YES];
	OFBHeaderViewPopupButtonCell* cell = [[OFBHeaderViewPopupButtonCell alloc] initTextCell:@"" pullsDown:YES];
	[res setCell:cell];
	[cell setBezelStyle:NSRecessedBezelStyle];
	[cell setArrowPosition:NSPopUpArrowAtBottom];
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

@implementation OFBHeaderViewPopupButtonCell
- (id)initTextCell:(NSString*)title pullsDown:(BOOL)pullsDown
{
	if (self = [super initTextCell:title pullsDown:pullsDown]) {
		NSShadow* shadow = [NSShadow new];
		[shadow setShadowColor:[NSColor colorWithCalibratedWhite:1 alpha:0.5]];
		[shadow setShadowOffset:NSMakeSize(0, -1)];
		[shadow setShadowBlurRadius:1];

		NSMutableParagraphStyle* parStyle = [NSMutableParagraphStyle new];
		[parStyle setLineBreakMode:NSLineBreakByTruncatingMiddle];

		NSFont* font = [NSFont boldSystemFontOfSize:[NSFont systemFontSizeForControlSize:NSRegularControlSize]-1];

		_activeAttrs = @{
			NSParagraphStyleAttributeName  : parStyle,
			NSFontAttributeName            : font,
			NSForegroundColorAttributeName : [NSColor colorWithCalibratedWhite:0.2 alpha:1],
			NSShadowAttributeName          : shadow,
		};
		_inactiveAttrs = @{
			NSParagraphStyleAttributeName  : parStyle,
			NSFontAttributeName            : font,
			NSForegroundColorAttributeName : [NSColor colorWithCalibratedWhite:0.5 alpha:1],
			NSShadowAttributeName          : shadow,
		};
	}
	return self;
}

- (NSRect)drawTitle:(NSAttributedString*)title withFrame:(NSRect)frame inView:(NSView*)controlView
{
	OFBHeaderView* headerView = (OFBHeaderView*)controlView.superview;
	if(headerView.inTabBar)
	{
		NSDictionary* attrs = headerView.active ? _activeAttrs : _inactiveAttrs;
		frame.origin.y    += 1;
		frame.size.height -= 1;
		return [super drawTitle:[[NSAttributedString alloc] initWithString:title.string attributes:attrs] withFrame:frame inView:controlView];
	}
	return [super drawTitle:title withFrame:frame inView:controlView];
}
@end

@implementation OFBHeaderView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		[self setupHeaderBackground];

		self.folderPopUpButton       = OakCreateFolderPopUpButton();
		self.goBackButton            = OakCreateImageButton(NSImageNameGoLeftTemplate);
		self.goBackButton.toolTip    = @"Go Back";
		self.goForwardButton         = OakCreateImageButton(NSImageNameGoRightTemplate);
		self.goForwardButton.toolTip = @"Go Forward";

		[self.folderPopUpButton.cell accessibilitySetOverrideValue:@"Folder" forAttribute:NSAccessibilityDescriptionAttribute];
		[self.goBackButton.cell accessibilitySetOverrideValue:self.goBackButton.toolTip forAttribute:NSAccessibilityDescriptionAttribute];
		[self.goForwardButton.cell accessibilitySetOverrideValue:self.goForwardButton.toolTip forAttribute:NSAccessibilityDescriptionAttribute];

		_bottomDivider = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]);

		NSDictionary* views = @{
			@"folder"        : self.folderPopUpButton,
			@"divider"       : OakCreateDividerImageView(),
			@"back"          : self.goBackButton,
			@"forward"       : self.goForwardButton,
			@"bottomDivider" : _bottomDivider,
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(-3)-[folder(>=75)]-(3)-[divider]-(2)-[back(==22)]-(2)-[forward(==back)]-(3)-|" options:0 metrics:nil views:views]];
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
		if(flag)
			[[OakTabBarStyle sharedInstance] setupTabBarView:self];
		else
			[self setupHeaderBackground];
		[self invalidateIntrinsicContentSize];
	}
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, 24);
}
@end
