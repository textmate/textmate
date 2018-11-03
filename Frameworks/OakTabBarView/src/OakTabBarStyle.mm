#import "OakTabBarStyle.h"
#import <OakAppKit/OakRolloverButton.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSImage Additions.h>

static NSString* kUserDefaultsTabItemMinWidthKey       = @"tabItemMinWidth";
static NSString* kUserDefaultsTabItemMaxWidthKey       = @"tabItemMaxWidth";
static NSString* kUserDefaultsTabItemLineBreakStyleKey = @"tabItemLineBreakStyle";

@interface OakTabBarStyle ()
{
	NSDictionary* _images;
	NSMutableDictionary* _activeTabTextStyles;
	NSMutableDictionary* _selectedTabTextStyles;
	NSMutableDictionary* _inactiveTabTextStyles;
}
@end

@implementation OakTabBarStyle
+ (instancetype)sharedInstance
{
	static OakTabBarStyle* sharedInstance = [self new];
	return sharedInstance;
}

+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsTabItemMinWidthKey:       @(120),
		kUserDefaultsTabItemMaxWidthKey:       @(250),
		kUserDefaultsTabItemLineBreakStyleKey: @(NSLineBreakByTruncatingMiddle),
	}];
}

- (NSDictionary*)imagesForNames:(NSDictionary*)imageNames
{
	NSMutableDictionary* res = [NSMutableDictionary dictionary];
	for(NSString* state in imageNames)
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionary];
		for(NSString* key in imageNames[state])
			dict[key] = [NSImage imageNamed:imageNames[state][key] inSameBundleAsClass:[self class]];
		res[state] = dict;
	}
	return res;
}

- (NSDictionary*)yosemiteImages
{
	NSDictionary* imageNames = @{
		@"tabBar": @{
			@"AW_normal":   @"TabAWBackground",
			@"IW_normal":   @"TabIWBackground",
		},
		@"tabItemSelected": @{
			@"AW_normal":   @"TabAWBackgroundSelected",
			@"IW_normal":   @"TabIWBackgroundSelected",
		},
		@"leftTabCap": @{
			@"AW_normal":   @"TabAWDivider",
			@"IW_normal":   @"TabIWDivider",
		},
		@"leftTabCapSelected": @{
			@"AW_normal":   @"TabAWDivider",
			@"IW_normal":   @"TabIWDivider",
		},
		@"rightTabCap": @{
			@"AW_normal":   @"TabAWDivider",
			@"IW_normal":   @"TabIWDivider",
		},
		@"rightTabCapSelected": @{
			@"AW_normal":   @"TabAWDivider",
			@"IW_normal":   @"TabIWDivider",
		},
		@"closeButton": @{
			@"AW_normal":   @"TabCloseThinTemplate",
			@"AW_pressed":  @"TabCloseThin_Pressed_Template",
			@"AW_rollover": @"TabCloseThin_Rollover_Template",
		},
		@"closeButtonModified": @{
			@"AW_normal":   @"TabCloseThin_Modified_Template",
			@"AW_pressed":  @"TabCloseThin_ModifiedPressed_Template",
			@"AW_rollover": @"TabCloseThin_ModifiedRollover_Template",
		},
		@"overflowButton": @{
			@"AW_normal":   @"TabOverflowThinTemplate",
		},
	};
	return [self imagesForNames:imageNames];
}

- (NSDictionary*)mojaveLightImages
{
	return [self yosemiteImages];
}

- (NSDictionary*)mojaveDarkImages
{
	NSDictionary* imageNames = @{
		@"tabBar": @{
			@"AW_normal":   @"TabAWBackground_Dark",
			@"IW_normal":   @"TabIWBackground_Dark",
		},
		@"tabItemSelected": @{
			@"AW_normal":   @"TabAWBackgroundSelected_Dark",
			@"IW_normal":   @"TabIWBackgroundSelected_Dark",
		},
		@"leftTabCap": @{
			@"AW_normal":   @"TabAWDivider_Dark",
			@"IW_normal":   @"TabIWDivider_Dark",
		},
		@"leftTabCapSelected": @{
			@"AW_normal":   @"TabAWDivider_Dark",
			@"IW_normal":   @"TabIWDivider_Dark",
		},
		@"rightTabCap": @{
			@"AW_normal":   @"TabAWDivider_Dark",
			@"IW_normal":   @"TabIWDivider_Dark",
		},
		@"rightTabCapSelected": @{
			@"AW_normal":   @"TabAWDivider_Dark",
			@"IW_normal":   @"TabIWDivider_Dark",
		},
		@"closeButton": @{
			@"AW_normal":   @"TabCloseThinTemplate",
			@"AW_pressed":  @"TabCloseThin_Pressed_Template",
			@"AW_rollover": @"TabCloseThin_Rollover_Template",
		},
		@"closeButtonModified": @{
			@"AW_normal":   @"TabCloseThin_Modified_Template",
			@"AW_pressed":  @"TabCloseThin_ModifiedPressed_Template",
			@"AW_rollover": @"TabCloseThin_ModifiedRollover_Template",
		},
		@"overflowButton": @{
			@"AW_normal":   @"TabOverflowThinTemplate",
		},
	};
	return [self imagesForNames:imageNames];
}

- (id)init
{
	if(self = [super init])
	{
		NSMutableParagraphStyle* parStyle = [NSMutableParagraphStyle new];
		[parStyle setLineBreakMode:(NSLineBreakMode)[[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsTabItemLineBreakStyleKey]];

		_activeTabTextStyles = @{
			NSParagraphStyleAttributeName:  parStyle,
			NSFontAttributeName:            [NSFont systemFontOfSize:11],
			NSForegroundColorAttributeName: [NSColor colorWithCalibratedWhite:0.2 alpha:1],
		}.mutableCopy;

		_inactiveTabTextStyles = _activeTabTextStyles.mutableCopy;
		_inactiveTabTextStyles[NSForegroundColorAttributeName] = [NSColor colorWithCalibratedWhite:0.5 alpha:1];

		// MAC_OS_X_VERSION_10_14
		if(@available(macos 10.14, *))
		{
			_activeTabTextStyles = @{
				NSParagraphStyleAttributeName:  parStyle,
				NSFontAttributeName:            [NSFont systemFontOfSize:11],
				NSForegroundColorAttributeName: [NSColor secondaryLabelColor],
			}.mutableCopy;

			_inactiveTabTextStyles = _activeTabTextStyles.mutableCopy;
			_inactiveTabTextStyles[NSForegroundColorAttributeName] = [NSColor tertiaryLabelColor];
			_selectedTabTextStyles = _activeTabTextStyles.mutableCopy;
			_selectedTabTextStyles[NSForegroundColorAttributeName] = [NSColor labelColor];

			NSAppearanceName appearanceName = [[NSApplication sharedApplication].effectiveAppearance bestMatchFromAppearancesWithNames:@[ NSAppearanceNameAqua, NSAppearanceNameDarkAqua ]];
			if([appearanceName isEqualToString:NSAppearanceNameDarkAqua])
					_images = [self mojaveDarkImages];
			else	_images = [self mojaveLightImages];

			_leftPadding  = -1;
			_rightPadding = 0;

			[[NSApplication sharedApplication] addObserver:self forKeyPath:@"effectiveAppearance" options:NSKeyValueObservingOptionNew context:nil];
		}
		else
		{
			_selectedTabTextStyles = _activeTabTextStyles.mutableCopy;
			_selectedTabTextStyles[NSForegroundColorAttributeName] = [NSColor controlTextColor];

			_images = [self yosemiteImages];
			_leftPadding  = -1;
			_rightPadding = 0;
		}

		NSImage* rightCapImage = _images[@"rightTabCap"][@"AW_normal"];
		_tabViewSpacing = rightCapImage ? -rightCapImage.size.width : 0;
		_minimumTabSize = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsTabItemMinWidthKey];
		_maximumTabSize = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsTabItemMaxWidthKey];
	}
	return self;
}

- (void)updateButton:(OakRolloverButton*)aButton forState:(NSString*)aState
{
	NSDictionary* images = _images[aState];
	if([images[@"AW_normal"] isTemplate])
		[[aButton cell] setBackgroundStyle:NSBackgroundStyleRaised];
	aButton.regularImage  = images[@"AW_normal"];
	aButton.pressedImage  = images[@"AW_pressed"];
	aButton.rolloverImage = images[@"AW_rollover"];
	aButton.inactiveRegularImage  = images[@"IW_normal"];
	aButton.inactivePressedImage  = images[@"IW_pressed"];
	aButton.inactiveRolloverImage = images[@"IW_rollover"];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	// MAC_OS_X_VERSION_10_14
	if(@available(macos 10.14, *))
	{
		if([keyPath isEqualToString:@"effectiveAppearance"])
		{
			NSAppearanceName appearanceName = [[NSApplication sharedApplication].effectiveAppearance bestMatchFromAppearancesWithNames:@[ NSAppearanceNameAqua, NSAppearanceNameDarkAqua ]];
			if([appearanceName isEqualToString:NSAppearanceNameDarkAqua])
					_images = [self mojaveDarkImages];
			else	_images = [self mojaveLightImages];
		}
	}
}

- (void)updateView:(OakBackgroundFillView*)aView forState:(NSString*)aState
{
	aView.activeBackgroundImage   = _images[aState][@"AW_normal"];
	aView.inactiveBackgroundImage = _images[aState][@"IW_normal"];
}

// ================
// = Tab Bar View =
// ================

- (void)setupTabBarView:(OakBackgroundFillView*)aView
{
	[self updateView:aView forState:@"tabBar"];
	[aView setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationVertical];
}

// =================
// = Tab Item View =
// =================

- (void)updateLeftCapView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected
{
	[self updateView:aView forState:(selected ? @"leftTabCapSelected" : @"leftTabCap")];
}

- (void)updateRightCapView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected
{
	[self updateView:aView forState:(selected ? @"rightTabCapSelected" : @"rightTabCap")];
}

- (void)updateTabItemView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected
{
	[self updateView:aView forState:(selected ? @"tabItemSelected" : nil)];
}

- (void)updateCloseButton:(OakRolloverButton*)aButton inSelectedTab:(BOOL)selected modified:(BOOL)modified
{
	[self updateButton:aButton forState:(modified ? @"closeButtonModified" : @"closeButton")];
}

- (void)updateOverflowButton:(OakRolloverButton*)aButton inSelectedTab:(BOOL)selected
{
	[self updateButton:aButton forState:@"overflowButton"];
}
@end
