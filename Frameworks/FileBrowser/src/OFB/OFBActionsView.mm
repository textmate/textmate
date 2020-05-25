#import "OFBActionsView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSImage Additions.h>

static NSButton* OakCreateImageButton (NSImage* image)
{
	NSButton* res = [NSButton new];
	[res setButtonType:NSButtonTypeMomentaryChange];
	[res setBordered:NO];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];
	return res;
}

@implementation OFBActionsView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.wantsLayer   = YES;
		self.material     = NSVisualEffectMaterialTitlebar;
		self.blendingMode = NSVisualEffectBlendingModeWithinWindow;
		self.state        = NSVisualEffectStateFollowsWindowActiveState;

		self.createButton       = OakCreateImageButton([NSImage imageNamed:NSImageNameAddTemplate]);
		self.actionsPopUpButton = OakCreateActionPopUpButton();
		self.reloadButton       = OakCreateImageButton([NSImage imageNamed:NSImageNameRefreshTemplate]);
		self.searchButton       = OakCreateImageButton([NSImage imageNamed:@"SearchTemplate" inSameBundleAsClass:[self class]]);
		self.favoritesButton    = OakCreateImageButton([NSImage imageNamed:@"FavoritesTemplate" inSameBundleAsClass:[self class]]);
		self.scmButton          = OakCreateImageButton([NSImage imageNamed:@"SCMTemplate" inSameBundleAsClass:[self class]]);

		self.createButton.toolTip       = @"Create new file";
		self.reloadButton.toolTip       = @"Reload file browser";
		self.searchButton.toolTip       = @"Search current folder";
		self.favoritesButton.toolTip    = @"Show favorites";
		self.scmButton.toolTip          = @"Show source control management status";

		self.reloadButton.image.accessibilityDescription    = self.reloadButton.toolTip;
		self.createButton.image.accessibilityDescription    = self.createButton.toolTip;
		self.searchButton.image.accessibilityDescription    = self.searchButton.toolTip;
		self.favoritesButton.image.accessibilityDescription = self.favoritesButton.toolTip;
		self.scmButton.image.accessibilityDescription       = self.scmButton.toolTip;

		NSView* wrappedActionsPopUpButton = [NSView new];
		OakAddAutoLayoutViewsToSuperview(@[ self.actionsPopUpButton ], wrappedActionsPopUpButton);
		[wrappedActionsPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[popup]|" options:0 metrics:nil views:@{ @"popup": self.actionsPopUpButton }]];
		[wrappedActionsPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[popup]|" options:0 metrics:nil views:@{ @"popup": self.actionsPopUpButton }]];

		NSView* topDivider = OakCreateNSBoxSeparator();

		NSDictionary* views = @{
			@"topDivider": topDivider,
			@"create":     self.createButton,
			@"divider":    OakCreateNSBoxSeparator(),
			@"actions":    wrappedActionsPopUpButton,
			@"reload":     self.reloadButton,
			@"search":     self.searchButton,
			@"favorites":  self.favoritesButton,
			@"scm":        self.scmButton,
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], self);
		OakSetupKeyViewLoop(@[ self, _createButton, _actionsPopUpButton, _reloadButton, _searchButton, _favoritesButton, _scmButton ]);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-8-[create]-8-[divider(==1)]-8-[actions(==31)]-(>=8)-[reload]-4-[search]-4-[favorites]-4-[scm]-(12)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[topDivider]|"                                                                                         options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[topDivider(==1)]-4-[divider(==15)]-5-|"                                                                     options:0 metrics:nil views:views]];
	}
	return self;
}
@end
