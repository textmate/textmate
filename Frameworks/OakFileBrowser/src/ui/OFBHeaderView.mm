#import "OFBHeaderView.h"

static NSButton* ImageButton (NSString* imageName)
{
	NSButton* res = [NSButton new];

	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBezelStyle:NSSmallSquareBezelStyle];
	[res setBordered:NO];

	NSImage* image = [[NSImage imageNamed:imageName] copy];
	[image setSize:NSMakeSize(13, 13)];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];

	return res;
}

static NSPopUpButton* PopUpButton ()
{
	NSPopUpButton* res = [NSPopUpButton new];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setBordered:NO];
	return res;
}

@implementation OFBHeaderView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.folderPopUpButton       = PopUpButton();
		self.goBackButton            = ImageButton(NSImageNameGoLeftTemplate);
		self.goBackButton.toolTip    = @"Go Back";
		self.goForwardButton         = ImageButton(NSImageNameGoRightTemplate);
		self.goForwardButton.toolTip = @"Go Forward";

		NSDictionary* views = @{
			@"folder"   : self.folderPopUpButton,
			@"back"     : self.goBackButton,
			@"forward"  : self.goForwardButton,
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(5)-[folder(>=75)]-[back(==21)][forward(==back)]-(5)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[folder(==26)]"                                     options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(1)-[back(==forward,==23)]-(1)-|"                 options:0 metrics:nil views:views]];
	}
	return self;
}

- (BOOL)isOpaque
{
	return YES;
}

- (void)drawRect:(NSRect)aRect
{
	[[[NSGradient alloc] initWithStartingColor:[NSColor grayColor] endingColor:[NSColor lightGrayColor]] drawInRect:self.bounds angle:90];
}
@end
