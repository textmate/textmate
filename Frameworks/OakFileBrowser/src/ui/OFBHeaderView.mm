#import "OFBHeaderView.h"
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakFileIconImage.h>

@interface OFBHeaderView ()
@property (nonatomic, retain) NSMutableArray* buttonConstraints;
@end

static NSButton* ImageButton (NSString* imageName)
{
	NSButton* res = [NSButton new];

	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBezelStyle:NSSmallSquareBezelStyle];
	[res setBordered:NO];
	[res setTranslatesAutoresizingMaskIntoConstraints:NO];

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
	[res setTranslatesAutoresizingMaskIntoConstraints:NO];

	NSMenu* menu = [NSMenu new];
	[menu setAutoenablesItems:NO];

	NSString* path = NSHomeDirectory();

	NSMenuItem* menuItem = [[NSMenuItem alloc] initWithTitle:[[NSFileManager defaultManager] displayNameAtPath:path] action:@selector(nop:) keyEquivalent:@""];
	[menuItem setIconForFile:path];
	[menu addItem:menuItem];

	[res setMenu:menu];

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

		for(NSView* view in @[ self.folderPopUpButton, self.goBackButton, self.goForwardButton ])
			[self addSubview:view];
	}
	return self;
}

- (void)updateConstraints
{
	if(self.buttonConstraints)
		[self removeConstraints:self.buttonConstraints];
	[super updateConstraints];

	NSDictionary* views = @{
		@"folder"   : self.folderPopUpButton,
		@"back"     : self.goBackButton,
		@"forward"  : self.goForwardButton,
	};

	self.buttonConstraints = [NSMutableArray new];
	[self.buttonConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(5)-[folder]-[back(==21)][forward(==back)]-(5)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[self.buttonConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[folder(==26)]"                                     options:0 metrics:nil views:views]];
	[self.buttonConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(1)-[back(==forward,==25)]-(1)-|"                 options:0 metrics:nil views:views]];

	[self addConstraints:self.buttonConstraints];
}

- (BOOL)isOpaque
{
	return YES;
}

- (void)drawRect:(NSRect)aRect
{
	NSRect bounds = self.bounds;

	NSGradient* gradient = [[NSGradient alloc] initWithStartingColor:[NSColor darkGrayColor] endingColor:[NSColor lightGrayColor]];
	[gradient drawInRect:bounds angle:90];

	[[NSColor blackColor] set];
	NSRectFill(NSMakeRect(NSMinX(bounds), 0, NSWidth(bounds), 1));
}
@end
