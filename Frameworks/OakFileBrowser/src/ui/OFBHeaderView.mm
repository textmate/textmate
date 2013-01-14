#import "OFBHeaderView.h"
#import <OakAppKit/OakAppKit.h>

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

@interface OFBHeaderView ()
@property (nonatomic) BOOL renderEnabled;
@end

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
			@"shadow"   : OakCreateViewWithColor([NSColor grayColor]),
			@"divider"  : OakCreateViewWithColor([NSColor lightGrayColor]),
			@"back"     : self.goBackButton,
			@"forward"  : self.goForwardButton,
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(1)-[folder(>=75)]-(3)-[shadow(==1)][divider(==1)]-(9)-[back(==11)]-(9)-[forward(==back)]-(9)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(2)-[folder]"           options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[shadow]|"               options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[divider]|"              options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(0)-[back(==22)]-(2)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(0)-[forward(==back)]"  options:0 metrics:nil views:views]];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (BOOL)isOpaque
{
	return YES;
}

- (void)viewWillMoveToWindow:(NSWindow*)newWindow
{
	if(self.window)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidBecomeMainNotification object:self.window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidResignMainNotification object:self.window];
	}

	if(newWindow)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMain:) name:NSWindowDidBecomeMainNotification object:newWindow];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMain:) name:NSWindowDidResignMainNotification object:newWindow];
	}
}

- (void)viewDidMoveToWindow
{
	self.renderEnabled = [self.window isMainWindow];
}

- (void)windowDidChangeMain:(NSNotification*)aNotification
{
	self.renderEnabled = [self.window isMainWindow];
}

- (void)setRenderEnabled:(BOOL)flag
{
	if(_renderEnabled != flag)
	{
		_renderEnabled = flag;
		[self setNeedsDisplay:YES];
	}
}

- (void)drawRect:(NSRect)aRect
{
	NSColor* topColor    = self.renderEnabled ? [NSColor lightGrayColor] : [NSColor lightGrayColor];
	NSColor* bottomColor = self.renderEnabled ? [NSColor grayColor]      : [NSColor lightGrayColor];
	[[[NSGradient alloc] initWithStartingColor:bottomColor endingColor:topColor] drawInRect:self.bounds angle:90];
}
@end
