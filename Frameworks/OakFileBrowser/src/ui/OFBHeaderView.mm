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
	[image setSize:NSMakeSize(9, 9)];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];

	return res;
}

static NSPopUpButton* PopUpButton ()
{
	NSPopUpButton* res = [NSPopUpButton new];
	[[res cell] setBackgroundStyle:NSBackgroundStyleLight];
	[res setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setBordered:NO];
	return res;
}

@interface OFBHeaderView ()
@property (nonatomic) BOOL renderEnabled;
@property (nonatomic) NSBox* lightDividerView;
@property (nonatomic) NSBox* darkDividerView;
@property (nonatomic) NSBox* bottomDividerView;
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

		self.darkDividerView   = OakCreateViewWithColor([NSColor colorWithCalibratedWhite:0.551 alpha:1.000]);
		self.lightDividerView  = OakCreateViewWithColor([NSColor colorWithCalibratedWhite:0.869 alpha:1.000]);
		self.bottomDividerView = OakCreateViewWithColor([NSColor colorWithCalibratedWhite:0.750 alpha:1.000]);

		NSDictionary* views = @{
			@"folder"   : self.folderPopUpButton,
			@"shadow"   : self.darkDividerView,
			@"divider"  : self.lightDividerView,
			@"back"     : self.goBackButton,
			@"forward"  : self.goForwardButton,
			@"bottom"   : self.bottomDividerView,
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(-3)-[folder(>=75)]-(3)-[shadow(==1)][divider(==1)]-(2)-[back(==22)]-(2)-[forward(==back)]-(3)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[bottom]|"                  options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(-1)-[folder(==26)]-(0)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[shadow][bottom(==1)]|"     options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[divider(==shadow)]"        options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(-1)-[back(==25)]"         options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(-1)-[forward(==back)]"    options:0 metrics:nil views:views]];
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

		if(flag)
		{
			self.darkDividerView.borderColor   = [NSColor colorWithCalibratedWhite:0.551 alpha:1.000];
			self.lightDividerView.borderColor  = [NSColor colorWithCalibratedWhite:0.869 alpha:1.000];
			self.bottomDividerView.borderColor = [NSColor colorWithCalibratedWhite:0.500 alpha:1.000];
		}
		else
		{
			self.darkDividerView.borderColor   = [NSColor colorWithCalibratedWhite:0.801 alpha:1.000];
			self.lightDividerView.borderColor  = [NSColor colorWithCalibratedWhite:0.869 alpha:0.000];
			self.bottomDividerView.borderColor = [NSColor colorWithCalibratedWhite:0.750 alpha:1.000];
		}

		[self setNeedsDisplay:YES];
	}
}

- (void)drawRect:(NSRect)aRect
{
	NSColor* topColor    = self.renderEnabled ? [NSColor colorWithCalibratedWhite:0.915 alpha:1.000] : [NSColor colorWithCalibratedWhite:0.915 alpha:1.000];
	NSColor* bottomColor = self.renderEnabled ? [NSColor colorWithCalibratedWhite:0.760 alpha:1.000] : [NSColor colorWithCalibratedWhite:0.915 alpha:1.000];
	[[[NSGradient alloc] initWithStartingColor:bottomColor endingColor:topColor] drawInRect:self.bounds angle:90];
}
@end
