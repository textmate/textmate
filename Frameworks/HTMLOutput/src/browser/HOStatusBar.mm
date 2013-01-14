#import "HOStatusBar.h"
#import <OakAppKit/NSImage Additions.h>

static NSButton* OakCreateImageButton (NSString* imageName)
{
	NSButton* res = [[NSButton new] autorelease];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBezelStyle:NSSmallSquareBezelStyle];
	[res setBordered:NO];

	NSImage* image = [[[NSImage imageNamed:imageName] copy] autorelease];
	[image setSize:NSMakeSize(13, 13)];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];

	return res;
}

static NSTextField* OakCreateTextField ()
{
	NSTextField* res = [[[NSTextField alloc] initWithFrame:NSZeroRect] autorelease];
	[res setBordered:NO];
	[res setEditable:NO];
	[res setSelectable:NO];
	[res setBezeled:NO];
	[res setDrawsBackground:NO];
	[res setFont:[NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]]];
	return res;
}

static NSImageView* OakCreateImageView (NSImage* image)
{
	NSImageView* res = [[[NSImageView alloc] initWithFrame:NSZeroRect] autorelease];
	[res setImage:image];
	return res;
}

@interface HOStatusBar ()
@property (nonatomic, retain) NSImage*             backgroundImage;
@property (nonatomic, retain) NSImageView*         firstSeparatorImageView;
@property (nonatomic, retain) NSImageView*         secondSeparatorImageView;
@property (nonatomic, retain) NSButton*            goBackButton;
@property (nonatomic, retain) NSButton*            goForwardButton;
@property (nonatomic, retain) NSTextField*         statusTextField;
@property (nonatomic, retain) NSProgressIndicator* progressIndicator;
@property (nonatomic, retain) NSProgressIndicator* spinner;
@property (nonatomic, retain) NSMutableArray*      layoutConstraints;
@property (nonatomic, assign) BOOL                 indeterminateProgress;
@end

@implementation HOStatusBar
- (id)initWithFrame:(NSRect)frame
{
	if(self = [super initWithFrame:frame])
	{
		_indeterminateProgress = YES;

		self.backgroundImage          = [NSImage imageNamed:@"Statusbar Background" inSameBundleAsClass:NSClassFromString(@"OakStatusBar")];
		self.firstSeparatorImageView  = OakCreateImageView([NSImage imageNamed:@"Statusbar Separator" inSameBundleAsClass:NSClassFromString(@"OakStatusBar")]);
		self.secondSeparatorImageView = OakCreateImageView([NSImage imageNamed:@"Statusbar Separator" inSameBundleAsClass:NSClassFromString(@"OakStatusBar")]);

		self.goBackButton             = OakCreateImageButton(NSImageNameGoLeftTemplate);
		self.goBackButton.toolTip     = @"Show the previous page";
		self.goBackButton.enabled     = NO;
		self.goBackButton.target      = self;
		self.goBackButton.action      = @selector(goBack:);

		self.goForwardButton          = OakCreateImageButton(NSImageNameGoRightTemplate);
		self.goForwardButton.toolTip  = @"Show the next page";
		self.goForwardButton.enabled  = NO;
		self.goForwardButton.target   = self;
		self.goForwardButton.action   = @selector(goForward:);

		self.statusTextField          = OakCreateTextField();
		[self.statusTextField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self.statusTextField.cell setLineBreakMode:NSLineBreakByTruncatingMiddle];

		_progressIndicator = [NSProgressIndicator new];
		_progressIndicator.controlSize          = NSSmallControlSize;
		_progressIndicator.maxValue             = 1;
		_progressIndicator.indeterminate        = NO;
		_progressIndicator.displayedWhenStopped = NO;
		_progressIndicator.bezeled              = NO;

		_spinner = [NSProgressIndicator new];
		_spinner.controlSize          = NSSmallControlSize;
		_spinner.style                = NSProgressIndicatorSpinningStyle;
		_spinner.displayedWhenStopped = NO;

		NSArray* views = @[ _firstSeparatorImageView, _secondSeparatorImageView, _goBackButton, _goForwardButton, _statusTextField, _spinner ];
		for(NSView* view in views)
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[_progressIndicator setTranslatesAutoresizingMaskIntoConstraints:NO];
	}
	return self;
}

- (void)dealloc
{
	self.backgroundImage          = nil;
	self.firstSeparatorImageView  = nil;
	self.secondSeparatorImageView = nil;
	self.goBackButton             = nil;
	self.goForwardButton          = nil;
	self.statusTextField          = nil;
	self.progressIndicator        = nil;
	self.spinner                  = nil;
	self.layoutConstraints        = nil;

	[super dealloc];
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, self.backgroundImage.size.height);
}

- (void)updateConstraints
{
	if(self.layoutConstraints)
		[self removeConstraints:self.layoutConstraints];
	self.layoutConstraints = [NSMutableArray array];

	[super updateConstraints];

	NSDictionary* views = @{
		@"back"     : _goBackButton,
		@"divider1" : _firstSeparatorImageView,
		@"forward"  : _goForwardButton,
		@"divider2" : _secondSeparatorImageView,
		@"status"   : _statusTextField,
		@"spinner"  : _indeterminateProgress ? _spinner : _progressIndicator,
	};

	NSArray* layout = @[
		@"H:|-(4)-[back(==9)]-(3)-[divider1(==1)]-(3)-[forward(==back)]-(4)-[divider2(==1)]",
		@"V:|[back(==forward)]",
		@"V:|[forward]",
		@"V:|[divider1(==15,==divider2)]",
	];

	for(NSString* str in layout)
		[_layoutConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:str options:0 metrics:nil views:views]];
	[_layoutConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[divider2]-[status(>=100)]-[spinner]-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];

	if(!_indeterminateProgress)
		[_layoutConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[spinner(>=50,<=150)]" options:0 metrics:nil views:views]];

	[self addConstraints:_layoutConstraints];
}

- (void)drawRect:(NSRect)aRect
{
	[_backgroundImage drawInRect:self.bounds fromRect:NSZeroRect operation:NSCompositeCopy fraction:1];
}

- (void)setIndeterminateProgress:(BOOL)newIndeterminateProgress
{
	if(_indeterminateProgress == newIndeterminateProgress)
		return;

	if(_indeterminateProgress = newIndeterminateProgress)
	{
		[_progressIndicator removeFromSuperview];
		[self addSubview:_spinner];
		if(_isBusy)
			[_spinner startAnimation:nil];
	}
	else
	{
		[self addSubview:_progressIndicator];
		if(_isBusy)
			[_spinner stopAnimation:nil];
		[_spinner removeFromSuperview];
	}
	[self setNeedsUpdateConstraints:YES];
}

- (void)setIsBusy:(BOOL)flag
{
	_isBusy = flag;
	if(_indeterminateProgress)
	{
		if(_isBusy)
				[_spinner startAnimation:nil];
		else	[_spinner stopAnimation:nil];
	}
}

- (void)goBack:(id)sender             { [NSApp sendAction:@selector(goBack:)    to:_delegate from:self]; }
- (void)goForward:(id)sender          { [NSApp sendAction:@selector(goForward:) to:_delegate from:self]; }

- (BOOL)canGoBack                     { return _goBackButton.isEnabled; }
- (BOOL)canGoForward                  { return _goForwardButton.isEnabled; }
- (NSString*)statusText               { return _statusTextField.stringValue; }
- (CGFloat)progress                   { return _progressIndicator.doubleValue; }

- (void)setCanGoBack:(BOOL)flag       { _goBackButton.enabled = flag; }
- (void)setCanGoForward:(BOOL)flag    { _goForwardButton.enabled = flag; }
- (void)setStatusText:(NSString*)text { _statusTextField.stringValue = text; }

- (void)setProgress:(CGFloat)value
{
	_progressIndicator.doubleValue = value;
	self.indeterminateProgress = value == 0;
}
@end
