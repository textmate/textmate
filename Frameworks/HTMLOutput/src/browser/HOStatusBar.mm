#import "HOStatusBar.h"
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>

static NSButton* OakCreateImageButton (NSImage* image)
{
	NSButton* res = [NSButton new];

	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBezelStyle:NSRecessedBezelStyle];
	[res setBordered:NO];

	image = [image copy];
	[image setTemplate:YES];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];

	return res;
}

static NSTextField* OakCreateTextField ()
{
	NSTextField* res = [[NSTextField alloc] initWithFrame:NSZeroRect];
	[res setBordered:NO];
	[res setEditable:NO];
	[res setSelectable:NO];
	[res setBezeled:NO];
	[res setDrawsBackground:NO];
	[res setFont:OakStatusBarFont()];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	return res;
}

@interface HOStatusBar ()
@property (nonatomic) NSImageView*         divider;
@property (nonatomic) NSButton*            goBackButton;
@property (nonatomic) NSButton*            goForwardButton;
@property (nonatomic) NSTextField*         statusTextField;
@property (nonatomic) NSProgressIndicator* progressIndicator;
@property (nonatomic) NSProgressIndicator* spinner;
@property (nonatomic) NSMutableArray*      layoutConstraints;
@property (nonatomic) BOOL                 indeterminateProgress;
@end

@implementation HOStatusBar
- (id)initWithFrame:(NSRect)frame
{
	if(self = [super initWithGradient:[[NSGradient alloc] initWithColorsAndLocations: [NSColor colorWithCalibratedWhite:1.000 alpha:0.68], 0.0, [NSColor colorWithCalibratedWhite:1.000 alpha:0.5], 0.0416, [NSColor colorWithCalibratedWhite:1.000 alpha:0.0], 1.0, nil] inactiveGradient:[[NSGradient alloc] initWithColorsAndLocations: [NSColor colorWithCalibratedWhite:1.000 alpha:0.68], 0.0, [NSColor colorWithCalibratedWhite:1.000 alpha:0.5], 0.0416, [NSColor colorWithCalibratedWhite:1.000 alpha:0.0], 1.0, nil]])
	{
		_indeterminateProgress = YES;

		_divider                  = OakCreateDividerImageView();

		_goBackButton             = OakCreateImageButton([NSImage imageNamed:NSImageNameGoLeftTemplate]);
		_goBackButton.toolTip     = @"Show the previous page";
		_goBackButton.enabled     = NO;
		_goBackButton.target      = self;
		_goBackButton.action      = @selector(goBack:);

		_goForwardButton          = OakCreateImageButton([NSImage imageNamed:NSImageNameGoRightTemplate]);
		_goForwardButton.toolTip  = @"Show the next page";
		_goForwardButton.enabled  = NO;
		_goForwardButton.target   = self;
		_goForwardButton.action   = @selector(goForward:);

		_statusTextField          = OakCreateTextField();
		[_statusTextField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_statusTextField.cell setLineBreakMode:NSLineBreakByTruncatingMiddle];

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

		NSArray* views = @[ _divider, _goBackButton, _goForwardButton, _statusTextField, _spinner ];
		for(NSView* view in views)
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[_progressIndicator setTranslatesAutoresizingMaskIntoConstraints:NO];
	}
	return self;
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, 24);
}

- (void)updateConstraints
{
	if(_layoutConstraints)
		[self removeConstraints:_layoutConstraints];
	_layoutConstraints = [NSMutableArray array];

	[super updateConstraints];

	NSDictionary* views = @{
		@"back"     : _goBackButton,
		@"forward"  : _goForwardButton,
		@"divider"  : _divider,
		@"status"   : _statusTextField,
		@"spinner"  : _indeterminateProgress ? _spinner : _progressIndicator,
	};

	NSArray* layout = @[
		@"H:|-(3)-[back(==22)]-(2)-[forward(==back)]-(2)-[divider]",
		@"V:|[back(==forward,==divider)]|",
		@"V:[status]-5-|",
	];

	for(NSString* str in layout)
		[_layoutConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:str options:0 metrics:nil views:views]];

	if(!_indeterminateProgress)
	{
		[_layoutConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[divider]-[status(>=100)]-[spinner(>=50,<=150)]-|" options:0 metrics:nil views:views]];
		[_layoutConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[spinner]-6-|" options:0 metrics:nil views:views]];
	}
	else
	{		
		[_layoutConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[divider]-[status(>=100)]-[spinner]-|" options:0 metrics:nil views:views]];
		[_layoutConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[spinner]-5-|" options:0 metrics:nil views:views]];
	}

	[self addConstraints:_layoutConstraints];
}

- (void)drawRect:(NSRect)aRect
{
	if([self.window contentBorderThicknessForEdge:NSMinYEdge] < NSMaxY(self.frame))
	{
		[[NSColor windowBackgroundColor] set];
		NSRectFill(aRect);
		[super drawRect:aRect];
	}
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
