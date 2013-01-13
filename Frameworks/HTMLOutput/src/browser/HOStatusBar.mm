#import "HOStatusBar.h"

@interface HOStatusBar ()
@property (nonatomic, retain) NSProgressIndicator* spinner;
@property (nonatomic, retain) NSProgressIndicator* progressIndicator;
- (void)update;
@end

@implementation HOStatusBar
- (id)initWithFrame:(NSRect)frame
{
	if(self = [super initWithFrame:frame])
	{
		self.statusText = @"";

		_progressIndicator = [NSProgressIndicator new];
		[_progressIndicator setMaxValue:1];
		[_progressIndicator setControlSize:NSSmallControlSize];
		[_progressIndicator setIndeterminate:NO];
		[_progressIndicator setDisplayedWhenStopped:NO];
		[_progressIndicator setFrame:NSMakeRect(0, 5, 96, 12)];
		[_progressIndicator setBezeled:NO];

		_spinner = [NSProgressIndicator new];
		[_spinner setControlSize:NSSmallControlSize];
		[_spinner setStyle:NSProgressIndicatorSpinningStyle];
		[_spinner setDisplayedWhenStopped:NO];
	}
	return self;
}

- (void)dealloc
{
	self.statusText        = nil;
	self.spinner           = nil;
	self.progressIndicator = nil;
	[super dealloc];
}

- (void)update
{
	std::vector<sb::cell_t> newCells;
	newCells.push_back(sb::cell_t::button(sb::cell_t::template_image(NSImageNameGoLeftTemplate), @selector(goBack:), _delegate).enabled(self.canGoBack).tool_tip("Go Back"));
	newCells.push_back(sb::cell_t::button(sb::cell_t::template_image(NSImageNameGoRightTemplate), @selector(goForward:), _delegate).enabled(self.canGoForward).tool_tip("Go Forward"));
	newCells.push_back(sb::cell_t::info(_statusText).size(20, CGFLOAT_MAX).no_separator());
	if(_progressIndicator.doubleValue > 0)
			newCells.push_back(sb::cell_t::info(_progressIndicator).size(50, 150));
	else	newCells.push_back(sb::cell_t::info(_spinner).size(16));
	newCells.push_back(sb::cell_t::info().size(30));

	[self setCells:newCells];
}

- (void)setStatusText:(NSString*)text
{
	if(_statusText != text && ![_statusText isEqualToString:text])
	{
		[_statusText release];
		_statusText = [text retain] ?: @"";
		[self update];
	}
}

- (void)setIsBusy:(BOOL)flag
{
	_isBusy = flag;
	if(_isBusy)
			[_spinner startAnimation:nil];
	else	[_spinner stopAnimation:nil];
	[self update];
}

- (CGFloat)progress
{
	return _progressIndicator.doubleValue;
}

- (void)setProgress:(CGFloat)value
{
	_progressIndicator.doubleValue = value;
	[self update];
}

- (void)setCanGoBack:(BOOL)flag
{
	_canGoBack = flag;
	[self update];
}

- (void)setCanGoForward:(BOOL)flag
{
	_canGoForward = flag;
	[self update];
}
@end
