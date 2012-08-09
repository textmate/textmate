#import "HOStatusBar.h"

@interface HOStatusBar ()
- (void)update;
@end

@implementation HOStatusBar
@synthesize isBusy, canGoBack, canGoForward, statusText, delegate;

- (id)initWithFrame:(NSRect)frame
{
	if(self = [super initWithFrame:frame])
	{
		statusText = @"";

		progressIndicator = [NSProgressIndicator new];
		[progressIndicator setMaxValue:1.0];
		[progressIndicator setControlSize:NSSmallControlSize];
		[progressIndicator setIndeterminate:NO];
		[progressIndicator setDisplayedWhenStopped:NO];
		[progressIndicator setFrame:NSMakeRect(0, 5, 96, 12)];
		[progressIndicator setBezeled:NO];

		spinner = [NSProgressIndicator new];
		[spinner setControlSize:NSSmallControlSize];
		[spinner setStyle:NSProgressIndicatorSpinningStyle];
		[spinner setDisplayedWhenStopped:NO];
	}
	return self;
}

- (void)dealloc
{
	[progressIndicator release];
	[spinner release];
	[statusText release];
	[super dealloc];
}

- (void)update
{
	std::vector<sb::cell_t> newCells;
	newCells.push_back(sb::cell_t::button(sb::cell_t::template_image(NSImageNameGoLeftTemplate), @selector(goBack:), delegate).enabled(self.canGoBack).tool_tip("Go Back"));
	newCells.push_back(sb::cell_t::button(sb::cell_t::template_image(NSImageNameGoRightTemplate), @selector(goForward:), delegate).enabled(self.canGoForward).tool_tip("Go Forward"));
	newCells.push_back(sb::cell_t::info(statusText).size(20, CGFLOAT_MAX).no_separator());
	if(progressIndicator.doubleValue > 0)
			newCells.push_back(sb::cell_t::info(progressIndicator).size(50, 150));
	else	newCells.push_back(sb::cell_t::info(spinner).size(16));
	newCells.push_back(sb::cell_t::info().size(30));

	[self setCells:newCells];
}

- (void)setStatusText:(NSString*)text
{
	if(text != statusText)
	{
		[statusText release];
		statusText = [text retain] ?: @"";
		[self update];
	}
}

- (void)setIsBusy:(BOOL)flag
{
	isBusy = flag;
	if(isBusy)
			[spinner startAnimation:nil];
	else	[spinner stopAnimation:nil];
	[self update];
}

- (double)progress
{
	return progressIndicator.doubleValue;
}

- (void)setProgress:(double)value
{
	progressIndicator.doubleValue = value;
	[self update];
}

- (void)setCanGoBack:(BOOL)flag
{
	canGoBack = flag;
	[self update];
}

- (void)setCanGoForward:(BOOL)flag
{
	canGoForward = flag;
	[self update];
}
@end
