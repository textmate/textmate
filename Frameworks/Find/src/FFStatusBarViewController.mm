#import "FFStatusBarViewController.h"

static id OakFormatStatusString (NSString* aString)
{
	NSMutableParagraphStyle* paragraphStyle = [[NSMutableParagraphStyle alloc] init];
	[paragraphStyle setLineBreakMode:NSLineBreakByTruncatingMiddle];

	NSDictionary* const regularAttrs = @{
		NSForegroundColorAttributeName: NSColor.labelColor,
		NSParagraphStyleAttributeName:  paragraphStyle,
	};

	NSDictionary* const dimmedAttrs = @{
		NSForegroundColorAttributeName: NSColor.tertiaryLabelColor,
		NSParagraphStyleAttributeName:  paragraphStyle,
	};

	NSAttributedString* const lineJoiner = [[NSAttributedString alloc] initWithString:@"¬" attributes:dimmedAttrs];
	NSAttributedString* const tabJoiner  = [[NSAttributedString alloc] initWithString:@"‣" attributes:dimmedAttrs];

	NSMutableAttributedString* res = [[NSMutableAttributedString alloc] initWithString:@"" attributes:regularAttrs];

	__block bool firstLine = true;
	[aString enumerateLinesUsingBlock:^(NSString* line, BOOL* stop){
		if(!std::exchange(firstLine, false))
			[res appendAttributedString:lineJoiner];

		bool firstTab = true;
		for(NSString* str in [line componentsSeparatedByString:@"\t"])
		{
			if(!std::exchange(firstTab, false))
				[res appendAttributedString:tabJoiner];
			[res appendAttributedString:[[NSAttributedString alloc] initWithString:str attributes:regularAttrs]];
		}
	}];

	return res;
}

@interface FFStatusBarViewController ()
{
	NSButton*            _stopButton;
	NSProgressIndicator* _progressIndicator;
	NSButton*            _statusTextButton;
}
@end

@implementation FFStatusBarViewController
- (NSButton*)stopButton
{
	if(!_stopButton)
	{
		_stopButton = [[NSButton alloc] initWithFrame:NSZeroRect];

		_stopButton.accessibilityLabel        = @"Stop Search";
		_stopButton.bordered                  = NO;
		_stopButton.buttonType                = NSButtonTypeMomentaryChange;
		_stopButton.controlSize               = NSControlSizeSmall;
		_stopButton.image                     = [NSImage imageNamed:NSImageNameStopProgressFreestandingTemplate];
		_stopButton.imagePosition             = NSImageOnly;
		_stopButton.toolTip                   = @"Stop Search";

		_stopButton.keyEquivalent             = @".";
		_stopButton.keyEquivalentModifierMask = NSEventModifierFlagCommand;

		_stopButton.target                    = self;
		_stopButton.action                    = @selector(didClickStopButton:);

		[_stopButton.cell setImageScaling:NSImageScaleProportionallyDown];
		[_stopButton setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
	}
	return _stopButton;
}

- (NSProgressIndicator*)progressIndicator
{
	if(!_progressIndicator)
	{
		_progressIndicator = [[NSProgressIndicator alloc] initWithFrame:NSZeroRect];
		_progressIndicator.controlSize          = NSControlSizeSmall;
		_progressIndicator.displayedWhenStopped = NO;
		_progressIndicator.style                = NSProgressIndicatorStyleSpinning;
	}
	return _progressIndicator;
}

- (NSButton*)statusTextButton
{
	if(!_statusTextButton)
	{
		_statusTextButton = [[NSButton alloc] initWithFrame:NSZeroRect];

		_statusTextButton.alignment   = NSTextAlignmentLeft;
		_statusTextButton.bordered    = NO;
		_statusTextButton.buttonType  = NSButtonTypeToggle;
		_statusTextButton.controlSize = NSControlSizeSmall;
		_statusTextButton.font        = [NSFont messageFontOfSize:[NSFont systemFontSizeForControlSize:NSControlSizeSmall]];

		_statusTextButton.attributedTitle          = OakFormatStatusString(_statusText);
		_statusTextButton.attributedAlternateTitle = OakFormatStatusString(_alternateStatusText);

		[_statusTextButton setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_statusTextButton setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationVertical];

		[_statusTextButton.heightAnchor constraintEqualToConstant:16].active = YES;
	}
	return _statusTextButton;
}

- (void)loadView
{
	NSStackView* stackView = [NSStackView stackViewWithViews:@[
		self.stopButton,
		self.progressIndicator,
		self.statusTextButton,
	]];
	stackView.edgeInsets = { .left = 20, .right = 20 };

	[stackView setCustomSpacing:2 afterView:_progressIndicator];
	[stackView setHuggingPriority:NSLayoutPriorityDefaultHigh-1 forOrientation:NSLayoutConstraintOrientationVertical];

	_stopButton.hidden        = !_progressIndicatorVisible;
	_progressIndicator.hidden = !_progressIndicatorVisible;

	self.view = stackView;
}

- (void)didClickStopButton:(id)sender
{
	if(_stopAction)
		[NSApp sendAction:_stopAction to:_stopTarget from:sender];
}

- (void)setProgressIndicatorVisible:(BOOL)flag
{
	_progressIndicatorVisible = flag;
	if(_progressIndicatorVisible)
			[_progressIndicator startAnimation:self];
	else	[_progressIndicator stopAnimation:self];

	_stopButton.hidden        = !flag;
	_progressIndicator.hidden = !flag;
}

- (void)setStatusText:(NSString*)newStatusText
{
	_statusText = newStatusText;
	_statusTextButton.attributedTitle = _statusTextButton.attributedAlternateTitle = OakFormatStatusString(_statusText);
}

- (void)setAlternateStatusText:(NSString*)newAlternateStatusText
{
	_alternateStatusText = newAlternateStatusText;
	_statusTextButton.attributedAlternateTitle = OakFormatStatusString(_alternateStatusText);
}
@end
