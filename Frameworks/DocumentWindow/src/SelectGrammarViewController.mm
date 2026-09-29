#import "SelectGrammarViewController.h"
#import <OakTextView/src/OakDocumentView.h>
#import <OakAppKit/src/OakUIConstructionFunctions.h>
#import <BundlesManager/src/BundlesManager.h>
#import <ns/src/ns.h>

@interface SelectGrammarViewController ()
{
	BOOL _didLoadView;
	id _eventMonitor;
}
@property (nonatomic) NSView*              divider;
@property (nonatomic) NSTextField*         label;
@property (nonatomic) NSButton*            installButton;
@property (nonatomic) NSButton*            notNowButton;
@property (nonatomic) NSButton*            neverButton;
@property (nonatomic) NSProgressIndicator* progressIndicator;

@property (nonatomic, readonly) NSString*  labelString;

@property (nonatomic) OakDocumentView*     documentView;
@property (nonatomic) BundleGrammar*       grammar;

@property (nonatomic, strong) void(^callback)(SelectGrammarResponse, BundleGrammar*);
@end

static NSButton* OakSmallButton (NSString* title, SEL action, id target, NSInteger tag)
{
	NSButton* res = OakCreateButton(title);
	[res setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
	res.font        = [NSFont messageFontOfSize:[NSFont systemFontSizeForControlSize:NSControlSizeSmall]];
	res.controlSize = NSControlSizeSmall;
	res.action      = action;
	res.target      = target;
	res.tag         = tag;
	return res;
}

@implementation SelectGrammarViewController
+ (NSSet*)keyPathsForValuesAffectingLabelString
{
	return [NSSet setWithObjects:@"grammar", @"documentDisplayName", nil];
}

- (NSString*)labelString
{
	if(_grammar && _documentDisplayName)
		return [NSString stringWithFormat:@"Would you like to install the “%@” bundle? This improves support for documents like “%@”.", _grammar.bundle.name, _documentDisplayName];
	else if(_grammar)
		return [NSString stringWithFormat:@"Would you like to install the “%@” bundle? This improves support for this document.", _grammar.bundle.name];
	else
		return @"Would you like to install additional support for this document?";
}

- (void)loadView
{
	if(_didLoadView)
		return;
	_didLoadView = YES;

	self.divider           = OakCreateNSBoxSeparator();
	self.label             = OakCreateLabel(self.labelString);
	self.neverButton       = OakSmallButton(@"Never", @selector(didClickButton:), self, SelectGrammarResponseNever);
	self.notNowButton      = OakSmallButton(@"Not Now", @selector(didClickButton:), self, SelectGrammarResponseNotNow);
	self.installButton     = OakSmallButton(@"Install", @selector(didClickButton:), self, SelectGrammarResponseInstall);
	self.progressIndicator = [[NSProgressIndicator alloc] initWithFrame:NSZeroRect];

	[_label setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];

	_progressIndicator = [NSProgressIndicator new];
	_progressIndicator.controlSize          = NSControlSizeSmall;
	_progressIndicator.maxValue             = 1;
	_progressIndicator.indeterminate        = YES;
	_progressIndicator.displayedWhenStopped = NO;
	_progressIndicator.bezeled              = NO;

	NSDictionary* views = @{
		@"divider":  self.divider,
		@"label":    self.label,
		@"progress": self.progressIndicator,
		@"never":    self.neverButton,
		@"notNow":   self.notNowButton,
		@"install":  self.installButton,
	};

	self.view = [[NSView alloc] initWithFrame:NSZeroRect];
	OakAddAutoLayoutViewsToSuperview([views allValues], self.view);

	[self.view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[divider]|" options:0 metrics:nil views:views]];
	[self.view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[label]-(>=8)-[notNow(==install)]-[install]-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[self.view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[label]-(>=8)-[never(==install)]-[install]-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[self.view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[label]-(>=8)-[progress]-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[self.view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(8)-[label]-(8)-[divider(==1)]|" options:0 metrics:nil views:views]];
	[self.view addConstraint:[NSLayoutConstraint constraintWithItem:_progressIndicator attribute:NSLayoutAttributeLeading relatedBy:NSLayoutRelationEqual toItem:_notNowButton attribute:NSLayoutAttributeLeading multiplier:1 constant:0]];

	self.neverButton.hidden = YES;

	_eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskFlagsChanged handler:^NSEvent*(NSEvent* event){
		NSUInteger modifierFlags = [self.view.window isKeyWindow] ? ([event modifierFlags] & (NSEventModifierFlagShift|NSEventModifierFlagControl|NSEventModifierFlagOption|NSEventModifierFlagCommand)) : 0;
		self.neverButton.hidden  = modifierFlags != NSEventModifierFlagOption;
		self.notNowButton.hidden = modifierFlags == NSEventModifierFlagOption;
		return event;
	}];
}

- (void)showGrammars:(NSArray<BundleGrammar*>*)grammars forView:(OakDocumentView*)documentView completionHandler:(void(^)(SelectGrammarResponse, BundleGrammar*))callback
{
	self.documentView = documentView;
	self.grammar      = [grammars firstObject];
	self.callback     = callback;

	[_documentView addAuxiliaryView:self.view atEdge:NSMaxYEdge];
}

- (void)dismiss
{
	[_documentView removeAuxiliaryView:self.view];

	[NSEvent removeMonitor:_eventMonitor];
	_eventMonitor = nil; // This retains ‘self’
}

- (void)didClickButton:(id)sender
{
	SelectGrammarResponse tag = [sender respondsToSelector:@selector(tag)] ? (SelectGrammarResponse)[sender tag] : SelectGrammarResponseNotNow;
	if(tag == SelectGrammarResponseInstall)
	{
		Bundle* bundle = _grammar.bundle;

		_installButton.hidden = YES;
		_notNowButton.hidden  = YES;
		_neverButton.hidden   = YES;

		_label.stringValue = [NSString stringWithFormat:@"Installing ‘%@’…", bundle.name];
		[_progressIndicator startAnimation:self];

		[BundlesManager.sharedInstance installBundles:@[ bundle ] completionHandler:^(NSArray<Bundle*>* bundles){
			[_progressIndicator stopAnimation:self];
			_callback(tag, _grammar);
			[self dismiss];
		}];
	}
	else
	{
		_callback(tag, _grammar);
		[self dismiss];
	}
}
@end
