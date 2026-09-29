#import "ProjectLayoutView.h"
#import <OakAppKit/src/OakUIConstructionFunctions.h>
#import <OakFoundation/src/OakFoundation.h>

NSString* const kUserDefaultsFileBrowserWidthKey = @"fileBrowserWidth";
NSString* const kUserDefaultsHTMLOutputSizeKey   = @"htmlOutputSize";

static CGFloat const kFileBrowserMinimumWidth = 240;
static CGFloat const kHTMLOutputMinimumHeight = 50;

static NSColor* DividerColor ()
{
	if(@available(macOS 10.14, *))
		return NSColor.separatorColor;
	return NSColor.gridColor;
}

@interface ProjectLayoutView ()
{
	CGFloat _fileBrowserWidth;
	NSSize  _htmlOutputSize;
}
@property (nonatomic) NSSplitViewController* contentSplitViewController;
@property (nonatomic) NSViewController* documentViewController;
@property (nonatomic) NSViewController* fileBrowserViewController;
@property (nonatomic) NSViewController* htmlOutputViewController;
@property (nonatomic) NSSplitViewItem* contentSplitViewItem;
@property (nonatomic) NSSplitViewItem* documentViewItem;
@property (nonatomic) NSSplitViewItem* fileBrowserViewItem;
@property (nonatomic) NSSplitViewItem* htmlOutputViewItem;
@property (nonatomic) BOOL needsFileBrowserResize;
@property (nonatomic) BOOL needsHTMLOutputResize;
@end

@implementation ProjectLayoutView
+ (void)initialize
{
	[NSUserDefaults.standardUserDefaults registerDefaults:@{
		kUserDefaultsFileBrowserWidthKey: @250,
		kUserDefaultsHTMLOutputSizeKey:   NSStringFromSize(NSMakeSize(200, 200))
	}];
}

- (id)init
{
	if(self = [super init])
	{
		_fileBrowserWidth = [NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsFileBrowserWidthKey];
		_htmlOutputSize   = NSSizeFromString([NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsHTMLOutputSizeKey]);

		self.view.translatesAutoresizingMaskIntoConstraints = NO;
		self.splitView.vertical     = YES;
		self.splitView.dividerStyle = NSSplitViewDividerStyleThin;
		self.splitView.autosaveName = @"Project Layout";
		[self.splitView setValue:DividerColor() forKey:@"dividerColor"];

		_contentSplitViewController = [[NSSplitViewController alloc] init];
		_contentSplitViewController.view.translatesAutoresizingMaskIntoConstraints = NO;
		_contentSplitViewController.splitView.vertical     = NO;
		_contentSplitViewController.splitView.dividerStyle = NSSplitViewDividerStyleThin;
		_contentSplitViewController.splitView.autosaveName = @"Project Layout Content";
		[_contentSplitViewController.splitView setValue:DividerColor() forKey:@"dividerColor"];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(splitViewDidResizeSubviews:) name:NSSplitViewDidResizeSubviewsNotification object:_contentSplitViewController.splitView];

		_documentViewController = [self viewControllerWithView:[[NSView alloc] initWithFrame:NSZeroRect]];
		_documentViewItem = [NSSplitViewItem splitViewItemWithViewController:_documentViewController];
		_documentViewItem.minimumThickness = 100;
		_documentViewItem.holdingPriority  = NSLayoutPriorityDefaultLow - 1;
		if(@available(macOS 11.0, *))
			_documentViewItem.titlebarSeparatorStyle = NSTitlebarSeparatorStyleNone;
		if(@available(macOS 26.0, *))
			_documentViewItem.automaticallyAdjustsSafeAreaInsets = YES;
		[_contentSplitViewController addSplitViewItem:_documentViewItem];

		_contentSplitViewItem = [NSSplitViewItem contentListWithViewController:_contentSplitViewController];
		_contentSplitViewItem.minimumThickness = 100;
		_contentSplitViewItem.holdingPriority  = NSLayoutPriorityDefaultLow - 1;
		if(@available(macOS 11.0, *))
			_contentSplitViewItem.titlebarSeparatorStyle = NSTitlebarSeparatorStyleNone;
		[self addSplitViewItem:_contentSplitViewItem];
	}
	return self;
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self];
}

- (NSViewController*)viewControllerWithView:(NSView*)view
{
	NSViewController* res = [[NSViewController alloc] initWithNibName:nil bundle:nil];
	res.view = [[NSView alloc] initWithFrame:NSZeroRect];
	[self replaceViewController:res view:view];
	return res;
}

- (void)replaceViewController:(NSViewController*)viewController view:(NSView*)view
{
	for(NSView* subview in viewController.view.subviews.copy)
		[subview removeFromSuperview];

	if(view)
	{
		OakAddAutoLayoutViewsToSuperview(@[ view ], viewController.view);
		[viewController.view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[view]|" options:0 metrics:nil views:@{ @"view": view }]];

		NSLayoutYAxisAnchor* topAnchor = viewController == _documentViewController ? viewController.view.safeAreaLayoutGuide.topAnchor : viewController.view.topAnchor;
		[view.topAnchor constraintEqualToAnchor:topAnchor].active = YES;
		[view.bottomAnchor constraintEqualToAnchor:viewController.view.bottomAnchor].active = YES;
	}
}

- (void)updateKeyViewLoop
{
	NSMutableArray<NSView*>* views = [NSMutableArray array];
	if(_documentView)
		[views addObject:_documentView];
	if(_htmlOutputView)
		[views addObject:_htmlOutputView];
	if(_fileBrowserView)
		[views addObject:_fileBrowserView];
	OakSetupKeyViewLoop(views);
}

- (void)setDocumentView:(NSView*)aDocumentView
{
	if(_documentView == aDocumentView)
		return;

	_documentView = aDocumentView;
	[self replaceViewController:_documentViewController view:_documentView];
	[self updateKeyViewLoop];
}

- (void)setFileBrowserView:(NSView*)aFileBrowserView
{
	if(_fileBrowserView == aFileBrowserView)
		return;

	_fileBrowserView = aFileBrowserView;
	if(_fileBrowserView)
	{
		if(!_fileBrowserViewController)
		{
			_fileBrowserViewController = [self viewControllerWithView:nil];
			[self replaceViewController:_fileBrowserViewController view:_fileBrowserView];
			_fileBrowserViewItem = [NSSplitViewItem sidebarWithViewController:_fileBrowserViewController];
			_fileBrowserViewItem.minimumThickness = kFileBrowserMinimumWidth;
			_fileBrowserViewItem.maximumThickness = NSSplitViewItemUnspecifiedDimension;
			_fileBrowserViewItem.canCollapse = NO;
			_fileBrowserViewItem.holdingPriority = NSLayoutPriorityDragThatCannotResizeWindow - 1;
			[self insertSplitViewItem:_fileBrowserViewItem atIndex:0];
		}
		else
		{
			[self replaceViewController:_fileBrowserViewController view:_fileBrowserView];
			if(![self.splitViewItems containsObject:_fileBrowserViewItem])
				[self insertSplitViewItem:_fileBrowserViewItem atIndex:0];
		}
		self.needsFileBrowserResize = YES;
	}
	else if(_fileBrowserViewItem && [self.splitViewItems containsObject:_fileBrowserViewItem])
	{
		[self removeSplitViewItem:_fileBrowserViewItem];
	}

	[self updateKeyViewLoop];
}

- (void)setHtmlOutputView:(NSView*)aHtmlOutputView
{
	if(_htmlOutputView == aHtmlOutputView)
		return;

	_htmlOutputView = aHtmlOutputView;
	if(_htmlOutputView)
	{
		if(!_htmlOutputViewController)
		{
			_htmlOutputViewController = [self viewControllerWithView:_htmlOutputView];
			_htmlOutputViewItem = [NSSplitViewItem splitViewItemWithViewController:_htmlOutputViewController];
			_htmlOutputViewItem.minimumThickness = kHTMLOutputMinimumHeight;
			_htmlOutputViewItem.canCollapse = YES;
			_htmlOutputViewItem.holdingPriority = NSLayoutPriorityDragThatCannotResizeWindow - 1;
			[_contentSplitViewController insertSplitViewItem:_htmlOutputViewItem atIndex:0];
		}
		else
		{
			[self replaceViewController:_htmlOutputViewController view:_htmlOutputView];
			if(![_contentSplitViewController.splitViewItems containsObject:_htmlOutputViewItem])
				[_contentSplitViewController insertSplitViewItem:_htmlOutputViewItem atIndex:0];
		}
		self.needsHTMLOutputResize = YES;
	}
	else if(_htmlOutputViewItem && [_contentSplitViewController.splitViewItems containsObject:_htmlOutputViewItem])
	{
		[_contentSplitViewController removeSplitViewItem:_htmlOutputViewItem];
	}

	[self updateKeyViewLoop];
}

- (CGFloat)fileBrowserWidth
{
	return _fileBrowserView ? round(NSWidth(_fileBrowserView.frame)) : _fileBrowserWidth;
}

- (void)setFileBrowserWidth:(CGFloat)aWidth
{
	_fileBrowserWidth = std::max<CGFloat>(kFileBrowserMinimumWidth, round(aWidth));
	[NSUserDefaults.standardUserDefaults setInteger:_fileBrowserWidth forKey:kUserDefaultsFileBrowserWidthKey];
	self.needsFileBrowserResize = YES;
	[self.view setNeedsLayout:YES];
}

- (NSSize)htmlOutputSize
{
	if(_htmlOutputView)
		_htmlOutputSize.height = round(NSHeight(_htmlOutputView.frame));
	return _htmlOutputSize;
}

- (void)setHtmlOutputSize:(NSSize)aSize
{
	_htmlOutputSize = aSize;
	_htmlOutputSize.height = std::max<CGFloat>(kHTMLOutputMinimumHeight, round(_htmlOutputSize.height));
	[NSUserDefaults.standardUserDefaults setObject:NSStringFromSize(_htmlOutputSize) forKey:kUserDefaultsHTMLOutputSizeKey];
	self.needsHTMLOutputResize = YES;
	[self.view setNeedsLayout:YES];
}

- (void)viewDidLayout
{
	[super viewDidLayout];

	if(_fileBrowserView && self.needsFileBrowserResize && NSWidth(self.splitView.bounds) > _fileBrowserWidth)
	{
		[self.splitView setPosition:_fileBrowserWidth ofDividerAtIndex:0];
		self.needsFileBrowserResize = NO;
	}

	if(_htmlOutputView && self.needsHTMLOutputResize)
	{
		if(NSHeight(_contentSplitViewController.splitView.bounds) > _htmlOutputSize.height)
		{
			[_contentSplitViewController.splitView setPosition:_htmlOutputSize.height ofDividerAtIndex:0];
			self.needsHTMLOutputResize = NO;
		}
	}
}

- (void)splitViewDidResizeSubviews:(NSNotification*)notification
{
	[super splitViewDidResizeSubviews:notification];

	if(_fileBrowserView && notification.object == self.splitView)
	{
		_fileBrowserWidth = std::max<CGFloat>(kFileBrowserMinimumWidth, round(NSWidth(_fileBrowserView.frame)));
		[NSUserDefaults.standardUserDefaults setInteger:_fileBrowserWidth forKey:kUserDefaultsFileBrowserWidthKey];
	}
	else if(_htmlOutputView && notification.object == _contentSplitViewController.splitView)
	{
		_htmlOutputSize.height = std::max<CGFloat>(kHTMLOutputMinimumHeight, round(NSHeight(_htmlOutputView.frame)));
		[NSUserDefaults.standardUserDefaults setObject:NSStringFromSize(_htmlOutputSize) forKey:kUserDefaultsHTMLOutputSizeKey];
	}
}
@end
