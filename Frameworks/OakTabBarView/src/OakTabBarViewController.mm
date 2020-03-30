#import "OakTabBarViewController.h"

@interface OakTabBarViewController () <OakTabBarViewDataSource>
{
	OakTabBarView* _tabBarView;

	BOOL _hasPendingReloadTabsFlag;
	BOOL _animateReloadFlag;
}
@end

@implementation OakTabBarViewController
- (OakTabBarView*)tabBarView
{
	if(!_tabBarView)
	{
		_tabBarView = [[OakTabBarView alloc] initWithFrame:NSZeroRect];
		_tabBarView.dataSource = self;
	}
	return _tabBarView;
}

- (void)loadView
{
	self.tabBarView.frameSize = self.tabBarView.intrinsicContentSize;
	self.fullScreenMinHeight = self.tabBarView.intrinsicContentSize.height;
	self.view = self.tabBarView;
}

- (id <OakTabBarViewDelegate>)delegate
{
	return self.tabBarView.delegate;
}

- (void)setDelegate:(id <OakTabBarViewDelegate>)newDelegate
{
	self.tabBarView.delegate = newDelegate;
}

- (void)scheduleReloadTabs
{
	if(_hasPendingReloadTabsFlag)
		return;
	_hasPendingReloadTabsFlag = YES;
	[self performSelector:@selector(reloadTabs:) withObject:nil afterDelay:0];
}

- (void)reloadTabs:(id)sender
{
	if(_animateReloadFlag)
	{
		[NSAnimationContext runAnimationGroup:^(NSAnimationContext* context){
			context.allowsImplicitAnimation = YES;
			[_tabBarView reloadData];
			[_tabBarView setSelectedTabIndex:_selectedIndex];
		} completionHandler:^{
		}];

		_animateReloadFlag = NO;
	}
	else
	{
		[_tabBarView reloadData];
		[_tabBarView setSelectedTabIndex:_selectedIndex];
	}

	_hasPendingReloadTabsFlag = NO;
}

- (void)setSelectedIndex:(NSUInteger)newSelectedIndex
{
	_selectedIndex = newSelectedIndex;
	if(!_hasPendingReloadTabsFlag)
		[_tabBarView setSelectedTabIndex:_selectedIndex];

	[self invalidateRestorableState];
}

- (void)setIdentifiers:(NSArray<NSUUID*>*)newIdentifiers
{
	if(_identifiers.count && ![_identifiers isEqualToArray:newIdentifiers])
		_animateReloadFlag = YES;

	_identifiers = newIdentifiers;
	[self scheduleReloadTabs];
}

- (void)setTitles:(NSArray<NSString*>*)newTitles
{
	_titles = newTitles;
	[self scheduleReloadTabs];
}

- (void)setModifiedStates:(NSArray<NSNumber*>*)newModifiedStates
{
	_modifiedStates = newModifiedStates;
	[self scheduleReloadTabs];
}

- (void)setURLs:(NSArray<NSURL*>*)newURLs
{
	_URLs = newURLs;
	[self scheduleReloadTabs];
}

- (void)setToolTips:(NSArray<NSString*>*)newToolTips
{
	_toolTips = newToolTips;
	[self scheduleReloadTabs];
}

// ===========================
// = OakTabBarViewDataSource =
// ===========================

- (NSUInteger)numberOfRowsInTabBarView:(OakTabBarView*)aTabBarView
{
	return _identifiers.count;
}

- (NSString*)tabBarView:(OakTabBarView*)aTabBarView titleForIndex:(NSUInteger)anIndex
{
	return _titles[anIndex];
}

- (NSString*)tabBarView:(OakTabBarView*)aTabBarView pathForIndex:(NSUInteger)anIndex
{
	NSURL* url = _URLs[anIndex];
	return [url respondsToSelector:@selector(filePathURL)] ? url.filePathURL.path : nil;
}

- (NSUUID*)tabBarView:(OakTabBarView*)aTabBarView UUIDForIndex:(NSUInteger)anIndex
{
	return _identifiers[anIndex];
}

- (BOOL)tabBarView:(OakTabBarView*)aTabBarView isEditedAtIndex:(NSUInteger)anIndex
{
	NSNumber* modified = _modifiedStates[anIndex];
	return [modified respondsToSelector:@selector(boolValue)] ? modified.boolValue : NO;
}
@end
