#import "OakTabBarViewController.h"
#import <OakAppKit/NSMenuItem Additions.h>

@interface NSArray<ObjectType> (SafeAccessor)
- (ObjectType)mySafeObjectAtIndex:(NSUInteger)index;
@end

@implementation NSArray (SafeAccessor)
- (id)mySafeObjectAtIndex:(NSUInteger)index
{
	id obj = index < self.count ? [self objectAtIndex:index] : nil;
	return obj != NSNull.null ? obj : nil;
}
@end

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

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(self.view.window.isKeyWindow)
	{
		for(int i = 0; i < _titles.count; ++i)
		{
			NSMenuItem* item = [aMenu addItemWithTitle:[_titles mySafeObjectAtIndex:i] action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:i < 8 ? [NSString stringWithFormat:@"%c", '1' + i] : @""];
			item.tag     = i;
			item.target  = self;
			item.toolTip = [_toolTips mySafeObjectAtIndex:i];
			if((aMenu.propertiesToUpdate & NSMenuPropertyItemImage) && i < _images.count)
				item.image = [_images mySafeObjectAtIndex:i];
			if(i == _tabBarView.selectedTabIndex)
				item.state = NSControlStateValueOn;
			else if([_modifiedStates mySafeObjectAtIndex:i].boolValue)
				[item setModifiedState:YES];
		}
	}

	if(aMenu.numberOfItems == 0)
	{
		[aMenu addItemWithTitle:@"No Tabs Open" action:@selector(nop:) keyEquivalent:@""];
	}
	else
	{
		[aMenu addItem:[NSMenuItem separatorItem]];

		NSMenuItem* item = [aMenu addItemWithTitle:@"Last Tab" action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:@"9"];
		item.tag     = _identifiers.count-1;
		item.target  = self;
		item.toolTip = [_toolTips mySafeObjectAtIndex:item.tag];
	}
}

- (void)takeSelectedTabIndexFrom:(id)sender
{
	self.selectedIndex = [sender tag];
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
	BOOL notifyObservers = _selectedIndex != newSelectedIndex;

	_selectedIndex = newSelectedIndex;
	if(!_hasPendingReloadTabsFlag)
		[_tabBarView setSelectedTabIndex:_selectedIndex];

	[self invalidateRestorableState];

	if(notifyObservers)
	{
		if(NSDictionary* info = [self infoForBinding:@"selectedIndex"])
		{
			id controller     = info[NSObservedObjectKey];
			NSString* keyPath = info[NSObservedKeyPathKey];
			if(controller && controller != [NSNull null] && keyPath && (id)keyPath != [NSNull null])
			{
				id newValue = [NSNumber numberWithUnsignedInteger:_selectedIndex];
				id oldValue = [controller valueForKeyPath:keyPath];
				if(!oldValue || ![oldValue isEqual:newValue])
					[controller setValue:newValue forKeyPath:keyPath];
			}
		}
	}
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
	return [_titles mySafeObjectAtIndex:anIndex];
}

- (NSString*)tabBarView:(OakTabBarView*)aTabBarView pathForIndex:(NSUInteger)anIndex
{
	return [_URLs mySafeObjectAtIndex:anIndex].filePathURL.path;
}

- (NSUUID*)tabBarView:(OakTabBarView*)aTabBarView UUIDForIndex:(NSUInteger)anIndex
{
	return [_identifiers mySafeObjectAtIndex:anIndex];
}

- (BOOL)tabBarView:(OakTabBarView*)aTabBarView isEditedAtIndex:(NSUInteger)anIndex
{
	return [_modifiedStates mySafeObjectAtIndex:anIndex].boolValue;
}
@end
