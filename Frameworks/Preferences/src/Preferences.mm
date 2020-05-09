#import "Preferences.h"
#import "FilesPreferences.h"
#import "ProjectsPreferences.h"
#import "BundlesPreferences.h"
#import "VariablesPreferences.h"
#import "SoftwareUpdatePreferences.h"
#import "TerminalPreferences.h"
#import "Keys.h"

static NSString* const kMASPreferencesFrameTopLeftKey = @"MASPreferences Frame Top Left";
static NSString* const kMASPreferencesSelectedViewKey = @"MASPreferences Selected Identifier View";

// =============================
// = PreferencesViewController =
// =============================

@interface PreferencesViewController : NSViewController
{
	NSUInteger                    _animationCounter;
	NSArray<NSLayoutConstraint*>* _viewFrameConstraints;
}
@property (nonatomic) NSString* selectedViewIdentifier;
@end

@implementation PreferencesViewController
- (void)loadView
{
	self.view = [[NSView alloc] initWithFrame:NSZeroRect];
	self.view.wantsLayer = YES;
}

- (void)viewWillAppear
{
	NSString* viewIdentifier = [NSUserDefaults.standardUserDefaults stringForKey:kMASPreferencesSelectedViewKey];
	self.selectedViewIdentifier = viewIdentifier ?: self.childViewControllers.firstObject.identifier;
}

- (void)transitionFromView:(NSView*)oldView toView:(NSView*)newView
{
	if(!newView)
		return;

	NSWindow* window = self.view.window;
	NSRect newFrame = window.frame;
	newFrame.size      = [window frameRectForContentRect:newView.frame].size;
	newFrame.origin.y += NSMaxY(window.frame) - NSMaxY(newFrame);

	if([window.firstResponder isKindOfClass:[NSView class]] && [(NSView*)window.firstResponder isDescendantOf:oldView])
		[window makeFirstResponder:window];

	auto oldViewFrameConstraints = _viewFrameConstraints;
	_viewFrameConstraints = @[
		[newView.widthAnchor  constraintEqualToConstant:NSWidth(newView.frame)  ],
		[newView.heightAnchor constraintEqualToConstant:NSHeight(newView.frame) ],
	];
	[NSLayoutConstraint activateConstraints:_viewFrameConstraints];

	NSUInteger animationCounter = ++_animationCounter;

	[NSAnimationContext runAnimationGroup:^(NSAnimationContext* context) {
		if(window.isVisible && oldView)
		{
			context.allowsImplicitAnimation = YES;
			context.duration = 0.2;
		}
		else
		{
			context.duration = 0;
		}

		if(oldView)
		{
			oldView.hidden = YES;
			[oldView removeFromSuperview];
			[NSLayoutConstraint deactivateConstraints:oldViewFrameConstraints];
		}

		newView.hidden = YES;
		[self.view addSubview:newView];
		self.view.nextKeyView = newView;
		newView.hidden = NO;

		[window setFrame:newFrame display:NO];
	}
	completionHandler:^{
		if(animationCounter == _animationCounter)
		{
			[NSLayoutConstraint deactivateConstraints:_viewFrameConstraints];
			_viewFrameConstraints = @[
				[newView.leadingAnchor  constraintEqualToAnchor:self.view.leadingAnchor ],
				[newView.bottomAnchor   constraintEqualToAnchor:self.view.bottomAnchor  ],
				[newView.topAnchor      constraintEqualToAnchor:self.view.topAnchor     ],
				[newView.trailingAnchor constraintEqualToAnchor:self.view.trailingAnchor],
			];
			[NSLayoutConstraint activateConstraints:_viewFrameConstraints];

			[window recalculateKeyViewLoop];
			if(window.firstResponder == window)
			{
				// selectKeyViewFollowingView: will select toolbar buttons when Full Keyboard Access is enabled

				std::set<NSView*> avoidLoops;
				for(NSView* keyView = newView; keyView && avoidLoops.find(keyView) == avoidLoops.end(); keyView = keyView.nextKeyView)
				{
					if(keyView.canBecomeKeyView)
					{
						[window makeFirstResponder:keyView];
						break;
					}
					avoidLoops.insert(keyView);
				}
			}
		}
	}];
}

- (void)setSelectedViewIdentifier:(NSString*)viewIdentifier
{
	if(_selectedViewIdentifier == viewIdentifier || [_selectedViewIdentifier isEqual:viewIdentifier])
		return;

	NSViewController* oldViewController = [self viewControllerForIdentifier:_selectedViewIdentifier];
	if(oldViewController && ![oldViewController commitEditing])
	{
		self.view.window.toolbar.selectedItemIdentifier = oldViewController.identifier;
		return;
	}

	_selectedViewIdentifier = viewIdentifier;
	self.view.window.toolbar.selectedItemIdentifier = viewIdentifier;
	[NSUserDefaults.standardUserDefaults setObject:_selectedViewIdentifier forKey:kMASPreferencesSelectedViewKey];

	NSViewController* newViewController = [self viewControllerForIdentifier:viewIdentifier];
	self.title = newViewController.title ?: @"Preferences";

	NSView* newView = newViewController.view;
	newView.translatesAutoresizingMaskIntoConstraints = NO;
	if(NSEqualSizes(newView.frame.size, NSZeroSize))
		newView.frame = { .size = newView.fittingSize };

	[self transitionFromView:oldViewController.view toView:newView];
}

- (NSViewController <PreferencesPaneProtocol>*)viewControllerForIdentifier:(NSString*)viewIdentifier
{
	for(NSViewController <PreferencesPaneProtocol>* viewController in self.childViewControllers)
	{
		if([viewController.identifier isEqual:viewIdentifier])
			return viewController;
	}
	return nil;
}
@end

// ===============================
// = PreferencesWindowController =
// ===============================

@interface Preferences () <NSToolbarDelegate, NSWindowDelegate>
@property (nonatomic) PreferencesViewController* preferencesViewController;
@end

@implementation Preferences
+ (instancetype)sharedInstance
{
	static Preferences* sharedInstance = [self new];
	return sharedInstance;
}

- (instancetype)init
{
	PreferencesViewController* contentViewController = [[PreferencesViewController alloc] init];

	NSWindow* window = [NSPanel windowWithContentViewController:contentViewController];
	if(NSString* topLeft = [NSUserDefaults.standardUserDefaults stringForKey:kMASPreferencesFrameTopLeftKey])
		[window setFrameTopLeftPoint:NSPointFromString(topLeft)];

	if((self = [super initWithWindow:window]))
	{
		_preferencesViewController = contentViewController;

		NSArray<NSViewController <PreferencesPaneProtocol>*>* viewControllers = @[
			[[FilesPreferences alloc] init],
			[[ProjectsPreferences alloc] init],
			[[BundlesPreferences alloc] init],
			[[VariablesPreferences alloc] init],
			[[SoftwareUpdatePreferences alloc] init],
			[[TerminalPreferences alloc] init]
		];

		for(NSViewController* viewController in viewControllers)
			[contentViewController addChildViewController:viewController];

		NSToolbar* toolbar = [[NSToolbar alloc] initWithIdentifier:@"Preferneces"];
		toolbar.allowsUserCustomization = NO;
		toolbar.delegate                = self;

		BOOL hasToolbarImages = NO;
		for(NSViewController* viewController in viewControllers)
			hasToolbarImages = hasToolbarImages || [viewController respondsToSelector:@selector(toolbarItemImage)];
		toolbar.displayMode = hasToolbarImages ? NSToolbarDisplayModeIconAndLabel : NSToolbarDisplayModeLabelOnly;

		window.autorecalculatesKeyViewLoop = NO;
		window.collectionBehavior          = NSWindowCollectionBehaviorMoveToActiveSpace|NSWindowCollectionBehaviorFullScreenAuxiliary;
		window.delegate                    = self;
		window.hidesOnDeactivate           = NO;
		window.toolbar                     = toolbar;
	}
	return self;
}

- (void)windowDidMove:(NSNotification*)aNotification
{
   [NSUserDefaults.standardUserDefaults setObject:NSStringFromPoint(NSMakePoint(NSMinX(self.window.frame), NSMaxY(self.window.frame))) forKey:kMASPreferencesFrameTopLeftKey];
}

- (void)selectViewAtRelativeOffset:(NSInteger)offset
{
	NSArray* identifiers = [self toolbarSelectableItemIdentifiers:self.window.toolbar];
	NSUInteger index = [identifiers indexOfObject:_preferencesViewController.selectedViewIdentifier];
	if(index != NSNotFound)
			_preferencesViewController.selectedViewIdentifier = identifiers[(index + identifiers.count + offset) % identifiers.count];
	else	_preferencesViewController.selectedViewIdentifier = offset < 0 ? identifiers.lastObject : identifiers.firstObject;
}

- (void)selectNextTab:(id)sender     { [self selectViewAtRelativeOffset:+1]; }
- (void)selectPreviousTab:(id)sender { [self selectViewAtRelativeOffset:-1]; }

- (void)updateShowTabMenu:(NSMenu*)aMenu
{
	if(!self.isWindowLoaded || !self.window.isKeyWindow)
		return;

	NSString* const selectedIdentifier = _preferencesViewController.selectedViewIdentifier;

	int i = 0;
	for(NSViewController* viewController in _preferencesViewController.childViewControllers)
	{
		NSMenuItem* item = [aMenu addItemWithTitle:viewController.title action:@selector(takeSelectedViewControllerIdentifierFrom:) keyEquivalent:i < 9 ? [NSString stringWithFormat:@"%c", '1' + i] : @""];
		item.representedObject = viewController.identifier;
		item.target = self;
		if([viewController.identifier isEqual:selectedIdentifier])
			item.state = NSControlStateValueOn;
		++i;
	}
}

- (void)takeSelectedViewControllerIdentifierFrom:(id)sender
{
	if([sender respondsToSelector:@selector(itemIdentifier)])
		_preferencesViewController.selectedViewIdentifier = [sender itemIdentifier];
	else if([sender respondsToSelector:@selector(representedObject)])
		_preferencesViewController.selectedViewIdentifier = [sender representedObject];
}

// ====================
// = Toolbar Delegate =
// ====================

- (NSToolbarItem*)toolbar:(NSToolbar*)toolbar itemForItemIdentifier:(NSToolbarItemIdentifier)itemIdentifier willBeInsertedIntoToolbar:(BOOL)flag
{
	if(NSViewController <PreferencesPaneProtocol>* viewController = [_preferencesViewController viewControllerForIdentifier:itemIdentifier])
	{
		NSToolbarItem* res = [[NSToolbarItem alloc] initWithItemIdentifier:itemIdentifier];
		res.label  = viewController.title;
		res.action = @selector(takeSelectedViewControllerIdentifierFrom:);
		res.target = self;

		if([viewController respondsToSelector:@selector(toolbarItemImage)])
			res.image = viewController.toolbarItemImage;

		return res;
	}
	return nil;
}

- (NSArray<NSToolbarItemIdentifier>*)toolbarAllowedItemIdentifiers:(NSToolbar*)toolbar
{
	NSMutableArray* res = [NSMutableArray array];
	for(NSViewController* viewController in _preferencesViewController.childViewControllers)
	{
		if(viewController.identifier)
			[res addObject:viewController.identifier];
	}
	return res;
}

- (NSArray<NSToolbarItemIdentifier>*)toolbarDefaultItemIdentifiers:(NSToolbar*)toolbar
{
	return [self toolbarAllowedItemIdentifiers:toolbar];
}

- (NSArray<NSToolbarItemIdentifier>*)toolbarSelectableItemIdentifiers:(NSToolbar*)toolbar
{
	return [self toolbarAllowedItemIdentifiers:toolbar];
}
@end
