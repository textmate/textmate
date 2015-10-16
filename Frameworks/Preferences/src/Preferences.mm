#import "Preferences.h"
#import "FilesPreferences.h"
#import "ProjectsPreferences.h"
#import "BundlesPreferences.h"
#import "VariablesPreferences.h"
#import "SoftwareUpdatePreferences.h"
#import "TerminalPreferences.h"
#import "Keys.h"
#import <MASPreferences/MASPreferencesWindowController.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(Preferences);

@implementation MASPreferencesWindowController (GoToNextPreviousTab)
- (IBAction)selectNextTab:(id)sender      { [self goNextTab:sender];     }
- (IBAction)selectPreviousTab:(id)sender  { [self goPreviousTab:sender]; }
@end

@interface Preferences ()
@property (nonatomic) MASPreferencesWindowController* windowController;
@property (nonatomic) NSArray* viewControllers;
@end

@implementation Preferences
+ (Preferences*)sharedInstance
{
	static Preferences* SharedInstance = [Preferences new];
	return SharedInstance;
}

+ (void)restoreWindowWithIdentifier:(NSString*)identifier state:(NSCoder*)state completionHandler:(void (^)(NSWindow*, NSError*))completionHandler
{
	completionHandler([self sharedInstance].windowController.window, nil);
}

- (NSWindowController*)windowController
{
	if(!_windowController)
	{
		self.viewControllers = @[
			[FilesPreferences new],
			[ProjectsPreferences new],
			[BundlesPreferences new],
			[VariablesPreferences new],
			[SoftwareUpdatePreferences new],
			[TerminalPreferences new]
		];

		self.windowController = [[MASPreferencesWindowController alloc] initWithViewControllers:self.viewControllers];
		_windowController.nextResponder = self;
		_windowController.window.identifier = @"preferences";
		_windowController.window.restorationClass = [self class];
		_windowController.window.restorable = YES;
		[_windowController.window invalidateRestorableState];
	}
	return _windowController;
}

- (void)showWindow:(id)sender
{
	[self.windowController showWindow:self];
}

- (void)takeSelectedViewControllerIndexFrom:(id)sender
{
	NSUInteger index = [sender tag];
	[self.windowController selectControllerAtIndex:index];
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(![self.windowController.window isKeyWindow])
		return;

	NSString* const selectedIdentifier = [self.windowController selectedViewController].identifier;

	int i = 0;
	for(NSViewController <MASPreferencesViewController>* viewController in _viewControllers)
	{
		NSMenuItem* item = [aMenu addItemWithTitle:viewController.toolbarItemLabel action:@selector(takeSelectedViewControllerIndexFrom:) keyEquivalent:i < 9 ? [NSString stringWithFormat:@"%c", '1' + i] : @""];
		item.tag = i;
		item.target = self;
		if([viewController.identifier isEqualToString:selectedIdentifier])
			[item setState:NSOnState];
		++i;
	}
}
@end
