#import "Preferences.h"
#import "FilesPreferences.h"
#import "ProjectsPreferences.h"
#import "BundlesPreferences.h"
#import "VariablesPreferences.h"
#import "SoftwareUpdatePreferences.h"
#import "TerminalPreferences.h"
#import "Keys.h"
#import <MASPreferences/MASPreferencesWindowController.h>
#import <OakAppKit/OakSubmenuController.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(Preferences);

@implementation MASPreferencesWindowController (GoToNextPreviousTab)
- (IBAction)selectNextTab:(id)sender      { [self goNextTab:sender];     }
- (IBAction)selectPreviousTab:(id)sender  { [self goPreviousTab:sender]; }
@end

static Preferences* SharedInstance;

@implementation Preferences
+ (Preferences*)sharedInstance
{
	return SharedInstance ?: [[Preferences new] autorelease];
}

- (id)init
{
	if(SharedInstance)
	{
		[self release];
	}
	else if(self = SharedInstance = [[super init] retain])
	{
		D(DBF_Preferences, bug("My bundle %s\n", [[[NSBundle bundleForClass:[self class]] bundlePath] UTF8String]););
	}
	return SharedInstance;
}

- (void)showWindow:(id)sender
{
	if(!windowController)
	{
		NSMutableArray* array = [NSMutableArray array];
		[array addObject:[[[FilesPreferences alloc] init] autorelease]];
		[array addObject:[[[ProjectsPreferences alloc] init] autorelease]];
		[array addObject:[[[BundlesPreferences alloc] init] autorelease]];
		[array addObject:[[[VariablesPreferences alloc] init] autorelease]];
		[array addObject:[[[SoftwareUpdatePreferences alloc] init] autorelease]];
		[array addObject:[[[TerminalPreferences alloc] init] autorelease]];
		viewControllers = [array retain];

		windowController = [[MASPreferencesWindowController alloc] initWithViewControllers:viewControllers];
		[windowController setNextResponder:self];
	}
	[windowController showWindow:self];
}

- (void)takeSelectedViewControllerIndexFrom:(id)sender
{
	NSUInteger index = [[OakSubmenuController sharedInstance] tagForSender:sender];
	[windowController selectControllerAtIndex:index];
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(![windowController.window isKeyWindow])
		return;

	NSString* const selectedIdentifier = [windowController selectedViewController].identifier;

	int i = 0;
	for(NSViewController <MASPreferencesViewController>* viewController in viewControllers)
	{
		NSMenuItem* item = [aMenu addItemWithTitle:viewController.toolbarItemLabel action:@selector(takeSelectedViewControllerIndexFrom:) keyEquivalent:i < 10 ? [NSString stringWithFormat:@"%c", '0' + ((i+1) % 10)] : @""];
		item.tag = i;
		item.target = self;
		if([viewController.identifier isEqualToString:selectedIdentifier])
			[item setState:NSOnState];
		++i;
	}
}
@end
