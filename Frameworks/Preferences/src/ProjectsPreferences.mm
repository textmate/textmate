#import "ProjectsPreferences.h"
#import "Keys.h"
#import <settings/settings.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakTabBarView.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakStringListTransformer.h>

@implementation ProjectsPreferences
- (id)init
{
	if(self = [super initWithNibName:@"ProjectsPreferences" label:@"Projects" image:[NSImage imageNamed:@"Projects" inSameBundleAsClass:[self class]]])
	{
		[OakStringListTransformer createTransformerWithName:@"OakFileBrowserPlacementSettingsTransformer" andObjectsArray:@[ @"left", @"right" ]];
		[OakStringListTransformer createTransformerWithName:@"OakHTMLOutputPlacementSettingsTransformer" andObjectsArray:@[ @"bottom", @"right", @"window" ]];

		self.defaultsProperties = @{
			@"foldersOnTop"            : kUserDefaultsFoldersOnTopKey,
			@"tabsAboveDocument"       : kUserDefaultsTabsAboveDocumentKey,
			@"showFileExtensions"      : kUserDefaultsShowFileExtensionsKey,
			@"disableTabBarCollapsing" : kUserDefaultsDisableTabBarCollapsingKey,
			@"disableAutoResize"       : kUserDefaultsDisableFileBrowserWindowResizeKey,
			@"autoRevealFile"          : kUserDefaultsAutoRevealFileKey,
			@"fileBrowserPlacement"    : kUserDefaultsFileBrowserPlacementKey,
			@"htmlOutputPlacement"     : kUserDefaultsHTMLOutputPlacementKey,
		};

		self.tmProperties = @{
			@"excludePattern" : [NSString stringWithCxxString:kSettingsExcludeKey],
			@"includePattern" : [NSString stringWithCxxString:kSettingsIncludeKey],
			@"binaryPattern"  : [NSString stringWithCxxString:kSettingsBinaryKey],
		};
	}
	return self;
}

- (void)selectOtherFileBrowserPath:(id)sender
{
	NSOpenPanel* openPanel = [NSOpenPanel openPanel];
	[openPanel setCanChooseFiles:NO];
	[openPanel setCanChooseDirectories:YES];
	[openPanel beginSheetModalForWindow:[self view].window completionHandler:^(NSInteger result) {
		if(result == NSOKButton)
			[[NSUserDefaults standardUserDefaults] setObject:[[openPanel URL] absoluteString] forKey:kUserDefaultsInitialFileBrowserURLKey];
		[self updatePathPopUp];
	}];
}

- (void)takeFileBrowserPathFrom:(id)sender
{
	[[NSUserDefaults standardUserDefaults] setObject:[[sender representedObject] absoluteString] forKey:kUserDefaultsInitialFileBrowserURLKey];
	[self updatePathPopUp];
}

- (NSMenuItem*)menuItemForURL:(NSURL*)aURL
{
	NSMenuItem* res = [[NSMenuItem alloc] initWithTitle:[[NSFileManager defaultManager] displayNameAtPath:[aURL path]] action:@selector(takeFileBrowserPathFrom:) keyEquivalent:@""];
	[res setTarget:self];
	[res setRepresentedObject:aURL];
	if([aURL isFileURL])
		[res setIconForFile:[aURL path]];
	return res;
}

- (void)updatePathPopUp
{
	NSMenu* menu = [fileBrowserPathPopUp menu];
	[menu removeAllItems];

	NSURL* url = [NSURL fileURLWithPath:NSHomeDirectory()];
	if(NSString* urlString = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsInitialFileBrowserURLKey])
		url = [NSURL URLWithString:urlString];

	NSArray* defaultPathList = @[ [NSHomeDirectory() stringByAppendingPathComponent:@"Desktop"], NSHomeDirectory(), @"/" ];
	if(![defaultPathList containsObject:[url path]])
	{
		[menu addItem:[self menuItemForURL:url]];
		[menu addItem:[NSMenuItem separatorItem]];
	}

	for(NSString* path in defaultPathList)
	{
		[menu addItem:[self menuItemForURL:[NSURL fileURLWithPath:path]]];
		if([path isEqualToString:[url path]])
			[fileBrowserPathPopUp selectItemAtIndex:[menu numberOfItems]-1];
	}

	[menu addItem:[NSMenuItem separatorItem]];
	[menu addItemWithTitle:@"Other…" action:@selector(selectOtherFileBrowserPath:) keyEquivalent:@""];
}

- (void)loadView
{
	[super loadView];
	[self updatePathPopUp];
}
@end
