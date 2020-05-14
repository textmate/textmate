@interface AppController : NSObject <NSMenuDelegate>
{
	NSMenu* bundlesMenu;
	NSMenu* themesMenu;
	NSMenu* spellingMenu;
	NSMenu* wrapColumnMenu;

	IBOutlet NSPanel* goToLinePanel;
	IBOutlet NSTextField* goToLineTextField;

	struct
	{
		std::string filter_string;
		BOOL key_equivalent;
		BOOL all_scopes;
		int search_type;
	} bundleItemSearch;
}

- (IBAction)orderFrontFindPanel:(id)sender;

- (IBAction)orderFrontGoToLinePanel:(id)sender;
- (IBAction)performGoToLine:(id)sender;

- (IBAction)showBundleItemChooser:(id)sender;

- (IBAction)performSoftwareUpdateCheck:(id)sender;
- (IBAction)showPreferences:(id)sender;
- (IBAction)showBundleEditor:(id)sender;

- (IBAction)newDocumentAndActivate:(id)sender;
- (IBAction)openDocumentAndActivate:(id)sender;

- (IBAction)runPageLayout:(id)sender;
- (IBAction)openFavorites:(id)sender;
@end

@interface AppController (Documents)
- (void)newDocument:(id)sender;
- (void)openDocument:(id)sender;
- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender;
@end

@interface AppController (BundlesMenu)
- (BOOL)validateThemeMenuItem:(NSMenuItem*)item;
@end

void OakOpenDocuments (NSArray* paths, BOOL treatFilePackageAsFolder = NO);
