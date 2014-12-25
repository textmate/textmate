@interface AppController : NSObject <NSMenuDelegate>
{
	IBOutlet NSMenu* bundlesMenu;
	IBOutlet NSMenu* themesMenu;
	IBOutlet NSMenu* spellingMenu;
	IBOutlet NSMenu* wrapColumnMenu;

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

void OakOpenDocuments (NSArray* paths);
