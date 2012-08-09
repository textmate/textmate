#import "PreferencesPane.h"

@interface TerminalPreferences : PreferencesPane
{
	IBOutlet NSTextField* installStatusText;
	IBOutlet NSTextField* installSummaryText;
	IBOutlet NSPopUpButton* installPathPopUp;
	IBOutlet NSButton* installButton;
	NSImage* installIndicaitorImage;
	IBOutlet NSTextField* rmateSummaryText;

	std::string statusTextFormat;
	std::string summaryTextFormat;
}
@property (nonatomic, retain) NSImage* installIndicaitorImage;
- (IBAction)performInstallMate:(id)sender;
- (IBAction)performUninstallMate:(id)sender;
@end
