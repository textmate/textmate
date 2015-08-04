#import "PreferencesPane.h"
#import <oak/misc.h>

PUBLIC @interface TerminalPreferences : PreferencesPane
{
	IBOutlet NSTextField* installStatusText;
	IBOutlet NSTextField* installSummaryText;
	IBOutlet NSPopUpButton* installPathPopUp;
	IBOutlet NSButton* installButton;
	IBOutlet NSTextField* rmateSummaryText;

	std::string statusTextFormat;
	std::string summaryTextFormat;
}
@property (nonatomic) NSImage* installIndicaitorImage;
- (IBAction)performInstallMate:(id)sender;
- (IBAction)performUninstallMate:(id)sender;

+ (void)updateMateIfRequired;
@end
