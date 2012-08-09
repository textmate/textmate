#import "PreferencesPane.h"
#import <OakAppKit/OakEncodingPopUpButton.h>

@interface FilesPreferences : PreferencesPane
{
	IBOutlet NSPopUpButton* documentTypesPopUp;
	IBOutlet OakEncodingPopUpButton* encodingPopUp;
	IBOutlet NSMenu* documentTypesMenu;
}
@end
