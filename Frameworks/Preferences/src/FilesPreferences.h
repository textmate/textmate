#import "PreferencesPane.h"
#import <OakAppKit/OakEncodingPopUpButton.h>

@interface FilesPreferences : PreferencesPane
{
	IBOutlet NSPopUpButton* newDocumentTypesPopUp;
	IBOutlet NSPopUpButton* unknownDocumentTypesPopUp;
	IBOutlet OakEncodingPopUpButton* encodingPopUp;
	IBOutlet NSMenu* newDocumentTypesMenu;
	IBOutlet NSMenu* unknownDocumentTypesMenu;
}
@end
