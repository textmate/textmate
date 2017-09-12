#import <MASPreferences/MASPreferencesViewController.h>

@interface PreferencesPane : NSViewController <MASPreferencesViewController>
@property (nonatomic, readonly) NSString*   viewIdentifier;
@property (nonatomic, readonly) NSString*   toolbarItemLabel;
@property (nonatomic, readonly) NSImage*    toolbarItemImage;
@property (nonatomic) NSDictionary*         defaultsProperties;
@property (nonatomic) NSDictionary*         tmProperties;

- (id)initWithNibName:(NSString*)aNibName label:(NSString*)aLabel image:(NSImage*)anImage;

- (IBAction)help:(id)sender;
@end
