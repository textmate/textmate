#import <MASPreferences/MASPreferencesViewController.h>

@interface PreferencesPane : NSViewController <MASPreferencesViewController>
{
	NSString* label;
	NSImage* image;
	NSDictionary* defaultsProperties; // property → defaults key
}
@property (nonatomic, readonly) NSString*   identifier;
@property (nonatomic, readonly) NSString*   toolbarItemLabel;
@property (nonatomic, readonly) NSImage*    toolbarItemImage;
@property (nonatomic, retain) NSDictionary* defaultsProperties;

- (id)initWithNibName:(NSString*)aNibName label:(NSString*)aLabel image:(NSImage*)anImage;

- (IBAction)help:(id)sender;
@end
