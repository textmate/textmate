#import "Preferences.h"

NSView* OakSetupGridViewWithSeparators (NSGridView* gridView, std::vector<NSUInteger> rows = { });

@interface PreferencesPane : NSViewController <PreferencesPaneProtocol>
@property (nonatomic, readonly) NSImage* toolbarItemImage;
@property (nonatomic) NSDictionary*      defaultsProperties;
@property (nonatomic) NSDictionary*      tmProperties;

- (id)initWithNibName:(NSNibName)aNibName label:(NSString*)aLabel image:(NSImage*)anImage;

- (IBAction)help:(id)sender;
@end
