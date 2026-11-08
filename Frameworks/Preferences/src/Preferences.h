@protocol PreferencesPaneProtocol <NSObject>
@optional
@property (nonatomic, readonly) NSImage* toolbarItemImage;
@end

NSImage* PreferencesToolbarImage (NSString* symbolName, NSString* description, NSImage* fallbackImage);

@interface Preferences : NSWindowController
@property (class, readonly) Preferences* sharedInstance;
@end
