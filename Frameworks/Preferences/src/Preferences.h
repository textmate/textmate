@protocol PreferencesPaneProtocol <NSObject>
@optional
@property (nonatomic, readonly) NSImage* toolbarItemImage;
@end

@interface Preferences : NSWindowController
@property (class, readonly) Preferences* sharedInstance;
@end
