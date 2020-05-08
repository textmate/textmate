@protocol PreferencesPaneProtocol <NSObject>
@optional
@property (nonatomic, readonly) NSImage* toolbarItemImage;
@property (nonatomic, readonly) NSView*  initialKeyView;
@end

@interface Preferences : NSWindowController
@property (class, readonly) Preferences* sharedInstance;
@end
