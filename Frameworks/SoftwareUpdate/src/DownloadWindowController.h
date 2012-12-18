#import <network/key_chain.h>

@interface DownloadWindowController : NSWindowController
@property (nonatomic, retain) NSString* versionOfDownload; // API
@property (nonatomic, readonly) BOOL isVisible;            // API

@property (nonatomic, retain) NSString* activityText;      // Text binding: “Downlading ‘TextMate_r1589.tbz’…”
@property (nonatomic, retain) NSString* statusText;        // Text binding: “Less than one minute”

@property (nonatomic, assign) BOOL isWorking;              // Progress bar binding (animate)
@property (nonatomic, assign) BOOL isIndeterminate;        // Progress bar binding (is indeterminate)
@property (nonatomic, assign) CGFloat progress;            // Progress bar binding (value)

@property (nonatomic, assign) BOOL canInstall;             // Install button binding (enabled)
@property (nonatomic, assign) BOOL canCancel;              // Cancel button binding (enabled)

- (id)initWithURL:(NSString*)aURL displayString:(NSString*)aDisplayString keyChain:(key_chain_t const&)aKeyChain;

- (IBAction)install:(id)sender;
- (IBAction)cancel:(id)sender;
@end
