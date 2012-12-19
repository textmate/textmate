#import <network/key_chain.h>

@class DownloadWindowController;

@protocol DownloadWindowControllerDelegate <NSObject>
- (void)install:(DownloadWindowController*)sender;
- (void)cancel:(DownloadWindowController*)sender;
@optional
- (void)windowWillClose:(DownloadWindowController*)sender;
@end

@interface DownloadWindowController : NSWindowController
@property (nonatomic, assign) id <DownloadWindowControllerDelegate> delegate;

@property (nonatomic, retain) NSString* activityText;      // Text binding: “Downlading ‘TextMate_r1589.tbz’…”
@property (nonatomic, retain) NSString* statusText;        // Text binding: “Less than one minute”

@property (nonatomic, assign) BOOL isWorking;              // Progress bar binding (animate)
@property (nonatomic, assign) BOOL isIndeterminate;        // Progress bar binding (is indeterminate)
@property (nonatomic, assign) CGFloat progress;            // Progress bar binding (value)

@property (nonatomic, assign) BOOL canInstall;             // Install button binding (enabled)
@property (nonatomic, assign) BOOL canCancel;              // Cancel button binding (enabled)

- (IBAction)install:(id)sender;
- (IBAction)cancel:(id)sender;
@end

@interface DownloadController : NSObject <DownloadWindowControllerDelegate>
- (id)initWithURL:(NSString*)aURL displayString:(NSString*)aDisplayString keyChain:(key_chain_t const&)aKeyChain;
- (void)startDownloadBackgroundUI:(BOOL)backgroundUIFlag;

@property (nonatomic, retain) NSString* versionOfDownload; // API
@property (nonatomic, readonly) BOOL isVisible;            // API
@end
