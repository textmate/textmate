@class DownloadWindowController;

@protocol DownloadWindowControllerDelegate <NSObject>
- (void)install:(DownloadWindowController*)sender;
- (void)cancel:(DownloadWindowController*)sender;
@optional
- (void)windowWillClose:(DownloadWindowController*)sender;
@end

@interface DownloadWindowController : NSWindowController
@property (nonatomic, weak)   id <DownloadWindowControllerDelegate> delegate;
@property (nonatomic, assign) BOOL showUpdateBadge;

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
