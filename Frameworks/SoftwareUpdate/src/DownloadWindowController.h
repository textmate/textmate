@class DownloadWindowController;

@protocol DownloadWindowControllerDelegate <NSObject>
- (void)install:(DownloadWindowController*)sender;
- (void)cancel:(DownloadWindowController*)sender;
@optional
- (void)windowWillClose:(DownloadWindowController*)sender;
@end

@interface DownloadWindowController : NSWindowController
@property (nonatomic, weak)   id <DownloadWindowControllerDelegate> delegate;
@property (nonatomic) BOOL showUpdateBadge;

@property (nonatomic) NSString* activityText;      // Text binding: “Downlading ‘TextMate_r1589.tbz’…”
@property (nonatomic) NSString* statusText;        // Text binding: “Less than one minute”

@property (nonatomic) BOOL isWorking;              // Progress bar binding (animate)
@property (nonatomic) BOOL isIndeterminate;        // Progress bar binding (is indeterminate)
@property (nonatomic) CGFloat progress;            // Progress bar binding (value)

@property (nonatomic) BOOL canInstall;             // Install button binding (enabled)
@property (nonatomic) BOOL canCancel;              // Cancel button binding (enabled)

- (IBAction)install:(id)sender;
- (IBAction)cancel:(id)sender;
@end
