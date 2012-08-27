#import <network/key_chain.h>

struct shared_state_t
{
	double progress = 0;
	bool stop = false;
};

typedef std::shared_ptr<shared_state_t> shared_state_ptr;

@interface DownloadWindowController : NSWindowController
{
	NSString* activityText;
	CGFloat progress;
	CGFloat secondsLeft;
	NSString* statusText;
	BOOL isDownloading;
	BOOL canInstall;
	BOOL isInstalling;
	BOOL showUpdateBadge;

	NSTimer* progressTimer;
	NSDate* downloadStartDate;
	NSString* versionOfDownload;

	NSString* url;
	key_chain_t keyChain;
	NSString* archive;
	shared_state_ptr sharedState;
}
@property (nonatomic, retain) NSString* versionOfDownload; // API
@property (nonatomic, readonly) BOOL isVisible;            // API

@property (nonatomic, retain) NSString* activityText;      // Text binding: “Downlading ‘TextMate_r1589.tbz’…”
@property (nonatomic, retain) NSString* statusText;        // Text binding: “Less than one minute”
@property (nonatomic, readonly) BOOL isWorking;            // Progress bar binding (animate)
@property (nonatomic, assign) CGFloat progress;            // Progress bar binding (value)
@property (nonatomic, assign) BOOL canInstall;             // Install Button binding
@property (nonatomic, assign) BOOL isInstalling;           // Install/Cancel Button bindings + Progress bar (is indeterminate)

- (id)initWithURL:(NSString*)aURL displayString:(NSString*)aDisplayString keyChain:(key_chain_t const&)aKeyChain;

- (IBAction)install:(id)sender;
- (IBAction)cancel:(id)sender;
@end
