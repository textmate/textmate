#import "SoftwareUpdate.h"
#import "OakDownloadManager.h"
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakSound.h>
#import <OakAppKit/OakTransitionViewController.h>
#import <OakAppKit/OakUIConstructionFunctions.h>

NSString* const kUserDefaultsLastSoftwareUpdateCheckKey                        = @"SoftwareUpdateLastPoll";
NSString* const kUserDefaultsSoftwareUpdateSuspendUntilKey                     = @"SoftwareUpdateSuspendUntil";
NSString* const kUserDefaultsDisableSoftwareUpdateKey                          = @"SoftwareUpdateDisablePolling";
NSString* const kUserDefaultsAskBeforeUpdatingKey                              = @"SoftwareUpdateAskBeforeUpdating";
NSString* const kUserDefaultsSoftwareUpdateChannelKey                          = @"SoftwareUpdateChannel";
NSString* const kUserDefaultsSoftwareUpdateDisableReadOnlyFileSystemWarningKey = @"SoftwareUpdateDisableReadOnlyFileSystemWarningKey";

NSString* const kSoftwareUpdateChannelRelease                                  = @"release";
NSString* const kSoftwareUpdateChannelPrerelease                               = @"beta";
NSString* const kSoftwareUpdateChannelCanary                                   = @"nightly";

// ============================
// = SUDownloadViewController =
// ============================

@interface SUDownloadViewController : NSViewController
- (instancetype)initWithCompletionHandler:(void(^)())completionHandler;
- (void)presentUIForBackgroundCheck:(BOOL)backgroundCheck remoteURL:(NSURL*)remoteURL remoteVersion:(NSString*)remoteVersion redownloadEnabled:(BOOL)allowRedownload;
@end

// ==================
// = SoftwareUpdate =
// ==================

@interface SoftwareUpdate ()
{
	NSBackgroundActivityScheduler* _updateCheckScheduler;
	NSTimeInterval                 _updateCheckInterval;
}
@property (nonatomic, readwrite, getter = isChecking) BOOL checking;
@property (nonatomic, readwrite) NSString* errorString;
@property (nonatomic) BOOL automaticUpdateCheckEnabled;
@end

@implementation SoftwareUpdate
+ (instancetype)sharedInstance
{
	static SoftwareUpdate* sharedInstance = [self new];
	return sharedInstance;
}

+ (void)initialize
{
	[NSUserDefaults.standardUserDefaults registerDefaults:@{
		kUserDefaultsSoftwareUpdateChannelKey: kSoftwareUpdateChannelRelease
	}];
}

- (instancetype)init
{
	if(self = [super init])
	{
		_updateCheckInterval = 60*60;

		[NSNotificationCenter.defaultCenter addObserverForName:NSUserDefaultsDidChangeNotification object:NSUserDefaults.standardUserDefaults queue:NSOperationQueue.mainQueue usingBlock:^(NSNotification* notification){
			self.automaticUpdateCheckEnabled = ![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisableSoftwareUpdateKey];
		}];
		self.automaticUpdateCheckEnabled = ![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisableSoftwareUpdateKey];
	}
	return self;
}

- (void)setAutomaticUpdateCheckEnabled:(BOOL)flag
{
	if(_automaticUpdateCheckEnabled == flag)
		return;

	[_updateCheckScheduler invalidate];
	_updateCheckScheduler = nil;

	if(_automaticUpdateCheckEnabled = flag)
	{
		_updateCheckScheduler = [[NSBackgroundActivityScheduler alloc] initWithIdentifier:[NSString stringWithFormat:@"%@.%@", NSBundle.mainBundle.bundleIdentifier, @"SoftwareUpdate"]];
		_updateCheckScheduler.interval = _updateCheckInterval;
		_updateCheckScheduler.repeats  = YES;
		[_updateCheckScheduler scheduleWithBlock:^(NSBackgroundActivityCompletionHandler completionHandler){
			if(NSDate* suspendUntil = [NSUserDefaults.standardUserDefaults objectForKey:kUserDefaultsSoftwareUpdateSuspendUntilKey])
			{
				if([suspendUntil timeIntervalSinceNow] > 0)
				{
					os_log(OS_LOG_DEFAULT, "Skip version check: Suspended until %{public}@", suspendUntil);
					completionHandler(NSBackgroundActivityResultFinished);
					return;
				}
				[NSUserDefaults.standardUserDefaults removeObjectForKey:kUserDefaultsSoftwareUpdateSuspendUntilKey];
			}

			[self checkForTestBuild:NO completionHandler:^(NSURL* remoteURL, NSString* remoteVersion, NSError* error){
				self.errorString = error ? [NSString stringWithFormat:@"Error: %@", error.localizedDescription] : nil;
				if(error)
				{
					os_log(OS_LOG_DEFAULT, "Failed to check for update: %{public}@", error.localizedDescription);
					completionHandler(NSBackgroundActivityResultFinished);
				}
				else
				{
					SUDownloadViewController* alertViewController = [[SUDownloadViewController alloc] initWithCompletionHandler:^{
						completionHandler(NSBackgroundActivityResultFinished);
					}];
					[alertViewController presentUIForBackgroundCheck:YES remoteURL:remoteURL remoteVersion:remoteVersion redownloadEnabled:NO];
				}
			}];
		}];
	}
}

- (void)checkForUpdate:(id)sender
{
	BOOL isOptionDown = OakIsAlternateKeyOrMouseEvent(NSEventModifierFlagOption);
	BOOL isShiftDown  = OakIsAlternateKeyOrMouseEvent(NSEventModifierFlagShift);

	[self checkForTestBuild:isOptionDown completionHandler:^(NSURL* remoteURL, NSString* remoteVersion, NSError* error){
		SUDownloadViewController* alertViewController = [[SUDownloadViewController alloc] init];
		if(error)
				[alertViewController presentError:error];
		else	[alertViewController presentUIForBackgroundCheck:NO remoteURL:remoteURL remoteVersion:remoteVersion redownloadEnabled:isShiftDown];
	}];
}

- (void)checkForTestBuild:(BOOL)testBuild completionHandler:(void(^)(NSURL* remoteURL, NSString* remoteVersion, NSError* error))completionHandler
{
	NSString* updateChannel = testBuild ? kSoftwareUpdateChannelCanary : [NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsSoftwareUpdateChannelKey];
	if(!updateChannel)
		return completionHandler(nil, nil, [NSError errorWithDomain:@"SoftwareUpdate" code:0 userInfo:@{ NSLocalizedDescriptionKey: @"No channel configured." }]);

	NSURL* url = _channels[updateChannel];
	if(!url)
		return completionHandler(nil, nil, [NSError errorWithDomain:@"SoftwareUpdate" code:0 userInfo:@{ NSLocalizedDescriptionKey: [NSString stringWithFormat:@"No channel named ‘%@’.", updateChannel] }]);

	os_activity_initiate("Software update check", OS_ACTIVITY_FLAG_DEFAULT, ^{
		self.checking = YES;

		NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:url cachePolicy:NSURLRequestUseProtocolCachePolicy timeoutInterval:60];
		[request setValue:OakDownloadManager.sharedInstance.userAgentString forHTTPHeaderField:@"User-Agent"];

		NSURLSessionDataTask* dataTask = [NSURLSession.sharedSession dataTaskWithRequest:request completionHandler:^(NSData* data, NSURLResponse* response, NSError* error){
			[NSUserDefaults.standardUserDefaults setObject:[NSDate date] forKey:kUserDefaultsLastSoftwareUpdateCheckKey];
			self.checking = NO;

			NSURL* remoteURL;
			NSString* remoteVersion;

			if(!error)
			{
				if(NSString* contentType = ((NSHTTPURLResponse*)response).allHeaderFields[@"Content-Type"])
				{
					NSDictionary* plist;
					if([contentType isEqualToString:@"application/json"])
							plist = [NSJSONSerialization JSONObjectWithData:data options:0 error:nullptr];
					else	plist = [NSPropertyListSerialization propertyListWithData:data options:NSPropertyListImmutable format:nil error:nil];

					if(plist)
					{
						remoteURL     = plist[@"url"] ? [NSURL URLWithString:plist[@"url"]] : nil;
						remoteVersion = plist[@"version"];
						if(!remoteURL || !remoteVersion)
							error = [NSError errorWithDomain:@"SoftwareUpdate" code:0 userInfo:@{ NSLocalizedDescriptionKey: @"Incomplete server response." }];
					}
					else
					{
						error = [NSError errorWithDomain:@"SoftwareUpdate" code:0 userInfo:@{ NSLocalizedDescriptionKey: @"Malformed server response." }];
					}
				}
				else
				{
					error = [NSError errorWithDomain:@"SoftwareUpdate" code:0 userInfo:@{ NSLocalizedDescriptionKey: @"Missing Content-Type in server response." }];
				}
			}

			dispatch_async(dispatch_get_main_queue(), ^{
				completionHandler(remoteURL, remoteVersion, error);
			});
		}];
		[dataTask resume];
	});
}
@end

// ============================
// = SUDownloadViewController =
// ============================

@interface SUInfoViewController : NSViewController
@property (nonatomic) NSTextField* messageTextField;
@property (nonatomic) NSTextField* informativeTextField;
@end

@interface SUProgressViewController : NSViewController
@property (nonatomic) NSTextField*         messageTextField;
@property (nonatomic) NSTextField*         informativeTextField;
@property (nonatomic) NSProgressIndicator* progressIndicator;
@property (nonatomic) NSProgress*          progress;
@end

@interface SUDownloadViewController ()
{
	SUDownloadViewController* _retainedSelf;

	void(^_completionHandler)();
	BOOL(^_runModalCompletionHandler)(NSModalResponse);

	NSURL* _downloadedArchiveURL;
	NSURL* _remoteURL;

	NSStackView* _buttonStackView;
}
@property (nonatomic, getter = isUpdateBadgeVisible) BOOL updateBadgeVisible;
@property (nonatomic) NSDictionary<NSString*, NSString*>* publicKeys;

@property (nonatomic) OakTransitionViewController*  contentViewController;
@property (nonatomic) SUInfoViewController*         infoViewController;
@property (nonatomic) SUProgressViewController*     progressViewController;
@property (nonatomic, readonly) NSArray<NSButton*>* buttons;
- (NSButton*)addButtonWithTitle:(NSString*)title;
@end

@implementation SUDownloadViewController
- (instancetype)initWithCompletionHandler:(void(^)())completionHandler
{
	if(self = [super initWithNibName:nil bundle:nil])
	{
		_completionHandler      = completionHandler;
		_publicKeys             = NSBundle.mainBundle.infoDictionary[@"TMSigningKeys"];

		_contentViewController  = [[OakTransitionViewController alloc] init];
		_infoViewController     = [[SUInfoViewController alloc] init];
		_progressViewController = [[SUProgressViewController alloc] init];

		self.title = @"";
	}
	return self;
}

- (instancetype)init
{
	return [self initWithCompletionHandler:nil];
}

- (void)dealloc
{
	if(_completionHandler)
		_completionHandler();
}

- (NSStackView*)buttonStackView
{
	if(!_buttonStackView)
	{
		_buttonStackView = [[NSStackView alloc] initWithFrame:NSZeroRect];
		_buttonStackView.spacing = 16;
		[_buttonStackView setHuggingPriority:NSLayoutPriorityDefaultHigh-1 forOrientation:NSLayoutConstraintOrientationVertical];
	}
	return _buttonStackView;
}

- (NSArray<NSButton*>*)buttons
{
	return self.buttonStackView.views.reverseObjectEnumerator.allObjects;
}

- (NSButton*)addButtonWithTitle:(NSString*)title
{
	NSUInteger countOfButtons = self.buttons.count;

	NSButton* button = [NSButton buttonWithTitle:title target:self action:@selector(didClickButton:)];
	button.tag = NSAlertFirstButtonReturn + countOfButtons;
	if(countOfButtons == 0)
		button.keyEquivalent = @"\r";
	else if([title isEqualToString:@"Cancel"])
		button.keyEquivalent = @"\e";

	[button setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
	NSLayoutConstraint* widthConstraint = [button.widthAnchor constraintEqualToConstant:86];
	widthConstraint.priority = NSLayoutPriorityDefaultHigh;
	widthConstraint.active = YES;

	[self.buttonStackView insertView:button atIndex:0 inGravity:NSStackViewGravityTrailing];

	return button;
}

- (void)loadView
{
	NSImage* image = [NSImage imageNamed:NSImageNameApplicationIcon];
	image.size = NSMakeSize(64, 64);

	NSDictionary* views = @{
		@"image":   [NSImageView imageViewWithImage:image],
		@"content": self.contentViewController.view,
		@"buttons": self.buttonStackView,
	};

	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];
	OakAddAutoLayoutViewsToSuperview(views.allValues, contentView);

	[NSLayoutConstraint activateConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(24)-[image(==64)]-(16)-[content]-|"          options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[NSLayoutConstraint activateConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[image]-(>=20)-[buttons]-|"                     options:0                         metrics:nil views:views]];
	[NSLayoutConstraint activateConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(16)-[image(==64)]-(>=20)-|"                  options:0                         metrics:nil views:views]];
	[NSLayoutConstraint activateConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[content]-(==20@750,>=20@250)-[buttons]-(18)-|" options:0                         metrics:nil views:views]];

	self.view = contentView;
}

- (void)viewWillAppear
{
	_retainedSelf = self;
}

- (void)viewDidDisappear
{
	self.updateBadgeVisible = NO;
	[_progressViewController.progress cancel];

	if(_downloadedArchiveURL)
	{
		NSError* error;
		if(![NSFileManager.defaultManager removeItemAtURL:_downloadedArchiveURL error:&error])
			os_log_error(OS_LOG_DEFAULT, "Unable to remove %{public}@: %{public}@", _downloadedArchiveURL.path, error.localizedDescription);
		_downloadedArchiveURL = nil;
	}

	_retainedSelf = nil;
}

- (BOOL)presentError:(NSError*)error
{
	self.contentViewController.subview = self.infoViewController.view;

	self.infoViewController.messageTextField.stringValue     = @"Error Checking for Update";
	self.infoViewController.informativeTextField.stringValue = error.localizedDescription;
	[self addButtonWithTitle:@"OK"];

	[self runModalWithCompletionHandler:nil];

	return YES;
}

- (void)presentAlertWithMessage:(NSString*)messageText informativeText:(NSString*)informativeText buttonTitles:(NSArray<NSString*>*)buttonTitles completionHandler:(BOOL(^)(NSModalResponse))completionHandler
{
	NSAlert* alert = [[NSAlert alloc] init];

	alert.messageText     = messageText;
	alert.informativeText = informativeText;

	for(NSString* title in buttonTitles)
		[alert addButtonWithTitle:title];

	[alert beginSheetModalForWindow:self.view.window completionHandler:^(NSModalResponse returnCode){
		if(!completionHandler || completionHandler(returnCode))
			[self.view.window close];
	}];
}

- (void)runModalWithCompletionHandler:(BOOL(^)(NSModalResponse))completionHandler
{
 	NSWindow* window = [NSPanel windowWithContentViewController:self];

	window.animationBehavior       = NSWindowAnimationBehaviorAlertPanel;
	window.excludedFromWindowsMenu = YES;
	window.hidesOnDeactivate       = NO;
	window.level                   = NSModalPanelWindowLevel;
	window.styleMask               = NSWindowStyleMaskTitled;

	// If we use -[NSApplication runModalForWindow:] then the window
	// won’t stay above document windows after the modal session ends
	_runModalCompletionHandler = completionHandler;
	[window makeKeyAndOrderFront:self];
}

- (void)didClickButton:(id)sender
{
	if(!_runModalCompletionHandler || _runModalCompletionHandler([sender tag]))
		[self.view.window close];
	_runModalCompletionHandler = nil;
}

- (void)setUpdateBadgeVisible:(BOOL)flag
{
	if(_updateBadgeVisible == flag)
		return;

	if(_updateBadgeVisible = flag)
	{
		if(NSImage* dlBadge = [NSImage imageNamed:@"Update Badge" inSameBundleAsClass:[self class]])
		{
			NSImage* appIcon = NSApp.applicationIconImage;
			NSApp.applicationIconImage = [NSImage imageWithSize:appIcon.size flipped:NO drawingHandler:^BOOL(NSRect dstRect){
				NSRect upperRightRect = NSIntersectionRect(dstRect, NSOffsetRect(dstRect, round(NSWidth(dstRect) * 2 / 3), (NSHeight(dstRect) * 2 / 3)));
				[appIcon drawInRect:dstRect fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:1];
				[dlBadge drawInRect:upperRightRect fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:1];
				return YES;
			}];
		}
	}
	else
	{
		NSApp.applicationIconImage = nil;
	}
}

- (void)presentUIForBackgroundCheck:(BOOL)backgroundCheck remoteURL:(NSURL*)remoteURL remoteVersion:(NSString*)remoteVersion redownloadEnabled:(BOOL)allowRedownload
{
	NSString* localVersion = [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleShortVersionString"];
	NSComparisonResult ordering = OakCompareVersionStrings(localVersion, remoteVersion);

	if(backgroundCheck && ordering != NSOrderedAscending)
		return;

	self.contentViewController.subview = self.infoViewController.view;

	if(ordering == NSOrderedAscending)
	{
		self.infoViewController.messageTextField.stringValue     = @"New Version Available";
		self.infoViewController.informativeTextField.stringValue = [NSString stringWithFormat: @"%@ %@ is now available. You have version %@. Would you like to download it now?", [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleName"], remoteVersion, localVersion];

		[self addButtonWithTitle:@"Download"];
		[self addButtonWithTitle:backgroundCheck ? @"Later" : @"Cancel"];
		self.buttons.lastObject.keyEquivalent = @"\e";
	}
	else if(ordering == NSOrderedSame)
	{
		self.infoViewController.messageTextField.stringValue     = @"Up To Date";
		self.infoViewController.informativeTextField.stringValue = [NSString stringWithFormat:@"You are running %@ which is the latest version available.", remoteVersion];

		[self addButtonWithTitle:@"OK"];
		if(allowRedownload)
			[self addButtonWithTitle:@"Redownload"];
	}
	else if(ordering == NSOrderedDescending)
	{
		self.infoViewController.messageTextField.stringValue     = @"You are Using a Prerelease";
		self.infoViewController.informativeTextField.stringValue = [NSString stringWithFormat:@"%@ is the latest version available. You have version %@.", remoteVersion, localVersion];

		[self addButtonWithTitle:@"OK"];
		[self addButtonWithTitle:[NSString stringWithFormat:@"Downgrade to %@", remoteVersion]];
	}

	[self runModalWithCompletionHandler:^BOOL(NSModalResponse response){
		if(response == (ordering == NSOrderedAscending ? NSAlertFirstButtonReturn : NSAlertSecondButtonReturn))
		{
			struct statfs sfsb;
			if(statfs(NSBundle.mainBundle.bundlePath.fileSystemRepresentation, &sfsb) == 0 && (sfsb.f_flags & MNT_RDONLY))
			{
				NSString* informativeText = [NSString stringWithFormat:@"%1$@ is running on a read-only file system and can therefore not be updated.\n\nIf you downloaded %1$@ from the internet then moving it out of the Downloads folder should solve the problem.", [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleName"]];
				[self presentAlertWithMessage:@"Read-only File System" informativeText:informativeText buttonTitles:@[ @"OK" ] completionHandler:^BOOL(NSModalResponse returnCode){
					return YES; // Close window
				}];
			}
			else
			{
				[self downloadSoftwareUpdateAtURL:remoteURL];
			}
			return NO; // Keep window open
		}
		else
		{
			if(backgroundCheck)
				[NSUserDefaults.standardUserDefaults setObject:[[NSDate date] dateByAddingTimeInterval:24*60*60] forKey:kUserDefaultsSoftwareUpdateSuspendUntilKey];
			return YES; // Close window
		}
	}];
}

- (void)cancel:(id)sender
{
	[self.view.window close];
}

- (void)takeURLToDownloadFrom:(NSButton*)sender
{
	[self downloadSoftwareUpdateAtURL:sender.cell.representedObject];
}

- (void)downloadSoftwareUpdateAtURL:(NSURL*)downloadURL
{
	_remoteURL = downloadURL;

	id <NSProgressReporting> progressReporting = [OakDownloadManager.sharedInstance downloadArchiveAtURL:downloadURL forReplacingURL:NSBundle.mainBundle.bundleURL publicKeys:self.publicKeys completionHandler:^(NSURL* extractedArchiveURL, NSError* error){
		self.progressViewController.progress = nil;

		if(extractedArchiveURL)
		{
			self.updateBadgeVisible = YES;
			if(NSApp.isActive)
				OakPlayUISound(OakSoundDidCompleteSomethingUISound);

			self.progressViewController.messageTextField.stringValue = [NSString stringWithFormat:@"Downloaded %@", downloadURL.lastPathComponent];

			self.buttons[0].enabled                = YES;
			self.buttons[0].cell.representedObject = extractedArchiveURL;
			self.buttons[0].action                 = @selector(takeURLToInstallFrom:);

			_downloadedArchiveURL = extractedArchiveURL; // Will be deleted in viewDidDisappear
		}
		else
		{
			self.progressViewController.messageTextField.stringValue     = @"Error Downloading Update";
			self.progressViewController.informativeTextField.stringValue = error.localizedDescription ?: @"";

			self.buttons[0].title                  = @"Retry";
			self.buttons[0].enabled                = YES;
			self.buttons[0].cell.representedObject = downloadURL;
			self.buttons[0].action                 = @selector(takeURLToDownloadFrom:);
		}
	}];

	self.progressViewController.progress = progressReporting.progress;
	self.contentViewController.subview = self.progressViewController.view;

	self.buttons[0].title         = @"Install & Relaunch";
	self.buttons[0].enabled       = NO;

	self.buttons[1].title         = @"Cancel";
	self.buttons[1].action        = @selector(cancel:);
	self.buttons[1].keyEquivalent = @"\e";
}

- (BOOL)isInstallableApplicationAtURL:(NSURL*)applicationURL
{
	NSError* error;
	NSNumber* boolean;
	BOOL res = [[applicationURL URLByAppendingPathComponent:[NSString stringWithFormat:@"Contents/MacOS/%@", [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleName"]] isDirectory:NO] getResourceValue:&boolean forKey:NSURLIsExecutableKey error:&error] && boolean.boolValue;
	if(!res && error)
		os_log_error(OS_LOG_DEFAULT, "Failed checking if %{public}@ has an executable: %{public}@", applicationURL.path, error.localizedDescription);
	return res;
}

- (void)takeURLToInstallFrom:(NSButton*)sender
{
	NSURL* applicationURL = sender.cell.representedObject;

	if([self isInstallableApplicationAtURL:applicationURL])
	{
		self.progressViewController.messageTextField.stringValue     = [NSString stringWithFormat:@"Installing %@…", [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleName"]];
		self.progressViewController.informativeTextField.stringValue = @"";
		self.progressViewController.progressIndicator.indeterminate  = YES;
		[self.progressViewController.progressIndicator startAnimation:self];

		self.buttons[0].enabled = NO;
		self.buttons[1].enabled = NO;

		NSError* error;
		if([NSFileManager.defaultManager replaceItemAtURL:NSBundle.mainBundle.bundleURL withItemAtURL:applicationURL backupItemName:nil options:NSFileManagerItemReplacementUsingNewMetadataOnly resultingItemURL:nil error:&error])
		{
			self.progressViewController.messageTextField.stringValue = [NSString stringWithFormat:@"Relaunching %@…", [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleName"]];

			NSString* script = [NSString stringWithFormat:@"{ kill %1$d; while ps -xp %1$d; do if (( ++n == 300 )); then exit; fi; sleep .2; done; open \"$0\" --args $1; } &>/dev/null &", getpid()];

			NSTask* task = [[NSTask alloc] init];
			task.launchPath     = @"/bin/sh";
			task.arguments      = @[ @"-c", script, NSBundle.mainBundle.bundlePath, @"-showReleaseNotes YES" ];
			task.standardInput  = NSFileHandle.fileHandleWithNullDevice;
			task.standardOutput = NSFileHandle.fileHandleWithNullDevice;
			task.standardError  = NSFileHandle.fileHandleWithNullDevice;

			@try {
				[task launch];
			}
			@catch (NSException* e) {
				os_log_error(OS_LOG_DEFAULT, "-[NSTask launch]: %{public}@", e.reason);
			}
		}
		else
		{
			[self.progressViewController.progressIndicator stopAnimation:self];
			self.progressViewController.progressIndicator.indeterminate = NO;

			[self presentAlertWithMessage:@"Failed to Install Update" informativeText:error.localizedDescription buttonTitles:@[ @"Retry", @"Cancel" ] completionHandler:^BOOL(NSModalResponse returnCode){
				if(returnCode == NSAlertFirstButtonReturn)
					[self takeURLToInstallFrom:sender];
				return returnCode == NSAlertSecondButtonReturn; // Close window if clicking “Cancel”
			}];
		}
	}
	else
	{
		[self presentAlertWithMessage:@"Integrity Check Failed" informativeText:@"The download is incomplete. This can happen if the system has been deleting temporary files.\n\nWould you like to redownload the update?" buttonTitles:@[ @"Redownload", @"Cancel" ] completionHandler:^BOOL(NSModalResponse returnCode){
			if(returnCode == NSAlertFirstButtonReturn)
			{
				[self downloadSoftwareUpdateAtURL:_remoteURL];

				NSError* error;
				if(![NSFileManager.defaultManager removeItemAtURL:applicationURL error:&error])
					os_log_error(OS_LOG_DEFAULT, "Unable to remove %{public}@: %{public}@", applicationURL.path, error.localizedDescription);

				_downloadedArchiveURL = nil;
			}
			return returnCode == NSAlertSecondButtonReturn; // Close window if clicking “Cancel”
		}];
	}
}
@end

// ========================
// = SUInfoViewController =
// ========================

@implementation SUInfoViewController
- (void)loadView
{
	_messageTextField     = [NSTextField labelWithString:@"New Version Available"];
	_informativeTextField = [NSTextField wrappingLabelWithString:@"Would you like to download and install?"];

	NSStackView* stackView = [NSStackView stackViewWithViews:@[
		_messageTextField, _informativeTextField
	]];

	stackView.orientation = NSUserInterfaceLayoutOrientationVertical;
	stackView.alignment   = NSLayoutAttributeLeading;
	[stackView setHuggingPriority:NSLayoutPriorityDefaultHigh-1 forOrientation:NSLayoutConstraintOrientationVertical];

	_messageTextField.selectable     = YES;
	_messageTextField.font           = [NSFont boldSystemFontOfSize:0];
	_informativeTextField.selectable = YES;
	_informativeTextField.font       = [NSFont messageFontOfSize:NSFont.smallSystemFontSize];

	[stackView.widthAnchor constraintEqualToConstant:298].active = YES;

	self.view = stackView;
}
@end

// ============================
// = SUProgressViewController =
// ============================

@interface SUProgressViewController ()
{
	NSTimer* _checkProgressTimer;
}
@end

@implementation SUProgressViewController
- (void)loadView
{
	_messageTextField     = [NSTextField labelWithString:@"Downloading Archive…"];
	_progressIndicator    = [[NSProgressIndicator alloc] initWithFrame:NSZeroRect];
	_informativeTextField = [NSTextField labelWithString:@"999.9 MB of 999.9 MB — About 59 minutes, 59 seconds remaining"];

	_messageTextField.selectable     = YES;
	_progressIndicator.maxValue      = 1;
	_progressIndicator.indeterminate = NO;
	_informativeTextField.font       = [NSFont monospacedDigitSystemFontOfSize:NSFont.smallSystemFontSize weight:NSFontWeightRegular];
	_informativeTextField.selectable = YES;

	NSStackView* stackView = [NSStackView stackViewWithViews:@[
		_messageTextField, _progressIndicator, _informativeTextField
	]];
	stackView.spacing     = 0;
	stackView.orientation = NSUserInterfaceLayoutOrientationVertical;
	stackView.alignment   = NSLayoutAttributeLeading;

	[stackView.widthAnchor constraintGreaterThanOrEqualToConstant:_informativeTextField.fittingSize.width + 20].active = YES;

	self.view = stackView;
}

- (void)viewWillAppear
{
	if(_progress)
		[self checkProgressTimerDidFire:nil];
}

- (void)viewDidDisappear
{
	[_checkProgressTimer invalidate];
}

- (void)setProgress:(NSProgress*)newProgress
{
	if(_progress && !newProgress)
		[self checkProgressTimerDidFire:nil];

	if(_progress = newProgress)
	{
		[self checkProgressTimerDidFire:nil];

		_checkProgressTimer = [NSTimer scheduledTimerWithTimeInterval:0.04 target:self selector:@selector(checkProgressTimerDidFire:) userInfo:nil repeats:YES];
		[self checkProgressTimerDidFire:_checkProgressTimer];
	}
	else
	{
		[_checkProgressTimer invalidate];
		_checkProgressTimer = nil;
	}
}

- (void)checkProgressTimerDidFire:(NSTimer*)timer
{
	_messageTextField.stringValue     = _progress.localizedDescription;
	_informativeTextField.stringValue = _progress.isIndeterminate ? @"Estimating time remaining." : _progress.localizedAdditionalDescription;
	_progressIndicator.doubleValue    = _progress.fractionCompleted;
}
@end
