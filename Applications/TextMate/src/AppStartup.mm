#import "AppStartup.h"
#import "AppController.h"
#import <DocumentWindow/DocumentController.h>
#import "ODBEditorSuite.h"
#import "TMPlugInController.h"
#import "RMateServer.h"
#import <Preferences/Keys.h>
#import <Preferences/TerminalPreferences.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSEvent Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <CrashReporter/CrashReporter.h>
#import <SoftwareUpdate/SoftwareUpdate.h>
#import <BundlesManager/BundlesManager.h>
#import <io/path.h>
#import <bundles/bundles.h>
#import <ns/ns.h>
#import <network/tbz.h>
#import <oak/server.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(AppStartup);

@implementation AppStartupController
@synthesize openEvent, openDocumentsArray;

- (void)userDefaultsDidChange:(id)sender
{
	BOOL disableRmate        = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableRMateServerKey];
	NSString* rmateInterface = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsRMateServerListenKey];
	int rmatePort            = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsRMateServerPortKey];
	setup_rmate_server(!disableRmate, [rmateInterface isEqualToString:kRMateServerListenRemote] ? INADDR_ANY : INADDR_LOOPBACK, rmatePort);
}

- (void)applicationWillFinishLaunching:(NSNotification*)aNotification
{
	D(DBF_AppStartup, bug("\n"););
	settings_t::set_default_settings_path([[[NSBundle mainBundle] pathForResource:@"Default" ofType:@"tmProperties"] fileSystemRepresentation]);
	settings_t::set_global_settings_path(path::join(path::home(), "Library/Application Support/TextMate/Global.tmProperties"));

	[[NSUserDefaults standardUserDefaults] registerDefaults:[NSDictionary dictionaryWithObjectsAndKeys:
		NO_obj, @"ApplePressAndHoldEnabled",
		@25,    @"NSRecentDocumentsLimit",
		nil]];
	RegisterDefaults();
	[[NSUserDefaults standardUserDefaults] setObject:NO_obj forKey:@"NSQuitAlwaysKeepsWindows"];
	disableSessionRestore = ([NSEvent slModifierFlags] & NSShiftKeyMask) == NSShiftKeyMask;

	std::string dest = path::join(path::home(), "Library/Application Support/TextMate/Managed");
	if(!path::exists(dest))
	{
		if(NSString* archive = [[NSBundle mainBundle] pathForResource:@"DefaultBundles" ofType:@"tbz"])
		{
			int input, output;
			std::string error;

			path::make_dir(dest);

			pid_t pid = network::launch_tbz(dest, input, output, error);
			if(pid != -1)
			{
				int fd = open([archive fileSystemRepresentation], O_RDONLY);
				if(fd != -1)
				{
					char buf[4096];
					ssize_t len;
					while((len = read(fd, buf, sizeof(buf))) > 0)
					{
						if(write(input, buf, len) != len)
						{
							fprintf(stderr, "*** error wrting bytes to tar\n");
							break;
						}
					}
					close(fd);
				}

				if(!network::finish_tbz(pid, input, output, error))
					fprintf(stderr, "%s\n", error.c_str());
			}
			else
			{
				fprintf(stderr, "%s\n", error.c_str());
			}
		}
	}

	bundles::build_index(path::join(path::home(), "Library/Application Support/TextMate/Cache"));
}

- (BOOL)application:(NSApplication*)theApplication openFile:(NSString*)aPath
{
	D(DBF_AppStartup, bug("%s\n", [aPath UTF8String]););
	self.openEvent          = [[NSAppleEventManager sharedAppleEventManager] currentAppleEvent];
	self.openDocumentsArray = @[ aPath ];
	return YES;
}

- (void)application:(NSApplication*)sender openFiles:(NSArray*)filenames
{
	D(DBF_AppStartup, bug("%s\n", [[filenames description] UTF8String]););
	self.openEvent          = [[NSAppleEventManager sharedAppleEventManager] currentAppleEvent];
	self.openDocumentsArray = filenames;
	[sender replyToOpenOrPrint:NSApplicationDelegateReplySuccess];
}

- (BOOL)applicationShouldOpenUntitledFile:(NSApplication*)anApplication
{
	D(DBF_AppStartup, bug("\n"););
	return NO;
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
	D(DBF_AppStartup, bug("\n"););

	OakSubmitNewCrashReportsInBackground(REST_API @"/crashes");

	[[TMPlugInController sharedInstance] loadAllPlugIns:nil];
	[self userDefaultsDidChange:nil]; // setup mate/rmate server

	[BundlesManager sharedInstance]; // trigger periodic polling of remote bundle index

	BOOL disableSessionRestorePrefs    = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableSessionRestoreKey];
	BOOL disableUntitledAtStartupPrefs = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableNewDocumentAtStartupKey];

	BOOL didRestoreSession = !disableSessionRestorePrefs && !disableSessionRestore && [appController loadSession:self];
	BOOL didOpenDocuments  = DidHandleODBEditorEvent([self.openEvent aeDesc]) || ([self.openDocumentsArray count] && (OakOpenDocuments(self.openDocumentsArray), YES));
	if(!disableUntitledAtStartupPrefs && !didRestoreSession && !didOpenDocuments && getenv("OAK_DISABLE_UNTITLED_FILE") == NULL)
		[[[DocumentController alloc] init] showWindow:nil];

	SoftwareUpdate* swUpdate = [SoftwareUpdate sharedInstance];
	[swUpdate setSignee:key_chain_t::key_t("org.textmate.duff", "Allan Odgaard", "-----BEGIN PUBLIC KEY-----\nMIIBtjCCASsGByqGSM44BAEwggEeAoGBAPIE9PpXPK3y2eBDJ0dnR/D8xR1TiT9m\n8DnPXYqkxwlqmjSShmJEmxYycnbliv2JpojYF4ikBUPJPuerlZfOvUBC99ERAgz7\nN1HYHfzFIxVo1oTKWurFJ1OOOsfg8AQDBDHnKpS1VnwVoDuvO05gK8jjQs9E5LcH\ne/opThzSrI7/AhUAy02E9H7EOwRyRNLofdtPxpa10o0CgYBKDfcBscidAoH4pkHR\nIOEGTCYl3G2Pd1yrblCp0nCCUEBCnvmrWVSXUTVa2/AyOZUTN9uZSC/Kq9XYgqwj\nhgzqa8h/a8yD+ao4q8WovwGeb6Iso3WlPl8waz6EAPR/nlUTnJ4jzr9t6iSH9owS\nvAmWrgeboia0CI2AH++liCDvigOBhAACgYAFWO66xFvmF2tVIB+4E7CwhrSi2uIk\ndeBrpmNcZZ+AVFy1RXJelNe/cZ1aXBYskn/57xigklpkfHR6DGqpEbm6KC/47Jfy\ny5GEx+F/eBWEePi90XnLinytjmXRmS2FNqX6D15XNG1xJfjociA8bzC7s4gfeTUd\nlpQkBq2z71yitA==\n-----END PUBLIC KEY-----\n")];
	[swUpdate setChannels:[NSDictionary dictionaryWithObjectsAndKeys:
		[NSURL URLWithString:REST_API @"/releases/release"],  kSoftwareUpdateChannelRelease,
		[NSURL URLWithString:REST_API @"/releases/beta"],     kSoftwareUpdateChannelBeta,
		[NSURL URLWithString:REST_API @"/releases/nightly"],  kSoftwareUpdateChannelNightly,
		nil]];

	self.openEvent          = nil;
	self.openDocumentsArray = nil;
	unsetenv("OAK_DISABLE_UNTITLED_FILE");

	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	[TerminalPreferences updateMateIfRequired];

	[appController setup];
}

- (BOOL)applicationShouldHandleReopen:(NSApplication*)theApplication hasVisibleWindows:(BOOL)flag
{
	D(DBF_AppStartup, bug("%s\n", BSTR(flag)););
	return NO;
}

- (IBAction)newDocument:(id)sender
{
	// avoid NSDocumentController’s implementation
}

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	if([menuItem action] == @selector(newDocument:))
		return NO;
	return YES;
}
@end