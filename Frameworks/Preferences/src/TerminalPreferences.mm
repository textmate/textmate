#import "TerminalPreferences.h"
#import "Keys.h"
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/OakSavePanel.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <io/path.h>
#import <io/exec.h>
#import <ns/ns.h>
#import <regexp/format_string.h>
#import <bundles/bundles.h>

static void CreateHyperLink (NSTextField* textField, NSString* text, NSString* url)
{
	[textField setAllowsEditingTextAttributes:YES];
	[textField setSelectable:YES];

	NSAttributedString* str = [textField attributedStringValue];
	NSRange range = [[str string] rangeOfString:text];

	NSMutableAttributedString* attrString = [[str mutableCopy] autorelease];
	[attrString beginEditing];
	[attrString addAttribute:NSLinkAttributeName value:url range:range];
	[attrString addAttribute:NSForegroundColorAttributeName value:[NSColor blueColor] range:range];
	[attrString addAttribute:NSUnderlineStyleAttributeName value:@(NSSingleUnderlineStyle) range:range];
	[attrString endEditing];

	[textField setAttributedStringValue:attrString];
}

static bool run_auth_command (AuthorizationRef& auth, std::string const& cmd, ...)
{
	if(!auth && AuthorizationCreate(NULL, kAuthorizationEmptyEnvironment, kAuthorizationFlagDefaults, &auth) != errAuthorizationSuccess)
		return false;

	std::vector<char*> args;

	va_list ap;
	va_start(ap, cmd);
	char* arg = NULL;
	while((arg = va_arg(ap, char*)) && *arg)
		args.push_back(arg);
	va_end(ap);

	args.push_back(NULL);

	bool res = false;
	if(AuthorizationExecuteWithPrivileges(auth, cmd.c_str(), kAuthorizationFlagDefaults, &args[0], NULL) == errAuthorizationSuccess)
	{
		int status;
		int pid = wait(&status);
		if(pid != -1 && WIFEXITED(status) && WEXITSTATUS(status) == 0)
				res = true;
		else	errno = WEXITSTATUS(status);
	}
	else
	{
		errno = EPERM;
	}
	return res;
}

static bool mk_dir (std::string const& path, AuthorizationRef& auth)
{
	struct stat buf;
	if(stat(path.c_str(), &buf) == 0)
	{
		if(S_ISDIR(buf.st_mode))
			return true;
	}
	else if(path != "/" && mk_dir(path::parent(path), auth))
	{
		if(access(path::parent(path).c_str(), W_OK) == 0)
		{
			if(mkdir(path.c_str(), S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IWGRP|S_IXGRP|S_IROTH|S_IWOTH|S_IXOTH) == 0)
				return true;
			perror(("mkdir(" + path + ")").c_str());
		}
		else
		{
			if(run_auth_command(auth, "/bin/mkdir", path.c_str(), NULL))
				return true;
			perror(("/bin/mkdir " + path).c_str());
		}
	}
	return false;
}

static bool rm_path (std::string const& path, AuthorizationRef& auth)
{
	struct stat buf;
	if(lstat(path.c_str(), &buf) != 0)
		return true;

	if(access(path::parent(path).c_str(), W_OK) == 0)
	{
		if(unlink(path.c_str()) == 0)
			return true;
		perror(("unlink " + path).c_str());
	}
	else
	{
		if(run_auth_command(auth, "/bin/rm", path.c_str(), NULL))
			return true;
		perror(("/bin/rm " + path).c_str());
	}
	return false;
}

static bool cp_path (std::string const& src, std::string const& dst, AuthorizationRef& auth)
{
	if(access(path::parent(dst).c_str(), W_OK) == 0)
	{
		if(copyfile(src.c_str(), dst.c_str(), NULL, COPYFILE_ALL | COPYFILE_NOFOLLOW_SRC) == 0)
			return true;
		perror(("copyfile(" + src + ", " + dst + ")").c_str());
	}
	else
	{
		if(run_auth_command(auth, "/bin/cp", "-p", src.c_str(), dst.c_str(), NULL))
			return true;
		perror(("/bin/cp -p " + src + " " + dst).c_str());
	}
	return false;
}

static bool install_mate (std::string const& src, std::string const& dst)
{
	AuthorizationRef auth = NULL;
	if(mk_dir(path::parent(dst), auth))
	{
		struct stat buf;
		if(lstat(dst.c_str(), &buf) == 0 && !rm_path(dst, auth))
			return false;
		return cp_path(src, dst, auth);
	}
	return false;
}

static bool uninstall_mate (std::string const& path)
{
	AuthorizationRef auth = NULL;
	return access(path.c_str(), F_OK) != 0 || rm_path(path, auth);
}

@implementation TerminalPreferences
@synthesize installIndicaitorImage;

- (id)init
{
	if(self = [super initWithNibName:@"TerminalPreferences" label:@"Terminal" image:[NSImage imageNamed:@"Terminal" inSameBundleAsClass:[self class]]])
	{
		[OakStringListTransformer createTransformerWithName:@"OakRMateInterfaceTransformer" andObjectsArray:@[ kRMateServerListenLocalhost, kRMateServerListenRemote ]];

		self.defaultsProperties = [NSDictionary dictionaryWithObjectsAndKeys:
			kUserDefaultsMateInstallPathKey,    @"path",
			kUserDefaultsDisableRMateServerKey, @"disableRMate",
			kUserDefaultsRMateServerListenKey,  @"interface",
			kUserDefaultsRMateServerPortKey,    @"port",
		nil];
	}
	return self;
}

- (void)selectInstallPath:(id)sender
{
	[OakSavePanel showWithPath:@"mate" directory:nil fowWindow:[self view].window delegate:self contextInfo:NULL];
}

- (void)savePanelDidEnd:(OakSavePanel*)sheet path:(NSString*)path contextInfo:(void*)info
{
	if(path)
			[self updatePopUp:path];
	else	[installPathPopUp selectItemAtIndex:0];
	[self updateUI:self];
}

- (void)updatePopUp:(NSString*)path
{
	NSMenu* menu = [installPathPopUp menu];
	[menu removeAllItems];

	path = [path stringByAbbreviatingWithTildeInPath];
	if(path && ![path isEqualToString:@"~/bin/mate"] && ![path isEqualToString:@"/usr/local/bin/mate"])
		[menu addItemWithTitle:path action:@selector(updateUI:) keyEquivalent:@""];
	[menu addItemWithTitle:@"/usr/local/bin/mate" action:@selector(updateUI:) keyEquivalent:@""];
	[menu addItemWithTitle:@"~/bin/mate" action:@selector(updateUI:) keyEquivalent:@""];
	[menu addItem:[NSMenuItem separatorItem]];
	[menu addItemWithTitle:@"Other…" action:@selector(selectInstallPath:) keyEquivalent:@""];

	if(path)
		[installPathPopUp selectItemWithTitle:path];
}

- (void)updateUI:(id)sender
{
	BOOL isInstalled = self.mateInstallPath ? YES : NO;

	std::map<std::string, std::string> variables;
	if(isInstalled)
		variables["installed"] = "installed";
	variables["mate_path"] = to_s([self.mateInstallPath stringByAbbreviatingWithTildeInPath] ?: [installPathPopUp titleOfSelectedItem]);

	[installStatusText setStringValue:[NSString stringWithCxxString:format_string::expand(statusTextFormat, variables)]];
	[installSummaryText setStringValue:[NSString stringWithCxxString:format_string::expand(summaryTextFormat, variables)]];
	self.installIndicaitorImage = [NSImage imageNamed:(isInstalled ? @"Light-on" : @"Light-off") inSameBundleAsClass:[self class]];

	[installPathPopUp setEnabled:isInstalled ? NO : YES];
	[installButton setAction:isInstalled ? @selector(performUninstallMate:) : @selector(performInstallMate:)];
	[installButton setState:isInstalled ? NSOnState : NSOffState];
}

- (void)loadView
{
	[super loadView];

	if(NSString* path = self.mateInstallPath)
	{
		if(access([path fileSystemRepresentation], F_OK) != 0)
		{
			[[NSUserDefaults standardUserDefaults] removeObjectForKey:kUserDefaultsMateInstallPathKey];
			[[NSUserDefaults standardUserDefaults] removeObjectForKey:kUserDefaultsMateInstallVersionKey];
		}
	}

	installPathPopUp.target = self;
	statusTextFormat  = to_s([installStatusText stringValue]);
	summaryTextFormat = to_s([installSummaryText stringValue]);
	[self updatePopUp:self.mateInstallPath];
	[self updateUI:self];

	CreateHyperLink(rmateSummaryText, @"rmate", [NSString stringWithFormat:@"txmt://open?url=%@", [[NSURL fileURLWithPath:[[NSBundle bundleForClass:[self class]] pathForResource:@"rmate" ofType:@""]] absoluteString]]);
	LSSetDefaultHandlerForURLScheme(CFSTR("txmt"), (CFStringRef)[[NSBundle mainBundle] bundleIdentifier]);
}

- (NSString*)mateInstallPath
{
	NSString* path = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsMateInstallPathKey];
	return [path stringByExpandingTildeInPath];
}

- (void)setMateInstallPath:(NSString*)aPath
{
	if(aPath)
			[[NSUserDefaults standardUserDefaults] setObject:aPath forKey:kUserDefaultsMateInstallPathKey];
	else	[[NSUserDefaults standardUserDefaults] removeObjectForKey:kUserDefaultsMateInstallPathKey];
}

- (void)installMateAs:(NSString*)dstPath
{
	if(NSString* srcPath = [[NSBundle mainBundle] pathForResource:@"mate" ofType:nil])
	{
		if(install_mate(to_s(srcPath), to_s(dstPath)))
		{
			[self setMateInstallPath:dstPath];
			std::string res = io::exec(to_s(srcPath), "--version", NULL);
			if(regexp::match_t const& m = regexp::search("\\Amate ([\\d.]+)", res.data(), res.data() + res.size()))
				[[NSUserDefaults standardUserDefaults] setObject:[NSString stringWithUTF8String:res.data() + m.begin(1) length:m.end(1) - m.begin(1)] forKey:kUserDefaultsMateInstallVersionKey];
		}
	}
	else
	{
		NSRunAlertPanel(@"Unable to find ‘mate’", @"The ‘mate’ binary is missing from the application bundle. We recommend that you re-download the application.", @"OK", nil, nil);
	}
	[self updateUI:self];
}

- (void)replaceWarningDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(void*)stack
{
	if(returnCode == NSAlertFirstButtonReturn)
		[self installMateAs:[[installPathPopUp titleOfSelectedItem] stringByExpandingTildeInPath]];
	[alert release];
}

- (IBAction)performInstallMate:(id)sender
{
	NSString* dstObjPath = [[installPathPopUp titleOfSelectedItem] stringByExpandingTildeInPath];

	struct stat buf;
	std::string dstPath = to_s(dstObjPath);
	if(lstat(dstPath.c_str(), &buf) == 0)
	{
		char const* itemType = "An item";
		if(S_ISREG(buf.st_mode))
			itemType = "A file";
		else if(S_ISDIR(buf.st_mode))
			itemType = "A folder";
		else if(S_ISLNK(buf.st_mode))
			itemType = "A link";

		std::string summary = text::format("%s with the name “mate” already exists in the folder %s. Do you want to replace it?", itemType, path::with_tilde(path::parent(dstPath)).c_str());

		NSAlert* alert = [[NSAlert alloc] init]; // released in didEndSelector
		[alert setAlertStyle:NSWarningAlertStyle];
		[alert setMessageText:@"File Already Exists"];
		[alert setInformativeText:[NSString stringWithCxxString:summary]];
		[alert addButtons:@"Replace", @"Cancel", nil];
		[alert beginSheetModalForWindow:[self.view window] modalDelegate:self didEndSelector:@selector(replaceWarningDidEnd:returnCode:contextInfo:) contextInfo:NULL];
	}
	else
	{
		[self installMateAs:dstObjPath];
	}
	[self updateUI:self];
}

- (IBAction)performUninstallMate:(id)sender
{
	if(uninstall_mate(to_s(self.mateInstallPath)))
		[self setMateInstallPath:nil];
	[self updateUI:self];
}
@end
