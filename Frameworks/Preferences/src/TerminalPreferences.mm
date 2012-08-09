#import "TerminalPreferences.h"
#import "Keys.h"
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/OakSavePanel.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <io/path.h>
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
			[self updatePopUp:[path stringByAbbreviatingWithTildeInPath]];
	else	[installPathPopUp selectItemAtIndex:0];
	[self updateUI:self];
}

- (void)updatePopUp:(NSString*)path
{
	NSMenu* menu = [installPathPopUp menu];
	[menu removeAllItems];

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
	NSString* path = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsMateInstallPathKey];

	std::map<std::string, std::string> variables;
	if(path)
		variables["installed"] = "installed";
	variables["mate_path"] = to_s(path ?: [installPathPopUp titleOfSelectedItem]);

	[installStatusText setStringValue:[NSString stringWithCxxString:format_string::expand(statusTextFormat, variables)]];
	[installSummaryText setStringValue:[NSString stringWithCxxString:format_string::expand(summaryTextFormat, variables)]];
	self.installIndicaitorImage = [NSImage imageNamed:(path ? @"Light-on" : @"Light-off") inSameBundleAsClass:[self class]];

	[installPathPopUp setEnabled:path ? NO : YES];
	[installButton setAction:path ? @selector(performUninstallMate:) : @selector(performInstallMate:)];
	[installButton setState:path ? NSOnState : NSOffState];
}

- (void)loadView
{
	[super loadView];

	if(NSString* path = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsMateInstallPathKey])
	{
		if(access([[path stringByExpandingTildeInPath] fileSystemRepresentation], F_OK) != 0)
			[[NSUserDefaults standardUserDefaults] removeObjectForKey:kUserDefaultsMateInstallPathKey];
	}

	installPathPopUp.target = self;
	statusTextFormat  = to_s([installStatusText stringValue]);
	summaryTextFormat = to_s([installSummaryText stringValue]);
	[self updatePopUp:[[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsMateInstallPathKey]];
	[self updateUI:self];

	CreateHyperLink(rmateSummaryText, @"rmate", [NSString stringWithFormat:@"txmt://open?url=%@", [[NSURL fileURLWithPath:[[NSBundle bundleForClass:[self class]] pathForResource:@"rmate" ofType:@""]] absoluteString]]);
	LSSetDefaultHandlerForURLScheme(CFSTR("txmt"), (CFStringRef)[[NSBundle mainBundle] bundleIdentifier]);
}

- (void)setMateInstallPath:(NSString*)aPath
{
	if(aPath)
			[[NSUserDefaults standardUserDefaults] setObject:aPath forKey:kUserDefaultsMateInstallPathKey];
	else	[[NSUserDefaults standardUserDefaults] removeObjectForKey:kUserDefaultsMateInstallPathKey];

	[self updateUI:self];
}

- (void)replaceWarningDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(void*)stack
{
	if(returnCode == NSAlertFirstButtonReturn)
	{
		auto env = bundles::environment(scope::scope_t());
		auto it = env.find("TM_SUPPORT_PATH");
		if(it != env.end())
		{
			std::string src = path::join(it->second, "bin/mate");
			std::string dst = to_s([[installPathPopUp titleOfSelectedItem] stringByExpandingTildeInPath]);

			if(install_mate(src, dst))
			{
				[self setMateInstallPath:[installPathPopUp titleOfSelectedItem]];
				[self updateUI:self];
			}
		}
	}

	[alert release];
}

- (IBAction)performInstallMate:(id)sender
{
	auto env = bundles::environment(scope::scope_t());
	auto it = env.find("TM_SUPPORT_PATH");
	if(it != env.end())
	{
		std::string srcPath = path::join(it->second, "bin/mate");
		std::string dstPath = to_s([[installPathPopUp titleOfSelectedItem] stringByExpandingTildeInPath]);

		struct stat buf;
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
			if(install_mate(srcPath, dstPath))
				[self setMateInstallPath:[installPathPopUp titleOfSelectedItem]];
		}
	}
	[self updateUI:self];
}

- (IBAction)performUninstallMate:(id)sender
{
	NSString* path = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsMateInstallPathKey];
	if(uninstall_mate(to_s([path stringByExpandingTildeInPath])))
		[self setMateInstallPath:nil];
	[self updateUI:self];
}
@end
