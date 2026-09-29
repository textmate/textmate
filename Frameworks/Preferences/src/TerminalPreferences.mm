#import "TerminalPreferences.h"
#import "Keys.h"
#import <OakAppKit/src/NSAlert Additions.h>
#import <OakAppKit/src/NSImage Additions.h>
#import <OakFoundation/src/NSString Additions.h>
#import <OakFoundation/src/OakStringListTransformer.h>
#import <SoftwareUpdate/src/SoftwareUpdate.h> // OakCompareVersionStrings()
#import <io/src/path.h>
#import <io/src/exec.h>
#import <ns/src/ns.h>
#import <regexp/src/format_string.h>
#import <bundles/src/bundles.h>
#import <oak/compat.h>

static void CreateHyperLink (NSTextField* textField, NSString* text, NSString* url)
{
	[textField setAllowsEditingTextAttributes:YES];
	[textField setSelectable:YES];

	NSAttributedString* str = [textField attributedStringValue];
	NSRange range = [[str string] rangeOfString:text];

	NSMutableAttributedString* attrString = [str mutableCopy];
	[attrString beginEditing];
	[attrString addAttribute:NSLinkAttributeName value:url range:range];
	[attrString addAttribute:NSForegroundColorAttributeName value:[NSColor blueColor] range:range];
	[attrString addAttribute:NSUnderlineStyleAttributeName value:@(NSUnderlineStyleSingle) range:range];
	[attrString endEditing];

	[textField setAttributedStringValue:attrString];
}

static bool run_auth_command (AuthorizationRef& auth, std::string const cmd, ...)
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
	if(oak::execute_with_privileges(auth, cmd, kAuthorizationFlagDefaults, &args[0], NULL) == errAuthorizationSuccess)
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
			if(mkdir(path.c_str(), S_IRWXU|S_IRWXG|S_IRWXO) == 0)
				return true;
			perrorf("TerminalPreferences: mkdir(\"%s\")", path.c_str());
		}
		else
		{
			if(run_auth_command(auth, "/bin/mkdir", path.c_str(), NULL))
				return true;
			perrorf("TerminalPreferences: /bin/mkdir \"%s\"", path.c_str());
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
		perrorf("TerminalPreferences: unlink \"%s\"", path.c_str());
	}
	else
	{
		if(run_auth_command(auth, "/bin/rm", path.c_str(), NULL))
			return true;
		perrorf("TerminalPreferences: /bin/rm \"%s\"", path.c_str());
	}
	return false;
}

static bool cp_requires_admin (std::string const& dst)
{
	return access(dst.c_str(), W_OK) != 0 && (access(dst.c_str(), X_OK) == 0 || access(path::parent(dst).c_str(), W_OK) != 0);
}

static bool cp_path (std::string const& src, std::string const& dst, AuthorizationRef& auth)
{
	if(!cp_requires_admin(dst))
	{
		if(copyfile(src.c_str(), dst.c_str(), NULL, COPYFILE_ALL | COPYFILE_NOFOLLOW_SRC) == 0)
			return true;
		perrorf("TerminalPreferences: copyfile(\"%s\", \"%s\", NULL, COPYFILE_ALL | COPYFILE_NOFOLLOW_SRC)", src.c_str(), dst.c_str());
	}
	else
	{
		if(run_auth_command(auth, "/bin/cp", "-p", src.c_str(), dst.c_str(), NULL))
			return true;
		perrorf("TerminalPreferences: /bin/cp -p \"%s\" \"%s\"", src.c_str(), dst.c_str());
	}
	return false;
}

static bool install_mate (std::string const& src, std::string const& dst)
{
	AuthorizationRef auth = NULL;
	if(mk_dir(path::parent(dst), auth))
	{
		struct stat buf;
		if(lstat(dst.c_str(), &buf) == 0 && !S_ISREG(buf.st_mode) && !rm_path(dst, auth))
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
- (id)init
{
	if(self = [super initWithNibName:@"TerminalPreferences" label:@"Terminal" image:[NSImage imageNamed:@"Terminal" inSameBundleAsClass:[self class]]])
	{
		[OakStringListTransformer createTransformerWithName:@"OakRMateInterfaceTransformer" andObjectsArray:@[ kRMateServerListenLocalhost, kRMateServerListenRemote ]];

		self.defaultsProperties = @{
			@"path":         kUserDefaultsMateInstallPathKey,
			@"disableRMate": kUserDefaultsDisableRMateServerKey,
			@"interface":    kUserDefaultsRMateServerListenKey,
			@"port":         kUserDefaultsRMateServerPortKey,
		};
	}
	return self;
}

- (void)selectInstallPath:(id)sender
{
	NSSavePanel* savePanel = [NSSavePanel savePanel];
	[savePanel setNameFieldStringValue:@"mate"];
	[savePanel beginSheetModalForWindow:[self view].window completionHandler:^(NSModalResponse result) {
		if(result == NSModalResponseOK)
				[self updatePopUp:[[savePanel.URL filePathURL] path]];
		else	[installPathPopUp selectItemAtIndex:0];
		[self updateUI:self];
	}];
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

	for(NSMenuItem* menuItem in menu.itemArray)
		menuItem.target = self;

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
	self.installIndicaitorImage = [NSImage imageNamed:(isInstalled ? NSImageNameStatusAvailable : NSImageNameStatusUnavailable)];

	[installPathPopUp setEnabled:isInstalled ? NO : YES];
	[installButton setAction:isInstalled ? @selector(performUninstallMate:) : @selector(performInstallMate:)];
	[installButton setState:isInstalled ? NSControlStateValueOn : NSControlStateValueOff];
}

- (void)loadView
{
	[super loadView];

	if(NSString* path = self.mateInstallPath)
	{
		if(access([path fileSystemRepresentation], F_OK) != 0)
		{
			[NSUserDefaults.standardUserDefaults removeObjectForKey:kUserDefaultsMateInstallPathKey];
			[NSUserDefaults.standardUserDefaults removeObjectForKey:kUserDefaultsMateInstallVersionKey];
		}
	}

	installPathPopUp.target = self;
	installButton.target = self;
	statusTextFormat  = to_s([installStatusText stringValue]);
	summaryTextFormat = to_s([installSummaryText stringValue]);
	[self updatePopUp:self.mateInstallPath];
	[self updateUI:self];

	CreateHyperLink(rmateSummaryText, @"rmate", @"https://github.com/textmate/rmate/");
	LSSetDefaultHandlerForURLScheme(CFSTR("txmt"), CFBundleGetIdentifier(CFBundleGetMainBundle()));
}

- (NSSize)preferredContentSize
{
	return self.view.frame.size;
}

- (NSString*)mateInstallPath
{
	NSString* path = [NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsMateInstallPathKey];
	return [path stringByExpandingTildeInPath];
}

- (void)setMateInstallPath:(NSString*)aPath
{
	if(aPath)
			[NSUserDefaults.standardUserDefaults setObject:aPath forKey:kUserDefaultsMateInstallPathKey];
	else	[NSUserDefaults.standardUserDefaults removeObjectForKey:kUserDefaultsMateInstallPathKey];
}

- (void)installMateAs:(NSString*)dstPath
{
	if(NSString* srcPath = [NSBundle.mainBundle pathForAuxiliaryExecutable:@"mate"])
	{
		if(install_mate(to_s(srcPath), to_s(dstPath)))
		{
			[self setMateInstallPath:dstPath];
			std::string res = io::exec(to_s(srcPath), "--version", NULL);
			if(regexp::match_t const& m = regexp::search("\\Amate ([\\d.]+)", res))
				[NSUserDefaults.standardUserDefaults setObject:[NSString stringWithCxxString:m[1]] forKey:kUserDefaultsMateInstallVersionKey];
		}
	}
	else
	{
		NSAlert* alert        = [[NSAlert alloc] init];
		alert.messageText     = @"Unable to find ‘mate’";
		alert.informativeText = @"The ‘mate’ binary is missing from the application bundle. We recommend that you re-download the application.";
		[alert addButtonWithTitle:@"OK"];
		[alert runModal];
	}
	[self updateUI:self];
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

		NSAlert* alert = [[NSAlert alloc] init];
		[alert setAlertStyle:NSAlertStyleWarning];
		[alert setMessageText:@"File Already Exists"];
		[alert setInformativeText:[NSString stringWithCxxString:summary]];
		[alert addButtons:@"Replace", @"Cancel", nil];
		[alert beginSheetModalForWindow:[self.view window] completionHandler:^(NSModalResponse returnCode){
			if(returnCode == NSAlertFirstButtonReturn)
				[self installMateAs:[[installPathPopUp titleOfSelectedItem] stringByExpandingTildeInPath]];
		}];
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

+ (void)updateMateIfRequired
{
	NSString* oldMate    = [[NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsMateInstallPathKey] stringByExpandingTildeInPath];
	NSString* oldVersion = [NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsMateInstallVersionKey];
	NSString* newMate    = [NSBundle.mainBundle pathForAuxiliaryExecutable:@"mate"];

	if(oldMate && newMate)
	{
		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
			std::string res = io::exec(to_s(newMate), "--version", NULL);
			if(regexp::match_t const& m = regexp::search("\\Amate ([\\d.]+)", res))
			{
				NSString* newVersion = [NSString stringWithCxxString:m[1]];
				if(OakCompareVersionStrings(oldVersion, newVersion) == NSOrderedAscending)
				{
					if(cp_requires_admin(to_s(oldMate)))
					{
						dispatch_async(dispatch_get_main_queue(), ^{
							NSAlert* alert        = [[NSAlert alloc] init];
							alert.messageText     = @"Update Shell Support";
							alert.informativeText = [NSString stringWithFormat:@"Would you like to update the installed version of mate to version %@?", newVersion];
							[alert addButtons:@"Update", @"Cancel", nil];
							if([alert runModal] == NSAlertFirstButtonReturn) // "Update"
							{
								if(!install_mate(to_s(newMate), to_s(oldMate)))
									return;
							}

							// Avoid asking again by storing the new version number
							[NSUserDefaults.standardUserDefaults setObject:newVersion forKey:kUserDefaultsMateInstallVersionKey];
						});
					}
					else
					{
						if(install_mate(to_s(newMate), to_s(oldMate)))
							[NSUserDefaults.standardUserDefaults setObject:newVersion forKey:kUserDefaultsMateInstallVersionKey];
					}
				}
			}
		});
	}
}
@end
