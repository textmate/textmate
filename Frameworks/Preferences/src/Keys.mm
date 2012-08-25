#import "Keys.h"
#import <OakFoundation/OakFoundation.h>
#import <plist/plist.h>
#import <plist/ascii.h>
#import <ns/ns.h>

static NSDictionary* default_environment ()
{
	static std::string const DefaultVariables =
		"{ variables = ("
		"	{ enabled = :false; name = 'PATH';            value = '$PATH:/opt/local/bin:/usr/local/bin:/usr/texbin'; },"

		"	{ enabled = :false; name = 'TM_C_POINTER';    value = '* ';                               },"
		"	{ enabled = :false; name = 'TM_CXX_FLAGS';    value = '-framework Carbon -liconv -include vector -include string -include map -include cstdio -funsigned-char -Wall -Wwrite-strings -Wformat=2 -Winit-self -Wmissing-include-dirs -Wno-parentheses -Wno-sign-compare -Wno-switch'; },"
		"	{ enabled = :false; name = 'TM_FULLNAME';     value = 'Scrooge McDuck';                   },"
		"	{ enabled = :false; name = 'TM_ORGANIZATION'; value = 'The Billionaires Club';            },"
		"	{ enabled = :false; name = 'TM_XHTML';        value = ' /';                               },"

		"	{ enabled = :false; name = 'TM_GIT';          value = '/opt/local/bin/git';               },"
		"	{ enabled = :false; name = 'TM_HG';           value = '/opt/local/bin/hg';                },"
		"	{ enabled = :false; name = 'TM_MAKE_FLAGS';   value = 'rj8';                              },"
		"); }";

	return [ns::to_dictionary(plist::parse_ascii(DefaultVariables)) objectForKey:@"variables"];
}

static NSDictionary* default_settings ()
{
	return [NSDictionary dictionaryWithObjectsAndKeys:
		YES_obj,                                            kUserDefaultsFoldersOnTopKey,
		NO_obj,                                             kUserDefaultsShowFileExtensionsKey,
		default_environment(),                              kUserDefaultsEnvironmentVariablesKey,
		NO_obj,                                             kUserDefaultsDisableBundleUpdatesKey,
		[NSDate distantPast],                               kUserDefaultsLastBundleUpdateCheckKey,
		NO_obj,                                             kUserDefaultsDisableRMateServerKey,
		kRMateServerListenLocalhost,                        kUserDefaultsRMateServerListenKey,
		@52698,                                             kUserDefaultsRMateServerPortKey,
		NSFullUserName(),                                   kUserDefaultsLicenseOwnerKey,
		YES_obj,                                            kUserDefaultsAntiAliasKey,
		YES_obj,                                            kUserDefaultsLineNumbersKey,
	nil];
}

static bool register_defaults ()
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:default_settings()];
	return true;
}

void RegisterDefaults ()
{
	static bool __attribute__ ((unused)) dummy = register_defaults();
}

// =========
// = Files =
// =========

NSString* const kUserDefaultsDisableSessionRestoreKey            = @"disableSessionRestore";
NSString* const kUserDefaultsDisableNewDocumentAtStartupKey      = @"disableNewDocumentAtStartup";
NSString* const kUserDefaultsDisableNewDocumentAtReactivationKey = @"disableNewDocumentAtReactivation";

// ============
// = Projects =
// ============

NSString* const kUserDefaultsFoldersOnTopKey            = @"foldersOnTop";
NSString* const kUserDefaultsShowFileExtensionsKey      = @"showFileExtensions";
NSString* const kUserDefaultsInitialFileBrowserURLKey   = @"initialFileBrowserURL";

// ===========
// = Bundles =
// ===========

// =============
// = Variables =
// =============

NSString* const kUserDefaultsEnvironmentVariablesKey    = @"environmentVariables";

// ===================
// = Software Update =
// ===================

NSString* const kUserDefaultsDisableBundleUpdatesKey    = @"disableBundleUpdates";
NSString* const kUserDefaultsLastBundleUpdateCheckKey   = @"lastBundleUpdateCheck";

// ============
// = Terminal =
// ============

NSString* const kUserDefaultsMateInstallPathKey         = @"mateInstallPath";
NSString* const kUserDefaultsMateInstallVersionKey      = @"mateInstallVersion";

NSString* const kUserDefaultsDisableRMateServerKey      = @"rmateServerDisabled";
NSString* const kUserDefaultsRMateServerListenKey       = @"rmateServerListen"; // localhost (default), remote
NSString* const kUserDefaultsRMateServerPortKey         = @"rmateServerPort";

NSString* const kRMateServerListenLocalhost             = @"localhost";
NSString* const kRMateServerListenRemote                = @"remote";

// ================
// = Registration =
// ================

NSString* const kUserDefaultsLicenseOwnerKey            = @"licenseOwnerName";

// ==============
// = Appearance =
// ==============

NSString* const kUserDefaultsAntiAliasKey               = @"antiAlias";
NSString* const kUserDefaultsLineNumbersKey             = @"lineNumbers";

// =========
// = Other =
// =========

NSString* const kUserDefaultsFolderSearchFollowLinksKey = @"folderSearchFollowLinks";
