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
	NSString* excludeGlob = @"{*.{o,pyc},Icon\\r,CVS,_darcs,_MTN,\\{arch\\},blib,*~.nib}";
	NSString* includeGlob = @"{.tm_properties,.htaccess}";
	NSString* binaryGlob  = @"*.{icns,ico,jpg,jpeg,m4v,nib,pdf,png,psd,pyc,rtf,tif,tiff,xib}";

	return [NSDictionary dictionaryWithObjectsAndKeys:
		@"3130E4FA-B10E-11D9-9F75-000D93589AF6",            kUserDefaultsNewDocumentTypeKey,
		@"UTF-8",                                           kUserDefaultsEncodingKey,
		NO_obj,                                             kUserDefaultsUseBOMKey,
		@"\n",                                              kUserDefaultsLineEndingsKey,
		YES_obj,                                            kUserDefaultsFoldersOnTopKey,
		NO_obj,                                             kUserDefaultsShowFileExtensionsKey,
		excludeGlob,                                        kUserDefaultsExcludePatternKey,
		includeGlob,                                        kUserDefaultsIncludePatternKey,
		binaryGlob,                                         kUserDefaultsBinaryPatternKey,
		default_environment(),                              kUserDefaultsEnvironmentVariablesKey,
		NO_obj,                                             kUserDefaultsDisableBundleUpdatesKey,
		[NSDate distantPast],                               kUserDefaultsLastBundleUpdateCheckKey,
		NO_obj,                                             kUserDefaultsDisableRMateServerKey,
		kRMateServerListenLocalhost,                        kUserDefaultsRMateServerListenKey,
		@52698,                                             kUserDefaultsRMateServerPortKey,
		NSFullUserName(),                                   kUserDefaultsLicenseOwnerKey,
		@"$TM_DISPLAYNAME",                                 kUserDefaultsWindowTitleKey,
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

NSString* const kUserDefaultsNewDocumentTypeKey                  = @"fileType";
NSString* const kUserDefaultsEncodingKey                         = @"encoding";
NSString* const kUserDefaultsUseBOMKey                           = @"useBOM"; // only when encoding = UTF-8
NSString* const kUserDefaultsLineEndingsKey                      = @"lineEndings";

// ============
// = Projects =
// ============

NSString* const kUserDefaultsFoldersOnTopKey            = @"foldersOnTop";
NSString* const kUserDefaultsShowFileExtensionsKey      = @"showFileExtensions";
NSString* const kUserDefaultsInitialFileBrowserURLKey   = @"initialFileBrowserURL";
NSString* const kUserDefaultsExcludePatternKey          = @"excludePattern";
NSString* const kUserDefaultsIncludePatternKey          = @"includePattern";
NSString* const kUserDefaultsBinaryPatternKey           = @"binaryPattern";

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

NSString* const kUserDefaultsWindowTitleKey             = @"windowTitle";
NSString* const kUserDefaultsAntiAliasKey               = @"antiAlias";
NSString* const kUserDefaultsLineNumbersKey             = @"lineNumbers";

// =========
// = Other =
// =========

NSString* const kUserDefaultsFolderSearchFollowLinksKey = @"folderSearchFollowLinks";
