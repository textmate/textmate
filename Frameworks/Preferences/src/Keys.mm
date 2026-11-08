#import "Keys.h"
#import <OakAppKit/OakPasteboard.h>
#import <OakTabBarView/OakTabBarView.h>
#import <BundlesManager/BundlesManager.h>

static NSArray* default_environment ()
{
	return @[
		@{ @"enabled": @NO, @"name": @"PATH",            @"value": @"$PATH:/opt/local/bin:/usr/local/bin:/usr/texbin" },
		@{ @"enabled": @NO, @"name": @"TM_C_POINTER",    @"value": @"* "                               },
		@{ @"enabled": @NO, @"name": @"TM_CXX_FLAGS",    @"value": @"-framework Carbon -liconv -include vector -include string -include map -include cstdio -funsigned-char -Wall -Wwrite-strings -Wformat=2 -Winit-self -Wmissing-include-dirs -Wno-parentheses -Wno-sign-compare -Wno-switch" },
		@{ @"enabled": @NO, @"name": @"TM_FULLNAME",     @"value": @"Scrooge McDuck"                   },
		@{ @"enabled": @NO, @"name": @"TM_ORGANIZATION", @"value": @"The Billionaires Club"            },
		@{ @"enabled": @NO, @"name": @"TM_XHTML",        @"value": @" /"                               },
		@{ @"enabled": @NO, @"name": @"TM_GIT",          @"value": @"/opt/local/bin/git"               },
		@{ @"enabled": @NO, @"name": @"TM_HG",           @"value": @"/opt/local/bin/hg"                },
		@{ @"enabled": @NO, @"name": @"TM_MAKE_FLAGS",   @"value": @"rj8"                              },
	];
}

static NSDictionary* default_settings ()
{
	return @{
		kUserDefaultsHTMLOutputPlacementKey:     @"window",
		kUserDefaultsFileBrowserPlacementKey:    @"right",
		kUserDefaultsShowFileExtensionsKey:      @NO,
		kUserDefaultsEnvironmentVariablesKey:    default_environment(),
		kUserDefaultsDisableBundleUpdatesKey:    @NO,
		kUserDefaultsDisableRMateServerKey:      @NO,
		kUserDefaultsRMateServerListenKey:       kRMateServerListenLocalhost,
		kUserDefaultsRMateServerPortKey:         @"52698",
		kUserDefaultsLicenseOwnerKey:            NSFullUserName(),
		kUserDefaultsLineNumbersKey:             @YES,
		kUserDefaultsFontSmoothingKey:            @(3),
		kUserDefaultsLineNumberScaleFactorKey:    @(0.8),
		kUserDefaultsTabItemMinWidthKey:          @(120),
		kUserDefaultsTabItemMaxWidthKey:          @(250),
		kUserDefaultsClipboardHistoryKeepAtLeast: @(25),
		kUserDefaultsClipboardHistoryKeepAtMost:  @(500),
		kUserDefaultsClipboardHistoryDaysToKeep:  @(30),
		kUserDefaultsLSPShowInlineDiagnosticsKey:    @YES,
	};
}

static bool register_defaults ()
{
	[NSUserDefaults.standardUserDefaults registerDefaults:default_settings()];
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
NSString* const kUserDefaultsShowFavoritesInsteadOfUntitledKey   = @"showFavoritesInsteadOfUntitled";

// ============
// = Projects =
// ============

NSString* const kUserDefaultsFoldersOnTopKey                   = @"foldersOnTop";
NSString* const kUserDefaultsShowFileExtensionsKey             = @"showFileExtensions";
NSString* const kUserDefaultsInitialFileBrowserURLKey          = @"initialFileBrowserURL";
NSString* const kUserDefaultsFileBrowserPlacementKey           = @"fileBrowserPlacement";
NSString* const kUserDefaultsFileBrowserSingleClickToOpenKey   = @"fileBrowserSingleClickToOpen";
NSString* const kUserDefaultsFileBrowserOpenAnimationDisabled  = @"fileBrowserOpenAnimationDisabled";
NSString* const kUserDefaultsFileBrowserStyleKey               = @"fileBrowserStyle";
NSString* const kUserDefaultsHTMLOutputPlacementKey            = @"htmlOutputPlacement";
NSString* const kUserDefaultsDisableFileBrowserWindowResizeKey = @"disableFileBrowserWindowResize";
NSString* const kUserDefaultsAutoRevealFileKey                 = @"autoRevealFile";
NSString* const kUserDefaultsDisableTabReorderingKey           = @"disableTabReordering";
NSString* const kUserDefaultsDisableTabAutoCloseKey            = @"disableTabAutoClose";
NSString* const kUserDefaultsDisableTabBarCollapsingKey        = @"disableTabBarCollapsing";
NSString* const kUserDefaultsAllowExpandingLinksKey            = @"allowExpandingLinks";
NSString* const kUserDefaultsAllowExpandingPackagesKey         = @"allowExpandingPackages";

// ===========
// = Bundles =
// ===========

// =============
// = Variables =
// =============

NSString* const kUserDefaultsEnvironmentVariablesKey    = @"environmentVariables";

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

NSString* const kUserDefaultsDisableAntiAliasKey        = @"disableAntiAlias";
NSString* const kUserDefaultsLineNumbersKey             = @"lineNumbers";
NSString* const kUserDefaultsLineNumberScaleFactorKey   = @"lineNumberScaleFactor";
NSString* const kUserDefaultsLineNumberFontNameKey      = @"lineNumberFontName";

// =======
// = LSP =
// =======

NSString* const kUserDefaultsLSPShowInlineDiagnosticsKey = @"lspShowInlineDiagnostics";

// ==============
// = Formatters =
// ==============

NSString* const kUserDefaultsFormatterConfigurationsKey = @"formatters";

// =========
// = Other =
// =========

NSString* const kUserDefaultsFolderSearchFollowLinksKey = @"folderSearchFollowLinks";

// ============
// = Advanced =
// ============

NSString* const kUserDefaultsDisableTypingPairsKey            = @"disableTypingPairs";
NSString* const kUserDefaultsFontSmoothingKey                 = @"fontSmoothing";
NSString* const kUserDefaultsHideStatusBarKey                 = @"hideStatusBar";
NSString* const kUserDefaultsTabItemMinWidthKey               = @"tabItemMinWidth";
NSString* const kUserDefaultsTabItemMaxWidthKey               = @"tabItemMaxWidth";
NSString* const kUserDefaultsKeepSearchResultsOnDoubleClick   = @"keepSearchResultsOnDoubleClick";
NSString* const kUserDefaultsAlwaysFindInDocument             = @"alwaysFindInDocument";
NSString* const kUserDefaultsDisableFolderStateRestore        = @"disableFolderStateRestore";
NSString* const kUserDefaultsDisableBundleSuggestionsKey      = @"disableBundleSuggestions";
NSString* const kUserDefaultsGrammarsToNeverSuggestKey        = @"grammarsToNeverSuggest";
