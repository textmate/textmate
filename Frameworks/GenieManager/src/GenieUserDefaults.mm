#import "GenieUserDefaults.h"

NSString* const GenieClipboardHistoryPath = @"~/Library/Application Support/Genie/ClipboardHistory.db";

NSString* const kGenieBundleIdentifier      = @"com.macromates.Genie";
NSString* const kGeniePrefsBundleIdentifier = @"com.macromates.GeniePrefs";

NSString* const kActivationKeyEventSettingsKey          = @"activationKeyEvent";
NSString* const kDisableLaunchAtLoginSettingsKey        = @"disableLaunchAtLogin";

NSString* const kEnableClipboardHistorySettingsKey      = @"enableClipboardHistory";
NSString* const kClipboardHistoryIgnoreAppsSettingsKey  = @"clipboardHistoryIgnoreApps";
NSString* const kClipboardHistoryExpireAfterSettingsKey = @"clipboardHistoryExpireAfter";

NSString* const kDisableSoftwareUpdateSettingsKey       = @"SoftwareUpdateDisablePolling";
NSString* const kSoftwareUpdatePrereleasesEnabled       = @"softwareUpdatePrereleasesEnabled";