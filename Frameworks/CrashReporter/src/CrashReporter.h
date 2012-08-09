#include <oak/misc.h>

PUBLIC extern NSString* const kUserDefaultsDisableCrashReportingKey;
PUBLIC extern NSString* const kUserDefaultsCrashReportsContactInfoKey;

PUBLIC void OakSubmitNewCrashReportsInBackground (NSString* url, NSString* processName = nil);
