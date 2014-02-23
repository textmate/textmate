#import "CrashReporter.h"
#import "find_reports.h"
#import <OakFoundation/NSString Additions.h>
#import <network/post.h>
#import <plist/date.h>
#import <io/path.h>
#import <text/format.h>
#import <ns/ns.h>
#import <oak/oak.h>

NSString* const kUserDefaultsDisableCrashReportingKey   = @"DisableCrashReports";
NSString* const kUserDefaultsCrashReportsContactInfoKey = @"CrashReportsContactInfo";
NSString* const kUserDefaultsCrashReportsSent           = @"CrashReportsSent";

static std::string hardware_info (int field, bool integer = false)
{
	char buf[1024];
	size_t bufSize = sizeof(buf);
	int request[] = { CTL_HW, field };

	if(sysctl(request, sizeofA(request), buf, &bufSize, NULL, 0) != -1)
	{
		if(integer && bufSize == 4)
			return std::to_string(*(int*)buf);
		return std::string(buf, buf + bufSize - 1);
	}

	return "???";
}

static std::string create_gzipped_file (std::string const& path)
{
	std::string res = path::temp("gzipped_crash_log");
	if(gzFile fp = gzopen(res.c_str(), "wb"))
	{
		std::string const text = path::content(path);;
		gzwrite(fp, text.data(), text.size());
		gzclose(fp);
	}
	else
	{
		unlink(res.c_str());
		res = NULL_STR;
	}
	return res;
}

@interface CrashReporter () <NSUserNotificationCenterDelegate>
@end

@implementation CrashReporter
+ (CrashReporter*)sharedInstance
{
	static CrashReporter* instance = [CrashReporter new];
	return instance;
}

- (void)setupUserDefaultsContact:(id)sender
{
	NSString* name = NSFullUserName();
	if(ABAddressBook* ab = [ABAddressBook sharedAddressBook])
	{
		ABMutableMultiValue* value = [[ab me] valueForProperty:kABEmailProperty];
		if(NSString* email = [value valueAtIndex:[value indexForIdentifier:[value primaryIdentifier]]])
			name = name ? [NSString stringWithFormat:@"%@ <%@>", name, email] : email;
	}

	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsCrashReportsContactInfoKey : name ?: @"Anonymous",
	}];
}

- (id)init
{
	if(self = [super init])
		[[NSUserNotificationCenter defaultUserNotificationCenter] setDelegate:self];
	return self;
}

- (BOOL)userNotificationCenter:(NSUserNotificationCenter*)center shouldPresentNotification:(NSUserNotification*)notification
{
	return YES;
}

- (void)userNotificationCenter:(NSUserNotificationCenter*)center didActivateNotification:(NSUserNotification*)notification
{
	NSDictionary* userInfo = notification.userInfo;
	if(NSString* urlString = userInfo[@"url"])
		[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:urlString]];
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
	if(NSClassFromString(@"NSUserNotification"))
	{
		if(NSDictionary* userInfo = [aNotification userInfo])
		{
			if(NSUserNotification* notification = userInfo[NSApplicationLaunchUserNotificationKey])
				[self userNotificationCenter:nil didActivateNotification:notification];
		}
	}
}

- (void)postNewCrashReportsToURLString:(NSString*)aURL
{
	if([[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableCrashReportingKey])
		return;

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{

		// has sent: reports we already posted
		std::set<std::string> hasSent;
		for(NSString* path in [[NSUserDefaults standardUserDefaults] arrayForKey:kUserDefaultsCrashReportsSent])
			hasSent.insert([path fileSystemRepresentation]);

		// can send: all reports from the last week
		std::set<std::string> canSend;
		oak::date_t const cutOffDate = oak::date_t::now() - 7*24*60*60;
		for(auto pair : find_reports(to_s([[NSProcessInfo processInfo] processName])))
		{
			if(cutOffDate < oak::date_t(pair.first))
				canSend.insert(pair.second);
		}

		// should send: “can send - has sent”
		std::vector<std::string> shouldSend;
		std::set_difference(canSend.begin(), canSend.end(), hasSent.begin(), hasSent.end(), back_inserter(shouldSend));

		std::string __block contact;
		if(!shouldSend.empty())
		{
			dispatch_sync(dispatch_get_main_queue(), ^{
				[self setupUserDefaultsContact:self];
				contact = to_s([[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsCrashReportsContactInfoKey]);
			});
		}

		std::set<std::string> didSend;
		for(auto report : shouldSend)
		{
			std::string gzippedReport = create_gzipped_file(report);
			if(gzippedReport != NULL_STR)
			{
				std::map<std::string, std::string> payload, response;
				payload["hardware"] = hardware_info(HW_MODEL) + "/" + hardware_info(HW_MACHINE) + "/" + hardware_info(HW_NCPU, true);
				payload["contact"]  = contact;
				payload["report"]   = "@" + gzippedReport;

				long rc = post_to_server(to_s(aURL), payload, &response);
				if(200 <= rc && rc < 300 || 400 <= rc && rc < 500) // we don’t resend reports on a 4xx failure.
				{
					didSend.insert(report);

					if(NSClassFromString(@"NSUserNotification"))
					{
						auto location = response.find("location");
						if(location != response.end())
						{
							NSString* path = [NSString stringWithCxxString:report];
							NSString* url  = [NSString stringWithCxxString:location->second];

							NSUserNotification* notification = [NSUserNotification new];
							notification.title           = @"Crash Report Sent";
							notification.informativeText = @"Diagnostic information has been sent to MacroMates.com regarding your last crash.";
							notification.userInfo        = @{ @"path" : path, @"url" : url };
							[[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:notification];
						}
					}
				}

				unlink(gzippedReport.c_str());
			}
		}

		hasSent.insert(didSend.begin(), didSend.end());
		std::vector<std::string> updatedHasSent;
		std::set_intersection(canSend.begin(), canSend.end(), hasSent.begin(), hasSent.end(), back_inserter(updatedHasSent));

		NSMutableArray* array = [NSMutableArray array];
		for(auto path : updatedHasSent)
			[array addObject:[NSString stringWithCxxString:path]];
		[[NSUserDefaults standardUserDefaults] setObject:array forKey:kUserDefaultsCrashReportsSent];

	});
}
@end
