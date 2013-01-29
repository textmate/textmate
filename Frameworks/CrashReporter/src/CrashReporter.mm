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
			return text::format("%d", *(int*)buf);
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

static void submit_crash_reports (std::string const& url, std::string const& processName)
{
	// could send: all reports from the last week
	std::set<std::string> couldSend;
	oak::date_t const cutOffDate = oak::date_t::now() - 7*24*60*60;
	citerate(pair, find_reports(processName))
	{
		if(cutOffDate < oak::date_t(pair->first))
			couldSend.insert(pair->second);
	}

	// has sent: reports we already posted
	std::set<std::string> hasSent;
	for(NSString* path in [[NSUserDefaults standardUserDefaults] arrayForKey:kUserDefaultsCrashReportsSent])
		hasSent.insert([path fileSystemRepresentation]);

	// should send: “could send - has sent”
	std::vector<std::string> shouldSend;
	std::set_difference(couldSend.begin(), couldSend.end(), hasSent.begin(), hasSent.end(), back_inserter(shouldSend));

	std::set<std::string> didSend;
	for(auto report : shouldSend)
	{
		std::string gzippedReport = create_gzipped_file(report);
		if(gzippedReport != NULL_STR)
		{
			std::map<std::string, std::string> payload;
			payload["hardware"] = hardware_info(HW_MODEL) + "/" + hardware_info(HW_MACHINE) + "/" + hardware_info(HW_NCPU, true);
			payload["contact"]  = to_s([[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsCrashReportsContactInfoKey]);
			payload["report"]   = "@" + gzippedReport;
			long rc = post_to_server(url, payload);
			if(200 <= rc && rc < 300 || 400 <= rc && rc < 500) // we don’t resend reports on a 4xx failure.
				didSend.insert(report);
			unlink(gzippedReport.c_str());
		}
	}

	hasSent.insert(didSend.begin(), didSend.end());
	std::vector<std::string> updatedHasSent;
	std::set_intersection(couldSend.begin(), couldSend.end(), hasSent.begin(), hasSent.end(), back_inserter(updatedHasSent));

	NSMutableArray* array = [NSMutableArray array];
	for(auto path : updatedHasSent)
		[array addObject:[NSString stringWithCxxString:path]];
	[[NSUserDefaults standardUserDefaults] setObject:array forKey:kUserDefaultsCrashReportsSent];
}

static NSString* contact_info ()
{
	NSString* name = NSFullUserName();
	@try {
		if(ABAddressBook* ab = [ABAddressBook sharedAddressBook])
		{
			ABMutableMultiValue* value = [[ab me] valueForProperty:kABEmailProperty];
			if(NSString* email = [value valueAtIndex:[value indexForIdentifier:[value primaryIdentifier]]])
				name = name ? [NSString stringWithFormat:@"%@ <%@>", name, email] : email;
		}
	}
	@catch (NSException* e) {
		NSLog(@"%@", e);
	}
	return name ?: @"Anonymous";
}

void OakSubmitNewCrashReportsInBackground (NSString* url, NSString* processName)
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{ kUserDefaultsCrashReportsContactInfoKey : contact_info() }];
	if(![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableCrashReportingKey])
	{
		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
			submit_crash_reports(to_s(url), to_s(processName ?: [[NSProcessInfo processInfo] processName]));
		});
	}
}
