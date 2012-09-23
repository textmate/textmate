#import "CrashReporter.h"
#import "find_reports.h"
#import <OakFoundation/NSString Additions.h>
#import <network/post.h>
#import <plist/date.h>
#import <io/path.h>
#import <regexp/regexp.h>
#import <text/format.h>
#import <oak/server.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import <oak/oak.h>

NSString* const kUserDefaultsDisableCrashReportingKey   = @"DisableCrashReports";
NSString* const kUserDefaultsCrashReportsContactInfoKey = @"CrashReportsContactInfo";

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

static std::string ContactInfo;

namespace
{
	static NSString* const kUserDefaultsCrashReportsSent = @"CrashReportsSent";

	struct post_reports_in_background_t
	{
		struct request_t { std::string url; std::vector<std::string> reports; };

		post_reports_in_background_t (std::string const& url, std::string const& processName)
		{
			_client_key = _server.register_client(this);

			// could send: all reports from the last week
			oak::date_t const cutOffDate = oak::date_t::now() - 7*24*60*60;
			citerate(pair, find_reports(processName))
			{
				if(cutOffDate < oak::date_t(pair->first))
					_could_send.insert(pair->second);
			}

			// has sent: reports we already posted
			for(NSString* path in [[NSUserDefaults standardUserDefaults] arrayForKey:kUserDefaultsCrashReportsSent])
				_has_sent.insert([path fileSystemRepresentation]);

			// should send: “could send - has sent”
			std::vector<std::string> shouldSend;
			std::set_difference(_could_send.begin(), _could_send.end(), _has_sent.begin(), _has_sent.end(), back_inserter(shouldSend));
			if(!shouldSend.empty())
				return _server.send_request(_client_key, (request_t){ url, shouldSend });

			delete this;
		}

		~post_reports_in_background_t ()
		{
			_server.unregister_client(_client_key);
		}

		static std::set<std::string> handle_request (request_t const& request)
		{
			std::map<std::string, std::string> map;
			map["hardware"] = hardware_info(HW_MODEL) + "/" + hardware_info(HW_MACHINE) + "/" + hardware_info(HW_NCPU, true);
			if(ContactInfo != NULL_STR)
				map["contact"] = ContactInfo;

			std::set<std::string> res;
			iterate(report, request.reports)
			{
				std::string file = path::temp("gzipped_crash_log");
				if(gzFile fp = gzopen(file.c_str(), "wb"))
				{
					std::string const text = path::content(*report);;
					gzwrite(fp, text.data(), text.size());
					gzclose(fp);

					map["report"] = "@" + file;
					long rc = post_to_server(request.url, map);
					if(200 <= rc && rc < 300 || 400 <= rc && rc < 500) // we don’t resend reports on a 4xx failure.
						res.insert(*report);
					unlink(file.c_str());
				}
			}
			return res;
		}

		void handle_reply (std::set<std::string> const& reportsSent)
		{
			_has_sent.insert(reportsSent.begin(), reportsSent.end());
			std::vector<std::string> updatedHasSent;
			std::set_intersection(_could_send.begin(), _could_send.end(), _has_sent.begin(), _has_sent.end(), back_inserter(updatedHasSent));

			NSMutableArray* array = [NSMutableArray array];
			for(auto path : updatedHasSent)
				[array addObject:[NSString stringWithCxxString:path]];
			[[NSUserDefaults standardUserDefaults] setObject:array forKey:kUserDefaultsCrashReportsSent];

			delete this;
		}

	private:
		std::set<std::string> _could_send, _has_sent;

		size_t _client_key;
		oak::server_t< post_reports_in_background_t, request_t > _server;
	};
}

static NSString* contact_info ()
{
	ABMutableMultiValue* value = [[[ABAddressBook sharedAddressBook] me] valueForProperty:kABEmailProperty];
	NSString* email = [value valueAtIndex:[value indexForIdentifier:[value primaryIdentifier]]];
	NSString* name = NSFullUserName();
	return (name && email) ? [NSString stringWithFormat:@"%@ <%@>", name, email] : (email ?: name);
}

void OakSubmitNewCrashReportsInBackground (NSString* url, NSString* processName)
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{ kUserDefaultsCrashReportsContactInfoKey : contact_info() }];
	if([[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableCrashReportingKey])
		return;

	ContactInfo = to_s([[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsCrashReportsContactInfoKey]);
	new post_reports_in_background_t(to_s(url), to_s(processName ?: [[NSProcessInfo processInfo] processName]));
}
