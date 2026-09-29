#import "CrashReporter.h"

#define UploadCrashReport 0

#if UploadCrashReport
#import <Preferences/src/Keys.h>
#import <UserNotifications/UserNotifications.h>
#import <oak/misc.h>

static NSString* const kUserDefaultsCrashReportsSent = @"CrashReportsSent";

static NSString* GetHardwareInfo (int field, BOOL isInteger = NO)
{
	char buf[1024];
	size_t bufSize = sizeof(buf);
	int request[] = { CTL_HW, field };

	if(sysctl(request, sizeofA(request), buf, &bufSize, nullptr, 0) != -1)
	{
		if(isInteger && bufSize == 4)
			return [NSString stringWithFormat:@"%d", *(int*)buf];
		return [[NSString alloc] initWithUTF8String:buf];
	}

	return @"???";
}
#endif

#if UploadCrashReport
@interface CrashReporter () <UNUserNotificationCenterDelegate>
@property (nonatomic) NSBackgroundActivityScheduler* activity;
@end
#endif

@implementation CrashReporter
+ (instancetype)sharedInstance
{
	static CrashReporter* sharedInstance = [self new];
	return sharedInstance;
}

#if UploadCrashReport
- (id)init
{
	if(self = [super init])
	{
		UNUserNotificationCenter.currentNotificationCenter.delegate = self;
	}
	return self;
}

- (void)userNotificationCenter:(UNUserNotificationCenter*)center didReceiveNotificationResponse:(UNNotificationResponse*)response withCompletionHandler:(void(^)(void))completionHandler API_AVAILABLE(macosx(10.14))
{
	if(NSString* urlString = response.notification.request.content.userInfo[@"url"])
		[NSWorkspace.sharedWorkspace openURL:[NSURL URLWithString:urlString]];
	completionHandler();
}

- (void)userNotificationCenter:(UNUserNotificationCenter*)center willPresentNotification:(UNNotification*)notification withCompletionHandler:(void(^)(UNNotificationPresentationOptions options))completionHandler API_AVAILABLE(macosx(10.14))
{
	completionHandler(UNNotificationPresentationOptionBanner);
}

- (void)postNewCrashReportsToURLString:(NSString*)urlString
{
	if([NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisableCrashReportingKey])
		return;

	self.activity = [[NSBackgroundActivityScheduler alloc] initWithIdentifier:[NSString stringWithFormat:@"%@.%@", NSBundle.mainBundle.bundleIdentifier, @"CrashReporting"]];
	self.activity.interval = 30;
	[self.activity scheduleWithBlock:^(NSBackgroundActivityCompletionHandler completionHandler){
		if([NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisableCrashReportingKey])
		{
			completionHandler(NSBackgroundActivityResultFinished);
			return;
		}

		NSDate* date   = [NSDate dateWithTimeIntervalSinceNow:-7*24*60*60];
		NSURL* url     = [NSURL URLWithString:urlString];
		NSString* name = NSProcessInfo.processInfo.processName;
		[self postCrashReportsNotBeforeDate:date toURL:url forProcessName:name];
		completionHandler(NSBackgroundActivityResultFinished);
	}];
}

- (void)postCrashReportsNotBeforeDate:(NSDate*)date toURL:(NSURL*)postURL forProcessName:(NSString*)processName
{
	NSArray<NSString*>* canSend = [self reportsForProcessName:processName notBeforeDate:date];

	NSMutableSet<NSString*>* shouldSend = [NSMutableSet setWithArray:canSend];
	if(NSArray<NSString*>* hasSent = [NSUserDefaults.standardUserDefaults stringArrayForKey:kUserDefaultsCrashReportsSent])
	{
		NSMutableSet* trimmedHasSent = [NSMutableSet setWithArray:hasSent];
		[trimmedHasSent intersectSet:[NSSet setWithArray:canSend]];
		if(trimmedHasSent.count < hasSent.count)
			[NSUserDefaults.standardUserDefaults setObject:trimmedHasSent.allObjects forKey:kUserDefaultsCrashReportsSent];

		[shouldSend minusSet:trimmedHasSent];
	}

	for(NSString* reportPath in shouldSend)
	{
		if(NSString* gzippedReport = [self pathForGZipCompressedFileAtPath:reportPath])
		{
			NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:postURL cachePolicy:NSURLRequestUseProtocolCachePolicy timeoutInterval:60];

			NSData* body = [self dataForURLRequest:request withFormValues:@{
				@"hardware": [NSString stringWithFormat:@"%@/%@/%@", GetHardwareInfo(HW_MODEL), GetHardwareInfo(HW_MACHINE), GetHardwareInfo(HW_NCPU, true)],
				@"contact":  [NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsCrashReportsContactInfoKey] ?: @"Anonymous",
				@"report":   [@"@" stringByAppendingString:gzippedReport],
			}];
			if(!body)
			{
				unlink(gzippedReport.fileSystemRepresentation);
				continue;
			}

			NSURLSessionUploadTask* uploadTask = [NSURLSession.sharedSession uploadTaskWithRequest:request fromData:body completionHandler:^(NSData* data, NSURLResponse* response, NSError* error){
				NSInteger rc = ((NSHTTPURLResponse*)response).statusCode;
				if(error)
				{
					os_log_error(OS_LOG_DEFAULT, "Failed uploading crash report to %{public}@: %{public}@", request.URL, error.localizedDescription);
				}
				else if((200 <= rc && rc < 300) || (400 <= rc && rc < 500)) // We don’t resend reports on a 4xx failure.
				{
					@synchronized(NSUserDefaults.standardUserDefaults) {
						NSArray<NSString*>* updatedHasSent = @[ reportPath ];
						if(NSArray<NSString*>* oldHasSent = [NSUserDefaults.standardUserDefaults stringArrayForKey:kUserDefaultsCrashReportsSent])
							updatedHasSent = [updatedHasSent arrayByAddingObjectsFromArray:oldHasSent];
						[NSUserDefaults.standardUserDefaults setObject:updatedHasSent forKey:kUserDefaultsCrashReportsSent];
					}

					if(NSString* locationURLString = ((NSHTTPURLResponse*)response).allHeaderFields[@"Location"])
					{
						os_log(OS_LOG_DEFAULT, "Crash report available at %{public}@", locationURLString);
						[UNUserNotificationCenter.currentNotificationCenter requestAuthorizationWithOptions:UNAuthorizationOptionAlert completionHandler:^(BOOL granted, NSError* error){
							if(granted)
							{
								UNMutableNotificationContent* content = [[UNMutableNotificationContent alloc] init];
								content.title    = @"Crash Report Sent";
								content.body     = @"Diagnostic information has been sent to MacroMates.com regarding your last crash.";
								content.userInfo = @{ @"path": reportPath, @"url": locationURLString };

								UNNotificationRequest* request = [UNNotificationRequest requestWithIdentifier:[NSUUID UUID].UUIDString content:content trigger:nil];
								[UNUserNotificationCenter.currentNotificationCenter addNotificationRequest:request withCompletionHandler:^(NSError* error){
									if(error)
										os_log_error(OS_LOG_DEFAULT, "Failed to show notification: %{public}@", error.localizedDescription);
								}];
							}
							else
							{
								os_log_info(OS_LOG_DEFAULT, "User notifications disallowed");
							}
						}];
					}
				}
				else
				{
					os_log_error(OS_LOG_DEFAULT, "Unexpected status code (%ld) from %{public}@", rc, request.URL);
				}
				unlink(gzippedReport.fileSystemRepresentation);
			}];
			[uploadTask resume];
		}
	}
}

// ==================
// = Helper Methods =
// ==================

- (NSArray<NSString*>*)reportsForProcessName:(NSString*)processName notBeforeDate:(NSDate*)cutOffDate
{
	NSString* const directory  = @"~/Library/Logs/DiagnosticReports".stringByExpandingTildeInPath;
	NSString* const timeFormat = [processName stringByAppendingString:@"_%F-%H%M%S"];

	NSMutableArray<NSString*>* res = [NSMutableArray array];
	for(NSString* fileName in [NSFileManager.defaultManager contentsOfDirectoryAtPath:directory error:nil])
	{
		if([fileName hasPrefix:processName])
		{
			struct tm bsdDate = { };
			if(strptime(fileName.UTF8String, timeFormat.UTF8String, &bsdDate))
			{
				time_t seconds = mktime(&bsdDate);
				if(seconds != -1 && seconds >= cutOffDate.timeIntervalSince1970)
					[res addObject:[directory stringByAppendingPathComponent:fileName]];
			}
		}
	}
	return res;
}

- (NSData*)dataForURLRequest:(NSMutableURLRequest*)urlRequest withFormValues:(NSDictionary<NSString*, NSString*>*)payload
{
	NSString* const boundary = [NSUUID UUID].UUIDString;

	urlRequest.HTTPMethod = @"POST";
	[urlRequest setValue:[NSString stringWithFormat:@"multipart/form-data; boundary=\"%@\"", boundary] forHTTPHeaderField:@"Content-Type"];

	NSMutableData* body = [NSMutableData data];
	__block BOOL success = YES;
	[payload enumerateKeysAndObjectsUsingBlock:^(NSString* name, NSString* value, BOOL* stop) {
		NSMutableArray<NSString*>* head = [NSMutableArray arrayWithObject:[NSString stringWithFormat:@"--%@", boundary]];
		NSData* data;

		if([value hasPrefix:@"@"])
		{
			NSString* path = [value substringFromIndex:1];

			[head addObject:[NSString stringWithFormat:@"Content-Disposition: form-data; name=\"%@\"; filename=\"%@\"", name, path.lastPathComponent]];
			[head addObject:@"Content-Type: application/octet-stream"];
			data = [NSData dataWithContentsOfFile:path];
		}
		else
		{
			[head addObject:[NSString stringWithFormat:@"Content-Disposition: form-data; name=\"%@\"", name]];
			data = [value dataUsingEncoding:NSUTF8StringEncoding];
		}
		if(!data)
		{
			os_log_error(OS_LOG_DEFAULT, "Failed creating crash report form field %{public}@", name);
			success = NO;
			*stop = YES;
			return;
		}

		[body appendData:[[head componentsJoinedByString:@"\r\n"] dataUsingEncoding:NSUTF8StringEncoding]];
		[body appendData:[@"\r\n\r\n" dataUsingEncoding:NSUTF8StringEncoding]];
		[body appendData:data];
		[body appendData:[@"\r\n" dataUsingEncoding:NSUTF8StringEncoding]];
	}];
	if(!success)
		return nil;
	[body appendData:[[NSString stringWithFormat:@"--%@--\r\n", boundary] dataUsingEncoding:NSUTF8StringEncoding]];

	return body;
}

- (NSString*)pathForGZipCompressedFileAtPath:(NSString*)path
{
	NSString* res;
	if(NSData* data = [NSData dataWithContentsOfFile:path])
	{
		NSString* gzPath = [NSString pathWithComponents:@[
			NSTemporaryDirectory(),
			NSBundle.mainBundle.bundleIdentifier ?: NSProcessInfo.processInfo.processName,
			[NSString stringWithFormat:@"%@.gz", path.lastPathComponent],
		]];

		if([NSFileManager.defaultManager createDirectoryAtPath:gzPath.stringByDeletingLastPathComponent withIntermediateDirectories:YES attributes:nil error:nil])
		{
			if(gzFile fp = gzopen(gzPath.fileSystemRepresentation, "wb"))
			{
				uint8_t const* bytes = (uint8_t const*)data.bytes;
				NSUInteger remaining = data.length;
				while(remaining)
				{
					unsigned int length = (unsigned int)MIN(remaining, (NSUInteger)UINT_MAX);
					if(gzwrite(fp, bytes, length) != length)
					{
						os_log_error(OS_LOG_DEFAULT, "Failed writing gzip file %{public}@", gzPath);
						break;
					}
					bytes += length;
					remaining -= length;
				}
				gzclose(fp);
				if(!remaining)
					res = gzPath;
			}
			else
			{
				os_log_error(OS_LOG_DEFAULT, "Failed creating file %{public}@", gzPath);
			}
		}
		else
		{
			os_log_error(OS_LOG_DEFAULT, "Failed creating directory %{public}@", gzPath.stringByDeletingLastPathComponent);
		}
	}
	else
	{
		os_log_error(OS_LOG_DEFAULT, "Failed reading %{public}@", path);
	}
	return res;
}
#else
- (void)postNewCrashReportsToURLString:(NSString*)urlString
{
}
#endif
@end
