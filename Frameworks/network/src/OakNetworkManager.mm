#import "OakNetworkManager.h"
#import <oak/misc.h>

static NSString* const OakHTTPHeaderSignee    = @"x-amz-meta-x-signee";
static NSString* const OakHTTPHeaderSignature = @"x-amz-meta-x-signature";

@interface OakNetworkManager ()
- (BOOL)data:(NSData*)contentData hasValidBase64EncodedSignature:(NSString*)encodedSignature usingPublicKeyString:(NSString*)publicKeyString;
@end

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

// ==========================
// = OakDownloadArchiveTask =
// ==========================

@interface OakDownloadArchiveTask : NSObject <NSProgressReporting, NSURLSessionDataDelegate>
{
	NSDictionary<NSString*, NSString*>* _publicKeys;
	NSString*                           _signee;
	NSString*                           _signature;

	void(^_completionHandler)(NSURL* extractedArchiveURL, NSError* error);

	NSMutableData*                      _data;

	NSURL*                              _fileURLToReplace;
	NSURL*                              _temporaryFileURL;

	NSTask*                             _extractorTask;
	NSFileHandle*                       _extractorFileHandle;
	dispatch_group_t                    _extractorDispatchGroup;
	NSError*                            _extractorError;

	NSDate*                             _sampleStartDate;
	NSUInteger                          _sampleCountOfBytesReceived;
}
@property (nonatomic) NSProgress* progress;
@property (nonatomic) NSData* extractorTaskOutputData;
@property (nonatomic) NSData* extractorTaskErrorData;
@end

@implementation OakDownloadArchiveTask
- (instancetype)initWithURL:(NSURL*)url forReplacingURL:(NSURL*)localURL publicKeys:(NSDictionary<NSString*, NSString*>*)publicKeys completionHandler:(void(^)(NSURL* extractedArchiveURL, NSError* error))completionHandler
{
	if(self = [super init])
	{
		_fileURLToReplace       = localURL;
		_publicKeys             = publicKeys;
		_completionHandler      = completionHandler;
		_progress               = [NSProgress progressWithTotalUnitCount:-1];
		_data                   = [NSMutableData data];
		_extractorDispatchGroup = dispatch_group_create();

		_progress.kind = NSProgressKindFile;
		if(@available(macos 10.13, *))
				_progress.fileOperationKind = NSProgressFileOperationKindDownloading;
		else	[_progress setUserInfoObject:NSProgressFileOperationKindDownloading forKey:NSProgressFileOperationKindKey];
		_progress.localizedDescription = [NSString stringWithFormat:@"Downloading %@…", url.lastPathComponent];

		NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:url cachePolicy:NSURLRequestUseProtocolCachePolicy timeoutInterval:60];
		[request setValue:OakNetworkManager.sharedInstance.userAgentString forHTTPHeaderField:@"User-Agent"];

		NSURLSession* session = [NSURLSession sessionWithConfiguration:NSURLSessionConfiguration.defaultSessionConfiguration delegate:self delegateQueue:NSOperationQueue.mainQueue];
		[[session dataTaskWithRequest:request] resume];
		[session finishTasksAndInvalidate];
	}
	return self;
}

- (void)dealloc
{
	NSError* error;
	if(_temporaryFileURL && ![NSFileManager.defaultManager removeItemAtURL:_temporaryFileURL error:&error])
		os_log_error(OS_LOG_DEFAULT, "Unable to remove %{public}@: %{public}@", _temporaryFileURL.path, error.localizedDescription);
}

- (NSFileHandle*)extractorFileHandle
{
	if(!_extractorFileHandle)
	{
		NSError* error;
		if(!(_temporaryFileURL = [NSFileManager.defaultManager URLForDirectory:NSItemReplacementDirectory inDomain:NSUserDomainMask appropriateForURL:_fileURLToReplace create:YES error:&error]))
		{
			os_log_error(OS_LOG_DEFAULT, "Failed to obtain NSItemReplacementDirectory: %{public}@", error.localizedDescription);
			_extractorError = error;
			return nil;
		}

		NSPipe* inputPipe  = [NSPipe pipe];
		NSPipe* outputPipe = [NSPipe pipe];
		NSPipe* errorPipe  = [NSPipe pipe];

		_extractorTask = [[NSTask alloc] init];
		_extractorTask.launchPath     = @"/usr/bin/tar";
		_extractorTask.arguments      = @[ @"-jxmkC", _temporaryFileURL.filePathURL.path, @"--strip-components", @"1", @"--disable-copyfile", @"--exclude", @"._*" ];
		_extractorTask.standardInput  = inputPipe;
		_extractorTask.standardOutput = outputPipe;
		_extractorTask.standardError  = errorPipe;

		dispatch_group_async(_extractorDispatchGroup, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			self.extractorTaskOutputData = [outputPipe.fileHandleForReading readDataToEndOfFile];
		});

		dispatch_group_async(_extractorDispatchGroup, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			self.extractorTaskErrorData = [errorPipe.fileHandleForReading readDataToEndOfFile];
		});

		dispatch_group_t group = _extractorDispatchGroup; // Avoid capturing ‘self’ in terminationHandler

		dispatch_group_enter(group);
		_extractorTask.terminationHandler = ^(NSTask* theTask){
			dispatch_group_leave(group);
		};

		if(@available(macos 10.13, *))
		{
			if(![_extractorTask launchAndReturnError:&error])
			{
				os_log_error(OS_LOG_DEFAULT, "Failed to launch tar: %{public}@", error.localizedDescription);

				dispatch_group_leave(_extractorDispatchGroup); // Completion handler will never be called
				[outputPipe.fileHandleForWriting closeFile];
				[errorPipe.fileHandleForWriting closeFile];

				_extractorTask  = nil;
				_extractorError = error;

				return nil;
			}
		}
		else
		{
			@try {
				[_extractorTask launch];
			}
			@catch (NSException* e) {
				os_log_error(OS_LOG_DEFAULT, "-[NSTask launch]: %{public}@", e.reason);

				dispatch_group_leave(_extractorDispatchGroup); // Completion handler will never be called
				[outputPipe.fileHandleForWriting closeFile];
				[errorPipe.fileHandleForWriting closeFile];

				_extractorTask  = nil;
				_extractorError = [NSError errorWithDomain:@"OakNetworkManager" code:0 userInfo:@{ NSLocalizedDescriptionKey: [NSString stringWithFormat:@"-[NSTask launch]: %@", e.reason] }];

				return nil;
			}
		}
		_extractorFileHandle = inputPipe.fileHandleForWriting;
	}
	return _extractorFileHandle;
}

- (void)URLSession:(NSURLSession*)session task:(NSURLSessionTask*)task willPerformHTTPRedirection:(NSHTTPURLResponse*)response newRequest:(NSURLRequest*)request completionHandler:(void(^)(NSURLRequest*))completionHandler
{
	_signee    = _signee    ?: response.allHeaderFields[OakHTTPHeaderSignee];
	_signature = _signature ?: response.allHeaderFields[OakHTTPHeaderSignature];
	completionHandler(request);
}

- (void)URLSession:(NSURLSession*)session dataTask:(NSURLSessionDataTask*)dataTask didReceiveResponse:(NSURLResponse*)response completionHandler:(void(^)(NSURLSessionResponseDisposition disposition))completionHandler
{
	_signee    = _signee    ?: ((NSHTTPURLResponse*)response).allHeaderFields[OakHTTPHeaderSignee];
	_signature = _signature ?: ((NSHTTPURLResponse*)response).allHeaderFields[OakHTTPHeaderSignature];
	completionHandler(NSURLSessionResponseAllow);
}

- (void)URLSession:(NSURLSession*)session dataTask:(NSURLSessionDataTask*)dataTask didReceiveData:(NSData*)data
{
	NSFileHandle* fileHandle = self.extractorFileHandle;
	if(fileHandle && !_progress.isCancelled)
	{
		[fileHandle writeData:data];
		[_data appendData:data];

		_progress.totalUnitCount     = dataTask.countOfBytesExpectedToReceive;
		_progress.completedUnitCount = dataTask.countOfBytesReceived;

		if(NSUInteger bytesLeft = dataTask.countOfBytesExpectedToReceive - dataTask.countOfBytesReceived)
		{
			NSTimeInterval secondsSampled = -_sampleStartDate.timeIntervalSinceNow;
			if(secondsSampled > 0.9 || !_sampleStartDate)
			{
				if(secondsSampled)
				{
					NSUInteger bytesReceived = dataTask.countOfBytesReceived - _sampleCountOfBytesReceived;
					[_progress setUserInfoObject:(bytesReceived ? @(ceil(bytesLeft * secondsSampled / bytesReceived)) : nil) forKey:NSProgressEstimatedTimeRemainingKey];
				}

				_sampleStartDate            = [NSDate date];
				_sampleCountOfBytesReceived = dataTask.countOfBytesReceived;
			}
		}
		else
		{
			[_progress setUserInfoObject:nil forKey:NSProgressEstimatedTimeRemainingKey];
		}
	}
	else
	{
		[dataTask cancel];
	}
}

- (void)URLSession:(NSURLSession*)session task:(NSURLSessionTask*)dataTask didCompleteWithError:(NSError*)downloadError
{
	[_extractorFileHandle closeFile];
	_progress.totalUnitCount = dataTask.countOfBytesReceived;

	if(NSError* error = _extractorError ?: (_extractorTask ? downloadError : [NSError errorWithDomain:@"OakNetworkManager" code:0 userInfo:@{ NSLocalizedDescriptionKey: @"Unable to launch tar." }]))
	{
		os_log_error(OS_LOG_DEFAULT, "%{public}@", error.localizedDescription);
		_completionHandler(nil, error);
	}
	else if(![OakNetworkManager.sharedInstance data:_data hasValidBase64EncodedSignature:_signature usingPublicKeyString:_publicKeys[_signee]])
	{
		os_log_error(OS_LOG_DEFAULT, "Unable to verify signature");
		_completionHandler(nil, [NSError errorWithDomain:@"OakNetworkManager" code:0 userInfo:@{ NSLocalizedDescriptionKey: @"Unable to verify signature." }]);
	}
	else
	{
		dispatch_group_notify(_extractorDispatchGroup, dispatch_get_main_queue(), ^{
			if(_extractorTask.terminationStatus == 0)
			{
				NSURL* url = _temporaryFileURL;
				_temporaryFileURL = nil;
				_completionHandler(url, nil);
			}
			else
			{
				NSString* errorString  = _extractorTaskErrorData.length  ? [[NSString alloc] initWithData:_extractorTaskErrorData  encoding:NSUTF8StringEncoding] : nil;
				NSString* outputString = _extractorTaskOutputData.length ? [[NSString alloc] initWithData:_extractorTaskOutputData encoding:NSUTF8StringEncoding] : nil;

				os_log_error(OS_LOG_DEFAULT, "Abnormal exit from tar: %d", _extractorTask.terminationStatus);
				if(errorString)
					os_log_error(OS_LOG_DEFAULT, "%{public}@", errorString);
				if(outputString)
					os_log_error(OS_LOG_DEFAULT, "%{public}@", outputString);

				NSString* description = errorString ?: outputString ?: [NSString stringWithFormat:@"Abnormal exit from tar: %d", _extractorTask.terminationStatus];
				description = [description stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
				description = [description stringByReplacingOccurrencesOfString:@"\n" withString:@" "];
				_completionHandler(nil, [NSError errorWithDomain:@"OakNetworkManager" code:0 userInfo:@{ NSLocalizedDescriptionKey: description }]);
			}
		});
	}
}
@end

// =====================
// = OakNetworkManager =
// =====================

@interface OakNetworkManager ()
{
	NSString* _userAgentString;
}
@end

@implementation OakNetworkManager
+ (instancetype)sharedInstance
{
	static OakNetworkManager* sharedInstance = [self new];
	return sharedInstance;
}

- (NSString*)userAgentString
{
	if(!_userAgentString)
	{
		uuid_t uuidBytes;
		timespec wait = { };
		gethostuuid(uuidBytes, &wait);

		NSString* appName    = [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleName"];
		NSString* appVersion = [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleShortVersionString"];
		NSUUID* uuid         = [[NSUUID alloc] initWithUUIDBytes:uuidBytes];

		NSOperatingSystemVersion osVersion = NSProcessInfo.processInfo.operatingSystemVersion;

		NSString* res = [NSString stringWithFormat:@"%@/%@/%@ %ld.%ld.%ld/%@/%@/%@",
			appName, appVersion, uuid.UUIDString,
			osVersion.majorVersion, osVersion.minorVersion, osVersion.patchVersion,
			GetHardwareInfo(HW_MACHINE),
			GetHardwareInfo(HW_MODEL),
			GetHardwareInfo(HW_NCPU, YES)
		];
		_userAgentString = res;
	}
	return _userAgentString;
}

- (void)downloadFileAtURL:(NSURL*)serverURL replacingFileAtURL:(NSURL*)localFileURL publicKeys:(NSDictionary<NSString*, NSString*>*)publicKeys completionHandler:(void(^)(BOOL wasUpdated, NSError* error))completionHandler
{
	NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:serverURL cachePolicy:NSURLRequestReloadIgnoringLocalCacheData timeoutInterval:60];
	[request setValue:self.userAgentString forHTTPHeaderField:@"User-Agent"];

	ssize_t size = getxattr(localFileURL.fileSystemRepresentation, "org.w3.http.etag", nullptr, 0, 0, 0);
	if(size != -1)
	{
		if(NSMutableData* tempData = [NSMutableData dataWithLength:size])
		{
			getxattr(localFileURL.fileSystemRepresentation, "org.w3.http.etag", tempData.mutableBytes, tempData.length, 0, 0);
			if(NSString* entityTag = [[NSString alloc] initWithData:tempData encoding:NSUTF8StringEncoding])
			{
				[request setValue:entityTag forHTTPHeaderField:@"If-None-Match"];
				os_log(OS_LOG_DEFAULT, "GET %{public}@ using entity tag %{public}@", serverURL.absoluteString, entityTag);
			}
		}
	}

	NSURLSessionDataTask* dataTask = [NSURLSession.sharedSession dataTaskWithRequest:request completionHandler:^(NSData* data, NSURLResponse* response, NSError* error){
		BOOL wasUpdated = NO;

		NSInteger statusCode = ((NSHTTPURLResponse*)response).statusCode;
		if(error || statusCode != 200)
		{
			if(!error && statusCode != 304)
				error = [NSError errorWithDomain:@"OakNetworkManager" code:0 userInfo:@{ NSLocalizedDescriptionKey: [NSString stringWithFormat:@"Server returned %ld for %@", statusCode, serverURL.absoluteString] }];
		}
		else
		{
			NSString* signee    = ((NSHTTPURLResponse*)response).allHeaderFields[OakHTTPHeaderSignee];
			NSString* signature = ((NSHTTPURLResponse*)response).allHeaderFields[OakHTTPHeaderSignature];
			if(signee && signature)
			{
				if(NSString* publicKey = publicKeys[signee])
				{
					if([self data:data hasValidBase64EncodedSignature:signature usingPublicKeyString:publicKey])
					{
						if([data writeToURL:localFileURL options:NSDataWritingAtomic error:&error])
						{
							wasUpdated = YES;

							if(NSString* newETag = ((NSHTTPURLResponse*)response).allHeaderFields[@"ETag"])
							{
								char const* str = newETag.UTF8String;
								if(setxattr(localFileURL.fileSystemRepresentation, "org.w3.http.etag", str, strlen(str), 0, 0) == -1)
									os_log_error(OS_LOG_DEFAULT, "setxattr(%{public}@): %{darwin.errno}d", localFileURL.path, errno);
							}
							else
							{
								os_log_error(OS_LOG_DEFAULT, "No ETag: %{public}@", serverURL.absoluteString);
							}
						}
					}
					else
					{
						error = [NSError errorWithDomain:@"OakNetworkManager" code:0 userInfo:@{ NSLocalizedDescriptionKey: @"Unable to verify signature." }];
					}
				}
				else
				{
					error = [NSError errorWithDomain:@"OakNetworkManager" code:0 userInfo:@{ NSLocalizedDescriptionKey: [NSString stringWithFormat:@"Unable to obtain public key for %@.", signee] }];
				}
			}
			else
			{
				error = [NSError errorWithDomain:@"OakNetworkManager" code:0 userInfo:@{ NSLocalizedDescriptionKey: @"Missing signature" }];
			}
		}

		completionHandler(wasUpdated, error);
	}];
	[dataTask resume];
}

- (id <NSProgressReporting>)downloadArchiveAtURL:(NSURL*)serverURL forReplacingURL:(nullable NSURL*)localURL publicKeys:(NSDictionary<NSString*, NSString*>*)publicKeys completionHandler:(void(^)(NSURL* extractedArchiveURL, NSError* error))completionHandler
{
	return [[OakDownloadArchiveTask alloc] initWithURL:serverURL forReplacingURL:localURL publicKeys:publicKeys completionHandler:completionHandler];
}

// ==================
// = Helper Methods =
// ==================

- (NSString*)signingKeyForPublicKeyString:(NSString*)publicKeyString
{
	id res = nil;
	if(NSData* publicKeyData = [publicKeyString dataUsingEncoding:NSUTF8StringEncoding])
	{
		SecItemImportExportKeyParameters params = { .keyUsage = nullptr, .keyAttributes = nullptr };
		SecExternalItemType type = kSecItemTypePublicKey;
		SecExternalFormat format = kSecFormatPEMSequence;

		CFArrayRef items = nullptr;
		OSStatus err = SecItemImport((CFDataRef)publicKeyData, nullptr, &format, &type, 0, &params, nullptr, &items);
		if(err == errSecSuccess)
		{
			if(SecKeyRef publicKey = (SecKeyRef)CFArrayGetValueAtIndex(items, 0))
				res = (__bridge id)publicKey;
			CFRelease(items);
		}
		else
		{
			if(CFStringRef message = SecCopyErrorMessageString(err, nullptr))
			{
				os_log_error(OS_LOG_DEFAULT, "SecItemImport() failed: %{public}@", message);
				CFRelease(message);
			}
		}
	}
	return res;
}

- (BOOL)data:(NSData*)contentData hasValidBase64EncodedSignature:(NSString*)encodedSignature usingPublicKeyString:(NSString*)publicKeyString
{
	if(!encodedSignature || !contentData || !publicKeyString)
		return NO;

	BOOL res = NO;
	if(NSData* signatureData = [[NSData alloc] initWithBase64EncodedString:encodedSignature options:0])
	{
		if(SecKeyRef publicKey = (SecKeyRef)CFBridgingRetain([self signingKeyForPublicKeyString:publicKeyString]))
		{
			CFErrorRef err = nullptr;
			if(SecTransformRef verifier = SecVerifyTransformCreate(publicKey, (CFDataRef)signatureData, &err))
			{
				if(SecTransformSetAttribute(verifier, kSecTransformInputAttributeName, (CFDataRef)contentData, &err))
				{
					if(SecTransformExecute(verifier, &err) == kCFBooleanTrue)
						res = YES;
					else if(err)
						os_log_error(OS_LOG_DEFAULT, "SecTransformExecute: %{public}@", err);
				}
				else
				{
					os_log_error(OS_LOG_DEFAULT, "SecTransformSetAttribute: %{public}@", err);
				}
				CFRelease(verifier);
			}
			else
			{
				os_log_error(OS_LOG_DEFAULT, "SecVerifyTransformCreate: %{public}@", err);
			}
			CFRelease(publicKey);
		}
	}
	else
	{
		os_log_error(OS_LOG_DEFAULT, "Unable to decode signature: %{public}@", encodedSignature);
	}
	return res;
}
@end
