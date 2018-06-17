#import "Utilities.h"
#import "GenieTask.h"
#import "GenieManager.h" // cacheFolderByAppendingPathComponent:
#import "hash.h"
#import <os/log.h>

#define UNICODE
#include <MediaInfo/MediaInfo.h>

static NSTimeInterval GetMediaDuration (NSString* path)
{
	char const* p = [path fileSystemRepresentation];
	size_t len = strlen(p)+1;
	wchar_t buf[len];
	mbstowcs(buf, p, len);

	MediaInfoLib::MediaInfo info;
	if(info.Open(buf) == 1)
	{
		std::wstring duration = info.Get(MediaInfoLib::Stream_Video, 0, L"Duration");
		info.Close();
		try {
			return std::stod(duration);
		}
		catch (...) {
		}
	}

	return 0;
}

// =================
// = GenieDatabase =
// =================

@interface GenieDatabase ()
{
	NSTimer* _saveTimer;
}
@property (nonatomic) NSString* path;
@property (nonatomic, getter = isDirty) BOOL dirty;
@end

@implementation GenieDatabase
- (instancetype)init
{
	return [self initWithPath:nil];
}

- (instancetype)initWithPath:(NSString*)aPath
{
	if(self = [super init])
	{
		_path = aPath;
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	}
	return self;
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self name:NSApplicationWillTerminateNotification object:NSApp];
}

- (void)setDirty:(BOOL)flag
{
	if(_saveTimer)
	{
		[_saveTimer invalidate];
		_saveTimer = nil;
	}

	if(_dirty = flag)
		_saveTimer = [NSTimer scheduledTimerWithTimeInterval:2 target:self selector:@selector(saveTimerDidFire:) userInfo:nil repeats:NO];
}

- (void)saveTimerDidFire:(NSTimer*)aTimer
{
	[self synchronizeIfNeeded];
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	[self synchronizeIfNeeded];
}

- (void)synchronizeIfNeeded
{
	if(self.isDirty)
	{
		[self writeToFile:self.path];
		self.dirty = NO;
	}
}

- (void)writeToFile:(NSString*)path
{
}
@end

// ======================
// = GenieMediaDuration =
// ======================

@interface GenieMediaDuration ()
{
	NSMutableDictionary<NSString*, NSArray<void(^)(NSTimeInterval)>*>* _callbacks;
	NSMutableDictionary<NSString*, NSDictionary*>* _durations;
}
@end

@implementation GenieMediaDuration
+ (instancetype)sharedInstance
{
	static GenieMediaDuration* sharedInstance = [[self alloc] init];
	return sharedInstance;
}

- (instancetype)init
{
	if(self = [super initWithPath:[[GenieManager.sharedInstance cacheFolderByAppendingPathComponent:nil] stringByAppendingPathComponent:@"MediaDuration.plist"]])
	{
		_callbacks = [NSMutableDictionary dictionary];

		NSDictionary* cache = [NSDictionary dictionaryWithContentsOfFile:self.path];
		_durations = [cache[@"durations"] mutableCopy] ?: [NSMutableDictionary dictionary];
	}
	return self;
}

- (void)writeToFile:(NSString*)aPath
{
	NSDictionary* cache = @{
		@"durations": _durations,
	};
	[cache writeToFile:aPath atomically:YES];
}

- (NSTimeInterval)durationForPath:(NSString*)aPath
{
	return aPath ? [_durations[aPath][@"duration"] doubleValue] : 0;
}

- (void)obtainDurationForPath:(NSString*)aPath andCallback:(void(^)(NSTimeInterval))aCallback
{
	if(!aPath)
		return;

	if(NSDictionary* record = _durations[aPath])
	{
		if(NSTimeInterval duration = [record[@"duration"] doubleValue])
			aCallback(duration);
	}
	else if(NSArray<void(^)(NSTimeInterval)>* existingCallbacks = _callbacks[aPath])
	{
		_callbacks[aPath] = [existingCallbacks arrayByAddingObject:aCallback];
	}
	else
	{
		_callbacks[aPath] = @[ aCallback ];

		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			NSTimeInterval duration = GetMediaDuration(aPath);
			dispatch_async(dispatch_get_main_queue(), ^{
				NSMutableDictionary* record = [NSMutableDictionary dictionary];
				record[@"created"]              = [NSDate date];
				record[@"duration"]             = duration ? @(duration/1000) : nil;
				record[@"fileModificationDate"] = [[NSFileManager.defaultManager attributesOfItemAtPath:aPath error:nullptr] fileModificationDate];
				_durations[aPath] = record;

				self.dirty = YES;

				NSArray<void(^)(NSTimeInterval)>* callbacks = _callbacks[aPath];
				_callbacks[aPath] = nil;

				if(duration)
				{
					for(void(^callback)(NSTimeInterval) in callbacks)
						callback(duration/1000);
				}
				else
				{
					os_log_info(OS_LOG_DEFAULT, "Failed to obtain duration for: %{public}@", aPath);
				}
			});
		});
	}
}
@end

// =====================
// = GenieFavoriteIcon =
// =====================

@interface GenieFavoriteIcon ()
{
	NSString* _script;
	NSString* _directory;
	NSMutableDictionary<NSString*, NSDictionary*>* _favorites;
	NSMutableDictionary<NSString*, NSDictionary*>* _images;

	NSMutableDictionary<NSString*, NSArray<void(^)(NSString*)>*>* _favoriteUrlCallbacks;
	NSMutableDictionary<NSString*, NSArray<void(^)(NSImage*)>*>*  _favoriteImageCallbacks;
}
@end

@implementation GenieFavoriteIcon
+ (instancetype)sharedInstance
{
	static GenieFavoriteIcon* sharedInstance = [[self alloc] init];
	return sharedInstance;
}

- (instancetype)init
{
	if(self = [super initWithPath:[[GenieManager.sharedInstance cacheFolderByAppendingPathComponent:nil] stringByAppendingPathComponent:@"FavoriteIcons.plist"]])
	{
		_favoriteUrlCallbacks   = [NSMutableDictionary dictionary];
		_favoriteImageCallbacks = [NSMutableDictionary dictionary];

		_script    = [[NSBundle bundleForClass:[self class]] pathForResource:@"get-favicon" ofType:@"rb"];
		_directory = [GenieManager.sharedInstance cacheFolderByAppendingPathComponent:@"FavoriteIcons"];

		NSDictionary* cache = [NSDictionary dictionaryWithContentsOfFile:self.path];
		_favorites = [cache[@"favorites"] mutableCopy] ?: [NSMutableDictionary dictionary];
		_images    = [cache[@"images"] mutableCopy]    ?: [NSMutableDictionary dictionary];
	}
	return self;
}

- (void)writeToFile:(NSString*)aPath
{
	NSDictionary* cache = @{
		@"favorites": _favorites,
		@"images":    _images,
	};
	[cache writeToFile:aPath atomically:YES];
}

// ========================
// = Fetch Image from URL =
// ========================

- (NSImage*)imageForURL:(NSString*)imageUrl
{
	if(NSDictionary* imageCache = _images[imageUrl])
	{
		if(NSString* imagePath = imageCache[@"imagePath"])
		{
			if(![imagePath hasPrefix:@"/"])
				imagePath = [_directory stringByAppendingPathComponent:imagePath];

			if([[NSFileManager defaultManager] fileExistsAtPath:imagePath])
			{
				if(NSImage* image = [[NSImage alloc] initByReferencingFile:imagePath])
					return image;
			}
		}
	}
	return nil;
}

- (void)obtainImageForURL:(NSString*)urlString andCallback:(void(^)(NSImage*))aCallback
{
	if(NSImage* image = [self imageForURL:urlString])
	{
		aCallback(image);
	}
	else if(NSArray<void(^)(NSImage*)>* existingCallbacks = _favoriteImageCallbacks[urlString])
	{
		_favoriteImageCallbacks[urlString] = [existingCallbacks arrayByAddingObject:aCallback];
	}
	else
	{
		_favoriteImageCallbacks[urlString] = @[ aCallback ];
		NSString* directory = _directory;

		NSURL* url = [NSURL URLWithString:urlString];
		NSURLSessionDownloadTask* downloadTask = [NSURLSession.sharedSession downloadTaskWithURL:url completionHandler:^(NSURL* location, NSURLResponse* response, NSError* error){
			if(location && !error)
			{
				NSString* srcPath = location.filePathURL.path;
				NSString* dstName = [hash(urlString) stringByAppendingPathExtension:[url.path pathExtension]];
				NSString* dstPath = [directory stringByAppendingPathComponent:dstName];
				if([NSFileManager.defaultManager moveItemAtPath:srcPath toPath:dstPath error:nullptr])
				{
					dispatch_async(dispatch_get_main_queue(), ^{
						_images[urlString] = @{
							@"created":   [NSDate date],
							@"imagePath": dstName,
						};
						self.dirty = YES;

						NSArray<void(^)(NSImage*)>* callbacks = _favoriteImageCallbacks[urlString];
						_favoriteImageCallbacks[urlString] = nil;

						if(NSImage* image = [[NSImage alloc] initByReferencingFile:dstPath])
						{
							for(void(^callback)(NSImage*) in callbacks)
								callback(image);
						}
					});
				}
			}
		}];
		[downloadTask resume];
	}
}

// ============================================
// = Fetch Favorite Icon Information from URL =
// ============================================

- (NSString*)favoriteIconURLForURL:(NSString*)urlString
{
	if(NSDictionary* record = _favorites[urlString])
	{
		if(NSArray* images = record[@"images"])
			return images.firstObject[@"url"];
	}
	return nil;
}

- (void)obtainFavoriteIconURLForURL:(NSString*)urlString andCallback:(void(^)(NSString*))aCallback
{
	if(NSString* imageUrl = [self favoriteIconURLForURL:urlString])
	{
		aCallback(imageUrl);
	}
	else if(NSArray<void(^)(NSString*)>* existingCallbacks = _favoriteUrlCallbacks[urlString])
	{
		_favoriteUrlCallbacks[urlString] = [existingCallbacks arrayByAddingObject:aCallback];
	}
	else
	{
		_favoriteUrlCallbacks[urlString] = @[ aCallback ];

		GenieTask* task = [[GenieTask alloc] initWithCommand:@[ _script, urlString ] directory:nil];
		task.timeOut = 30; // 30s
		[task launch:^(int rc, NSData* stdoutData, NSData* stderrData){
			NSArray<void(^)(NSString*)>* callbacks = _favoriteUrlCallbacks[urlString];
			_favoriteUrlCallbacks[urlString] = nil;

			if(rc == 0 && stdoutData.length)
			{
				@try {
					NSDictionary* json = [NSJSONSerialization JSONObjectWithData:stdoutData options:0 error:nullptr];
					if(json && [json isKindOfClass:[NSArray class]])
					{
						NSMutableDictionary* record = [NSMutableDictionary dictionary];
						record[@"created"] = [NSDate date];
						record[@"images"]  = json;
						_favorites[urlString] = record;

						self.dirty = YES;

						if(NSString* favoriteURLString = [self favoriteIconURLForURL:urlString])
						{
							for(void(^callback)(NSString*) in callbacks)
								callback(favoriteURLString);
						}
					}
				}
				@catch (NSException* e) {
					os_log_error(OS_LOG_DEFAULT, "exception parsing JSON output: %{public}@", e);
				}
			}
			else
			{
				NSString* stdoutStr = [[NSString alloc] initWithData:stdoutData encoding:NSUTF8StringEncoding];
				NSString* stderrStr = [[NSString alloc] initWithData:stderrData encoding:NSUTF8StringEncoding];
				os_log_error(OS_LOG_DEFAULT, "%{public}@ returned with code %d\n%{public}@\n%{public}@", [_script lastPathComponent], rc, stderrStr, stdoutStr);
			}
		}];
	}
}

// ==============
// = Public API =
// ==============

- (NSString*)urlStringWithoutQueryFromURL:(NSURL*)aURL
{
	NSString* str = aURL.absoluteString;
	for(NSString* specialChar in @[ @"?", @"#" ])
	{
		NSRange query = [str rangeOfString:specialChar];
		if(query.location != NSNotFound)
			str = [str substringToIndex:query.location];
	}
	return str;
}

- (NSImage*)favoriteIconForURL:(NSURL*)aURL
{
	NSString* urlString = [self urlStringWithoutQueryFromURL:aURL];
	if(NSString* imageUrl = [self favoriteIconURLForURL:urlString])
		return [self imageForURL:imageUrl];
	return nil;
}

- (void)obtainFavoriteIconForURL:(NSURL*)aURL andCallback:(void(^)(NSImage*))aCallback
{
	NSString* urlString = [self urlStringWithoutQueryFromURL:aURL];
	[self obtainFavoriteIconURLForURL:urlString andCallback:^(NSString* imageUrl){
		[self obtainImageForURL:imageUrl andCallback:^(NSImage* image){
			aCallback(image);
		}];
	}];
}
@end
