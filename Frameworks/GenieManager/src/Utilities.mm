#import "Utilities.h"
#import "GenieTask.h"
#import "GenieManager.h" // cacheFolderByAppendingPathComponent:
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
	NSString* _cachePath;
	NSMutableDictionary<NSString*, NSString*>* _cache;
	NSMutableDictionary<NSString*, NSArray<void(^)(NSImage*)>*>* _callbacks;
	BOOL _dirty;
}
@end

@implementation GenieFavoriteIcon
+ (instancetype)sharedInstance
{
	static GenieFavoriteIcon* sharedInstance = [self new];
	return sharedInstance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_script    = [[NSBundle bundleForClass:[self class]] pathForResource:@"get-favicon" ofType:@"rb"];
		_callbacks = [NSMutableDictionary dictionary];

		_directory = [GenieManager.sharedInstance cacheFolderByAppendingPathComponent:@"FavIcons"];
		_cachePath = [_directory stringByAppendingPathComponent:@"Index.plist"];
		_cache     = [NSMutableDictionary dictionaryWithContentsOfFile:_cachePath] ?: [NSMutableDictionary dictionary];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	}
	return self;
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	[self synchronize];
}

- (void)synchronize
{
	if(_dirty)
	{
		[_cache writeToFile:_cachePath atomically:YES];
		_dirty = NO;
	}
}

- (NSString*)stringFromURL:(NSURL*)aURL
{
	NSString* str = [aURL absoluteString];
	for(NSString* specialChar in @[ @"?", @"#" ])
	{
		NSRange query = [str rangeOfString:specialChar];
		if(query.location != NSNotFound)
			str = [str substringToIndex:query.location];
	}
	return str;
}

- (NSImage*)imageForURL:(NSURL*)aURL
{
	NSString* urlString = [self stringFromURL:aURL];
	if(NSString* path = _cache[urlString])
	{
		if([[NSFileManager defaultManager] fileExistsAtPath:path])
		{
			if(NSImage* image = [[NSImage alloc] initByReferencingFile:path])
				return image;
		}
	}
	return nil;
}

- (void)obtainImageForURL:(NSURL*)aURL andCallback:(void(^)(NSImage*))aCallback
{
	NSString* urlString = [self stringFromURL:aURL];

	if(NSArray<void(^)(NSImage*)>* existingCallbacks = _callbacks[urlString])
	{
		_callbacks[urlString] = [existingCallbacks arrayByAddingObject:aCallback];
	}
	else
	{
		_callbacks[urlString] = @[ aCallback ];

		GenieTask* task = [[GenieTask alloc] initWithCommand:@[ _script, urlString ] directory:_directory];
		task.timeOut = 30; // 30s
		[task launch:^(int rc, NSData* stdoutData, NSData* stderrData){
			NSArray<void(^)(NSImage*)>* callbacks = _callbacks[urlString];
			_callbacks[urlString] = nil;

			if(rc == 0 && stdoutData.length)
			{
				NSString* path = [[NSString alloc] initWithData:stdoutData encoding:NSUTF8StringEncoding];
				path = [[_directory stringByAppendingPathComponent:path] stringByStandardizingPath];
				if(path && [[NSFileManager defaultManager] fileExistsAtPath:path])
				{
					_cache[urlString] = path;
					_dirty = YES;

					if(NSImage* image = [[NSImage alloc] initByReferencingFile:path])
					{
						for(void(^callback)(NSImage*) in callbacks)
							callback(image);
					}
				}
			}
			else
			{
				NSString* stdoutStr = [[NSString alloc] initWithData:stdoutData encoding:NSUTF8StringEncoding];
				NSString* stderrStr = [[NSString alloc] initWithData:stderrData encoding:NSUTF8StringEncoding];
				os_log_error(OS_LOG_DEFAULT, "%@ returned with code %d\n%@\n%@", [_script lastPathComponent], rc, stderrStr, stdoutStr);
			}
		}];
	}
}
@end
