#import "LSPFileWatcher.h"
#import <sys/stat.h>

static NSArray<NSString*>* const kDefaultExcludes = @[
	@".git", @".hg", @".svn",
	@"node_modules", @"vendor", @".vendor",
	@"build", @"dist", @".cache", @".direnv",
	@"__pycache__", @".tox", @".venv", @"venv",
	@".mypy_cache", @".next", @".nuxt",
	@"target", @"Pods",
];

@interface LSPFileWatcher ()
{
	NSString* _rootDirectory;
	NSArray<NSString*>* _excludes;
	NSMutableSet<NSString*>* _extensions;
	NSMutableSet<NSString*>* _exactNames;
	// Directory-keyed snapshot: dirPath → {filePath → mtime}
	NSMutableDictionary<NSString*, NSMutableDictionary<NSString*, NSValue*>*>* _snapshot;
}
@end

@implementation LSPFileWatcher

- (instancetype)initWithRootDirectory:(NSString*)root excludes:(NSArray<NSString*>*)excludes
{
	if(self = [super init])
	{
		_rootDirectory = root;
		NSMutableArray* allExcludes = [kDefaultExcludes mutableCopy];
		if(excludes.count)
			[allExcludes addObjectsFromArray:excludes];
		_excludes   = [allExcludes copy];
		_extensions = [NSMutableSet new];
		_exactNames = [NSMutableSet new];
		_snapshot   = [NSMutableDictionary new];
	}
	return self;
}

- (void)addExtensions:(NSSet<NSString*>*)exts
{
	[_extensions unionSet:exts];
}

- (void)addExactNames:(NSSet<NSString*>*)names
{
	[_exactNames unionSet:names];
}

- (BOOL)shouldExcludeDirectory:(NSURL*)url
{
	NSString* name = url.lastPathComponent;
	for(NSString* exclude in _excludes)
	{
		if([exclude containsString:@"/"])
		{
			// Multi-component exclude: check relative path from root
			NSString* fullPath = url.path;
			NSString* rootPrefix = [_rootDirectory hasSuffix:@"/"] ? _rootDirectory : [_rootDirectory stringByAppendingString:@"/"];
			if([fullPath hasPrefix:rootPrefix])
			{
				NSString* relativePath = [fullPath substringFromIndex:rootPrefix.length];
				if([relativePath isEqualToString:exclude]
				|| [relativePath hasPrefix:[exclude stringByAppendingString:@"/"]]
				|| [relativePath hasSuffix:[@"/" stringByAppendingString:exclude]])
					return YES;
			}
		}
		else if([name isEqualToString:exclude])
		{
			return YES;
		}
	}
	return NO;
}

- (BOOL)fileMatchesFilters:(NSString*)path watchAll:(BOOL)watchAll extensions:(NSSet<NSString*>*)extensions exactNames:(NSSet<NSString*>*)exactNames
{
	if(watchAll)
		return YES;

	NSString* filename = path.lastPathComponent;

	if([exactNames containsObject:filename])
		return YES;

	if(extensions.count)
	{
		NSString* ext = path.pathExtension;
		if(ext.length)
		{
			NSString* dotExt = [@"." stringByAppendingString:ext.lowercaseString];
			if([extensions containsObject:dotExt])
				return YES;
		}
	}

	return NO;
}

- (BOOL)fileMatchesFilters:(NSString*)path
{
	return [self fileMatchesFilters:path watchAll:_watchAll extensions:_extensions exactNames:_exactNames];
}

- (NSDictionary<NSString*, NSValue*>*)scanDirectory:(NSString*)directory watchAll:(BOOL)watchAll extensions:(NSSet<NSString*>*)extensions exactNames:(NSSet<NSString*>*)exactNames
{
	NSMutableDictionary<NSString*, NSValue*>* result = [NSMutableDictionary new];
	NSFileManager* fm = [NSFileManager defaultManager];

	NSDirectoryEnumerator* enumerator = [fm enumeratorAtURL:[NSURL fileURLWithPath:directory]
		includingPropertiesForKeys:@[NSURLIsDirectoryKey, NSURLIsSymbolicLinkKey]
		options:0
		errorHandler:nil];

	for(NSURL* url in enumerator)
	{
		NSNumber* isDir = nil;
		[url getResourceValue:&isDir forKey:NSURLIsDirectoryKey error:nil];

		if(isDir.boolValue)
		{
			if([self shouldExcludeDirectory:url])
				[enumerator skipDescendants];
			continue;
		}

		NSNumber* isSymlink = nil;
		[url getResourceValue:&isSymlink forKey:NSURLIsSymbolicLinkKey error:nil];
		if(isSymlink.boolValue)
			continue;

		NSString* path = url.path;
		if(![self fileMatchesFilters:path watchAll:watchAll extensions:extensions exactNames:exactNames])
			continue;

		struct stat st;
		if(stat(path.fileSystemRepresentation, &st) == 0)
		{
			struct timespec mtime = st.st_mtimespec;
			NSValue* mtimeValue = [NSValue valueWithBytes:&mtime objCType:@encode(struct timespec)];
			result[path] = mtimeValue;
		}
	}

	return result;
}

- (NSDictionary<NSString*, NSValue*>*)scanDirectory:(NSString*)directory
{
	return [self scanDirectory:directory watchAll:_watchAll extensions:_extensions exactNames:_exactNames];
}

- (void)populateSnapshotFromScan:(NSDictionary<NSString*, NSValue*>*)scan
{
	[_snapshot removeAllObjects];
	for(NSString* path in scan)
	{
		NSString* dir = path.stringByDeletingLastPathComponent;
		NSMutableDictionary<NSString*, NSValue*>* dirEntries = _snapshot[dir];
		if(!dirEntries)
		{
			dirEntries = [NSMutableDictionary new];
			_snapshot[dir] = dirEntries;
		}
		dirEntries[path] = scan[path];
	}
}

- (void)performInitialScanOnQueue:(dispatch_queue_t)queue completion:(void(^)(void))completion
{
	// Snapshot filter state for thread-safe access on background queue
	BOOL watchAll = _watchAll;
	NSSet<NSString*>* extensions = [_extensions copy];
	NSSet<NSString*>* exactNames = [_exactNames copy];

	dispatch_async(queue, ^{
		NSDictionary<NSString*, NSValue*>* result = [self scanDirectory:_rootDirectory watchAll:watchAll extensions:extensions exactNames:exactNames];

		NSUInteger count = result.count;
		if(count > 10000)
			NSLog(@"[LSP] File watcher snapshot contains %lu files — consider adding lspFileWatchExclude", (unsigned long)count);
		else
			NSLog(@"[LSP] File watcher initial scan complete: %lu files", (unsigned long)count);

		dispatch_async(dispatch_get_main_queue(), ^{
			[self populateSnapshotFromScan:result];
			if(completion)
				completion();
		});
	});
}

// Snapshot diffing — must be called on main thread
- (NSArray<NSDictionary*>*)diffScanResult:(NSDictionary<NSString*, NSValue*>*)currentState forDirectory:(NSString*)dirPath
{
	NSMutableArray<NSDictionary*>* changes = [NSMutableArray new];

	// Collect all snapshot directories that fall under the changed path
	NSString* dirPrefix = [dirPath hasSuffix:@"/"] ? dirPath : [dirPath stringByAppendingString:@"/"];
	NSMutableDictionary<NSString*, NSValue*>* oldEntries = [NSMutableDictionary new];

	for(NSString* snapshotDir in _snapshot.allKeys)
	{
		if([snapshotDir isEqualToString:dirPath] || [snapshotDir hasPrefix:dirPrefix])
		{
			[oldEntries addEntriesFromDictionary:_snapshot[snapshotDir]];
		}
	}

	// New or changed files
	for(NSString* path in currentState)
	{
		NSValue* oldMtime = oldEntries[path];
		NSString* dir = path.stringByDeletingLastPathComponent;

		if(!oldMtime)
		{
			NSURL* fileURL = [NSURL fileURLWithPath:path];
			[changes addObject:@{@"uri": fileURL.absoluteString, @"type": @1}];

			if(!_snapshot[dir])
				_snapshot[dir] = [NSMutableDictionary new];
			_snapshot[dir][path] = currentState[path];
		}
		else
		{
			struct timespec oldTs, newTs;
			[oldMtime getValue:&oldTs];
			[currentState[path] getValue:&newTs];

			if(oldTs.tv_sec != newTs.tv_sec || oldTs.tv_nsec != newTs.tv_nsec)
			{
				NSURL* fileURL = [NSURL fileURLWithPath:path];
				[changes addObject:@{@"uri": fileURL.absoluteString, @"type": @2}];
				_snapshot[dir][path] = currentState[path];
			}
			[oldEntries removeObjectForKey:path];
		}
	}

	// Deleted files (in old snapshot but not in current scan)
	for(NSString* path in oldEntries)
	{
		NSURL* fileURL = [NSURL fileURLWithPath:path];
		[changes addObject:@{@"uri": fileURL.absoluteString, @"type": @3}];

		NSString* dir = path.stringByDeletingLastPathComponent;
		[_snapshot[dir] removeObjectForKey:path];
		if(_snapshot[dir].count == 0)
			[_snapshot removeObjectForKey:dir];
	}

	return changes;
}

- (void)asyncDiffForChangedDirectory:(NSString*)dirPath onQueue:(dispatch_queue_t)queue completion:(void(^)(NSArray<NSDictionary*>*))completion
{
	BOOL watchAll = _watchAll;
	NSSet<NSString*>* extensions = [_extensions copy];
	NSSet<NSString*>* exactNames = [_exactNames copy];

	dispatch_async(queue, ^{
		NSDictionary<NSString*, NSValue*>* currentState = [self scanDirectory:dirPath watchAll:watchAll extensions:extensions exactNames:exactNames];
		dispatch_async(dispatch_get_main_queue(), ^{
			NSArray<NSDictionary*>* changes = [self diffScanResult:currentState forDirectory:dirPath];
			if(completion)
				completion(changes);
		});
	});
}

- (void)clearSnapshot
{
	[_snapshot removeAllObjects];
}

@end
