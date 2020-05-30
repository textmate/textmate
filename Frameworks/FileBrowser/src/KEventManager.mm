#import "KEventManager.h"

static os_log_t const kLogEventManager = os_log_create("KEventManager", "");

static NSString* to_ns (NSUInteger mask)
{
	struct { NSUInteger flag; NSString* description; } const flags[] = {
		{ DISPATCH_VNODE_DELETE,  @"DISPATCH_VNODE_DELETE"  },
		{ DISPATCH_VNODE_WRITE,   @"DISPATCH_VNODE_WRITE"   },
		{ DISPATCH_VNODE_EXTEND,  @"DISPATCH_VNODE_EXTEND"  },
		{ DISPATCH_VNODE_ATTRIB,  @"DISPATCH_VNODE_ATTRIB"  },
		{ DISPATCH_VNODE_LINK,    @"DISPATCH_VNODE_LINK"    },
		{ DISPATCH_VNODE_RENAME,  @"DISPATCH_VNODE_RENAME"  },
		{ DISPATCH_VNODE_REVOKE,  @"DISPATCH_VNODE_REVOKE"  },
		{ DISPATCH_VNODE_FUNLOCK, @"DISPATCH_VNODE_FUNLOCK" },
	};

	NSMutableArray<NSString*>* array = [NSMutableArray array];
	for(auto info : flags)
	{
		if((mask & info.flag) != 0)
			[array addObject:info.description];
	}
	return [array componentsJoinedByString:@"|"];
}

static bool paths_share_inode (NSString* lhs, NSString* rhs)
{
	struct stat lhsStatBuf, rhsStatBuf;
	if(stat(lhs.fileSystemRepresentation, &lhsStatBuf) == 0 && stat(rhs.fileSystemRepresentation, &rhsStatBuf) == 0)
		return lhsStatBuf.st_ino == rhsStatBuf.st_ino && lhsStatBuf.st_dev == rhsStatBuf.st_dev;
	return false;
}

@class KEventManagerCallback;

@interface KEventManagerNode : NSObject
- (void)addCallback:(KEventManagerCallback*)callback;
- (void)removeCallback:(KEventManagerCallback*)callback;
@end

@interface KEventManager ()
- (KEventManagerNode*)nodeForURL:(NSURL*)url makeIfNecessary:(BOOL)flag;
@end

// =========================
// = KEventManagerCallback =
// =========================

@interface KEventManagerCallback : NSObject
@property (nonatomic, readonly) void(^handler)(NSURL*, NSUInteger);
@property (nonatomic) KEventManagerNode* node;
@end

@implementation KEventManagerCallback
- (instancetype)initWithBlock:(void(^)(NSURL*, NSUInteger))handler
{
	if(self = [super init])
	{
		_handler = handler;
	}
	return self;
}

- (void)removeFromKEventManagerNode
{
	[_node removeCallback:self];
}
@end

// =====================
// = KEventManagerNode =
// =====================

@interface KEventManagerNode ()
{
	dispatch_source_t _dispatchSource;
	int _fileDescriptor;
	NSMutableArray<KEventManagerCallback*>* _callbacks;
}
@property (nonatomic) NSMapTable<NSString*, KEventManagerNode*>* childNodesMap;
@property (nonatomic) KEventManagerNode* parentNode;

@property (nonatomic, readonly) NSString* pathComponent;
@property (nonatomic, readonly) NSEnumerator<KEventManagerNode*>* childNodes;
@property (nonatomic, getter = isAccessible) BOOL accessible;
- (instancetype)initWithPathComponent:(NSString*)pathComponent parentNode:(KEventManagerNode*)parentNode;
- (void)dumpNodesWithIndent:(NSUInteger)level;
@end

@implementation KEventManagerNode
- (instancetype)initWithPathComponent:(NSString*)pathComponent parentNode:(KEventManagerNode*)parentNode
{
	if(self = [super init])
	{
		_fileDescriptor = -1;
		_pathComponent  = pathComponent;
		_childNodesMap  = [NSMapTable strongToWeakObjectsMapTable];
		_callbacks      = [NSMutableArray array];

		[self addToParentNode:parentNode];
		[self checkAccessible];
	}
	return self;
}

- (void)dealloc
{
	os_log_debug(kLogEventManager, "[%{public}@ dealloc] %{public}@", [self class], _pathComponent);
	[self removeFromParent];
	self.accessible = NO;
}

- (void)addCallback:(KEventManagerCallback*)callback
{
	[_callbacks addObject:callback];
	callback.node = self;
}

- (void)removeCallback:(KEventManagerCallback*)callback
{
	[_callbacks removeObject:callback];
	callback.node = nil;
}

- (NSEnumerator<KEventManagerNode*>*)childNodes
{
	return _childNodesMap.objectEnumerator;
}

- (NSString*)path
{
	return _parentNode.pathComponent ? [_parentNode.path stringByAppendingPathComponent:_pathComponent] : _pathComponent;
}

- (void)addChildNode:(KEventManagerNode*)child
{
	[_childNodesMap setObject:child forKey:child.pathComponent];
	child.parentNode = self;
}

- (void)removeChildNode:(KEventManagerNode*)child
{
	[_childNodesMap removeObjectForKey:child.pathComponent];
	child.parentNode = nil;
}

- (void)addToParentNode:(KEventManagerNode*)parentNode
{
	[parentNode addChildNode:self];
}

- (void)removeFromParent
{
	[_parentNode removeChildNode:self];
}

- (void)setAccessible:(BOOL)isAccessible
{
	if(_accessible == isAccessible)
		return;

	if(_accessible = isAccessible)
	{
		[self setUpEventSource];

		for(KEventManagerNode* childNode in self.childNodes)
			[childNode checkAccessible];
	}
	else
	{
		[self tearDownEventSource];

		for(KEventManagerNode* childNode in self.childNodes)
			childNode.accessible = NO;
	}

	NSUInteger mask = _accessible == YES ? DISPATCH_VNODE_WRITE : DISPATCH_VNODE_DELETE;
	for(KEventManagerCallback* callback in [_callbacks copy])
		callback.handler([NSURL fileURLWithPath:self.path], mask);
}

- (void)checkAccessible
{
	if(NSString* path = self.path)
		self.accessible = access(path.fileSystemRepresentation, F_OK) != -1;
}

- (void)setUpEventSource
{
	if(NSString* path = self.path)
	{
		if(_fileDescriptor != -1)
		{
			os_log_error(kLogEventManager, "[%{public}@ setUpEventSource] Event source already exists for %{public}@", [self class], path);
			return;
		}

		_fileDescriptor = open(self.path.fileSystemRepresentation, O_EVTONLY|O_CLOEXEC, 0);
		if(_fileDescriptor != -1)
		{
			os_log_debug(kLogEventManager, "[%{public}@ setUpEventSource] %{public}@", [self class], path);

			if(dispatch_source_t dispatchSource = dispatch_source_create(DISPATCH_SOURCE_TYPE_VNODE, _fileDescriptor, DISPATCH_VNODE_DELETE|DISPATCH_VNODE_WRITE|DISPATCH_VNODE_EXTEND|DISPATCH_VNODE_RENAME|DISPATCH_VNODE_REVOKE, dispatch_get_main_queue()))
			{
				__weak KEventManagerNode* weakSelf = self;
				dispatch_source_set_event_handler(dispatchSource, ^{
					[weakSelf handleKEvent:dispatch_source_get_data(dispatchSource)];
				});

				_dispatchSource = dispatchSource;
				dispatch_resume(_dispatchSource);
			}
		}
		else
		{
			os_log_error(kLogEventManager, "[%{public}@ setUpEventSource] Unable to access %{public}@", [self class], path);
		}
	}
}

- (void)tearDownEventSource
{
	os_log_debug(kLogEventManager, "[%{public}@ tearDownEventSource] %{public}@", [self class], self.path);

	if(_dispatchSource)
	{
		dispatch_source_cancel(_dispatchSource);
		_dispatchSource = nullptr;
	}

	if(_fileDescriptor != -1)
	{
		close(_fileDescriptor);
		_fileDescriptor = -1;
	}
	else
	{
		os_log_error(kLogEventManager, "[%{public}@ tearDownEventSource] No event source for %{public}@", [self class], self.path);
	}
}

- (void)handleKEvent:(NSUInteger)mask
{
	os_log_debug(kLogEventManager, "[%{public}@ handleKEvent:%{public}@] %{public}@", [self class], to_ns(mask), self.path);

	if((mask & DISPATCH_VNODE_RENAME) != 0)
	{
		char buf[MAXPATHLEN];
		if(fcntl(_fileDescriptor, F_GETPATH, buf) == 0)
		{
			NSString* oldPath   = self.path;
			NSString* oldParent = oldPath.stringByDeletingLastPathComponent;
			NSString* newPath   = @(buf);
			NSString* newParent = newPath.stringByDeletingLastPathComponent;

			NSURLRelationship relationship;

			if([oldPath isEqualToString:newPath])
			{
				os_log_error(kLogEventManager, "[%{public}@ handleKEvent:%{public}@] Path unchanged %{public}@", [self class], to_ns(mask), newPath);
			}
			else if([NSFileManager.defaultManager getRelationship:&relationship ofDirectory:NSTrashDirectory inDomain:NSAllDomainsMask toItemAtURL:[NSURL fileURLWithPath:newPath] error:nullptr] && relationship == NSURLRelationshipContains)
			{
				os_log_info(kLogEventManager, "[%{public}@ handleKEvent:%{public}@] Item moved to trash %{public}@ → %{public}@", [self class], to_ns(mask), oldPath, newPath);
				[self didDeleteObservedPath];
			}
			else
			{
				if(![oldParent isEqualToString:newParent] && paths_share_inode(oldParent, newParent))
				{
					newPath = [oldParent stringByAppendingPathComponent:newPath.lastPathComponent];
					os_log_info(kLogEventManager, "[%{public}@ handleKEvent:%{public}@] Preserve symbolic name of ancestor %{public}@ → %{public}@ (ignored directory %{public}@)", [self class], to_ns(mask), oldPath, newPath, newParent);
				}

				if(access(oldPath.fileSystemRepresentation, F_OK) == 0)
				{
					if(paths_share_inode(oldPath, newPath))
					{
						os_log_info(kLogEventManager, "[%{public}@ handleKEvent:%{public}@] Old path exists after rename (%{public}@) with same inode, likely case change, use new path (%{public}@)", [self class], to_ns(mask), oldPath, newPath);
						[self didMoveObservedPathToPath:newPath];
					}
					else
					{
						os_log_info(kLogEventManager, "[%{public}@ handleKEvent:%{public}@] Old path exists after rename (%{public}@) ignore new path (%{public}@)", [self class], to_ns(mask), oldPath, newPath);
						[self tearDownEventSource];
						[self setUpEventSource];
						[self didUpdateObservedPath];

						// Original folder was replaced with new one, so check accessibility of children
						for(KEventManagerNode* childNode in self.childNodes)
							[childNode checkAccessible];
					}
				}
				else
				{
					os_log_info(kLogEventManager, "[%{public}@ handleKEvent:%{public}@] Rename %{public}@ → %{public}@", [self class], to_ns(mask), oldPath, newPath);
					[self didMoveObservedPathToPath:newPath];
				}
			}
		}
		else
		{
			os_log_error(kLogEventManager, "[%{public}@ handleKEvent:%{public}@] Unable to obtain new path for %{public}@", [self class], to_ns(mask), self.path);
		}
	}

	if((mask & DISPATCH_VNODE_WRITE) != 0 || (mask & DISPATCH_VNODE_EXTEND) != 0)
	{
		for(KEventManagerNode* childNode in self.childNodes)
		{
			if(childNode.accessible == NO)
				[childNode checkAccessible];
		}
		[self didUpdateObservedPath];
	}

	if((mask & DISPATCH_VNODE_DELETE) != 0 || (mask & DISPATCH_VNODE_REVOKE) != 0)
	{
		if((mask & DISPATCH_VNODE_RENAME) == 0)
			[self didDeleteObservedPath];
	}
}

- (void)didUpdateObservedPath
{
	NSUInteger mask = DISPATCH_VNODE_WRITE;
	for(KEventManagerCallback* callback in [_callbacks copy])
		callback.handler([NSURL fileURLWithPath:self.path], mask);
}

- (void)didMoveObservedPathToPath:(NSString*)newPath
{
	KEventManagerNode* newNode = [KEventManager.sharedInstance nodeForURL:[NSURL fileURLWithPath:newPath] makeIfNecessary:YES];

	for(KEventManagerNode* childNode in self.childNodes)
		[childNode addToParentNode:newNode];
	[_childNodesMap removeAllObjects];

	for(KEventManagerCallback* callback in [_callbacks copy])
		[newNode addCallback:callback];
	[_callbacks removeAllObjects];

	[newNode didRenameObservedPath];
}

- (void)didRenameObservedPath
{
	NSUInteger mask = DISPATCH_VNODE_RENAME;
	for(KEventManagerCallback* callback in [_callbacks copy])
		callback.handler([NSURL fileURLWithPath:self.path], mask);

	for(KEventManagerNode* childNode in self.childNodes)
		[childNode didRenameObservedPath];
}

- (void)didDeleteObservedPath
{
	// FIXME This may send DELETE followed by WRITE to observers (when overwritten)
	self.accessible = NO; // Remove observer from old inode and send DELETE
	[self checkAccessible]; // Check if new file has been written to observed path
}

- (void)dumpNodesWithIndent:(NSUInteger)level
{
	NSLog(@"%s- %@ (%lu, accessible %@)", std::string(2*level, ' ').c_str(), _pathComponent, _callbacks.count, _accessible ? @"YES" : @"NO\n");

	for(KEventManagerNode* childNode in self.childNodes)
		[childNode dumpNodesWithIndent:level + 1];
}
@end

// =================
// = KEventManager =
// =================

@interface KEventManager ()
@property (nonatomic, readonly) KEventManagerNode* rootNode;
@end

@implementation KEventManager
+ (instancetype)sharedInstance
{
	static KEventManager* sharedInstance = [self new];
	return sharedInstance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_rootNode = [[KEventManagerNode alloc] initWithPathComponent:nil parentNode:nil];
	}
	return self;
}

- (void)dealloc
{
	os_log_debug(kLogEventManager, "[%{public}@ dealloc]", [self class]);
}

- (KEventManagerNode*)nodeForURL:(NSURL*)url makeIfNecessary:(BOOL)flag
{
	NSMutableArray<NSString*>* pathComponents = [NSMutableArray array];

	NSNumber* isVolume;
	while(!([url getResourceValue:&isVolume forKey:NSURLIsVolumeKey error:nil] && isVolume.boolValue))
	{
		if(url.path.length < url.URLByDeletingLastPathComponent.path.length)
		{
			os_log_error(kLogEventManager, "-[KEventManager nodeForURL:makeIfNecessary:] Unable to obtain wellformed parent for %{public}@", url);
			return nil;
		}

		[pathComponents addObject:url.path.lastPathComponent];
		url = url.URLByDeletingLastPathComponent;
	}
	[pathComponents addObject:url.path];

	if(!isVolume)
	{
		os_log_error(kLogEventManager, "-[KEventManager nodeForURL:%{public}@ makeIfNecessary:%{public}s] No volume found in URL", url, flag ? "YES" : "NO");
		return nil;
	}

	KEventManagerNode* res = _rootNode;
	for(NSString* pathComponent in pathComponents.reverseObjectEnumerator)
	{
		KEventManagerNode* child = [res.childNodesMap objectForKey:pathComponent];
		if(!child && flag)
			child = [[KEventManagerNode alloc] initWithPathComponent:pathComponent parentNode:res];
		res = child;
	}
	return res;
}

// ==============
// = Public API =
// ==============

- (id)addObserverToItemAtURL:(NSURL*)url usingBlock:(void(^)(NSURL*, NSUInteger))handler
{
	KEventManagerCallback* callback = [[KEventManagerCallback alloc] initWithBlock:handler];
	[[self nodeForURL:url makeIfNecessary:YES] addCallback:callback];
	return callback;
}

- (void)removeObserver:(id)someObserver
{
	[(KEventManagerCallback*)someObserver removeFromKEventManagerNode];
}

- (void)dumpNodes
{
	@autoreleasepool {
		for(KEventManagerNode* node in _rootNode.childNodes)
			[node dumpNodesWithIndent:0];
	}
}
@end
