#import "TMFileReference.h"

NSNotificationName const TMURLWillCloseNotification = @"TMURLWillCloseNotification";

@interface TMFileReference ()
{
	NSUInteger _openCount;
	NSUInteger _modifiedCount;
	NSImage* _image;
	NSImage* _icon;
}
@property (nonatomic, readonly) NSURL* URL;
@end

@implementation TMFileReference
+ (NSSet*)keyPathsForValuesAffectingIcon { return [NSSet setWithObjects:@"image", @"modified", nil]; }

+ (TMFileReference*)fileReferenceWithURL:(NSURL*)url
{
	NSAssert(NSThread.isMainThread, @"This method must be called on main thread");

	static NSMapTable<NSURL*, TMFileReference*>* _mapTable = [NSMapTable strongToWeakObjectsMapTable];

	if(!url)
		return nil;

	TMFileReference* bindingProxy = [_mapTable objectForKey:url.absoluteURL];
	if(!bindingProxy)
	{
		bindingProxy = [[TMFileReference alloc] initWithURL:url.absoluteURL];
		[_mapTable setObject:bindingProxy forKey:url.absoluteURL];
	}
	return bindingProxy;
}

+ (TMFileReference*)fileReferenceWithImage:(NSImage*)image
{
	return [[TMFileReference alloc] initWithImage:image];
}

+ (NSImage*)imageForURL:(NSURL*)url size:(NSSize)size
{
	NSImage* image = [[TMFileReference fileReferenceWithURL:url].image copy];
	image.size = size;
	return image;
}

- (instancetype)initWithURL:(NSURL*)url
{
	if(self = [super init])
		_URL = url;
	return self;
}

- (instancetype)initWithImage:(NSImage*)image
{
	if(self = [super init])
		_image = image;
	return self;
}

#ifndef NDEBUG
- (void)dealloc
{
	NSAssert(_openCount == 0, @"-[TMFileReference dealloc] Open count is not zero: %lu", _openCount);
}
#endif

- (NSUInteger)hash                    { return [_URL hash]; }
- (BOOL)isEqual:(TMFileReference*)rhs { return [rhs isKindOfClass:[self class]] && [_URL isEqual:rhs.URL]; }

- (BOOL)isClosable
{
	return _openCount != 0;
}

- (BOOL)isModified
{
	return _modifiedCount != 0;
}

- (NSImage*)icon
{
	NSImage* res = self.image;
	return self.isModified ? [NSImage imageWithSize:res.size flipped:NO drawingHandler:^BOOL(NSRect dstRect){
		[res drawInRect:dstRect fromRect:NSZeroRect operation:NSCompositingOperationCopy fraction:0.4];
		return YES;
	}] : res;
}

- (void)performClose:(id)sender
{
	[NSNotificationCenter.defaultCenter postNotificationName:TMURLWillCloseNotification object:self userInfo:@{ @"URL": _URL }];
}

- (void)increaseOpenCount
{
	[self willChangeValueForKey:@"closable"];
	++_openCount;
	[self didChangeValueForKey:@"closable"];
}

- (void)decreaseOpenCount
{
	NSAssert(_openCount != 0, @"-[TMFileReference decreaseOpenCount] Open count is zero");

	[self willChangeValueForKey:@"closable"];
	--_openCount;
	[self didChangeValueForKey:@"closable"];
}

- (void)increaseModifiedCount
{
	NSAssert(_openCount != 0, @"-[TMFileReference increaseModifiedCount] Open count is zero");

	[self willChangeValueForKey:@"modified"];
	++_modifiedCount;
	[self didChangeValueForKey:@"modified"];
}

- (void)decreaseModifiedCount
{
	NSAssert(_modifiedCount != 0, @"-[TMFileReference decreaseModifiedCount] Modified count is zero");

	[self willChangeValueForKey:@"modified"];
	--_modifiedCount;
	[self didChangeValueForKey:@"modified"];
}

// =========
// = Image =
// =========

- (void)setSCMStatus:(scm::status::type)newSCMStatus
{
	if(_SCMStatus == newSCMStatus)
		return;
	_SCMStatus = newSCMStatus;

	if(_image)
	{
		[self willChangeValueForKey:@"image"];
		_image = nil;
		[self didChangeValueForKey:@"image"];
	}
}

static NSImage* ImageNamed (NSString* imageName)
{
	if(!imageName)
		return nil;

	static NSMutableDictionary* imageCache = [NSMutableDictionary dictionary];

	@synchronized(imageCache) {
		if(!imageCache[imageName])
		{
			if(NSURL* imageURL = [NSBundle.mainBundle URLForResource:imageName withExtension:@"icns" subdirectory:@"DocumentTypes"])
				imageCache[imageName] = [[NSImage alloc] initWithContentsOfURL:imageURL];
		}
		return imageCache[imageName];
	}
}

- (NSImage*)image
{
	static NSDictionary* customBindings;
	static dispatch_once_t onceToken = 0;

	dispatch_once(&onceToken, ^{
		NSMutableDictionary* res = [NSMutableDictionary dictionary];
		NSDictionary* dict = [NSDictionary dictionaryWithContentsOfFile:[NSBundle.mainBundle pathForResource:@"DocumentTypes/bindings" ofType:@"plist"]];
		[dict enumerateKeysAndObjectsUsingBlock:^(NSString* file, NSArray<NSString*>* extensions, BOOL* stop){
			for(NSString* extension in extensions)
				res[extension.lowercaseString] = file;
		}];
		customBindings = [res copy];
	});

	if(!_image)
	{
		NSURL* url                  = _URL;
		scm::status::type scmStatus = _SCMStatus;

		_image = [NSImage imageWithSize:NSMakeSize(16, 16) flipped:NO drawingHandler:^BOOL(NSRect dstRect){
			BOOL drawLinkBadge = NO;

			NSImage* image;
			if(scmStatus == scm::status::deleted)
			{
				image = [NSWorkspace.sharedWorkspace iconForFileType:NSFileTypeForHFSTypeCode((OSType)kUnknownFSObjectIcon)];
			}
			else if(url.isFileURL)
			{
				NSError* error;
				if(!url.hasDirectoryPath)
				{
					NSString* pathName  = url.lastPathComponent.lowercaseString;
					NSString* imageName = customBindings[pathName];

					NSRange range = [pathName rangeOfString:@"."];
					if(range.location != NSNotFound)
					{
						imageName = customBindings[[pathName substringFromIndex:NSMaxRange(range)]];
						imageName = imageName ?: customBindings[pathName.pathExtension];
					}

					if(image = ImageNamed(imageName))
					{
						NSURLFileResourceType type;
						if([url getResourceValue:&type forKey:NSURLFileResourceTypeKey error:&error])
								drawLinkBadge = [type isEqual:NSURLFileResourceTypeSymbolicLink];
						else	os_log_error(OS_LOG_DEFAULT, "No NSURLFileResourceTypeKey for %{public}@: %{public}@", url.path.stringByAbbreviatingWithTildeInPath, error);
					}
				}

				if(!image)
				{
					if(![url getResourceValue:&image forKey:NSURLEffectiveIconKey error:&error])
					{
						os_log_error(OS_LOG_DEFAULT, "No NSURLEffectiveIconKey for %{public}@: %{public}@", url, error);
						return NO;
					}
				}
			}
			else
			{
				if([url.scheme isEqualToString:@"computer"])
					image = [NSImage imageNamed:NSImageNameComputer];
				else if(url.hasDirectoryPath)
					image = [NSWorkspace.sharedWorkspace iconForFileType:NSFileTypeForHFSTypeCode((OSType)kGenericFolderIcon)];
				else
					image = [NSWorkspace.sharedWorkspace iconForFileType:NSFileTypeForHFSTypeCode((OSType)kGenericDocumentIcon)];
			}

			[image drawInRect:dstRect fromRect:NSZeroRect operation:NSCompositingOperationCopy fraction:1];

			if(scmStatus != scm::status::none)
			{
				NSImage* badge;
				switch(scmStatus)
				{
					case scm::status::conflicted:   badge = ImageNamed(@"scm-badge-conflicted");  break;
					case scm::status::modified:     badge = ImageNamed(@"scm-badge-modified");    break;
					case scm::status::added:        badge = ImageNamed(@"scm-badge-added");       break;
					case scm::status::deleted:      badge = ImageNamed(@"scm-badge-deleted");     break;
					case scm::status::unversioned:  badge = ImageNamed(@"scm-badge-unversioned"); break;
					case scm::status::mixed:        badge = ImageNamed(@"scm-badge-mixed");       break;
				}

				if(badge)
					[badge drawInRect:dstRect fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:1];
			}

			if(drawLinkBadge)
			{
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
				IconRef iconRef;
				if(GetIconRef(kOnSystemDisk, kSystemIconsCreator, kAliasBadgeIcon, &iconRef) == noErr)
				{
					NSImage* badge = [[NSImage alloc] initWithIconRef:iconRef];
					[badge drawInRect:dstRect fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:1];
					ReleaseIconRef(iconRef);
				}
#pragma clang diagnostic pop
			}

			return YES;
		}];
	}
	return _image;
}
@end
