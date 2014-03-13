#import "OakFileIconImage.h"
#import "NSImage Additions.h"
#import <io/path.h>
#import <scm/scm.h>

// ========================
// = Obtain Various Icons =
// ========================

static NSImage* CustomIconForPath (NSString* path)
{
	static NSMutableDictionary* bindings = [NSMutableDictionary new];

	static dispatch_once_t onceToken = 0;
	dispatch_once(&onceToken, ^{
		NSDictionary* map = [NSDictionary dictionaryWithContentsOfFile:[[NSBundle bundleForClass:[OakFileIconImage class]] pathForResource:@"bindings" ofType:@"plist"]];
		for(NSString* key in map)
		{
			for(NSString* ext in map[key])
				bindings[[ext lowercaseString]] = key;
		}
	});

	NSString* pathName = [[path lastPathComponent] lowercaseString];
	NSString* imageName = bindings[pathName];

	NSRange range = [pathName rangeOfString:@"."];
	if(range.location != NSNotFound)
	{
		imageName = bindings[[pathName substringFromIndex:NSMaxRange(range)]];
		imageName = imageName ?: bindings[[pathName pathExtension]];
	}

	NSImage* res = nil;
	if(imageName)
	{
		static NSMutableDictionary* images = [NSMutableDictionary new];
		@synchronized(images) {
			if(!(res = images[imageName]))
			{
				if(res = [NSImage imageNamed:imageName inSameBundleAsClass:[OakFileIconImage class]])
					images[imageName] = res;
			}
		}
	}
	return res;
}

static NSImage* IconBadgeForAlias ()
{
	IconRef iconRef;
	if(GetIconRef(kOnSystemDisk, kSystemIconsCreator, kAliasBadgeIcon, &iconRef) == noErr)
	{
		NSImage* badge = [[NSImage alloc] initWithIconRef:iconRef];
		ReleaseIconRef(iconRef);
		return badge;
	}
	return nil;
}

static NSImage* SystemIconForHFSType (OSType hfsFileTypeCode)
{
	return [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(hfsFileTypeCode)];
}

static NSImage* BadgeForSCMStatus (scm::status::type scmStatus)
{
	switch(scmStatus)
	{
		case scm::status::conflicted:   return [NSImage imageNamed:@"Conflicted"  inSameBundleAsClass:[OakFileIconImage class]];
		case scm::status::modified:     return [NSImage imageNamed:@"Modified"    inSameBundleAsClass:[OakFileIconImage class]];
		case scm::status::added:        return [NSImage imageNamed:@"Added"       inSameBundleAsClass:[OakFileIconImage class]];
		case scm::status::deleted:      return [NSImage imageNamed:@"Deleted"     inSameBundleAsClass:[OakFileIconImage class]];
		case scm::status::unversioned:  return [NSImage imageNamed:@"Unversioned" inSameBundleAsClass:[OakFileIconImage class]];
		case scm::status::mixed:        return [NSImage imageNamed:@"Mixed"       inSameBundleAsClass:[OakFileIconImage class]];
	}
	return nil;
}

// ===============================
// = Custom Image Representation =
// ===============================

@interface OakFileIconImageRep : NSImageRep
#if !defined(MAC_OS_X_VERSION_10_7) || (MAC_OS_X_VERSION_MIN_REQUIRED <= MAC_OS_X_VERSION_10_7)
@property (nonatomic, assign) OakFileIconImage* fileIconImage;
#else
@property (nonatomic, weak) OakFileIconImage* fileIconImage;
#endif
@property (nonatomic) NSArray* imageStack;
@end

@implementation OakFileIconImageRep
- (id)copyWithZone:(NSZone*)zone
{
	OakFileIconImageRep* copy = [super copyWithZone:zone];
	copy->_fileIconImage = _fileIconImage;
	copy->_imageStack    = _imageStack;
	return copy;
}

- (NSArray*)imageStack
{
	if(!_imageStack)
	{
		NSMutableArray* res = [NSMutableArray array];
		_imageStack = res;

		if(_fileIconImage.path && _fileIconImage.exists)
		{
			if(!_fileIconImage.isDirectory)
			{
				if(NSImage* customImage = CustomIconForPath(_fileIconImage.path))
				{
					[res addObject:customImage];
					if(_fileIconImage.isAlias)
					{
						if(NSImage* imageBadge = IconBadgeForAlias())
							[res addObject:imageBadge];
					}
				}
			}

			if(res.count == 0)
			{
				NSImage* image;
				if(![[NSURL fileURLWithPath:_fileIconImage.path isDirectory:_fileIconImage.isDirectory] getResourceValue:&image forKey:NSURLEffectiveIconKey error:NULL])
					image = [[NSWorkspace sharedWorkspace] iconForFile:_fileIconImage.path];

				if(image)
					[res addObject:image];
			}
		}
		else if(_fileIconImage.exists)
		{
			if(NSImage* image = [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(_fileIconImage.isDirectory ? kGenericFolderIcon : kGenericDocumentIcon)])
				[res addObject:image];
		}
		else
		{
			[res addObject:SystemIconForHFSType(kUnknownFSObjectIcon)];
		}

		if(NSImage* scmStatusImage = BadgeForSCMStatus(_fileIconImage.scmStatus))
			[res addObject:scmStatusImage];
	}
	return _imageStack;
}

- (BOOL)draw
{
	NSImage* buffer = nil;
	if(_fileIconImage.isModified)
	{
		buffer = [[NSImage alloc] initWithSize:[self size]];
		[buffer lockFocus];
	}

	NSCompositingOperation op = NSCompositeCopy;
	for(NSImage* image in self.imageStack)
	{
		[image drawInRect:(NSRect){ NSZeroPoint, self.size } fromRect:NSZeroRect operation:op fraction:1];
		op = NSCompositeSourceOver;
	}

	if(_fileIconImage.isModified)
	{
		[buffer unlockFocus];
		[buffer drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:0.4];
	}

	return YES;
}
@end

// ====================
// = OakFileIconImage =
// ====================

@interface OakFileIconImage ()
{
	BOOL              _exists;
	BOOL              _directory;
	BOOL              _alias;
	scm::status::type _scmStatus;
	NSUInteger        _needsSetupMask;
}
@property (nonatomic) OakFileIconImageRep* fileIconImageRep;
@end

enum {
	kNeedExists    = 1,
	kNeedDirectory = 2,
	kNeedAlias     = 4,
	kNeedSCMStatus = 8
};

@implementation OakFileIconImage
- (id)initWithSize:(NSSize)aSize
{
	if((self = [super initWithSize:aSize]))
	{
		_fileIconImageRep = [OakFileIconImageRep new];
		_fileIconImageRep.fileIconImage = self;
		[self addRepresentation:_fileIconImageRep];
		_exists = YES;
	}
	return self;
}

- (id)init
{
	return [self initWithSize:NSZeroSize];
}

- (id)initWithPath:(NSString*)aPath isModified:(BOOL)modifiedFlag size:(NSSize)aSize
{
	if((self = [self initWithSize:aSize]))
	{
		_path       = aPath;
		_modified   = modifiedFlag;
		_exists     = NO;

		_needsSetupMask = (kNeedExists|kNeedDirectory|kNeedAlias|kNeedSCMStatus);
	}
	return self;
}

- (void)checkSetupMask
{
	if(!_path || !_needsSetupMask)
		return;

	std::string const path = [_path fileSystemRepresentation];
	if((_needsSetupMask & (kNeedExists|kNeedDirectory|kNeedAlias)) != 0)
	{
		struct stat buf;
		if(lstat(path.c_str(), &buf) == 0)
		{
			_exists    = YES;
			_directory = S_ISDIR(buf.st_mode);
			_alias     = S_ISLNK(buf.st_mode);
		}
		_needsSetupMask &= ~(kNeedExists|kNeedDirectory|kNeedAlias);
	}

	if((_needsSetupMask & kNeedSCMStatus) == kNeedSCMStatus)
	{
		if(auto scmDriver = scm::info(path::parent(path)))
			_scmStatus = scmDriver->status(path);
		_needsSetupMask &= ~kNeedSCMStatus;
	}
}

- (id)copyWithZone:(NSZone*)zone
{
	OakFileIconImage* copy = [super copyWithZone:zone];
	copy->_fileIconImageRep = _fileIconImageRep;
	copy->_path             = _path;
	copy->_exists           = _exists;
	copy->_directory        = _directory;
	copy->_alias            = _alias;
	copy->_scmStatus        = _scmStatus;
	copy->_needsSetupMask   = _needsSetupMask;
	copy->_modified         = _modified;
	return copy;
}

- (BOOL)exists                 { [self checkSetupMask]; return _exists;    }
- (BOOL)isDirectory            { [self checkSetupMask]; return _directory; }
- (BOOL)isAlias                { [self checkSetupMask]; return _alias;     }
- (scm::status::type)scmStatus { [self checkSetupMask]; return _scmStatus; }

- (void)recache
{
	self.fileIconImageRep.imageStack = nil;
	[super recache];
}

- (void)setPath:(NSString*)newPath
{
	if(_path != newPath && ![_path isEqualToString:newPath])
	{
		_path = newPath;
		[self recache];
	}
}

- (void)setExists:(BOOL)newExists
{
	_needsSetupMask &= ~kNeedExists;
	if(_exists != newExists)
	{
		_exists = newExists;
		[self recache];
	}
}

- (void)setDirectory:(BOOL)newDirectory
{
	_needsSetupMask &= ~kNeedDirectory;
	if(_directory != newDirectory)
	{
		_directory = newDirectory;
		[self recache];
	}
}

- (void)setAlias:(BOOL)newAlias
{
	_needsSetupMask &= ~kNeedAlias;
	if(_alias != newAlias)
	{
		_alias = newAlias;
		[self recache];
	}
}

- (void)setScmStatus:(scm::status::type)newScmStatus
{
	_needsSetupMask &= ~kNeedSCMStatus;
	if(_scmStatus != newScmStatus)
	{
		_scmStatus = newScmStatus;
		[self recache];
	}
}

- (void)setModified:(BOOL)newModified
{
	if(_modified != newModified)
	{
		_modified = newModified;
		[self recache];
	}
}

+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag size:(NSSize)aSize { return [[self alloc] initWithPath:aPath isModified:flag size:aSize]; }
+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag                    { return [self fileIconImageWithPath:aPath isModified:flag size:NSMakeSize(16, 16)]; }
+ (id)fileIconImageWithPath:(NSString*)aPath size:(NSSize)aSize                       { return [self fileIconImageWithPath:aPath isModified:NO size:aSize]; }
@end
