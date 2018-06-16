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
{
	NSUInteger        _needsSetupMask;
	BOOL              _exists;
	BOOL              _directory;
	BOOL              _alias;
	scm::status::type _scmStatus;
}
@property (nonatomic)                       NSArray*          imageStack;
@property (nonatomic)                       NSString*         path;
@property (nonatomic)                       BOOL              exists;
@property (nonatomic, getter = isDirectory) BOOL              directory;
@property (nonatomic, getter = isAlias)     BOOL              alias;
@property (nonatomic)                       scm::status::type scmStatus;
@property (nonatomic, getter = isModified)  BOOL              modified;
@end

enum {
	kNeedExists    = 1,
	kNeedDirectory = 2,
	kNeedAlias     = 4,
	kNeedSCMStatus = 8
};

@implementation OakFileIconImageRep
- (id)init
{
	if((self = [super init]))
	{
		_exists = YES;
	}
	return self;
}

- (id)initWithPath:(NSString*)aPath isModified:(BOOL)modifiedFlag
{
	if((self = [self init]))
	{
		_path           = aPath;
		_exists         = NO;
		_modified       = modifiedFlag;
		_needsSetupMask = (kNeedExists|kNeedDirectory|kNeedAlias|kNeedSCMStatus);
	}
	return self;
}

- (id)copyWithZone:(NSZone*)zone
{
	OakFileIconImageRep* copy = [super copyWithZone:zone];
	copy->_imageStack     = _imageStack;
	copy->_path           = _path;
	copy->_exists         = _exists;
	copy->_directory      = _directory;
	copy->_alias          = _alias;
	copy->_scmStatus      = _scmStatus;
	copy->_needsSetupMask = _needsSetupMask;
	copy->_modified       = _modified;
	return copy;
}

- (NSArray*)imageStack
{
	if(!_imageStack)
	{
		NSMutableArray* res = [NSMutableArray array];
		_imageStack = res;

		if(self.path && self.exists)
		{
			if(!self.isDirectory)
			{
				if(NSImage* customImage = CustomIconForPath(self.path))
				{
					[res addObject:customImage];
					if(self.isAlias)
					{
						if(NSImage* imageBadge = IconBadgeForAlias())
							[res addObject:imageBadge];
					}
				}
			}

			if(res.count == 0)
			{
				NSImage* image;
				if(![[NSURL fileURLWithPath:self.path isDirectory:self.isDirectory] getResourceValue:&image forKey:NSURLEffectiveIconKey error:NULL])
					image = [[NSWorkspace sharedWorkspace] iconForFile:self.path];

				if(image)
					[res addObject:image];
			}
		}
		else if(self.exists)
		{
			if(NSImage* image = [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(self.isDirectory ? kGenericFolderIcon : kGenericDocumentIcon)])
				[res addObject:image];
		}
		else
		{
			[res addObject:SystemIconForHFSType(kUnknownFSObjectIcon)];
		}

		if(NSImage* scmStatusImage = BadgeForSCMStatus(self.scmStatus))
			[res addObject:scmStatusImage];
	}
	return _imageStack;
}

- (BOOL)draw
{
	NSSize dstSize = self.size;

	NSImage* buffer = nil;
	if(self.isModified)
	{
		// If we use `self.size` for the off-screen image buffer then
		// we get a scaled up 16×16 image in the file chooser (⌘T).
		//
		// My theory is that when drawing the image, the image size
		// is 16×16 but the graphics context uses a transformation so
		// that each point is multiple pixels, which our off-screen
		// image does not replicate.
		//
		// I do not know how to get the actual “pixel size” of the
		// destination, so using the largest image is a workaround
		// and knowing the actual size could give a better result.

		for(NSImage* image in self.imageStack)
		{
			if(dstSize.width * dstSize.height < image.size.width * image.size.height)
				dstSize = image.size;
		}

		buffer = [[NSImage alloc] initWithSize:dstSize];
		[buffer lockFocus];
	}

	NSCompositingOperation op = NSCompositingOperationCopy;
	for(NSImage* image in self.imageStack)
	{
		[image drawInRect:(NSRect){ NSZeroPoint, dstSize } fromRect:NSZeroRect operation:op fraction:1];
		op = NSCompositingOperationSourceOver;
	}

	if(self.isModified)
	{
		[buffer unlockFocus];
		[buffer drawInRect:(NSRect){ NSZeroPoint, self.size } fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:0.4];
	}

	return YES;
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
			_exists    = !(_needsSetupMask & kNeedExists)    ? _exists    : YES;
			_directory = !(_needsSetupMask & kNeedDirectory) ? _directory : S_ISDIR(buf.st_mode);
			_alias     = !(_needsSetupMask & kNeedAlias)     ? _alias     : S_ISLNK(buf.st_mode);
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

- (BOOL)exists                 { [self checkSetupMask]; return _exists;    }
- (BOOL)isDirectory            { [self checkSetupMask]; return _directory; }
- (BOOL)isAlias                { [self checkSetupMask]; return _alias;     }
- (scm::status::type)scmStatus { [self checkSetupMask]; return _scmStatus; }

- (void)setExists:(BOOL)newExists
{
	_needsSetupMask &= ~kNeedExists;
	_exists = newExists;
}

- (void)setDirectory:(BOOL)newDirectory
{
	_needsSetupMask &= ~kNeedDirectory;
	_directory = newDirectory;
}

- (void)setAlias:(BOOL)newAlias
{
	_needsSetupMask &= ~kNeedAlias;
	_alias = newAlias;
}

- (void)setScmStatus:(scm::status::type)newScmStatus
{
	_needsSetupMask &= ~kNeedSCMStatus;
	_scmStatus = newScmStatus;
}
@end

// ====================
// = OakFileIconImage =
// ====================

@interface OakFileIconImage ()
@property (nonatomic) OakFileIconImageRep* fileIconImageRep;
@end

@implementation OakFileIconImage
- (id)initWithRepresentation:(OakFileIconImageRep*)aRepresentation size:(NSSize)aSize
{
	if((self = [super initWithSize:aSize]))
	{
		_fileIconImageRep = aRepresentation;
		[self addRepresentation:_fileIconImageRep];
	}
	return self;
}

- (id)initWithPath:(NSString*)aPath isModified:(BOOL)modifiedFlag size:(NSSize)aSize
{
	return [self initWithRepresentation:[[OakFileIconImageRep alloc] initWithPath:aPath isModified:modifiedFlag] size:aSize];
}

- (id)initWithSize:(NSSize)aSize
{
	return [self initWithRepresentation:[[OakFileIconImageRep alloc] init] size:aSize];
}

- (id)init
{
	return [self initWithSize:NSZeroSize];
}

- (id)copyWithZone:(NSZone*)zone
{
	OakFileIconImage* copy = [super copyWithZone:zone];
	copy->_fileIconImageRep = (OakFileIconImageRep*)[[copy representations] firstObject];
	return copy;
}

- (NSString*)path              { return self.fileIconImageRep.path;        }
- (BOOL)exists                 { return self.fileIconImageRep.exists;      }
- (BOOL)isDirectory            { return self.fileIconImageRep.isDirectory; }
- (BOOL)isAlias                { return self.fileIconImageRep.isAlias;     }
- (scm::status::type)scmStatus { return self.fileIconImageRep.scmStatus;   }
- (BOOL)isModified             { return self.fileIconImageRep.isModified;  }

- (void)recache
{
	self.fileIconImageRep.imageStack = nil;
	[super recache];
}

- (void)setPath:(NSString*)newPath
{
	self.fileIconImageRep.path = newPath;
	[self recache];
}

- (void)setExists:(BOOL)newExists
{
	self.fileIconImageRep.exists = newExists;
	[self recache];
}

- (void)setDirectory:(BOOL)newDirectory
{
	self.fileIconImageRep.directory = newDirectory;
	[self recache];
}

- (void)setAlias:(BOOL)newAlias
{
	self.fileIconImageRep.alias = newAlias;
	[self recache];
}

- (void)setScmStatus:(scm::status::type)newScmStatus
{
	self.fileIconImageRep.scmStatus = newScmStatus;
	[self recache];
}

- (void)setModified:(BOOL)newModified
{
	self.fileIconImageRep.modified = newModified;
	[self recache];
}

+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag size:(NSSize)aSize { return [[self alloc] initWithPath:aPath isModified:flag size:aSize]; }
+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag                    { return [self fileIconImageWithPath:aPath isModified:flag size:NSMakeSize(16, 16)]; }
+ (id)fileIconImageWithPath:(NSString*)aPath size:(NSSize)aSize                       { return [self fileIconImageWithPath:aPath isModified:NO size:aSize]; }
@end
