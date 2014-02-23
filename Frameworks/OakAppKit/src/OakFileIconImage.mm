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
@property (nonatomic)                       NSString*         path;
@property (nonatomic)                       BOOL              exists;
@property (nonatomic, getter = isDirectory) BOOL              directory;
@property (nonatomic, getter = isAlias)     BOOL              alias;
@property (nonatomic)                       scm::status::type scmStatus;
@property (nonatomic, getter = isModified)  BOOL              modified;
@property (nonatomic)                       NSArray*          imageStack;
@end

@implementation OakFileIconImageRep
- (id)copyWithZone:(NSZone*)zone
{
	OakFileIconImageRep* copy = [super copyWithZone:zone];
	copy->_path       = _path;
	copy->_exists     = _exists;
	copy->_directory  = _directory;
	copy->_alias      = _alias;
	copy->_scmStatus  = _scmStatus;
	copy->_modified   = _modified;
	copy->_imageStack = _imageStack;
	return copy;
}

- (NSArray*)imageStack
{
	if(!_imageStack)
	{
		NSMutableArray* res = [NSMutableArray array];
		_imageStack = res;

		if(_path && _exists)
		{
			if(!_directory)
			{
				if(NSImage* customImage = CustomIconForPath(_path))
				{
					[res addObject:customImage];
					if(_alias)
					{
						if(NSImage* imageBadge = IconBadgeForAlias())
							[res addObject:imageBadge];
					}
				}
			}

			if(res.count == 0)
			{
				NSImage* image;
				if(![[NSURL fileURLWithPath:_path isDirectory:_directory] getResourceValue:&image forKey:NSURLEffectiveIconKey error:NULL])
					image = [[NSWorkspace sharedWorkspace] iconForFile:_path];

				if(image)
					[res addObject:image];
			}
		}
		else if(_exists)
		{
			if(NSImage* image = [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(_directory ? kGenericFolderIcon : kGenericDocumentIcon)])
				[res addObject:image];
		}
		else
		{
			[res addObject:SystemIconForHFSType(kUnknownFSObjectIcon)];
		}

		if(NSImage* scmStatusImage = BadgeForSCMStatus(_scmStatus))
			[res addObject:scmStatusImage];
	}
	return _imageStack;
}

- (BOOL)draw
{
	NSImage* buffer = nil;
	if(self.isModified)
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

	if(self.isModified)
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
@property (nonatomic) OakFileIconImageRep* fileIconImageRep;
@end

@implementation OakFileIconImage
- (id)initWithSize:(NSSize)aSize
{
	if((self = [super initWithSize:aSize]))
	{
		_fileIconImageRep = [OakFileIconImageRep new];
		[self addRepresentation:_fileIconImageRep];
		self.exists = YES;
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
		self.path     = aPath;
		self.modified = modifiedFlag;
		self.exists   = NO;

		if(aPath)
		{
			std::string path = [aPath fileSystemRepresentation];

			struct stat buf;
			if(lstat(path.c_str(), &buf) == 0)
			{
				self.exists    = YES;
				self.directory = S_ISDIR(buf.st_mode);
				self.alias     = S_ISLNK(buf.st_mode);
			}

			if(auto scmDriver = scm::info(path::parent(path)))
				self.scmStatus = scmDriver->status(path);
		}
	}
	return self;
}

- (id)copyWithZone:(NSZone*)zone
{
	OakFileIconImage* copy = [super copyWithZone:zone];
	copy->_fileIconImageRep = _fileIconImageRep;
	return copy;
}

- (NSString*)path              { return _fileIconImageRep.path;        }
- (BOOL)exists                 { return _fileIconImageRep.exists;      }
- (BOOL)isDirectory            { return _fileIconImageRep.isDirectory; }
- (BOOL)isAlias                { return _fileIconImageRep.isAlias;     }
- (scm::status::type)scmStatus { return _fileIconImageRep.scmStatus;   }
- (BOOL)isModified             { return _fileIconImageRep.isModified;  }

- (void)recache
{
	self.fileIconImageRep.imageStack = nil;
	[super recache];
}

- (void)setPath:(NSString*)newPath
{
	if(self.path != newPath && ![self.path isEqualToString:newPath])
	{
		_fileIconImageRep.path = newPath;
		[self recache];
	}
}

- (void)setExists:(BOOL)newExists
{
	if(self.exists != newExists)
	{
		_fileIconImageRep.exists = newExists;
		[self recache];
	}
}

- (void)setDirectory:(BOOL)newDirectory
{
	if(self.isDirectory != newDirectory)
	{
		_fileIconImageRep.directory = newDirectory;
		[self recache];
	}
}

- (void)setAlias:(BOOL)newAlias
{
	if(self.isAlias != newAlias)
	{
		_fileIconImageRep.alias = newAlias;
		[self recache];
	}
}

- (void)setScmStatus:(scm::status::type)newScmStatus
{
	if(self.scmStatus != newScmStatus)
	{
		_fileIconImageRep.scmStatus = newScmStatus;
		[self recache];
	}
}

- (void)setModified:(BOOL)newModified
{
	if(self.isModified != newModified)
	{
		_fileIconImageRep.modified = newModified;
		[self recache];
	}
}

+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag size:(NSSize)aSize { return [[self alloc] initWithPath:aPath isModified:flag size:aSize]; }
+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag                    { return [self fileIconImageWithPath:aPath isModified:flag size:NSMakeSize(16, 16)]; }
+ (id)fileIconImageWithPath:(NSString*)aPath size:(NSSize)aSize                       { return [self fileIconImageWithPath:aPath isModified:NO size:aSize]; }
@end
