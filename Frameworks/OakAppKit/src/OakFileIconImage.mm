#import "OakFileIconImage.h"
#import "NSImage Additions.h"
#import <io/path.h>
#import <oak/CocoaSTL.h>

@interface OakFileIconImage ()
- (void)refreshFileStatus;
@property (nonatomic, retain) NSImage* base;
@property (nonatomic, retain) NSImage* badge;
@end

// ===============================
// = Custom Image Representation =
// ===============================

@interface OakFileIconImageRep : NSImageRep
{
	OakFileIconImage* image;
}
- (id)initWithImage:(OakFileIconImage*)anImage;
@end

@implementation OakFileIconImageRep
- (id)initWithImage:(OakFileIconImage*)anImage
{
	if((self = [super init]))
	{
		image = anImage;
		self.size = anImage.size;
	}
	return self;
}

- (id)copyWithZone:(NSZone*)zone
{
	OakFileIconImageRep* copy = [super copyWithZone:zone];
	copy->image = image;
	return copy;
}

- (void)dealloc
{
	[super dealloc];
}

- (BOOL)draw
{
	[image refreshFileStatus];

	NSImage* buffer = nil;
	if(image.isModified)
	{
		buffer = [[[NSImage alloc] initWithSize:[self size]] autorelease];
		[buffer lockFocus];
	}

	[image.base drawInRect:(NSRect){ NSZeroPoint, self.size } fromRect:NSZeroRect operation:NSCompositeCopy fraction:1];
	[image.badge drawInRect:(NSRect){ NSZeroPoint, self.size } fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1];

	if(image.isModified)
	{
		[buffer unlockFocus];
		[buffer drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:0.4];
	}

	return YES;
}
@end

static NSImage* IconImageForPath (NSString* path)
{
	std::multimap<ssize_t, NSString*> ordering;
	NSDictionary* bindings = [NSDictionary dictionaryWithContentsOfFile:[[NSBundle bundleForClass:[OakFileIconImage class]] pathForResource:@"bindings" ofType:@"plist"]];
	iterate(pair, bindings)
	{
		for(NSString* ext in pair->second)
		{
			if(ssize_t rank = path::rank([path UTF8String], [ext UTF8String]))
				ordering.insert(std::make_pair(rank, pair->first));
		}
	}

	if(!ordering.empty())
	{
		struct stat buf;
		if(lstat([path fileSystemRepresentation], &buf) == 0)
		{
			if(S_ISREG(buf.st_mode) || S_ISLNK(buf.st_mode))
			{
				NSImage* res = [NSImage imageNamed:ordering.begin()->second inSameBundleAsClass:[OakFileIconImage class]];

				IconRef iconRef;
				if(S_ISLNK(buf.st_mode) && GetIconRef(kOnSystemDisk, kSystemIconsCreator, kAliasBadgeIcon, &iconRef) == noErr)
				{
					NSImage* badge = [[[NSImage alloc] initWithIconRef:iconRef] autorelease];
					ReleaseIconRef(iconRef);

					res = [[res copy] autorelease];
					[res setSize:NSMakeSize(16, 16)];
					[res lockFocus];
					[badge drawInRect:(NSRect){ NSZeroPoint, res.size } fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1];
					[res unlockFocus];
				}
				return res;
			}
		}
	}

	IconRef iconRef;
	FSRef fileRef;

	bool didGetIconRef = false;
	if([path isEqualToString:@"/"]) // this is an optimization — not sure why, but it takes a second or so to get this icon on my system
		didGetIconRef = GetIconRef(kOnSystemDisk, 0, kGenericHardDiskIcon, &iconRef) == noErr;
	else if(FSPathMakeRefWithOptions((UInt8 const*)[path fileSystemRepresentation], kFSPathMakeRefDoNotFollowLeafSymlink, &fileRef, NULL) == noErr)
		didGetIconRef = GetIconRefFromFileInfo(&fileRef, 0, NULL, 0, NULL, kIconServicesNormalUsageFlag, &iconRef, NULL) == noErr;

	NSImage* image = nil;
	if(didGetIconRef)
	{
		image = [[[NSImage alloc] initWithIconRef:iconRef] autorelease]; // the reason we use IconRef is that it adds a badge to symbolic links
		ReleaseIconRef(iconRef);
	}

	return image ?: [[NSWorkspace sharedWorkspace] iconForFile:path];
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

@implementation OakFileIconImage
@synthesize path, isModified, base, badge;

- (void)setExistsOnDisk:(BOOL)flag
{
	if(existsOnDisk != flag)
	{
		existsOnDisk = flag;
		self.base    = existsOnDisk ? IconImageForPath(path) : [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(kUnknownFSObjectIcon)];
	}
}

- (void)setScmStatus:(scm::status::type)newScmStatus
{
	if(scmStatus != newScmStatus)
	{
		scmStatus = newScmStatus;
		self.badge = [[BadgeForSCMStatus(scmStatus) copy] autorelease];
	}
}

- (void)refreshFileStatus
{
	if(!path)
		return;

	if(!scmDriver)
		scmDriver = scm::info([path fileSystemRepresentation]);

	[self setExistsOnDisk:lstat([path fileSystemRepresentation], &(struct stat){ 0 }) == 0];
	[self setScmStatus:scmDriver ? scmDriver->status([path fileSystemRepresentation]) : scm::status::none];
}

- (void)setSize:(NSSize)aSize
{
	for(NSImageRep* imageRep in [self representations])
		[imageRep setSize:aSize];
	[super setSize:aSize];
}

- (id)initWithWithPath:(NSString*)aPath isModified:(BOOL)flag size:(NSSize)aSize
{
	if((self = [super initWithSize:aSize]))
	{
		self.path    = aPath;
		isModified   = flag;
		existsOnDisk = NO;
		scmStatus    = scm::status::none;

		self.base    = [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(kUnknownFSObjectIcon)];

		[self addRepresentation:[[[OakFileIconImageRep alloc] initWithImage:self] autorelease]];
	}
	return self;
}

- (void)dealloc
{
	[path release];
	[base release];
	[badge release];
	[super dealloc];
}

+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag size:(NSSize)aSize { return [[[self alloc] initWithWithPath:aPath isModified:flag size:aSize] autorelease]; }
+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag                    { return [self fileIconImageWithPath:aPath isModified:flag size:NSMakeSize(16, 16)]; }
+ (id)fileIconImageWithPath:(NSString*)aPath size:(NSSize)aSize                       { return [self fileIconImageWithPath:aPath isModified:NO size:aSize]; }
@end