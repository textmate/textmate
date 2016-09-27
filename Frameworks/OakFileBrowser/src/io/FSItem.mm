#import "FSItem.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakFileManager.h>
#import <OakAppKit/IOAlertPanel.h>
#import <ns/ns.h>
#import <io/path.h>
#import <oak/oak.h>
#import <oak/debug.h>

static ino_t inode (std::string const& path)
{
	struct stat buf;
	if(lstat(path.c_str(), &buf) == 0)
		return buf.st_ino;
	return 0;
}

@implementation FSItem { OBJC_WATCH_LEAKS(FSItem); }
- (FSItem*)initWithURL:(NSURL*)anURL
{
	if((self = [super init]))
	{
		self.url = anURL;
		if([anURL isFileURL])
		{
			self.icon         = [OakFileIconImage fileIconImageWithPath:[anURL path] size:NSMakeSize(16, 16)];
			self.displayName  = [NSString stringWithCxxString:path::display_name([[anURL path] fileSystemRepresentation])];
			self.leaf         = ![[anURL absoluteString] hasSuffix:@"/"];
			self.sortAsFolder = !self.leaf;
			self.target       = self.leaf ? nil : anURL;
		}
	}
	return self;
}

+ (FSItem*)itemWithURL:(NSURL*)anURL
{
	return [[self alloc] initWithURL:anURL];
}

- (id)copyWithZone:(NSZone*)zone
{
	return self;
}

- (BOOL)isEqual:(id)otherObject
{
	return otherObject && [otherObject isKindOfClass:[self class]] && [self.url isEqual:[otherObject url]] && self.scmStatus == [otherObject scmStatus] && self.isMissing == [otherObject isMissing];
}

- (NSUInteger)hash
{
	return [self.url hash];
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"FSItem (%p): %@ (%ld children)", self, [self.url absoluteString], [self.children count]];
}

- (NSString*)path
{
	return [self.url path];
}

- (scm::status::type)scmStatus
{
	return [_icon isKindOfClass:[OakFileIconImage class]] ? ((OakFileIconImage*)_icon).scmStatus : scm::status::unknown;
}

- (void)setScmStatus:(scm::status::type)newScmStatus
{
	if([_icon isKindOfClass:[OakFileIconImage class]] && newScmStatus != self.scmStatus)
	{
		OakFileIconImage* icon = [(OakFileIconImage*)_icon copy];
		icon.scmStatus = newScmStatus;
		self.icon = icon;
	}
}

- (BOOL)isModified
{
	return [_icon isKindOfClass:[OakFileIconImage class]] ? ((OakFileIconImage*)_icon).isModified : NO;
}

- (void)setModified:(BOOL)newState
{
	if([_icon isKindOfClass:[OakFileIconImage class]] && newState != self.isModified)
	{
		OakFileIconImage* icon = [(OakFileIconImage*)_icon copy];
		icon.modified = newState;
		self.icon = icon;
	}
}

- (BOOL)isMissing
{
	return [_icon isKindOfClass:[OakFileIconImage class]] ? !((OakFileIconImage*)_icon).exists : NO;
}

- (void)setMissing:(BOOL)newState
{
	if([_icon isKindOfClass:[OakFileIconImage class]] && newState != self.isMissing)
	{
		OakFileIconImage* icon = [(OakFileIconImage*)_icon copy];
		icon.exists = !newState;
		self.icon = icon;
	}
}

- (void)setLabelIndex:(NSInteger)newLabelIndex
{
	if(_labelIndex == newLabelIndex)
		return;

	_labelIndex = newLabelIndex;
	if([_url isFileURL])
	{
		if(!path::set_label_index([self.path fileSystemRepresentation], newLabelIndex))
			OakRunIOAlertPanel("Failed to change label color for “%s”.", [self.path fileSystemRepresentation]);
	}
}

- (BOOL)renameToName:(NSString*)newBasename view:(NSView*)view
{
	if(!_url.isFileURL || OakIsEmptyString(newBasename))
		return NO;

	std::string src = [_url.path fileSystemRepresentation];
	std::string dst = path::join(path::parent(src), [[newBasename stringByReplacingOccurrencesOfString:@"/" withString:@":"] fileSystemRepresentation]);
	if(src == dst)
		return NO;

	// ‘dst’ is only allowed to exist on case-insensitive file systems (Foo.txt → foo.txt)
	if(!path::exists(dst) || inode(src) == inode(dst))
	{
		NSURL* dstURL = [NSURL fileURLWithPath:to_ns(dst)];
		if([[OakFileManager sharedInstance] renameItemAtURL:_url toURL:dstURL view:view])
		{
			self.url         = dstURL;
			self.displayName = to_ns(path::display_name(dst));
			return YES;
		}
	}
	else
	{
		errno = EEXIST;
		OakRunIOAlertPanel("Failed to rename the file at “%s”.", path::name(src).c_str());
	}
	return NO;
}

- (FSItemURLType)urlType
{
	if(_urlType == FSItemURLTypeUnknown && [(self.target ?: self.url) isFileURL])
	{
		uint32_t flags = path::info([[(self.target ?: self.url) path] fileSystemRepresentation]);
		if(!path::exists([[(self.target ?: self.url) path] fileSystemRepresentation]))
			_urlType = FSItemURLTypeMissing;
		else if(flags & path::flag::alias)
			_urlType = FSItemURLTypeAlias;
		else if(flags & path::flag::package)
			_urlType = FSItemURLTypePackage;
		else if(flags & path::flag::directory)
			_urlType = FSItemURLTypeFolder;
		else if(flags & path::flag::file)
			_urlType = FSItemURLTypeFile;
	}
	return _urlType;
}
@end
