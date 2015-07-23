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
	return [otherObject isKindOfClass:[self class]] && [self.url isEqual:[otherObject url]] && self.scmStatus == [otherObject scmStatus];
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

- (BOOL)setNewDisplayName:(NSString*)newDisplayName view:(NSView*)view
{
	if(![_url isFileURL] || OakIsEmptyString(newDisplayName))
		return NO;

	std::string src = [[_url path] fileSystemRepresentation];
	std::string dst = path::join(path::parent(src), [[newDisplayName stringByReplacingOccurrencesOfString:@"/" withString:@":"] fileSystemRepresentation]);

	// “hidden extension” is ignored if Finder is set to show all file extensions, if there are multiple extensions, or if no application is assigned to the extension.
	std::string const baseName    = path::name(src);
	std::string const displayName = path::display_name(src);
	bool hiddenExtension = baseName != displayName && (path::info(src) & path::flag::hidden_extension);

	BOOL res = NO;
	if(src == dst && hiddenExtension)
	{
		NSURL* dstURL = [NSURL fileURLWithPath:[NSString stringWithCxxString:dst]];
		NSError* error;
		if(!(res = [dstURL setResourceValue:@NO forKey:NSURLHasHiddenExtensionKey error:&error]))
			NSLog(@"error: failed to show extension for %@: %@", _url, error);
	}
	else
	{
		if(hiddenExtension)
			dst += path::extension(src);

		if(src != dst)
		{
			// ‘dst’ is allowed to exist on case-insensitive file systems (Foo.txt → foo.txt)
			if(path::exists(dst) && inode(src) != inode(dst))
			{
				errno = EEXIST;
				OakRunIOAlertPanel("Failed to rename the file at “%s”.", path::name(src).c_str());
			}
			else
			{
				NSURL* dstURL = [NSURL fileURLWithPath:[NSString stringWithCxxString:dst]];
				if(res = [[OakFileManager sharedInstance] renameItemAtURL:_url toURL:dstURL view:view])
				{
					self.url         = dstURL;
					self.displayName = [NSString stringWithCxxString:path::display_name(dst)];
				}
			}
		}
	}
	return res;
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
