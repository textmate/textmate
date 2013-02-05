#import "FSItem.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <io/path.h>
#import <oak/oak.h>
#import <oak/debug.h>

@implementation FSItem { OBJC_WATCH_LEAKS(FSItem); }
- (FSItem*)initWithURL:(NSURL*)anURL
{
	if((self = [super init]))
	{
		self.url = anURL;
		if([anURL isFileURL])
		{
			self.icon         = [OakFileIconImage fileIconImageWithPath:[anURL path] size:NSMakeSize(16, 16)];
			self.name         = [NSString stringWithCxxString:path::display_name([[anURL path] fileSystemRepresentation])];
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
	return [otherObject isKindOfClass:[self class]] && [self.url isEqual:[otherObject url]];
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"FSItem (%p): %@", self, [self.url absoluteString]];
}

- (NSString*)path
{
	return [self.url path];
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
