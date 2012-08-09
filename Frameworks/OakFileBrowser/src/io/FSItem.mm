#import "FSItem.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <io/path.h>
#import <oak/oak.h>

@implementation FSItem
@synthesize icon, name, toolTip, labelIndex, url, urlType, target, children, leaf, group, sortAsFolder;

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
	return [[[self alloc] initWithURL:anURL] autorelease];
}

- (id)copyWithZone:(NSZone*)zone
{
	return [self retain];
}

- (void)dealloc
{
	[icon release];
	[name release];
	[toolTip release];
	[url release];
	[target release];
	[children release];
	[super dealloc];
}

- (BOOL)isEqual:(id)otherObject
{
	return [otherObject isKindOfClass:[self class]] && [url isEqual:[otherObject url]];
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"FSItem: %@", [url absoluteString]];
}

- (NSString*)path
{
	return [self.url path];
}

- (FSItemURLType)urlType
{
	if(urlType == FSItemURLTypeUnknown && [(target ?: url) isFileURL])
	{
		uint32_t flags = path::info([[(target ?: url) path] fileSystemRepresentation]);
		if(!path::exists([[(target ?: url) path] fileSystemRepresentation]))
			urlType = FSItemURLTypeMissing;
		else if(flags & path::flag::alias)
			urlType = FSItemURLTypeAlias;
		else if(flags & path::flag::package)
			urlType = FSItemURLTypePackage;
		else if(flags & path::flag::directory)
			urlType = FSItemURLTypeFolder;
		else if(flags & path::flag::file)
			urlType = FSItemURLTypeFile;
	}
	return urlType;
}
@end
