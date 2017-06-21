#import "NSImage Additions.h"

@implementation NSImage (ImageFromBundle)

+ (NSImage *)imageNamed:(NSString *)aName isTemplate:(BOOL)templ
{
	if(!aName) {
		return nil;
	}
	NSImage* res = [NSImage imageNamed:aName];
	[res setTemplate:templ];
	return res;
}

+ (NSImage*)imageNamed:(NSString*)aName inSameBundleAsClass:(id)aClass
{
	if(!aName)
		return nil;

	NSBundle* bundle = [NSBundle bundleForClass:aClass];
	NSString* name   = [NSString stringWithFormat:@"%@.%@", [bundle bundleIdentifier], aName];

	static NSMutableDictionary* cache = [NSMutableDictionary new];
	if(NSImage* res = [cache objectForKey:name])
		return res;

	if(NSImage* image = [bundle imageForResource:aName])
	{
		[cache setObject:image forKey:name];
		return image;
	}

	return nil;
}

@end
