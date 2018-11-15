#import "OakFinderTag.h"
#import <OakFoundation/OakFoundation.h>
#import <OakAppKit/NSColor Additions.h>
#import <io/path.h>

static struct label_colors_t { NSString* name; NSString* backgroundColor; NSString* foregroundColor; } const labelColors[] =
{
	{ @"Clear",  @"#69696900", @"#696969" },
	{ @"Gray",   @"#939396",   @"#7E7E82" },
	{ @"Green",  @"#5EC53F",   @"#42B71F" },
	{ @"Purple", @"#C46FDA",   @"#B855D1" },
	{ @"Blue",   @"#3FA8F0",   @"#2096EC" },
	{ @"Yellow", @"#F0C63A",   @"#EDB916" },
	{ @"Red",    @"#FB494A",   @"#FB282C" },
	{ @"Orange", @"#FD9938",   @"#FD8510" },
};

@interface OakFinderTag ()
@property (nonatomic) NSUInteger label;
@end

@implementation OakFinderTag
- (instancetype)initWithDisplayName:(NSString*)name label:(NSUInteger)label
{
	if(self = [super init])
	{
		_displayName = name;
		_label = label;
	}
	return self;
}

+ (instancetype)tagWithDisplayName:(NSString*)name label:(NSUInteger)label
{
	return [[OakFinderTag alloc] initWithDisplayName:name label:label];
}

- (BOOL)hasLabelColor
{
	return _label == 0 ? NO : YES;
}

- (NSColor*)backgroundColor
{
	return [NSColor colorWithString:labelColors[_label].backgroundColor];
}

- (NSColor*)foregroundColor
{
	return [NSColor colorWithString:labelColors[_label].foregroundColor];
}

- (id)copyWithZone:(NSZone*)zone { return [[OakFinderTag alloc] initWithDisplayName:_displayName label:_label]; }
- (NSUInteger)hash               { return [_displayName hash]; }
- (BOOL)isEqual:(id)otherObject  { return [otherObject isKindOfClass:[self class]] && [_displayName isEqualToString:[otherObject displayName]]; }
- (NSString*)description         { return [NSString stringWithFormat:@"<%@: %@ (%@)>", self.class, _displayName, @(_label)]; }
@end

@implementation OakFinderTagManager
+ (NSArray<OakFinderTag*>*)finderTagsForURL:(NSURL*)aURL
{
	if(aURL.filePathURL)
	{
		std::string const bplist = path::get_attr(aURL.fileSystemRepresentation, "com.apple.metadata:_kMDItemUserTags");
		if(bplist != NULL_STR)
		{
			NSData* data = [NSData dataWithBytes:(void*)bplist.data() length:bplist.size()];
			return [self finderTagsFromData:data];
		}
	}
	return @[ ];
}

+ (NSArray<OakFinderTag*>*)finderTagsFromData:(NSData*)data
{
	id plist = [NSPropertyListSerialization propertyListWithData:data options:NSPropertyListImmutable format:nil error:nil];
	NSMutableArray<OakFinderTag*>* finderTags = [NSMutableArray array];
	for(NSString* tag in plist)
	{
		NSArray* tagComponents = [tag componentsSeparatedByString:@"\n"];
		if([tagComponents count] == 2)
				[finderTags addObject:[OakFinderTag tagWithDisplayName:tagComponents[0] label:[tagComponents[1] integerValue]]];
		else	[finderTags addObject:[OakFinderTag tagWithDisplayName:tagComponents[0] label:0]];
	}

	return [finderTags copy];
}

+ (NSArray<OakFinderTag*>*)favoriteFinderTags
{
	NSUserDefaults* finderDefaults = [[NSUserDefaults alloc] initWithSuiteName:@"com.apple.finder"];
	NSArray<NSString*>* favoriteTagNames = [finderDefaults arrayForKey:@"FavoriteTagNames"];
	if(!favoriteTagNames)
		favoriteTagNames = @[ @"Red", @"Orange", @"Yellow", @"Green", @"Blue", @"Purple" ];

	NSMutableArray<OakFinderTag*>* tags = [NSMutableArray new];
	for(NSString* name in favoriteTagNames)
	{
		if(OakIsEmptyString(name))
			continue;

		auto it = std::find_if(std::begin(labelColors), std::end(labelColors), [=](label_colors_t const& labelColors){ return [labelColors.name isEqualToString:name]; });
		NSUInteger label = it != std::end(labelColors) ? std::distance(std::begin(labelColors), it) : 0;

		[tags addObject:[[OakFinderTag alloc] initWithDisplayName:name label:label]];
	}
	return [tags copy];
}
@end
