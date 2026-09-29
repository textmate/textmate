#import "OakFinderTag.h"
#import "NSColor Additions.h"
#import <OakFoundation/src/OakFoundation.h>
#import <io/src/path.h>
#import <ns/src/ns.h>

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

- (NSColor*)labelColor
{
	switch(_label)
	{
		case 1: return [NSColor systemGrayColor];
		case 2: return [NSColor systemGreenColor];
		case 3: return [NSColor systemPurpleColor];
		case 4: return [NSColor systemBlueColor];
		case 5: return [NSColor systemYellowColor];
		case 6: return [NSColor systemRedColor];
		case 7: return [NSColor systemOrangeColor];
		default: return nil;
	}
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
	static std::map<std::string, int> const labelColors =
	{
		{ "Gray",   1 },
		{ "Green",  2 },
		{ "Purple", 3 },
		{ "Blue",   4 },
		{ "Yellow", 5 },
		{ "Red",    6 },
		{ "Orange", 7 },
	};

	NSUserDefaults* finderDefaults = [[NSUserDefaults alloc] initWithSuiteName:@"com.apple.finder"];
	NSArray<NSString*>* favoriteTagNames = [finderDefaults arrayForKey:@"FavoriteTagNames"];
	if(!favoriteTagNames)
		favoriteTagNames = @[ @"Red", @"Orange", @"Yellow", @"Green", @"Blue", @"Purple", @"Gray" ];

	NSMutableArray<OakFinderTag*>* tags = [NSMutableArray new];
	for(NSString* name in favoriteTagNames)
	{
		if(OakIsEmptyString(name))
			continue;

		auto it = labelColors.find(to_s(name));
		NSUInteger label = it != labelColors.end() ? it->second : 0;

		[tags addObject:[[OakFinderTag alloc] initWithDisplayName:name label:label]];
	}
	return [tags copy];
}
@end
