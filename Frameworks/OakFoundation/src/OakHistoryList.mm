#import "OakHistoryList.h"
#import "OakFoundation.h"
#import "NSArray Additions.h"
#import "NSString Additions.h"
#import <oak/debug.h>

OAK_DEBUG_VAR(Find_HistoryList);

static void StoreObjectAtKeyPath (id obj, std::string const& keyPath)
{
	NSUserDefaults* defaults = [NSUserDefaults standardUserDefaults];

	std::string::size_type sep = keyPath.find('.');
	if(sep == std::string::npos)
	{
		[defaults setObject:obj forKey:[NSString stringWithCxxString:keyPath]];
	}
	else
	{
		NSString* primary   = [NSString stringWithCxxString:keyPath.substr(0, sep)];
		NSString* secondary = [NSString stringWithCxxString:keyPath.substr(sep+1)];

		NSMutableDictionary* dict = [NSMutableDictionary dictionary];
		if(NSDictionary* existingDict = [defaults dictionaryForKey:primary])
			[dict setValuesForKeysWithDictionary:existingDict];
		[dict setObject:obj forKey:secondary];
		[defaults setObject:dict forKey:primary];
	}
}

static id RetrieveObjectAtKeyPath (std::string const& keyPath)
{
	NSUserDefaults* defaults = [NSUserDefaults standardUserDefaults];

	std::string::size_type sep = keyPath.find('.');
	if(sep == std::string::npos)
		return [defaults objectForKey:[NSString stringWithCxxString:keyPath]];

	NSString* primary   = [NSString stringWithCxxString:keyPath.substr(0, sep)];
	NSString* secondary = [NSString stringWithCxxString:keyPath.substr(sep+1)];
	return [[defaults dictionaryForKey:primary] objectForKey:secondary];
}

@interface OakHistoryList ()
@property (nonatomic, copy) NSString* name;
@property (nonatomic) NSMutableArray* list;
@property (nonatomic) NSUInteger stackSize;
@end

@implementation OakHistoryList
- (id)initWithName:(NSString*)defaultsName stackSize:(NSUInteger)size defaultItems:(id)firstItem, ...
{
	D(DBF_Find_HistoryList, bug("Creating list with name %s and %zu items\n", [defaultsName UTF8String], (size_t)size););
	if(self = [self init])
	{
		self.stackSize = size;
		self.name      = defaultsName;
		self.list      = [[NSMutableArray alloc] initWithCapacity:size];

		if(NSArray* array = RetrieveObjectAtKeyPath([self.name UTF8String]))
		{
			[self.list setArray:array];
		}
		else
		{
			va_list ap;
			va_start(ap, firstItem);
			while(firstItem)
			{
				[self.list addObject:firstItem];
				firstItem = va_arg(ap, id);
			}
			va_end(ap);
		}

		while([self.list count] > self.stackSize)
			[self.list removeLastObject];
	}
	return self;
}

- (id)initWithName:(NSString*)defaultsName stackSize:(NSUInteger)size
{
	return [self initWithName:defaultsName stackSize:size defaultItems:nil];
}

- (void)addObject:(id)newItem;
{
	D(DBF_Find_HistoryList, bug("adding %s to list %s\n", [[newItem description] UTF8String], [self.name UTF8String]););
	if(OakIsEmptyString(newItem) || [newItem isEqual:[self.list firstObject]])
		return;

	[self willChangeValueForKey:@"head"];
	[self willChangeValueForKey:@"currentObject"];
	[self willChangeValueForKey:@"list"];

	[self.list removeObject:newItem];

	if([self.list count] == self.stackSize)
		[self.list removeLastObject];

	[self.list insertObject:newItem atIndex:0];

	[self didChangeValueForKey:@"list"];
	[self didChangeValueForKey:@"currentObject"];
	[self didChangeValueForKey:@"head"];

	StoreObjectAtKeyPath(self.list, [self.name UTF8String]);
}

- (NSEnumerator*)objectEnumerator;
{
	return [self.list objectEnumerator];
}

- (id)objectAtIndex:(NSUInteger)index;
{
	return [self.list objectAtIndex:index];
}

- (NSUInteger)count;
{
	return [self.list count];
}

- (id)head
{
	return [self.list firstObject];
}

- (void)setHead:(id)newHead
{
	[self addObject:newHead];
}
@end
