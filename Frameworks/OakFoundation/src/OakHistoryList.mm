#import "OakHistoryList.h"
#import "OakFoundation.h"
#import "NSString Additions.h"
#import <oak/debug.h>

static NSArray<NSString*>* SplitKeyPath (NSString* keyPath)
{
	NSRange r = [keyPath rangeOfString:@"."];
	return r.location == NSNotFound ? @[ keyPath ] : @[ [keyPath substringToIndex:r.location], [keyPath substringFromIndex:NSMaxRange(r)] ];
}

static void StoreObjectAtKeyPath (id obj, NSString* keyPath)
{
	NSArray<NSString*>* pathArray = SplitKeyPath(keyPath);
	if(pathArray.count == 1)
	{
		[NSUserDefaults.standardUserDefaults setObject:obj forKey:keyPath];
	}
	else if(pathArray.count == 2)
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionary];
		if(NSDictionary* existingDict = [NSUserDefaults.standardUserDefaults dictionaryForKey:pathArray.firstObject])
			[dict setValuesForKeysWithDictionary:existingDict];
		[dict setObject:obj forKey:pathArray.lastObject];
		[NSUserDefaults.standardUserDefaults setObject:dict forKey:pathArray.firstObject];
	}
}

static id RetrieveObjectAtKeyPath (NSString* keyPath)
{
	NSArray<NSString*>* pathArray = SplitKeyPath(keyPath);
	if(pathArray.count == 1)
		return [NSUserDefaults.standardUserDefaults objectForKey:keyPath];
	else if(pathArray.count == 2)
		return [[NSUserDefaults.standardUserDefaults dictionaryForKey:pathArray.firstObject] objectForKey:pathArray.lastObject];
	return nil;
}

@interface OakHistoryList ()
@property (nonatomic, copy) NSString* name;
@property (nonatomic) NSMutableArray* list;
@property (nonatomic) NSUInteger stackSize;
@end

@implementation OakHistoryList
- (instancetype)initWithName:(NSString*)defaultsName stackSize:(NSUInteger)size
{
	if(self = [self init])
	{
		self.stackSize = size;
		self.name      = defaultsName;
		self.list      = [[NSMutableArray alloc] initWithCapacity:size];

		if(NSArray* array = RetrieveObjectAtKeyPath(self.name))
			[self.list setArray:[array subarrayWithRange:NSMakeRange(0, MIN(array.count, self.stackSize))]];
	}
	return self;
}

- (instancetype)initWithName:(NSString*)defaultsName stackSize:(NSUInteger)size fallbackUserDefaultsKey:(NSString*)fallbackDefaultsName
{
	if(self = [self initWithName:defaultsName stackSize:size])
	{
		if(self.list.count == 0)
		{
			if(NSArray* array = [NSUserDefaults.standardUserDefaults arrayForKey:fallbackDefaultsName])
				[self.list setArray:array];
		}
	}
	return self;
}

- (instancetype)initWithName:(NSString*)defaultsName stackSize:(NSUInteger)size defaultItems:(id)firstItem, ...
{
	if(self = [self initWithName:defaultsName stackSize:size])
	{
		if(self.list.count == 0)
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
	}
	return self;
}

- (void)addObject:(id)newItem;
{
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

	StoreObjectAtKeyPath(self.list, self.name);
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
