#import <OakFoundation/OakFoundation.h>
#import "GenieLRUDatabase.h"
#import "GenieItem.h"

@interface GenieLRUDatabase ()
{
	NSMutableDictionary<NSString*, NSMutableArray<NSDictionary*>*>* _storage;
	BOOL _dirty;
}
@end

@implementation GenieLRUDatabase
+ (instancetype)sharedInstance
{
	static GenieLRUDatabase* sharedInstance = [self new];
	return sharedInstance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_storage = [NSMutableDictionary dictionary];
		for(NSDictionary* event in [[NSUserDefaults standardUserDefaults] arrayForKey:@"events"] ?: [NSArray array])
			[self addEvent:event];
		_dirty = NO;
	}
	return self;
}

- (void)addEvent:(NSDictionary*)event
{
	if(NSString* identifier = event[@"identifier"] ?: event[@"title"])
	{
		if(NSString* dataSource = event[@"dataSource"])
		{
			identifier = [dataSource stringByAppendingFormat:@"\034%@", identifier];
		}
		else if(NSArray* parents = event[@"parents"])
		{
			for(NSUInteger i = parents.count; i > 0; )
			{
				NSDictionary* parent = parents[--i];
				if(NSString* parentIdentifier = parent[@"identifier"] ?: parent[@"title"])
					identifier = [parentIdentifier stringByAppendingFormat:@"\034%@", identifier];

				if(NSString* dataSource = parent[@"dataSource"])
				{
					identifier = [dataSource stringByAppendingFormat:@"\034%@", identifier];
					break;
				}
			}
		}

		NSMutableArray* array = _storage[identifier];
		if(!array)
			array = _storage[identifier] = [NSMutableArray array];

		[array addObject:event];

		if(_dirty == NO)
		{
			_dirty = YES;
			[self performSelector:@selector(synchronize:) withObject:nil afterDelay:10];
		}
	}
}

- (void)synchronize:(id)sender
{
	if(!_dirty)
		return;
	_dirty = NO;

	NSMutableArray* events = [NSMutableArray array];
	for(NSArray* items in _storage.allValues)
		[events addObjectsFromArray:items];

	NSArray* chronological = [events sortedArrayUsingComparator:^NSComparisonResult(id lhs, id rhs){
		return [(lhs[@"date"] ?: [NSDate distantPast]) compare:(rhs[@"date"] ?: [NSDate distantPast])];
	}];

	[[NSUserDefaults standardUserDefaults] setObject:chronological forKey:@"events"];
}

- (NSString*)lastQueryStringForItem:(GenieItem*)item
{
	if(NSString* identifier = item.identifierWithContext)
	{
		for(NSDictionary* event in _storage[identifier].reverseObjectEnumerator)
		{
			if(OakNotEmptyString(event[@"query"]))
				return event[@"query"];
		}
	}
	return nil;
}

- (NSDate*)lruForItem:(GenieItem*)item withFilter:(NSString*)filter
{
	if(NSString* identifier = item.identifierWithContext)
	{
		for(NSDictionary* event in _storage[identifier].reverseObjectEnumerator)
		{
			if(!filter || [event[@"filter"] isEqualToString:filter])
				return event[@"date"] ?: [NSDate distantPast];
		}
	}
	return nil;
}

- (NSDate*)lruForItem:(GenieItem*)item withFilterPrefix:(NSString*)filterPrefix
{
	if(NSString* identifier = item.identifierWithContext)
	{
		for(NSDictionary* event in _storage[identifier].reverseObjectEnumerator)
		{
			if([event[@"filter"] hasPrefix:filterPrefix])
				return event[@"date"] ?: [NSDate distantPast];
		}
	}
	return nil;
}
@end
