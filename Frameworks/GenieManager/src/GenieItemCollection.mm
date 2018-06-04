#import "GenieItemCollection.h"
#import "GenieItem.h"
#import "GenieManager.h"
#import "GenieLRUDatabase.h"
#import <OakFoundation/OakFoundation.h>
#import <text/ranker.h>
#import <ns/ns.h>
#import <oak/debug.h>

static BOOL SimpleMatch (NSString* needle, NSString* haystack)
{
	haystack = [haystack lowercaseString];
	NSUInteger offset = 0;
	for(NSString* str in [needle componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]])
	{
		if(OakNotEmptyString(str))
		{
			NSRange match = [haystack rangeOfString:str options:0 range:NSMakeRange(offset, haystack.length - offset)];
			if(match.location == NSNotFound)
				return NO;
			offset = NSMaxRange(match);
		}
	}
	return YES;
}

@interface GenieItemCollection ()
{
	NSInteger _countOfBusyItems;
	NSArray<GenieItem*>* _enabledItems;
	NSArray<GenieItem*>* _replacementItems;
	GenieLRUDatabase* _database;
}
@property (nonatomic) NSArray<GenieItem*>* items;
@property (nonatomic, readwrite) NSArray<GenieItem*>* arrangedObjects;
@end

static void* kGenieItemBusyBinding             = &kGenieItemBusyBinding;
static void* kGenieItemReplacementItemsBinding = &kGenieItemReplacementItemsBinding;

@implementation GenieItemCollection
+ (instancetype)defaultCollection
{
	return [[self alloc] initWithItems:GenieManager.sharedInstance.items];
}

- (instancetype)initWithItems:(NSArray<GenieItem*>*)items
{
	if(self = [super init])
	{
		_database  = GenieLRUDatabase.sharedInstance;
		self.items = items;
	}
	return self;
}

- (void)dealloc
{
	self.items = nil;
}

- (void)setItems:(NSArray<GenieItem*>*)newItems
{
	if(_enabledItems)
	{
		[_enabledItems removeObserver:self fromObjectsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, _enabledItems.count)] forKeyPath:@"busy" context:kGenieItemBusyBinding];
		[_enabledItems removeObserver:self fromObjectsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, _enabledItems.count)] forKeyPath:@"replacementItems" context:kGenieItemReplacementItemsBinding];
	}

	_items        = newItems;
	_enabledItems = [_items filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"disabled != YES && kind != %ld", kGenieItemKindPredicateGroup]];

	NSArray* busyItems = [_enabledItems filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"busy = YES"]];
	_countOfBusyItems = busyItems.count;

	if(_enabledItems)
	{
		[_enabledItems addObserver:self toObjectsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, _enabledItems.count)] forKeyPath:@"busy" options:NSKeyValueObservingOptionNew|NSKeyValueObservingOptionOld context:kGenieItemBusyBinding];
		[_enabledItems addObserver:self toObjectsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, _enabledItems.count)] forKeyPath:@"replacementItems" options:NSKeyValueObservingOptionNew|NSKeyValueObservingOptionOld context:kGenieItemReplacementItemsBinding];
	}

	_replacementItems = [_enabledItems valueForKeyPath:@"replacementItems.@unionOfArrays.self"];
	[self rearrangeObjects];

	self.busy = _countOfBusyItems != 0;
}

- (NSArray<GenieItem*>*)selectedObjects
{
	return _selectionIndexes ? [_arrangedObjects objectsAtIndexes:_selectionIndexes] : @[ ];
}

- (void)rearrangeObjects
{
	NSIndexSet* oldSelection  = [self.selectionIndexes copy];
	NSArray* oldSelectedItems = self.selectedObjects;

	// ====================
	// = Filter the items =
	// ====================

	typedef NS_ENUM(NSInteger, RankingGroups) {
		RankingGroupExactMatch = 0,
		RankingGroupPrefixMatch,
		RankingGroupHasRankMatch,
		RankingGroupOtherMatch,
		RankingGroupFallbackMatch,
		RankingGroupNoMatch,
	};

	NSMutableArray* filteredItems = [NSMutableArray array];

	if(OakIsEmptyString(_filter.normalizedString))
	{
		for(GenieItem* item in _replacementItems)
		{
			RankingGroups group = RankingGroupOtherMatch;
			id rank = nil;

			if(item.isFallback)
				group = RankingGroupFallbackMatch;
			else if(item.disableLRUOrdering == NO && (rank = [_database lruForItem:item withFilter:nil]))
				group = RankingGroupPrefixMatch;

			[filteredItems addObject:@{
				@"group": @(group),
				@"rank":  (rank ?: @NO),
				@"item":  item
			}];
		}
	}
	else
	{
		for(GenieItem* item in _replacementItems)
		{
			RankingGroups group = RankingGroupNoMatch;
			id rank = nil;

			NSString* filter = item.acceptsQuery ? _filter.filterString : _filter.normalizedString;
			if(item.isFallback)
			{
				group = RankingGroupFallbackMatch;
			}
			else if(item.disableFuzzyMatching && !SimpleMatch(filter, item.match))
			{
				continue;
			}
			else
			{
				if(double score = oak::rank(to_s(filter), to_s(item.match)))
				{
					if(item.disableRankOrdering)
					{
						group = RankingGroupOtherMatch;
					}
					else if(item.disableLearning == NO && (rank = [_database lruForItem:item withFilter:filter]))
					{
						group = RankingGroupExactMatch;
					}
					else if(item.disableLearning == NO && (rank = [_database lruForItem:item withFilterPrefix:filter]))
					{
						group = RankingGroupPrefixMatch;
					}
					else
					{
						group = RankingGroupHasRankMatch;
						rank = @(score);
					}
				}
			}

			if(group != RankingGroupNoMatch)
			{
				[filteredItems addObject:@{
					@"group": @(group),
					@"rank":  (rank ?: @NO),
					@"item":  item
				}];
			}
		}
	}

	NSComparator comparator = ^NSComparisonResult(NSDictionary* lhs, NSDictionary* rhs){
		NSComparisonResult res = [lhs[@"group"] compare:rhs[@"group"]];
		return res == NSOrderedSame ? [rhs[@"rank"] compare:lhs[@"rank"]] : res;
	};

	NSMutableArray<GenieItem*>* newItems = [NSMutableArray array];
	for(NSDictionary* item in [filteredItems sortedArrayWithOptions:NSSortStable usingComparator:comparator])
	{
		GenieItem* realItem = item[@"item"];
		[newItems addObject:realItem];
		if(id htmlOutputItem = realItem.htmlOutputItem)
			[newItems addObject:htmlOutputItem];
	}

	if([_arrangedObjects isEqualToArray:newItems])
		return;

	NSIndexSet* newIndexSet;
	if(oldSelection && ![oldSelection isEqualToIndexSet:[NSIndexSet indexSetWithIndex:0]])
	{
		// TODO Objects should be tested using their identifier property (if they have one)
		newIndexSet = [newItems indexesOfObjectsPassingTest:^BOOL(GenieItem* item, NSUInteger index, BOOL* stop){
			return [oldSelectedItems containsObject:item];
		}];
	}

	[self willChangeValueForKey:@"selectionIndexes"];
	[self willChangeValueForKey:@"arrangedObjects"];
	_arrangedObjects  = newItems;
	_selectionIndexes = newIndexSet.count ? newIndexSet : (newItems.count ? [NSIndexSet indexSetWithIndex:0] : nil);
	[self didChangeValueForKey:@"arrangedObjects"];
	[self didChangeValueForKey:@"selectionIndexes"];
}

- (void)setQueryString:(NSString*)newQueryString
{
	_queryString = newQueryString;
	NSString* oldFilterString = _filter.normalizedString;
	_filter = [GenieFilter filterWithString:_queryString];
	[_enabledItems makeObjectsPerformSelector:@selector(setFilter:) withObject:_filter];

	if(_filter.normalizedString == oldFilterString || [_filter.normalizedString isEqualToString:oldFilterString])
		return;

	[self rearrangeObjects];
}

- (void)setLive:(BOOL)flag
{
	_live = flag;
	for(GenieItem* item in _enabledItems)
		item.live = flag;
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)anObject change:(NSDictionary*)someChanges context:(void*)context
{
	if(context == kGenieItemBusyBinding)
	{
		BOOL wasBusy = [someChanges[NSKeyValueChangeOldKey] boolValue];
		BOOL isBusy  = [someChanges[NSKeyValueChangeNewKey] boolValue];
		if(wasBusy != isBusy)
		{
			_countOfBusyItems += isBusy ? +1 : -1;

			BOOL hasBusyItems = _countOfBusyItems != 0;
			if(_busy != hasBusyItems)
				self.busy = hasBusyItems;
		}
	}
	else if(context == kGenieItemReplacementItemsBinding)
	{
		_replacementItems = [_enabledItems valueForKeyPath:@"replacementItems.@unionOfArrays.self"];
		[self rearrangeObjects];
	}
	else
	{
		[super observeValueForKeyPath:keyPath ofObject:anObject change:someChanges context:context];
	}
}

- (void)logArrangedObjectsRecursively:(BOOL)recursiveFlag
{
	NSMutableArray<NSEnumerator*>* stack = [NSMutableArray array];
	[stack addObject:[self.arrangedObjects objectEnumerator]];
	while(stack.count)
	{
		while(GenieItem* item = stack.lastObject.nextObject)
		{
			if(item.disabled || item.isTemplate)
			{
				NSLog(@"%s unexpected item: %@", sel_getName(_cmd), item);
				continue;
			}

			NSMutableString* indent = [NSMutableString string];
			for(NSUInteger i = 0; i < stack.count-1; ++i)
				[indent appendString:@"  "];

			NSLog(@"%s %@%@ | %@", sel_getName(_cmd), indent, item.title, item.subtitle);

			if(recursiveFlag)
			{
				if(NSArray* children = item.children)
				{
					children = [children valueForKeyPath:@"replacementItems.@unionOfArrays.self"];
					[stack addObject:[children objectEnumerator]];
				}
			}
		}
		[stack removeObjectAtIndex:stack.count-1];
	}
}
@end
