#import "OakHistoryController.h"
#import <oak/algorithm.h>
#import <oak/debug.h>

@implementation OakHistoryController
@synthesize historyIndex;

- (id)init
{
	if(self = [super init])
	{
		historyArray = [NSMutableArray new];
		historyIndex = -1;
		recentLocationsArray = [NSMutableArray new];
	}
	return self;
}

- (void)dealloc
{
	[historyArray release];
	[recentLocationsArray release];
	[super dealloc];
}

- (NSDictionary*)state
{
	NSMutableArray* history = [NSMutableArray array];
	for(NSDictionary* entry in historyArray)
	{
		NSMutableDictionary* dict = [entry mutableCopy];
		[dict setObject:[[dict objectForKey:@"url"] absoluteString] forKey:@"url"];
		[history addObject:dict];
		[dict release];
	}
	return @{ @"history" : history, @"historyIndex" : @(historyIndex) };
}

- (void)setState:(NSDictionary*)newState
{
	[recentLocationsArray removeAllObjects];
	[historyArray removeAllObjects];
	for(NSDictionary* entry in [newState objectForKey:@"history"])
	{
		id value = nil;
		if((value = [entry objectForKey:@"path"]) && [value isKindOfClass:[NSString class]])
			value = [NSURL fileURLWithPath:value isDirectory:YES];
		else if((value = [entry objectForKey:@"url"]) && [value isKindOfClass:[NSString class]])
			value = [NSURL URLWithString:value];
		else
			continue;

		if(value)
		{
			NSMutableDictionary* dict = [entry mutableCopy];
			[dict removeObjectForKey:@"path"];
			[dict setObject:value forKey:@"url"];
			[historyArray addObject:dict];
			[dict release];
		}
	}
	historyIndex = oak::cap<NSInteger>(-1, [[newState objectForKey:@"historyIndex"] intValue], [historyArray count]-1);
}

- (NSURL*)previousURL             { return historyIndex > 0                      ? [[historyArray objectAtIndex:historyIndex-1] objectForKey:@"url"] : nil; }
- (NSURL*)nextURL                 { return historyIndex+1 < [historyArray count] ? [[historyArray objectAtIndex:historyIndex+1] objectForKey:@"url"] : nil; }
- (NSURL*)currentURL              { return historyIndex != -1                    ? [[historyArray objectAtIndex:historyIndex] objectForKey:@"url"] : nil; }
- (CGFloat)currentURLScrollOffset { return historyIndex != -1 ? [[[historyArray objectAtIndex:historyIndex] objectForKey:@"scrollOffset"] floatValue] : 0; }

- (void)addURLToHistory:(NSURL*)url
{
	ASSERT(url);
	[recentLocationsArray removeObject:url];
	[recentLocationsArray addObject:url];

	if(++historyIndex < [historyArray count])
		[historyArray removeObjectsInRange:NSMakeRange(historyIndex, [historyArray count] - historyIndex)];

	[historyArray addObject:@{ @"url" : url }];
}

- (BOOL)advance:(id)sender { return historyIndex+1 < [historyArray count] ? (++historyIndex, YES) : NO; }
- (BOOL)retreat:(id)sender { return historyIndex > 0                      ? (--historyIndex, YES) : NO; }

- (void)setCurrentURLScrollOffset:(CGFloat)offset
{
	if(historyIndex != -1)
	{
		NSMutableDictionary* dict = [[[historyArray objectAtIndex:historyIndex] mutableCopy] autorelease];
		if(offset)
				[dict setObject:@(offset) forKey:@"scrollOffset"];
		else	[dict removeObjectForKey:@"scrollOffset"];
		[historyArray replaceObjectAtIndex:historyIndex withObject:dict];
	}
}

- (NSArray*)recentLocations
{
	return recentLocationsArray;
}

- (NSInteger)historyCount
{
	return historyArray.count;
}

- (NSURL*)urlAtIndex:(NSInteger)index
{
	ASSERT(index >= 0 && index < historyArray.count);
	return [[historyArray objectAtIndex:index] objectForKey:@"url"];
}
@end
