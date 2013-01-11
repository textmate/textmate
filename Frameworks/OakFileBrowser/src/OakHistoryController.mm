#import "OakHistoryController.h"
#import <oak/algorithm.h>
#import <oak/debug.h>

@interface OakHistoryController ()
@property (nonatomic) NSMutableArray* recentLocations;
@property (nonatomic) NSMutableArray* history;
@end

@implementation OakHistoryController { OBJC_WATCH_LEAKS(OakHistoryController); }
- (id)init
{
	if(self = [super init])
	{
		_history         = [NSMutableArray new];
		_historyIndex    = -1;
		_recentLocations = [NSMutableArray new];
	}
	return self;
}

- (NSDictionary*)state
{
	NSMutableArray* history = [NSMutableArray array];
	for(NSDictionary* entry in _history)
	{
		NSMutableDictionary* dict = [entry mutableCopy];
		dict[@"url"] = [dict[@"url"] absoluteString];
		[history addObject:dict];
	}
	return @{ @"history" : history, @"historyIndex" : @(_historyIndex) };
}

- (void)setState:(NSDictionary*)newState
{
	[_recentLocations removeAllObjects];
	[_history removeAllObjects];
	for(NSDictionary* entry in newState[@"history"])
	{
		id value = nil;
		if((value = entry[@"path"]) && [value isKindOfClass:[NSString class]])
			value = [NSURL fileURLWithPath:value isDirectory:YES];
		else if((value = entry[@"url"]) && [value isKindOfClass:[NSString class]])
			value = [NSURL URLWithString:value];
		else
			continue;

		if(value)
		{
			NSMutableDictionary* dict = [entry mutableCopy];
			[dict removeObjectForKey:@"path"];
			dict[@"url"] = value;
			[_history addObject:dict];
		}
	}
	_historyIndex = oak::cap<NSInteger>(-1, [newState[@"historyIndex"] intValue], [_history count]-1);
}

- (NSURL*)previousURL             { return _historyIndex > 0                      ? _history[_historyIndex-1][@"url"] : nil; }
- (NSURL*)nextURL                 { return _historyIndex+1 < [_history count]     ? _history[_historyIndex+1][@"url"] : nil; }
- (NSURL*)currentURL              { return _historyIndex != -1                    ? _history[_historyIndex][@"url"]   : nil; }
- (CGFloat)currentURLScrollOffset { return _historyIndex != -1 ? [_history[_historyIndex][@"scrollOffset"] floatValue] : 0; }

- (void)addURLToHistory:(NSURL*)url
{
	ASSERT(url);
	[_recentLocations removeObject:url];
	[_recentLocations addObject:url];

	if(++_historyIndex < [_history count])
		[_history removeObjectsInRange:NSMakeRange(_historyIndex, [_history count] - _historyIndex)];

	[_history addObject:@{ @"url" : url }];
}

- (BOOL)advance:(id)sender { return _historyIndex+1 < [_history count] ? (++_historyIndex, YES) : NO; }
- (BOOL)retreat:(id)sender { return _historyIndex > 0                  ? (--_historyIndex, YES) : NO; }

- (void)setCurrentURLScrollOffset:(CGFloat)offset
{
	if(_historyIndex != -1)
	{
		NSMutableDictionary* dict = [_history[_historyIndex] mutableCopy];
		if(offset)
				dict[@"scrollOffset"] = @(offset);
		else	[dict removeObjectForKey:@"scrollOffset"];
		_history[_historyIndex] = dict;
	}
}

- (NSInteger)historyCount
{
	return [_history count];
}

- (NSURL*)urlAtIndex:(NSInteger)index
{
	ASSERT(index >= 0 && index < [_history count]);
	return _history[index][@"url"];
}
@end
