#import "GenieRunner.h"
#import "GenieTask.h"
#import "GenieManager.h"

@interface GenieItemTaskInfo : NSObject
@property (nonatomic, readonly) NSArray* scriptWithArguments;
@property (nonatomic, readonly) NSMutableArray<NSDictionary*>* jsonValues;
@property (nonatomic, readonly) NSMutableSet* invalidateIdentifiers;
@property (nonatomic, readonly) NSString* title;
@property (nonatomic, readonly) NSString* uiTitle;
@property (nonatomic, readonly) NSString* directory;
@property (nonatomic, readonly) NSDictionary* environment;
@property (nonatomic, readonly) NSTimeInterval timeOut;
@end

@implementation GenieItemTaskInfo
- (instancetype)initWithItem:(GenieItem*)item
{
	if(self = [super init])
	{
		_title                 = item.title;
		_uiTitle               = item.uiTitle;

		_scriptWithArguments   = item.scriptWithArguments;
		_directory             = item.directory;
		_environment           = item.environment;
		_timeOut               = [[item staticValueForKey:@"timeOut"] intValue];

		_jsonValues            = [NSMutableArray array];
		_invalidateIdentifiers = [NSMutableSet set];

		[self addGenieItem:item];
	}
	return self;
}

- (void)addGenieItem:(GenieItem*)item
{
	if(NSDictionary* jsonItem = item.asJSONObject)
		[_jsonValues addObject:jsonItem];

	if(NSString* invalidate = item.invalidate)
		[_invalidateIdentifiers addObject:invalidate];
}
@end

BOOL RunGenieItems (NSArray<GenieItem*>* items)
{
	NSMutableDictionary<NSString*, GenieItemTaskInfo*>* actions = [NSMutableDictionary dictionary];
	NSMutableArray<NSURL*>* urls      = [NSMutableArray array];
	NSMutableArray<NSString*>* files  = [NSMutableArray array];
	NSMutableArray<NSString*>* values = [NSMutableArray array];

	for(GenieItem* item in items)
	{
		if(NSString* key = [item.scriptWithArguments componentsJoinedByString:@"\034"])
		{
			if(GenieItemTaskInfo* taskInfo = actions[key])
					[taskInfo addGenieItem:item];
			else	actions[key] = [[GenieItemTaskInfo alloc] initWithItem:item];
		}
		else if(NSString* urlString = item.url)
		{
			if(NSURL* url = [NSURL URLWithString:urlString])
				[urls addObject:url];
		}
		else if(NSString* file = item.file)
		{
			[files addObject:file];
		}
		else if(NSString* value = item.value)
		{
			[values addObject:value];
		}
	}

	if(!(actions.count || urls.count || files.count || values.count))
		return NO;

	[GenieManager.sharedInstance runAsInactive:^{
		for(NSURL* url in urls)
			[[NSWorkspace sharedWorkspace] openURL:url];

		for(NSString* file in files)
			[[NSWorkspace sharedWorkspace] openFile:file];

		if(values.count)
		{
			NSString* string = [values componentsJoinedByString:@"\n"];
			[[NSPasteboard generalPasteboard] declareTypes:@[ NSStringPboardType ] owner:nil];
			[[NSPasteboard generalPasteboard] setString:string forType:NSStringPboardType];
		}

		for(GenieItemTaskInfo* taskInfo in actions.allValues)
		{
			GenieTask* task = [[GenieTask alloc] initWithCommand:taskInfo.scriptWithArguments directory:taskInfo.directory];
			task.environment = taskInfo.environment;
			task.timeOut     = taskInfo.timeOut;

			@try {
				NSData* jsonData = [NSJSONSerialization dataWithJSONObject:@{ @"items": taskInfo.jsonValues } options:0 error:nullptr];
				task.standardInputString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
			}
			@catch (NSException* e) {
				NSLog(@"Exception creating JSON: %@", e);
			}

			[task launch:^(int rc, NSData* stdoutData, NSData* stderrData){
				for(NSString* identifier in taskInfo.invalidateIdentifiers)
					[GenieItem expireItemsForIdentifier:identifier];

				NSString* stdoutStr = [[NSString alloc] initWithData:stdoutData encoding:NSUTF8StringEncoding];
				NSString* stderrStr = [[NSString alloc] initWithData:stderrData encoding:NSUTF8StringEncoding];

				if(rc != 0)
				{
					[GenieManager.sharedInstance runAsActive:^{
						NSAlert* alert = [[NSAlert alloc] init];
						alert.messageText     = taskInfo.title;
						alert.informativeText = stderrStr.length ? stderrStr : (stdoutStr.length ? stdoutStr : [NSString stringWithFormat:@"Command returned status code %d.", rc]);
						[alert runModal];
						[NSApp hide:nil];
					}];
				}
				else if(stdoutStr.length || stderrStr.length)
				{
					NSUserNotification* notification = [NSUserNotification new];
					notification.title           = taskInfo.uiTitle,
					notification.informativeText = stdoutStr.length ? stdoutStr : stderrStr;
					notification.hasActionButton = NO;
					[[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:notification];
				}
			}];
		}
	}];

	return YES;
}
