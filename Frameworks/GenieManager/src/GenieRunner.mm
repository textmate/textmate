#import "GenieRunner.h"
#import "GenieTask.h"
#import "GenieManager.h"

BOOL RunGenieItems (NSArray<GenieItem*>* items)
{
	NSMutableDictionary<NSString*, NSMutableArray<GenieItem*>*>* actions = [NSMutableDictionary dictionary];
	NSMutableArray<NSURL*>* urls      = [NSMutableArray array];
	NSMutableArray<NSString*>* files  = [NSMutableArray array];
	NSMutableArray<NSString*>* values = [NSMutableArray array];

	for(GenieItem* item in items)
	{
		if(NSArray* program = item.scriptWithArguments)
		{
			NSString* key = [program componentsJoinedByString:@"|"];
			if(actions[key])
					[actions[key] addObject:item];
			else	actions[key] = [NSMutableArray arrayWithObject:item];
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

	for(NSMutableArray<GenieItem*>* items in actions.allValues)
	{
		NSMutableSet<NSString*>* invalidateIdentifiers = [NSMutableSet set];

		NSMutableArray* jsonValues = [NSMutableArray array];
		for(GenieItem* item in items)
		{
			if(NSDictionary* jsonItem = item.asJSONObject)
				[jsonValues addObject:jsonItem];

			if(NSString* invalidate = item.invalidate)
				[invalidateIdentifiers addObject:invalidate];
		}

		GenieItem* item = items.firstObject;

		GenieTask* task = [[GenieTask alloc] initWithCommand:item.scriptWithArguments directory:item.directory];
		task.environment = item.environment;
		task.timeOut     = [[item staticValueForKey:@"timeOut"] intValue];

		@try {
			NSData* jsonData = [NSJSONSerialization dataWithJSONObject:@{ @"items": jsonValues } options:0 error:nullptr];
			task.standardInputString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
		}
		@catch (NSException* e) {
			NSLog(@"Exception creating JSON: %@", e);
		}

		// These may depend on ${query} or ${clipboard} which may have changed in the callback
		NSString* title   = item.title;
		NSString* uiTitle = item.uiTitle ?: title;

		[task launch:^(int rc, NSData* stdoutData, NSData* stderrData){
			for(NSString* identifier in invalidateIdentifiers)
				[GenieItem expireItemsForIdentifier:identifier];

			NSString* stdoutStr = [[NSString alloc] initWithData:stdoutData encoding:NSUTF8StringEncoding];
			NSString* stderrStr = [[NSString alloc] initWithData:stderrData encoding:NSUTF8StringEncoding];

			if(rc != 0)
			{
				[GenieManager.sharedInstance runAsActive:^{
					NSAlert* alert = [[NSAlert alloc] init];
					alert.messageText     = title;
					alert.informativeText = stderrStr.length ? stderrStr : (stdoutStr.length ? stdoutStr : [NSString stringWithFormat:@"Command returned status code %d.", rc]);
					[alert runModal];
					[NSApp hide:nil];
				}];
			}
			else if(stdoutStr.length || stderrStr.length)
			{
				NSUserNotification* notification = [NSUserNotification new];
				notification.title           = uiTitle,
				notification.informativeText = stdoutStr.length ? stdoutStr : stderrStr;
				notification.hasActionButton = NO;
				[[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:notification];
			}
		}];
	}
	return YES;
}
