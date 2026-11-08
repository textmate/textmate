#import "OakFoundation.h"
#import "NSString Additions.h"

std::string OakMoveToTrash (std::string const& path)
{
	NSURL* resultingItemURL;
	if([NSFileManager.defaultManager trashItemAtURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:path]] resultingItemURL:&resultingItemURL error:nil])
		return resultingItemURL.fileSystemRepresentation ?: NULL_STR;
	else
		return NULL_STR;
}

BOOL OakIsEmptyString (NSString* str)
{
	return !str || [str isEqualToString:@""];
}

BOOL OakNotEmptyString (NSString* str)
{
	return str && ![str isEqualToString:@""];
}

void OakObserveUserDefaults (id<OakUserDefaultsObserver> obj)
{
	__weak id<OakUserDefaultsObserver> weakObject = obj;
	__weak __block id token;
	void (^handler)(NSNotification*) = ^(NSNotification* notification) {
		if(weakObject)
		{
			dispatch_async(dispatch_get_main_queue(), ^{
				if(id<OakUserDefaultsObserver> strongObject = weakObject)
					[strongObject userDefaultsDidChange:notification];
				else
					[NSNotificationCenter.defaultCenter removeObserver:token];
			});
		}
		else
		{
			[NSNotificationCenter.defaultCenter removeObserver:token];
		}
	};
	token = [NSNotificationCenter.defaultCenter addObserverForName:NSUserDefaultsDidChangeNotification object:NSUserDefaults.standardUserDefaults queue:nil usingBlock:handler];
}
