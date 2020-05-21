#import "OakFoundation.h"
#import "NSString Additions.h"

std::string OakMoveToTrash (std::string const& path)
{
	NSURL* resultingItemURL;
	if([NSFileManager.defaultManager trashItemAtURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:path]] resultingItemURL:&resultingItemURL error:nil])
			return resultingItemURL.fileSystemRepresentation ?: NULL_STR;
	else	return NULL_STR;
}

BOOL OakIsEmptyString (NSString* str)
{
	return !str || [str isEqualToString:@""];
}

BOOL OakNotEmptyString (NSString* str)
{
	return str && ![str isEqualToString:@""];
}

void OakObserveUserDefaults (id <OakUserDefaultsObserver> obj)
{
	__weak id <OakUserDefaultsObserver> weakObject = obj;
	__weak __block id token = [NSNotificationCenter.defaultCenter addObserverForName:NSUserDefaultsDidChangeNotification object:NSUserDefaults.standardUserDefaults queue:NSOperationQueue.mainQueue usingBlock:^(NSNotification* notification){
		if(id <OakUserDefaultsObserver> strongObject = weakObject)
				[strongObject userDefaultsDidChange:notification];
		else	[NSNotificationCenter.defaultCenter removeObserver:token];
	}];
}
