@class AppController;

@interface AppStartupController : NSObject
{
	IBOutlet AppController* appController;

	NSAppleEventDescriptor* openEvent;
	NSArray* openDocumentsArray;
	BOOL disableSessionRestore;
}
@property (nonatomic, retain) NSAppleEventDescriptor* openEvent;
@property (nonatomic, retain) NSArray* openDocumentsArray;
@end
