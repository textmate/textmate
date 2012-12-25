@class AppController;

@interface AppStartupController : NSObject
{
	IBOutlet AppController* appController;
	BOOL disableSessionRestore;
}
@property (nonatomic, retain) NSAppleEventDescriptor* openEvent;
@property (nonatomic, retain) NSArray* openDocumentsArray;
@end
