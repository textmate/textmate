@interface OakSubmenuController : NSObject <NSMenuDelegate>
{
	IBOutlet NSMenu* goToMenu;
	IBOutlet NSMenu* marksMenu;

	NSInteger tag;
	id representedObject;
}
+ (OakSubmenuController*)sharedInstance;
- (NSInteger)tagForSender:(id)aSender;
- (id)representedObjectForSender:(id)aSender;
@end
