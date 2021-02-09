@class OakKeyEquivalentView;

@interface PropertiesViewController : NSViewController
{
	IBOutlet NSObjectController* objectController;
	IBOutlet NSView* alignmentView;
	IBOutlet OakKeyEquivalentView* keyEquivalentView;
}
- (id)initWithName:(NSString*)aName;
@property (nonatomic) NSMutableDictionary* properties;
@property (nonatomic, readonly) CGFloat labelWidth;
@end
