@interface OakBundleItemCell : NSTextFieldCell
{
	NSString* keyEquivalent;
	NSAttributedString* attributedTabTrigger;
}
@property (nonatomic, retain) NSString* keyEquivalent;
@property (nonatomic, retain) NSString* tabTrigger;
@property (nonatomic, retain) NSAttributedString* attributedTabTrigger;
@end
