void GenieAddAutoLayoutViewsToSuperview (NSDictionary* views, NSView* superview);
void GenieSetupKeyViewLoop (NSArray<__kindof NSResponder*>* superviews, BOOL setFirstResponder = NO);

@interface TextViewController : NSViewController
+ (instancetype)textViewController;
+ (instancetype)readOnlyTextViewController;
@property (nonatomic) NSTextView* textView;
@end

NSScrollView* GenieCreateTextView (BOOL editable = YES);
