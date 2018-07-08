@class OakFinderTag;

@interface OFBFinderTagsChooser : NSView
@property (nonatomic, weak) id target;
@property (nonatomic) SEL action;
@property (nonatomic) NSFont* font;
@property (nonatomic, getter=isEnabled) BOOL enabled;
@property (nonatomic) NSArray<OakFinderTag*>* selectedTags;
@property (nonatomic) NSArray<OakFinderTag*>* selectedTagsToRemove;
@property (nonatomic) OakFinderTag* chosenTag;
@property (nonatomic, readonly) BOOL removeChosenTag;
+ (OFBFinderTagsChooser*)finderTagsChooserForMenu:(NSMenu*)aMenu;
@end
