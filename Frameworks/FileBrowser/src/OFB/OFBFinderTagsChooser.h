@class OakFinderTag;

@interface OFBFinderTagsChooser : NSView
@property (nonatomic, weak) id target;
@property (nonatomic) SEL action;
@property (nonatomic) OakFinderTag* chosenTag;
@property (nonatomic, readonly) BOOL removeChosenTag;
+ (OFBFinderTagsChooser*)finderTagsChooserWithSelectedTags:(NSArray<OakFinderTag*>*)selectedTags andSelectedTagsToRemove:(NSArray<OakFinderTag*>*)selectedTagsToRemove forMenu:(NSMenu*)aMenu;
@end
