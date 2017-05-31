@interface OFBFinderTagsChooser : NSView
@property (nonatomic, weak) id target;
@property (nonatomic) SEL action;
@property (nonatomic) NSFont* font;
@property (nonatomic, getter=isEnabled) BOOL enabled;
@property (nonatomic) NSArray<NSString*>* selectedFavoriteTags;
@property (nonatomic) NSArray<NSString*>* selectedFavoriteTagsToRemove;
@property (nonatomic) NSString* chosenTag;
@property (nonatomic, readonly) BOOL removeChosenTag;
+ (OFBFinderTagsChooser*)finderTagsChooserForMenu:(NSMenu*)aMenu;
@end
