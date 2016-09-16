@class OakDocument;
@class OakDocumentMatch;

@interface FFResultNode : NSObject
@property (nonatomic, weak) FFResultNode* parent;
@property (nonatomic, readonly) NSUInteger countOfLeafs;
@property (nonatomic, readonly) NSUInteger countOfExcluded;
@property (nonatomic, readonly) NSUInteger countOfReadOnly;
@property (nonatomic, readonly) NSUInteger countOfExcludedReadOnly;

+ (FFResultNode*)resultNodeWithMatch:(OakDocumentMatch*)aMatch baseDirectory:(NSString*)base;
+ (FFResultNode*)resultNodeWithMatch:(OakDocumentMatch*)aMatch;

- (void)addResultNode:(FFResultNode*)aMatch;
- (void)removeFromParent;

- (FFResultNode*)firstResultNode;
- (FFResultNode*)lastResultNode;

- (NSUInteger)lineSpan;
- (NSAttributedString*)excerptWithReplacement:(NSString*)replacementString font:(NSFont*)font;

@property (nonatomic) NSString* replaceString;
@property (nonatomic) NSArray* children;
@property (nonatomic) BOOL excluded;
@property (nonatomic, getter = isReadOnly) BOOL readOnly;

@property (nonatomic, readonly) OakDocumentMatch* match;
@property (nonatomic, readonly) OakDocument* document;
@property (nonatomic, readonly) NSString* path;
@property (nonatomic) NSAttributedString* displayPath;
@end
