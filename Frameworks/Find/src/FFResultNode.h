#import <document/document.h>

@class FFMatch;

@interface FFResultNode : NSObject
@property (nonatomic, weak) FFResultNode* parent;
@property (nonatomic, weak) FFResultNode* next;
@property (nonatomic, weak) FFResultNode* previous;
@property (nonatomic, readonly) NSUInteger countOfLeafs;
@property (nonatomic, readonly) NSUInteger countOfExcluded;

+ (FFResultNode*)resultNodeWithMatch:(FFMatch*)aMatch baseDirectory:(NSString*)base;
+ (FFResultNode*)resultNodeWithMatch:(FFMatch*)aMatch;

- (void)addResultNode:(FFResultNode*)aMatch;
- (void)removeFromParent;

- (FFResultNode*)firstResultNode;
- (FFResultNode*)lastResultNode;

- (NSUInteger)lineSpan;
- (NSAttributedString*)excerptWithReplacement:(NSString*)replacementString;
- (NSImage*)icon;

@property (nonatomic) FFMatch* match;
@property (nonatomic) NSArray* children;
@property (nonatomic) BOOL excluded;
@property (nonatomic) BOOL replacementDone;
@property (nonatomic) BOOL ignored;
@property (nonatomic) NSImage* icon;

@property (nonatomic, readonly) document::document_ptr document;
@property (nonatomic, readonly) NSString* path;
@property (nonatomic, readonly) NSString* identifier;
@property (nonatomic) NSAttributedString* displayPath;
@end
