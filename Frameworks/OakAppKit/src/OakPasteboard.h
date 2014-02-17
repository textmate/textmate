#import <oak/oak.h>
#import <regexp/find.h> // for find::options_t

extern PUBLIC NSString* const NSReplacePboard;
extern PUBLIC NSString* const OakPasteboardDidChangeNotification;

extern PUBLIC NSString* const kUserDefaultsFindWrapAround;
extern PUBLIC NSString* const kUserDefaultsFindIgnoreCase;

extern PUBLIC NSString* const OakFindIgnoreWhitespaceOption;
extern PUBLIC NSString* const OakFindFullWordsOption;
extern PUBLIC NSString* const OakFindRegularExpressionOption;

PUBLIC @interface OakPasteboardEntry : NSObject
@property (nonatomic, copy) NSString* string;
@property (nonatomic, copy) NSDictionary* options;

@property (nonatomic) BOOL fullWordMatch;
@property (nonatomic) BOOL ignoreWhitespace;
@property (nonatomic) BOOL regularExpression;

- (find::options_t)findOptions;
- (void)setFindOptions:(find::options_t)findOptions;
@end

PUBLIC @interface OakPasteboard : NSObject
+ (OakPasteboard*)pasteboardWithName:(NSString*)aName;

- (void)addEntryWithString:(NSString*)aString;
- (void)addEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions;

@property (nonatomic) BOOL avoidsDuplicates;

- (OakPasteboardEntry*)previous;
- (OakPasteboardEntry*)current;
- (OakPasteboardEntry*)next;

@property (nonatomic) NSDictionary* auxiliaryOptionsForCurrent;

- (void)selectItemAtPosition:(NSPoint)aLocation andCall:(SEL)aSelector;
- (void)selectItemForControl:(NSView*)controlView;
@end
