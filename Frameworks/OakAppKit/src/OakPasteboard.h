#import <oak/oak.h>
#import <regexp/find.h> // for find::options_t

extern PUBLIC NSString* const OakPasteboardDidChangeNotification;

extern PUBLIC NSString* const kUserDefaultsFindWrapAround;
extern PUBLIC NSString* const kUserDefaultsFindIgnoreCase;

extern PUBLIC NSString* const OakFindIgnoreWhitespaceOption;
extern PUBLIC NSString* const OakFindFullWordsOption;
extern PUBLIC NSString* const OakFindRegularExpressionOption;

PUBLIC @interface OakPasteboardEntry : NSManagedObject
@property (nonatomic) NSString* string;
@property (nonatomic) NSDictionary* options;
@property (nonatomic) NSDate* date;

@property (nonatomic) BOOL fullWordMatch;
@property (nonatomic) BOOL ignoreWhitespace;
@property (nonatomic) BOOL regularExpression;

@property (nonatomic, readonly) find::options_t findOptions;
@end

PUBLIC @interface OakPasteboard : NSManagedObject
@property (class, readonly) OakPasteboard* generalPasteboard;
@property (class, readonly) OakPasteboard* findPasteboard;
@property (class, readonly) OakPasteboard* replacePasteboard;

- (void)addEntryWithString:(NSString*)aString;
- (void)addEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions;
- (void)removeAllEntries;
- (NSArrayController*)arrayController;

- (OakPasteboardEntry*)previous;
- (OakPasteboardEntry*)current;
- (OakPasteboardEntry*)next;

@property (nonatomic) NSString* name;
@property (nonatomic) OakPasteboardEntry* currentEntry;
@property (nonatomic) NSDictionary* auxiliaryOptionsForCurrent;

- (void)selectItemForControl:(NSView*)controlView;
@end
