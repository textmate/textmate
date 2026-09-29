#import <oak/oak.h>
#import <regexp/src/find.h> // for find::options_t

extern NSNotificationName const OakPasteboardDidChangeNotification;

extern NSString* const kUserDefaultsFindWrapAround;
extern NSString* const kUserDefaultsFindIgnoreCase;

extern NSString* const OakFindIgnoreWhitespaceOption;
extern NSString* const OakFindFullWordsOption;
extern NSString* const OakFindRegularExpressionOption;

@interface OakPasteboardEntry : NSObject
@property (nonatomic, readonly) NSString* string;
@property (nonatomic, readonly) NSArray<NSString*>* strings;
@property (nonatomic, readonly) NSDictionary* options;
@property (nonatomic, getter = isFlagged) BOOL flagged;
@property (nonatomic, readonly) NSUInteger historyId; // This is only to be used by OakPasteboardChooser

@property (nonatomic, readonly) BOOL fullWordMatch;
@property (nonatomic, readonly) BOOL ignoreWhitespace;
@property (nonatomic, readonly) BOOL regularExpression;

@property (nonatomic, readonly) find::options_t findOptions;
@end

@interface OakPasteboard : NSObject
@property (class, readonly) OakPasteboard* generalPasteboard;
@property (class, readonly) OakPasteboard* findPasteboard;
@property (class, readonly) OakPasteboard* replacePasteboard;

- (void)addEntryWithString:(NSString*)aString;
- (void)addEntryWithString:(NSString*)aString options:(NSDictionary*)someOptions;
- (OakPasteboardEntry*)addEntryWithStrings:(NSArray<NSString*>*)someStrings options:(NSDictionary*)someOptions;
- (void)removeEntries:(NSArray<OakPasteboardEntry*>*)pasteboardEntries;
- (void)removeAllEntries;
- (NSArray<OakPasteboardEntry*>*)entries;

- (void)updatePasteboardWithEntry:(OakPasteboardEntry*)pasteboardEntry;
- (void)updatePasteboardWithEntries:(NSArray<OakPasteboardEntry*>*)pasteboardEntries;

- (OakPasteboardEntry*)previous;
- (OakPasteboardEntry*)current;
- (OakPasteboardEntry*)next;

@property (nonatomic, readonly) NSString* name;
@property (nonatomic, readonly) OakPasteboardEntry* currentEntry;

- (void)selectItemForControl:(NSView*)controlView;
@end
