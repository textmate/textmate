#import <oak/oak.h>
#import <regexp/find.h> // for find::options_t

extern PUBLIC NSString* const NSReplacePboard;
extern PUBLIC NSString* const OakPasteboardDidChangeNotification;

extern PUBLIC NSString* const kUserDefaultsFindWrapAround;
extern PUBLIC NSString* const kUserDefaultsFindIgnoreCase;

extern PUBLIC NSString* const OakFindIgnoreWhitespaceOption;
extern PUBLIC NSString* const OakFindFullWordsOption;
extern PUBLIC NSString* const OakFindRegularExpressionOption;

@interface OakPasteboardEntry : NSObject
{
	NSString* string;
	NSMutableDictionary* options;
}
+ (OakPasteboardEntry*)pasteboardEntryWithString:(NSString*)aString;
+ (OakPasteboardEntry*)pasteboardEntryWithString:(NSString*)aString andOptions:(NSDictionary*)someOptions;

@property (nonatomic, copy) NSString* string;
@property (nonatomic, copy) NSDictionary* options;

@property (nonatomic, assign) BOOL fullWordMatch;
@property (nonatomic, assign) BOOL ignoreWhitespace;
@property (nonatomic, assign) BOOL regularExpression;

- (find::options_t)findOptions;
- (void)setFindOptions:(find::options_t)findOptions;
@end

@interface OakPasteboard : NSObject
{
@private
	NSString* pasteboardName;
	NSMutableArray* entries;
	NSDictionary* auxiliaryOptionsForCurrent;
	NSUInteger index;
	NSInteger changeCount;
	BOOL avoidsDuplicates;
}
+ (OakPasteboard*)pasteboardWithName:(NSString*)aName;
- (void)addEntry:(OakPasteboardEntry*)anEntry;

@property (nonatomic, assign) BOOL avoidsDuplicates;

- (OakPasteboardEntry*)previous;
- (OakPasteboardEntry*)current;
- (OakPasteboardEntry*)next;

@property (nonatomic, retain) NSDictionary* auxiliaryOptionsForCurrent;

- (void)selectItemAtPosition:(NSPoint)aLocation andCall:(SEL)aSelector;
- (void)selectItemForControl:(NSView*)controlView;
@end
