#import <text/types.h>
#import <authorization/authorization.h>
#import <selection/types.h>
#import <command/parser.h>
#import <regexp/find.h> // find::options_t
#import <scm/scm.h>

extern NSNotificationName const OakDocumentContentDidChangeNotification;
extern NSNotificationName const OakDocumentMarksDidChangeNotification;
extern NSNotificationName const OakDocumentWillReloadNotification;
extern NSNotificationName const OakDocumentDidReloadNotification;
extern NSNotificationName const OakDocumentWillSaveNotification;
extern NSNotificationName const OakDocumentDidSaveNotification;
extern NSNotificationName const OakDocumentWillCloseNotification;
extern NSNotificationName const OakDocumentWillShowAlertNotification;
extern NSString* OakDocumentBookmarkIdentifier;

typedef NS_ENUM(NSInteger, OakDocumentIOResult) {
	OakDocumentIOResultSuccess = 0,
	OakDocumentIOResultCancel,
	OakDocumentIOResultFailure,
	OakDocumentIOResultCount
};

@class BundleGrammar;
@class OakDocument;
@class OakDocumentEditor;

@interface OakDocumentMatch : NSObject
@property (nonatomic) OakDocument* document;
@property (nonatomic) uint32_t checksum;
@property (nonatomic) NSUInteger first;
@property (nonatomic) NSUInteger last;
@property (nonatomic) text::range_t range;
@property (nonatomic, readonly) NSUInteger lineNumber;
@property (nonatomic) std::map<std::string, std::string> captures;
@property (nonatomic) NSString* excerpt;
@property (nonatomic) NSUInteger excerptOffset;
@property (nonatomic) NSString* newlines;
@property (nonatomic) BOOL headTruncated;
@property (nonatomic) BOOL tailTruncated;
@end

@interface OakDocument : NSObject
+ (instancetype)documentWithPath:(NSString*)aPath;
+ (instancetype)documentWithData:(NSData*)someData fileType:(NSString*)aFileType customName:(NSString*)aName;
+ (instancetype)documentWithString:(NSString*)content fileType:(NSString*)aFileType customName:(NSString*)aName;
+ (instancetype)documentWithIdentifier:(NSUUID*)anIdentifier;

@property (nonatomic) NSUUID* identifier;
@property (nonatomic) NSString* path;
@property (nonatomic) NSString* directory;    // Where to find settings for untitled documents
@property (nonatomic) NSString* virtualPath;  // Used for file type detection and settings (rmate)
@property (nonatomic) NSString* customName;
@property (nonatomic, readonly) NSString* displayName;
@property (nonatomic, readonly) NSImage* icon;
@property (nonatomic) NSString* backupPath;

@property (nonatomic) NSString* fileType;     // Lazy: Depends on path and firstLine
@property (nonatomic) NSString* diskEncoding;
@property (nonatomic) NSString* diskNewlines;
@property (nonatomic, readonly) scm::status::type scmStatus;

- (NSString*)displayNameWithExtension:(BOOL)flag;

- (void)loadModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID))block;
- (void)saveModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID))block;

- (void)open; // This is currently implicit in loadModalForWindow:completionHandler:
- (void)close;

- (void)markDocumentSaved;
- (BOOL)saveBackup:(id)sender;

@property (nonatomic) osx::authorization_t authorization;

@property (nonatomic, getter = isOnDisk)                   BOOL onDisk;
@property (nonatomic, getter = isOpen, readonly)           BOOL open;
@property (nonatomic, getter = isLoaded, readonly)         BOOL loaded;
@property (nonatomic, getter = isBufferEmpty, readonly)    BOOL bufferEmpty;
@property (nonatomic, getter = isLoading, readonly)        BOOL loading;
@property (nonatomic, getter = isInViewingMode)            BOOL inViewingMode;
@property (nonatomic, getter = isDocumentEdited, readonly) BOOL documentEdited;
@property (nonatomic, getter = isRecentTrackingDisabled)   BOOL recentTrackingDisabled;
@property (nonatomic)                                      BOOL keepBackupFile;

// Storage for OakTextView
@property (nonatomic) NSString* selection;
@property (nonatomic) ng::index_t visibleIndex;
@property (nonatomic) NSDictionary* matchCaptures; // Captures from last regexp match

- (void)setMarkOfType:(NSString*)aMark atPosition:(text::pos_t const&)aPos content:(NSString*)value;
- (void)removeMarkOfType:(NSString*)aMark atPosition:(text::pos_t const&)aPos;
- (void)removeAllMarksOfType:(NSString*)aMark;
- (NSString*)stringifyMarksOfType:(NSString*)aMark;
+ (void)removeAllMarksOfType:(NSString*)aMark;

- (void)enumerateSymbolsUsingBlock:(void(^)(text::pos_t const& pos, NSString* symbol))block;
- (void)enumerateBookmarksUsingBlock:(void(^)(text::pos_t const& pos, NSString* excerpt))block;
- (void)enumerateBookmarksAtLine:(NSUInteger)line block:(void(^)(text::pos_t const& pos, NSString* type, NSString* payload))block;
- (void)enumerateByteRangesUsingBlock:(void(^)(char const* bytes, NSRange byteRange, BOOL* stop))block;
- (NSArray<OakDocumentMatch*>*)matchesForString:(NSString*)searchString options:(find::options_t)options;
- (NSArray<OakDocumentMatch*>*)matchesForString:(NSString*)searchString options:(find::options_t)options bufferSize:(NSUInteger*)bufferSize;
- (BOOL)performReplacements:(std::multimap<std::pair<size_t, size_t>, std::string> const&)someReplacements checksum:(uint32_t)crc32;
@property (nonatomic) NSString* content;

- (NSArray<BundleGrammar*>*)proposedGrammars;
- (std::map<std::string, std::string>)variables;

@property (nonatomic, readonly) BOOL canUndo;
@property (nonatomic, readonly) BOOL canRedo;
- (void)beginUndoGrouping;
- (void)endUndoGrouping;
- (void)undo;
- (void)redo;

// Read from .tm_properties so will update if we change fileType
@property (nonatomic, getter = isContinuousSpellCheckingEnabled) BOOL continuousSpellCheckingEnabled;
@property (nonatomic) NSString* spellingLanguage;

// Read from .tm_properties so will update if we change fileType
@property (nonatomic) NSUInteger tabSize;
@property (nonatomic) BOOL softTabs;

- (void)runPrintOperationModalForWindow:(NSWindow*)aWindow fontName:(NSString*)aFontName;

- (void)registerDocumentEditor:(OakDocumentEditor*)anEditor;
- (void)unregisterDocumentEditor:(OakDocumentEditor*)anEditor;
@property (nonatomic, readonly) NSArray<OakDocumentEditor*>* documentEditors;

// Sent to the first OakDocumentEditor instance
- (BOOL)handleOutput:(std::string const&)string placement:(output::type)place format:(output_format::type)format caret:(output_caret::type)caret inputRanges:(ng::ranges_t const&)ranges environment:(std::map<std::string, std::string> const&)environment;
@end
