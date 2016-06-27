#import <text/types.h>
#import <authorization/authorization.h>
#import <undo/undo.h> // ng::buffer_t and ng::undo_manager_t types

PUBLIC extern NSString* OakDocumentContentDidChangeNotification;
PUBLIC extern NSString* OakDocumentMarksDidChangeNotification;
PUBLIC extern NSString* OakDocumentDidSaveNotification;
PUBLIC extern NSString* OakDocumentWillCloseNotification;

PUBLIC @interface OakDocument : NSObject
+ (instancetype)documentWithPath:(NSString*)aPath;
+ (instancetype)documentWithData:(NSData*)someData fileType:(NSString*)aFileType customName:(NSString*)aName;
+ (instancetype)documentWithIdentifier:(NSUUID*)anIdentifier;

@property (nonatomic) NSUUID* identifier;
@property (nonatomic) NSString* path;
@property (nonatomic) NSString* virtualPath;  // Used for file type detection and settings (rmate)
@property (nonatomic) NSString* customName;
@property (nonatomic, readonly) NSString* displayName;

@property (nonatomic) NSString* fileType;     // Lazy: Depends on path and firstLine
@property (nonatomic) NSString* diskEncoding;
@property (nonatomic) NSString* diskNewlines;

- (void)loadModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(BOOL success, NSString* errorMessage, oak::uuid_t const& filterUUID))block;
- (void)close;

@property (nonatomic) osx::authorization_t authorization;

@property (nonatomic, getter = isOnDisk)                   BOOL onDisk;
@property (nonatomic, getter = isOpen, readonly)           BOOL open;
@property (nonatomic, getter = isInViewingMode)            BOOL inViewingMode;
@property (nonatomic, getter = isDocumentEdited, readonly) BOOL documentEdited;
@property (nonatomic, getter = isRecentTrackingDisabled)   BOOL recentTrackingDisabled;
@property (nonatomic)                                      BOOL keepBackupFile;

- (void)setMarkOfType:(NSString*)aMark atPosition:(text::pos_t const&)aPos content:(NSString*)value;
- (void)removeMarkOfType:(NSString*)aMark atPosition:(text::pos_t const&)aPos;
- (void)removeAllMarksOfType:(NSString*)aMark;
- (NSString*)stringifyMarksOfType:(NSString*)aMark;
+ (void)removeAllMarksOfType:(NSString*)aMark;

- (void)enumerateSymbolsUsingBlock:(void(^)(text::pos_t const& pos, NSString* symbol))block;
- (void)enumerateByteRangesUsingBlock:(void(^)(char const* bytes, NSRange byteRange, BOOL* stop))block;
@property (nonatomic) NSString* content;

@property (nonatomic, readonly) BOOL canUndo;
@property (nonatomic, readonly) BOOL canRedo;

// Read from .tm_properties so will update if we change fileType
@property (nonatomic, getter = isContinuousSpellCheckingEnabled) BOOL continuousSpellCheckingEnabled;
@property (nonatomic) NSString* spellingLanguage;

// Read from .tm_properties so will update if we change fileType
@property (nonatomic) NSUInteger tabSize;
@property (nonatomic) BOOL softTabs;

- (void)runPrintOperationModalForWindow:(NSWindow*)aWindow fontName:(NSString*)aFontName;
@end
