@interface GenieFilter : NSObject
@property (nonatomic, readonly) NSString* string;
@property (nonatomic, readonly) NSString* normalizedString; // lowercased string
@property (nonatomic, readonly) NSString* filterString;     // lowercased string up until first space
@property (nonatomic, readonly) NSString* queryString;      // original string from the first space
+ (GenieFilter*)filterWithString:(NSString*)aString;
- (instancetype)initWithString:(NSString*)aString;
@end

typedef NS_ENUM(NSUInteger, GenieItemKind) {
	kGenieItemKindUnused = 0,
	kGenieItemKindGroup,
	kGenieItemKindWebAddress,
	kGenieItemKindRunScript,
	kGenieItemKindOpenFile,
	kGenieItemKindSpotlight,
	kGenieItemKindSqlite,
	kGenieItemKindCommandResult,
	kGenieItemKindRecentDocuments,
	kGenieItemKindPredicateGroup,
	kGenieItemKindCopyValue,
};

@interface GenieItem : NSObject <QLPreviewItem>
+ (void)expireItemsForIdentifier:(NSString*)identifier;

@property (nonatomic, getter = isDisabled) BOOL disabled;
@property (nonatomic, readonly) BOOL readOnly;
@property (nonatomic, readonly) NSString* identifier;
@property (nonatomic, readonly) NSString* identifierWithContext;
@property (nonatomic, readonly, weak) GenieItem* parentItem;
@property (nonatomic) GenieItemKind kind;

- (BOOL)isTemplate;
- (BOOL)isPlaceholder;

@property (nonatomic, readonly) NSMutableArray<GenieItem*>* children;
@property (nonatomic, readonly) NSString* directory;
@property (nonatomic) NSDictionary* environment;

@property (nonatomic) GenieFilter* filter;
@property (nonatomic, readonly) BOOL acceptsQuery;
@property (nonatomic, readonly, getter = isFallback) BOOL fallback;

@property (nonatomic, getter = isLive) BOOL live;
@property (nonatomic, readonly, getter = isBusy) BOOL busy;
@property (nonatomic, readonly) NSArray<GenieItem*>* replacementItems;
@property (nonatomic) BOOL dataSourceNeedsUpdate;
- (void)refreshDataSource;

@property (nonatomic, readonly) BOOL disableLRUOrdering;
@property (nonatomic, readonly) BOOL disableLearning;
@property (nonatomic, readonly) BOOL disableFuzzyMatching;
@property (nonatomic, readonly) BOOL disableRankOrdering;

@property (nonatomic, readonly) BOOL hasHTMLOutput;
@property (nonatomic, readonly) id htmlOutputItem;
- (void)updateHTML;

@property (nonatomic, readonly) NSArray* allKeys;
- (id)staticValueForKey:(NSString*)aKey;
- (BOOL)matchesPredicate:(NSPredicate*)aPredicate;

@property (nonatomic, readonly) NSString*     title;
@property (nonatomic, readonly) NSString*     subtitle;
@property (nonatomic, readonly) NSString*     match;
@property (nonatomic, readonly) NSString*     file;
@property (nonatomic, readonly) NSString*     url;
@property (nonatomic, readonly) NSString*     value;
@property (nonatomic, readonly) NSString*     uiTitle;
@property (nonatomic, readonly) NSString*     invalidate;
@property (nonatomic, readonly) NSString*     bundleIdentifier;
@property (nonatomic, readonly) NSString*     sqlDatabase;
@property (nonatomic, readonly) NSString*     mdApplicationCanOpen;

@property (nonatomic, readonly) NSImage*      iconImage;
@property (nonatomic, readonly) NSArray*      scriptWithArguments;
@property (nonatomic, readonly) NSArray*      clipboardRepresentations;

@property (nonatomic, readonly) NSDictionary* asJSONObject;

@property (readonly) NSURL* previewItemURL;

- (instancetype)initWithValues:(NSDictionary*)someValues parentItem:(GenieItem*)parentItem directory:(NSString*)directory;
- (id)copyWithNewParent:(GenieItem*)parentItem;

// =============================
// = Used by Genie Preferences =
// =============================

@property (nonatomic)           NSString*       primitiveTitle;
@property (nonatomic)           NSString*       primitiveSubtitle;
@property (nonatomic)           NSString*       primitiveMatch;
@property (nonatomic)           NSString*       primitiveFile;
@property (nonatomic)           NSString*       primitiveUrl;
@property (nonatomic)           NSDictionary*   iconDescription;

@property (nonatomic)           NSString*       predicate;

@property (nonatomic)           NSString*       primitiveBundleIdentifier;

@property (nonatomic)           NSString*       mdQuery;
@property (nonatomic, readonly) NSMutableArray* mdScope;
@property (nonatomic)           NSString*       sortBy;
@property (nonatomic)           BOOL            descending;

@property (nonatomic)           NSString*       script;
@property (nonatomic, readonly) NSMutableArray* mutableScriptArguments;

@property (nonatomic)           NSString*       primitiveSqlDatabase;
@property (nonatomic)           NSString*       sqlQuery;

@property (nonatomic)           NSString*       displayName;
@property (nonatomic, readonly) BOOL            canEditDisplayName;

@property (nonatomic)           NSString*       plistDump;
@property (nonatomic, readonly) NSUInteger      countOfAdvancedKeys;

@property (nonatomic, readonly) NSDictionary*   rawValues;
@end
