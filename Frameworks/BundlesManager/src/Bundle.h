@class BundleGrammar;

@interface Bundle : NSObject
- (instancetype)initWithIdentifier:(NSUUID*)anIdentifier;

@property (nonatomic) NSUUID*                       identifier;
@property (nonatomic) NSString*                     name;
@property (nonatomic) NSString*                     minimumAppVersion; // E.g. ‘2.0-alpha.9519’
@property (nonatomic) NSString*                     category;
@property (nonatomic) NSURL*                        htmlURL;
@property (nonatomic) NSString*                     summary;
@property (nonatomic) NSString*                     contactName;
@property (nonatomic) NSString*                     contactEmail;
@property (nonatomic) NSURL*                        downloadURL;
@property (nonatomic) NSDate*                       downloadLastUpdated;
@property (nonatomic) NSInteger                     downloadSize;
@property (nonatomic, getter = isMandatory)   BOOL  mandatory;
@property (nonatomic, getter = isRecommended) BOOL  recommended;
@property (nonatomic) NSArray<BundleGrammar*>*      grammars;
@property (nonatomic) NSArray<Bundle*>*             dependencies;

// From local index
@property (nonatomic, getter = isInstalled)  BOOL      installed;
@property (nonatomic)                        NSString* path;
@property (nonatomic)                        NSDate*   lastUpdated;
@property (nonatomic, getter = isDependency) BOOL      dependency; // Another bundle depends on us

// Generated
@property (nonatomic, readonly)                        BOOL hasUpdate;
@property (nonatomic, getter = isCompatible, readonly) BOOL compatible; // Works with current version of TextMate
@end

@interface BundleGrammar : NSObject
@property (nonatomic, weak) Bundle*       bundle;
@property (nonatomic) NSUUID*             identifier;
@property (nonatomic) NSString*           name;
@property (nonatomic) NSString*           fileType;       // E.g. ‘source.ruby’
@property (nonatomic) NSArray<NSString*>* filePatterns;   // Array of extensions or file globs
@property (nonatomic) NSString*           firstLineMatch; // E.g. ‘^#!/.*\bruby’
@end
