#import <scm/status.h>

@class FSDataSource;

enum FSItemURLType { FSItemURLTypeUnknown = 0, FSItemURLTypeFile, FSItemURLTypeFolder, FSItemURLTypePackage, FSItemURLTypeAlias, FSItemURLTypeMissing };

@interface FSItem : NSObject
@property (nonatomic) NSImage* icon;
@property (nonatomic) NSString* displayName;
@property (nonatomic) NSString* toolTip;
@property (nonatomic) NSInteger labelIndex;
@property (nonatomic) NSURL* url;
@property (nonatomic) FSItemURLType urlType;
@property (nonatomic) NSURL* target;
@property (nonatomic) NSArray* children;
@property (nonatomic) scm::status::type scmStatus;
@property (nonatomic) BOOL leaf;
@property (nonatomic) BOOL group;
@property (nonatomic) BOOL sortAsFolder;
@property (nonatomic) BOOL link;
@property (nonatomic, getter = isModified) BOOL modified;
@property (nonatomic, getter = isOpen) BOOL open;

@property (nonatomic, readonly) NSString* path; // legacy

+ (FSItem*)itemWithURL:(NSURL*)anURL;
- (FSItem*)initWithURL:(NSURL*)anURL;
- (BOOL)setNewDisplayName:(NSString*)newDisplayName view:(NSView*)view;
@end
