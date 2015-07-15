#import <OakAppKit/OakFileIconImage.h>

@class FSDataSource;

enum FSItemURLType { FSItemURLTypeUnknown = 0, FSItemURLTypeFile, FSItemURLTypeFolder, FSItemURLTypePackage, FSItemURLTypeAlias, FSItemURLTypeMissing };

@interface FSItem : NSObject
@property (nonatomic) OakFileIconImage* icon;
@property (nonatomic) NSString* displayName;
@property (nonatomic) NSString* toolTip;
@property (nonatomic) NSInteger labelIndex;
@property (nonatomic) NSURL* url;
@property (nonatomic) FSItemURLType urlType;
@property (nonatomic) NSURL* target;
@property (nonatomic) NSArray* children;
@property (nonatomic) BOOL leaf;
@property (nonatomic) BOOL group;
@property (nonatomic) BOOL sortAsFolder;
@property (nonatomic) BOOL link;

@property (nonatomic, readonly) NSString* path; // legacy

+ (FSItem*)itemWithURL:(NSURL*)anURL;
- (FSItem*)initWithURL:(NSURL*)anURL;
@end
