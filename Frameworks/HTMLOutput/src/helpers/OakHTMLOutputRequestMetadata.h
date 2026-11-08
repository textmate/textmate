#import <Foundation/Foundation.h>

@interface OakHTMLOutputRequestMetadata : NSObject
@property (nonatomic) NSFileHandle* fileHandle;
@property (nonatomic) NSUUID* commandIdentifier;
@property (nonatomic) NSNumber* processIdentifier;
@property (nonatomic) NSString* processName;
@property (nonatomic, weak) id command; // the OakCommand instance

+ (void)setMetadata:(OakHTMLOutputRequestMetadata*)metadata forURLString:(NSString*)urlString;
+ (OakHTMLOutputRequestMetadata*)metadataForURLString:(NSString*)urlString;
+ (void)removeMetadataForURLString:(NSString*)urlString;
@end
