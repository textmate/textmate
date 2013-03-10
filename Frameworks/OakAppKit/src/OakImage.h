#import <oak/misc.h>

PUBLIC @interface OakImage : NSImage
@property (nonatomic) NSImage* base;
@property (nonatomic) NSImage* badge;
@property (nonatomic) CGRectEdge edge;

+ (OakImage*)imageWithBase:(NSImage*)imageBase;
+ (OakImage*)imageWithBase:(NSImage*)imageBase badge:(NSImage*)badgeImage;
+ (OakImage*)imageWithBase:(NSImage*)imageBase badge:(NSImage*)badgeImage edge:(CGRectEdge)badgeEdge;
@end
