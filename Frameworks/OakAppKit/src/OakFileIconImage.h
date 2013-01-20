#import <oak/misc.h>
#import <scm/status.h>

PUBLIC @interface OakFileIconImage : NSImage
@property (nonatomic, retain)               NSString*         path;
@property (nonatomic)                       BOOL              exists;
@property (nonatomic, getter = isDirectory) BOOL              directory;
@property (nonatomic, getter = isAlias)     BOOL              alias;
@property (nonatomic)                       scm::status::type scmStatus;
@property (nonatomic, getter = isModified)  BOOL              modified;
+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag size:(NSSize)aSize;
+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag;
+ (id)fileIconImageWithPath:(NSString*)aPath size:(NSSize)aSize;
@end
