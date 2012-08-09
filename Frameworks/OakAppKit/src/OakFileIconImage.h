#import <scm/scm.h>

@interface OakFileIconImage : NSImage
{
	scm::info_ptr scmDriver;

	NSString* path;
	BOOL isModified;
	BOOL existsOnDisk;
	scm::status::type scmStatus;

	NSImage* base;
	NSImage* badge;
}
+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag size:(NSSize)aSize;
+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag;
+ (id)fileIconImageWithPath:(NSString*)aPath size:(NSSize)aSize;
@property (nonatomic, retain) NSString* path;
@property (nonatomic, assign) BOOL isModified;
@end
