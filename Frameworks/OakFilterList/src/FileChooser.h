#import "OakChooser.h"

extern NSUInteger const kFileChooserAllSourceIndex;
extern NSUInteger const kFileChooserOpenDocumentsSourceIndex;
extern NSUInteger const kFileChooserUncommittedChangesSourceIndex;

@class OakDocument;

PUBLIC @interface FileChooser : OakChooser
@property (nonatomic) NSString* path;
@property (nonatomic) NSUUID* currentDocument;
@property (nonatomic) NSUInteger sourceIndex;

+ (instancetype)sharedInstance;
@end
