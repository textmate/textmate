#ifndef OakFilterList_EXPORTS
#import <OakFilterList/OakFilterList.h>
#else
#import "../OakFilterList.h"
#endif
#import <OakFoundation/OakTimer.h>
#import <document/document.h>
#import <oak/misc.h>

PUBLIC @interface OakFileChooser : NSObject <FilterListDataSource>
+ (id)fileChooserWithPath:(NSString*)aPath projectPath:(NSString*)project;
@property (nonatomic, assign)   NSUInteger sourceIndex;
@property (nonatomic, retain)   NSString*  path;
@property (nonatomic, readonly) NSString*  projectPath;

@property (nonatomic, retain) NSString* filterString;
@property (nonatomic, retain) NSString* globString;
@property (nonatomic, retain) NSString* excludeDocumentWithIdentifier;
@end
