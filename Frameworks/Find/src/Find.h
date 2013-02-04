#import <OakFoundation/OakFindProtocol.h>
#import "scan_path.h" // for find::folder_scan_settings_t used in std::map

namespace find
{
	namespace in { enum { document, selection, folder, open_files }; }
}

PUBLIC @interface Find : NSResponder <OakFindServerProtocol>
@property (nonatomic, copy) NSString* projectFolder;
@property (nonatomic, copy) NSString* searchFolder;
@property (nonatomic, copy) NSString* projectIdentifier;
@property (nonatomic, copy) NSString* documentIdentifier;

@property (nonatomic, readonly) BOOL isVisible;

@property (nonatomic, assign) int searchScope;

+ (Find*)sharedInstance;

- (IBAction)showFindPanel:(id)sender;
- (IBAction)showFolderSelectionPanel:(id)sender;
- (IBAction)takeFindOptionToToggleFrom:(id)sender;

// =========================
// = OakFindProtocolServer =
// =========================

@property (nonatomic, readonly) find_operation_t findOperation;
@property (nonatomic, readonly) NSString*        findString;
@property (nonatomic, readonly) NSString*        replaceString;
@property (nonatomic, readonly) find::options_t  findOptions;

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition;
- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString;
@end
