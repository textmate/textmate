#import <OakFoundation/NSString Additions.h>

@interface NSSavePanel (DeselectExtension)
- (void)deselectExtension;
@end

@interface NSSavePanel (HiddenFiles)
- (void)setShowsHiddenFilesCheckBox:(BOOL)flag;
@end