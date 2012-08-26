#include <file/encoding.h>

@class OakEncodingSaveOptionsViewController;

@interface OakSavePanel : NSObject
{
	OakEncodingSaveOptionsViewController* optionsViewController;
}
+ (void)showWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow delegate:(id)aDelegate encoding:(encoding::type const&)encoding;
@end

@interface NSObject (OakSavePanelDelegate)
- (void)savePanelDidEnd:(OakSavePanel*)sheet path:(NSString*)aPath encoding:(encoding::type const&)encoding;
@end
