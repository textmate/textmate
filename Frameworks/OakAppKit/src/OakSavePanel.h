@class OakEncodingSaveOptionsViewController;

@interface OakSavePanel : NSObject
{
	OakEncodingSaveOptionsViewController* optionsViewController;
}
+ (void)showWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow delegate:(id)aDelegate encoding:(std::string const&)encoding newlines:(std::string const&)newlines useBOM:(BOOL)useBOM;
@end

@interface NSObject (OakSavePanelDelegate)
- (void)savePanelDidEnd:(OakSavePanel*)sheet path:(NSString*)aPath encoding:(std::string const&)encoding newlines:(std::string const&)newlines useBOM:(BOOL)useBOM;
@end
