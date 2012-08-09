@interface OakSavePanel : NSObject
{
	id delegate;
	void* contextInfo;
}
+ (void)showWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow delegate:(id)aDelegate contextInfo:(void*)info;
@end

@interface NSObject (OakSavePanelDelegate)
- (void)savePanelDidEnd:(OakSavePanel*)sheet path:(NSString*)aPath contextInfo:(void*)info;
@end
