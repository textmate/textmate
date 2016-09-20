@interface NSMenuItem (FileIcon)
- (void)updateTitle:(NSString*)newTitle;
- (void)setIconForFile:(NSString*)path;
- (void)setKeyEquivalentCxxString:(std::string const&)aKeyEquivalent;
- (void)setActivationString:(NSString*)anActivationString withFont:(NSFont*)aFont;
- (void)setInactiveKeyEquivalentCxxString:(std::string const&)aKeyEquivalent;
- (void)setTabTriggerCxxString:(std::string const&)aTabTrigger;
- (void)setModifiedState:(BOOL)flag;
- (void)setDynamicTitle:(NSString*)title;
@end
