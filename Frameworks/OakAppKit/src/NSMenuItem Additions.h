@interface NSMenuItem (FileIcon)
- (void)updateTitle:(NSString*)newTitle;
- (void)setIconForFile:(NSString*)path;
- (void)setKeyEquivalentCxxString:(std::string const&)aKeyEquivalent;
- (void)setInactiveKeyEquivalentCxxString:(std::string const&)aKeyEquivalent;
- (void)setTabTriggerCxxString:(std::string const&)aTabTrigger;
- (void)setModifiedState:(BOOL)flag;
@end

