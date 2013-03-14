@interface NSMenuItem (FileIcon)
- (void)setIconForFile:(NSString*)path;
- (void)setKeyEquivalentCxxString:(std::string const&)aKeyEquivalent;
- (void)setInactiveKeyEquivalentCxxString:(std::string const&)aKeyEquivalent;
- (void)setTabTriggerCxxString:(std::string const&)aTabTrigger;
- (void)setModifiedState:(BOOL)flag;
@end

