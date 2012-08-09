@interface NSView (PopupAddition)
- (void)showMenu:(NSMenu*)menu inRect:(NSRect)rect withSelectedIndex:(NSInteger)index font:(NSFont*)font popup:(BOOL)isPopup;
@end
