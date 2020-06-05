#ifndef OAKFOUNDATION_H_816ED92F
#define OAKFOUNDATION_H_816ED92F

#ifdef __cplusplus
std::string OakMoveToTrash (std::string const& path);
#endif

#ifdef __OBJC__
@protocol OakUserDefaultsObserver <NSObject>
- (void)userDefaultsDidChange:(NSNotification*)aNotification;
@end

BOOL OakIsEmptyString (NSString* str);
BOOL OakNotEmptyString (NSString* str);
void OakObserveUserDefaults (id <OakUserDefaultsObserver> obj);
#endif

#endif /* end of include guard: OAKFOUNDATION_H_816ED92F */
