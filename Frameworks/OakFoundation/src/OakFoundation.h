#ifndef OAKFOUNDATION_H_816ED92F
#define OAKFOUNDATION_H_816ED92F

#import <oak/misc.h>

#ifdef __cplusplus
PUBLIC std::string OakMoveToTrash (std::string const& path);
#endif

#ifdef __OBJC__
@protocol OakUserDefaultsObserver <NSObject>
- (void)userDefaultsDidChange:(NSNotification*)aNotification;
@end

PUBLIC BOOL OakIsEmptyString (NSString* str);
PUBLIC BOOL OakNotEmptyString (NSString* str);
PUBLIC void OakObserveUserDefaults (id <OakUserDefaultsObserver> obj);
#endif

#endif /* end of include guard: OAKFOUNDATION_H_816ED92F */
