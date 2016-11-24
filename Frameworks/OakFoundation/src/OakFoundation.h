#ifndef OAKFOUNDATION_H_816ED92F
#define OAKFOUNDATION_H_816ED92F

#import <oak/misc.h>

#ifdef __cplusplus
PUBLIC std::string OakMoveToTrash (std::string const& path);
#endif

#ifdef __OBJC__
PUBLIC BOOL OakIsEmptyString (NSString* str);
PUBLIC BOOL OakNotEmptyString (NSString* str);
#endif

#endif /* end of include guard: OAKFOUNDATION_H_816ED92F */
