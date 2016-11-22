#import <oak/misc.h>

#ifdef __cplusplus
PUBLIC std::string OakMoveToTrash (std::string const& path);
#endif

#ifdef __OBJC__
PUBLIC BOOL OakIsEmptyString (NSString* str);
PUBLIC BOOL OakNotEmptyString (NSString* str);
#endif
