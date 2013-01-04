#include <OakFoundation/OakFoundation.h>

PUBLIC extern NSURL* kURLLocationComputer;
PUBLIC extern NSURL* kURLLocationHome;
PUBLIC extern NSURL* kURLLocationDesktop;
PUBLIC extern NSURL* kURLLocationFavorites;
PUBLIC extern NSURL* kURLLocationBundles;

PUBLIC NSString* DisplayName (NSURL* url, size_t numberOfParents = 0);
PUBLIC NSImage* IconImage (NSURL* url, NSSize size = (NSSize){16, 16});
PUBLIC NSURL* ParentForURL (NSURL* url);
