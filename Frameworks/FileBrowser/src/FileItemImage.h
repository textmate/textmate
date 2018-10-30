#import <oak/misc.h>
#import <scm/status.h>

PUBLIC NSImage* CreateIconImageForURL (NSURL* url, BOOL isModified, BOOL isMissing, BOOL isDirectory, BOOL isSymbolicLink, scm::status::type scmStatus);
