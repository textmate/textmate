#ifndef PRELUDE_M_PCH_KZLXVFRT
#define PRELUDE_M_PCH_KZLXVFRT

#include "prelude.c"
#include "prelude-mac.h"

#import <objc/objc-runtime.h>
#import <AddressBook/AddressBook.h>
#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>
#import <ExceptionHandling/NSExceptionHandler.h>
#import <QuartzCore/QuartzCore.h> // this includes CoreAnimation (Cocoa classes), at least with the 10.4 SDK

#if MAC_OS_X_VERSION_MAX_ALLOWED <= MAC_OS_X_VERSION_10_4
typedef int          NSInteger;
typedef unsigned int NSUInteger;
#endif

#endif /* end of include guard: PRELUDE_M_PCH_KZLXVFRT */
