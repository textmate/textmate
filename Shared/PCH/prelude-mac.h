#ifndef PRELUDE_MAC_H_X1SR1JB2
#define PRELUDE_MAC_H_X1SR1JB2

#import "prelude.c"

#import <AudioToolbox/AudioToolbox.h>
#import <Carbon/Carbon.h>
#import <CommonCrypto/CommonDigest.h>
#import <CoreFoundation/CoreFoundation.h>
#import <CoreServices/CoreServices.h>
#import <Security/Security.h>
#import <SystemConfiguration/SystemConfiguration.h>

#import <libkern/OSAtomic.h>
#import <machine/byte_order.h>

#if defined(MAC_OS_X_VERSION_10_12) && (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_12)
#import <os/log.h>
#endif

#endif /* end of include guard: PRELUDE_MAC_H_X1SR1JB2 */
