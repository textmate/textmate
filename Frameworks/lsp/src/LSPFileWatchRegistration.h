#ifndef LSP_FILE_WATCH_REGISTRATION_H_F9A3B2E1
#define LSP_FILE_WATCH_REGISTRATION_H_F9A3B2E1

#import <Foundation/Foundation.h>

@interface LSPFileWatchRegistration : NSObject
@property (nonatomic) NSString* registrationId;
@property (nonatomic) NSSet<NSString*>* extensions;
@property (nonatomic) NSSet<NSString*>* exactNames;
@property (nonatomic) int watchKind;
@property (nonatomic) NSString* basePath;
@property (nonatomic) BOOL watchAll;
@end

#endif /* LSP_FILE_WATCH_REGISTRATION_H_F9A3B2E1 */
