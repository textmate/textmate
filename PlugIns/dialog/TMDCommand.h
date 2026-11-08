#import "CLIProxy.h"

@interface TMDCommand : NSObject
+ (void)registerObject:(id)anObject forCommand:(NSString*)aCommand;
+ (NSDictionary*)registeredCommands;

+ (id)objectForCommand:(NSString*)aCommand;

+ (id)readPropertyList:(NSFileHandle*)aFileHandle error:(NSError**)error;
+ (void)writePropertyList:(id)aPlist toFileHandle:(NSFileHandle*)aFileHandle;

- (NSString*)commandDescription;
- (NSString*)usageForInvocation:(NSString*)invocation;
@end
