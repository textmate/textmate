//
//  TMDSemaphore.mm
//  TM dialog server
//
//  Created by Chris Thomas on 2006-12-06.
//

#include <semaphore.h>

@interface TMDSemaphore : NSObject
{
	sem_t* semaphore;
	NSString* name;
}

+ (TMDSemaphore*)semaphoreForTokenInt:(int)token;
+ (TMDSemaphore*)semaphoreForTokenString:(const char *)token;
+ (TMDSemaphore*)semaphoreWithName:(NSString *)name;

- (id)initWithName:(NSString *)name;

- (void)wait;
- (void)stopWaiting;
@end
