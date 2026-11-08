#import "EditorContextChanges.h"

@implementation EditorContextChanges {
    NSString* _epoch;
    NSUInteger _sequence;
    NSMutableArray<NSDictionary*>* _events;
}
- (instancetype)init {
    if((self = [super init])) { _epoch = NSUUID.UUID.UUIDString; _events = [NSMutableArray array]; }
    return self;
}
- (NSString*)cursor { return [NSString stringWithFormat:@"%@/%lu", _epoch, (unsigned long)_sequence]; }
- (void)record:(NSString*)type documentId:(NSString*)documentId {
    NSAssert(NSThread.isMainThread, @"Record editor changes on the main thread");
    [_events addObject:@{ @"sequence": @(++_sequence), @"type": type, @"documentId": documentId ?: (id)NSNull.null }];
    if(_events.count > 128) [_events removeObjectAtIndex:0];
}
- (NSDictionary*)changesAfter:(NSString*)cursor {
    NSAssert(NSThread.isMainThread, @"Read editor changes on the main thread");
    NSString* prefix = [_epoch stringByAppendingString:@"/"];
    unsigned long long after = 0;
    BOOL valid = [cursor hasPrefix:prefix];
    if(valid) {
        NSString* suffix = [cursor substringFromIndex:prefix.length];
        NSScanner* scanner = [NSScanner scannerWithString:suffix];
        valid = [scanner scanUnsignedLongLong:&after] && scanner.isAtEnd &&
            [[NSString stringWithFormat:@"%llu", after] isEqual:suffix] && after <= _sequence;
    }
    BOOL reset = !valid || (_events.count && after+1 < [_events.firstObject[@"sequence"] unsignedLongLongValue]);
    NSMutableArray* events = [NSMutableArray array];
    if(!reset) for(NSDictionary* event in _events)
        if([event[@"sequence"] unsignedLongLongValue] > after) [events addObject:event];
    return @{ @"cursor": self.cursor, @"changed": @(reset || after < _sequence), @"reset": @(reset), @"events": events };
}
@end
