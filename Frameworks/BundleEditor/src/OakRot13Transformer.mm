#import "OakRot13Transformer.h"
#import <OakFoundation/src/NSString Additions.h>
#import <text/src/decode.h>

@implementation OakRot13Transformer
+ (void)register                        { [NSValueTransformer setValueTransformer:[OakRot13Transformer new] forName:@"OakRot13Transformer"]; }
+ (Class)transformedValueClass          { return [NSString class]; }
+ (BOOL)allowsReverseTransformation     { return YES; }
- (id)transformedValue:(id)value        { return [NSString stringWithCxxString:decode::rot13([value UTF8String] ?: "")]; }
- (id)reverseTransformedValue:(id)value { return [NSString stringWithCxxString:decode::rot13([value UTF8String] ?: "")]; }
@end
