#import <Cocoa/Cocoa.h>

@interface NSResponder (OakPlaceholderActions)
// Placeholder menu items use this selector so automatic menu validation leaves
// them disabled when no real responder implements it.
- (IBAction)nop:(id)sender;
@end
