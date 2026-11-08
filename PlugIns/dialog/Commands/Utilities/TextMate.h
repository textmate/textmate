#import <Foundation/Foundation.h>

@interface NSObject (OakTextView)
- (NSPoint)positionForWindowUnderCaret;
@end

void insert_text (NSString* someText);
void insert_snippet (NSString* aSnippet);
