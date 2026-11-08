#import "../src/DocumentWindowController.h"
#import "../src/EditorContextService.h"
#import <EditorContextProtocol.h>
@class OakTextView;
@interface ContextDiagnosticTestClient : NSObject
@property BOOL initialized;
@property BOOL running;
@property BOOL indexing;
@end
@implementation ContextDiagnosticTestClient
@end
@interface DocumentWindowController (EditorContextTests)
@property OakTextView* textView;
- (void)setDocuments:(NSArray*)documents;
- (void)setSelectedDocument:(OakDocument*)document;
- (EditorContextSnapshot*)editorContextForMethod:(NSString*)method arguments:(NSDictionary*)arguments;
- (NSDictionary*)testEditorContextForMethod:(NSString*)method arguments:(NSDictionary*)arguments;
@end
@implementation DocumentWindowController (EditorContextTestRendering)
- (NSDictionary*)testEditorContextForMethod:(NSString*)method arguments:(NSDictionary*)arguments {
    return [[self editorContextForMethod:method arguments:arguments] renderBefore:editor_context::now()+3];
}
@end
