#import "PreviewProvider.h"

#import <UniformTypeIdentifiers/UniformTypeIdentifiers.h>
#import "../Shared/QuickLookRenderer.h"

@implementation PreviewProvider

- (void)providePreviewForFileRequest:(QLFilePreviewRequest*)request completionHandler:(void (^)(QLPreviewReply*, NSError*))handler
{
	NSURL* fileURL = request.fileURL;
	if(!fileURL)
	{
		handler(nil, [NSError errorWithDomain:@"com.macromates.TextMate.QuickLook" code:2 userInfo:@{ NSLocalizedDescriptionKey: @"Quick Look did not provide a file URL." }]);
		return;
	}

	QLPreviewReply* reply = [[QLPreviewReply alloc] initWithDataOfContentType:UTTypeRTF contentSize:CGSizeMake(900, 700) dataCreationBlock:^NSData*(QLPreviewReply* replyToUpdate, NSError** error) {
		TMQuickLookRenderedContent* content = TMQuickLookRenderURL(fileURL, NO, error);
		if(!content)
			return nil;
		replyToUpdate.title = fileURL.lastPathComponent;
		return TMQuickLookRTFData(content, error);
	}];
	handler(reply, nil);
}

@end
