#import "ThumbnailProvider.h"

#import "../Shared/QuickLookRenderer.h"

@implementation ThumbnailProvider

- (void)provideThumbnailForFileRequest:(QLFileThumbnailRequest*)request completionHandler:(void (^)(QLThumbnailReply*, NSError*))handler
{
	NSURL* fileURL = request.fileURL;
	CGSize size = request.maximumSize;
	QLThumbnailReply* reply = [QLThumbnailReply replyWithContextSize:size currentContextDrawingBlock:^BOOL {
		NSError* error = nil;
		TMQuickLookRenderedContent* content = TMQuickLookRenderURL(fileURL, YES, &error);
		if(!content)
			return NO;

		[content.backgroundColor setFill];
		NSRectFill(NSMakeRect(0, 0, size.width, size.height));
		NSRect textRect = NSInsetRect(NSMakeRect(0, 0, size.width, size.height), 5, 5);
		[content.attributedString drawWithRect:textRect options:NSStringDrawingUsesLineFragmentOrigin | NSStringDrawingTruncatesLastVisibleLine];
		return YES;
	}];
	handler(reply, nil);
}

@end
