#import <Cocoa/Cocoa.h>

NS_ASSUME_NONNULL_BEGIN

@interface TMQuickLookRenderedContent : NSObject

@property (nonatomic, readonly) NSAttributedString* attributedString;
@property (nonatomic, readonly) NSColor* backgroundColor;
@property (nonatomic, readonly) NSString* fileType;

@end

FOUNDATION_EXPORT TMQuickLookRenderedContent* _Nullable TMQuickLookRenderURL(NSURL* url, BOOL thumbnail, NSError** error);
FOUNDATION_EXPORT NSData* _Nullable TMQuickLookRTFData(TMQuickLookRenderedContent* content, NSError** error);

NS_ASSUME_NONNULL_END
