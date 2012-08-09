@class OakEncodingPopUpButton;

@interface EncodingViewController : NSViewController
{
	IBOutlet OakEncodingPopUpButton* popUpButton;
	IBOutlet NSTextView* textView;

	char const* first;
	char const* last;
}
@property (nonatomic, readonly) NSString* currentEncoding;
- (id)initWithFirst:(char const*)firstPointer last:(char const*)lastPointer;
@end
