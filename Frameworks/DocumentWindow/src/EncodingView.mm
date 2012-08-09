#import "EncodingView.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakEncodingPopUpButton.h>
#import <text/hexdump.h>
#import <oak/oak.h>
#import <oak/debug.h>

@implementation EncodingViewController
- (id)initWithFirst:(char const*)firstPointer last:(char const*)lastPointer
{
	if(self = [super initWithNibName:@"EncodingView" bundle:[NSBundle bundleForClass:[self class]]])
	{
		first = firstPointer;
		last  = lastPointer;
	}
	return self;
}

- (NSString*)currentEncoding
{
	return popUpButton.encoding;
}

- (void)loadView
{
	[super loadView];
	ASSERT(self.view); ASSERT(popUpButton); ASSERT(textView);

	[[textView textStorage] setAttributedString:[[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:text::to_hex(first, last)] attributes:@{ NSFontAttributeName : [NSFont userFixedPitchFontOfSize:12] }] autorelease]];
	[textView setEditable:NO];

	int lines = oak::cap(5, (last - first) / 16, 20);
	NSSize size = self.view.frame.size;
	[self.view setFrameSize:NSMakeSize(size.width, size.height - 300 + 16*lines + 1)];
}
@end
