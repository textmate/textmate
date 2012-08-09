#import "OakView.h"

@interface OakKeyEquivalentView : OakView
{
	NSString* eventString;
	NSString* displayString;
	NSMutableArray* observers;
	NSRect clearButtonRect;
	void* hotkeyToken;
	BOOL disableGlobalHotkeys;
	BOOL showClearButton;
	BOOL mouseInClearButton;
	BOOL recording;
	BOOL mouseDown;
}
@property (nonatomic, retain) NSString* eventString;
@property (nonatomic, assign) BOOL disableGlobalHotkeys;
@property (nonatomic, assign) BOOL recording;
@end
