@class OakCommand;
@class OakDocument;
@class OakTextView;

// Posted by DocumentWindowController when the user switches to a different tab.
// Object is the NSWindow; userInfo contains @"document" (OakDocument*).
extern NSNotificationName const OakWindowDidChangeSelectedDocumentNotification;

// Posted by OakTextView when the caret moves. Object is the OakTextView*;
// userInfo contains @"lineNumber" (NSNumber*, 0-based) and @"lineCount" (NSNumber*).
extern NSNotificationName const OakTextViewCaretDidChangeNotification;

typedef NS_OPTIONS(NSUInteger, OakCommandRefresherOptions) {
	OakCommandRefresherDocumentAsInput   = (1 << 0),
	OakCommandRefresherDocumentDidChange = (1 << 1),
	OakCommandRefresherDocumentDidSave   = (1 << 2),
	OakCommandRefresherDocumentDidClose  = (1 << 3),
	OakCommandRefresherOnTabSwitch       = (1 << 4),
};

@interface OakCommandRefresher : NSResponder
+ (OakCommandRefresher*)scheduleRefreshForCommand:(OakCommand*)aCommand document:(OakDocument*)document window:(NSWindow*)window options:(OakCommandRefresherOptions)options variables:(std::map<std::string, std::string> const&)variables;
+ (OakCommandRefresher*)findRefresherForCommandUUID:(NSUUID*)anIdentifier document:(OakDocument*)document window:(NSWindow*)window;
- (void)bringHTMLOutputToFront:(id)sender;
- (void)teardown;
- (void)enableScrollSyncWithTextView:(OakTextView*)textView;
- (void)enableLineSyncWithTextView:(OakTextView*)textView;
@property (nonatomic, readonly) OakCommand* command;
@end
