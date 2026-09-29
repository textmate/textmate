extern NSString* const CompareMateIgnoredFileNamesDefaultsKey;
extern NSNotificationName const CompareMateIgnoredFileNamesDidChangeNotification;

@interface WindowController : NSWindowController <NSWindowRestoration>
- (instancetype)initWithLeftPath:(NSString*)leftPath rightPath:(NSString*)rightPath;
- (IBAction)nextChange:(id)sender;
- (IBAction)previousChange:(id)sender;
- (IBAction)copyChangeToLeft:(id)sender;
- (IBAction)copyChangeToRight:(id)sender;
- (IBAction)saveDocument:(id)sender;
- (IBAction)saveDocumentAs:(id)sender;
@end

@interface FolderWindowController : NSWindowController <NSWindowRestoration>
- (instancetype)initWithLeftPath:(NSString*)leftPath rightPath:(NSString*)rightPath;
- (BOOL)canOpenSelectedFile;
- (IBAction)openSelectedFile:(id)sender;
- (IBAction)nextChange:(id)sender;
- (IBAction)previousChange:(id)sender;
- (IBAction)copyChangeToLeft:(id)sender;
- (IBAction)copyChangeToRight:(id)sender;
- (IBAction)moveChangeToLeft:(id)sender;
- (IBAction)moveChangeToRight:(id)sender;
@end
