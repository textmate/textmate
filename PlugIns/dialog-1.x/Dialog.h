#import <Cocoa/Cocoa.h>

@protocol TMPlugInController
- (CGFloat)version;
@end

static NSInteger TextMateDialogServerProtocolVersion = 9;

@protocol TextMateDialogServerProtocol
- (NSInteger)textMateDialogServerProtocolVersion;
- (id)showNib:(NSString*)aNibPath withParameters:(id)someParameters andInitialValues:(NSDictionary*)initialValues dynamicClasses:(NSDictionary*)dynamicClasses modal:(BOOL)flag center:(BOOL)shouldCenter async:(BOOL)async;

// Async window support
- (id)listNibTokens;

- (id)updateNib:(id)token withParameters:(id)someParameters;
- (id)closeNib:(id)token;
- (id)retrieveNibResults:(id)token;

// Alert
- (id)showAlertForPath:(NSString*)filePath withParameters:(NSDictionary *)parameters modal:(BOOL)modal;

// Menu
- (id)showMenuWithOptions:(NSDictionary*)someOptions;
@end
