//
//  TMDNibController.h
//  Dialog2
//

@interface TMDNibController : NSObject <NSWindowDelegate>
+ (TMDNibController*)controllerForToken:(NSString*)aToken;
+ (NSArray*)controllers;

@property (nonatomic, readonly) NSString* token;
@property (nonatomic) NSWindow* window;
- (id)initWithNibPath:(NSString*)aPath;
- (void)showWindowAndCenter:(BOOL)shouldCenter;

- (void)addClientFileHandle:(NSFileHandle*)aFileHandle modal:(BOOL)flag;
- (void)updateParametersWith:(id)plist;
- (void)tearDown;
@end
