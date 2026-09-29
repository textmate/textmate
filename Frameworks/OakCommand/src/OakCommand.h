#include <command/src/parser.h>

extern NSNotificationName const OakCommandDidTerminateNotification;
extern NSString* const OakCommandErrorDomain;

NS_ENUM(NSInteger) {
	OakCommandRequirementsMissingError,
	OakCommandAbnormalTerminationError
};

@class OakHTMLOutputView;

@interface OakCommand : NSObject
@property (nonatomic, weak) NSResponder* firstResponder;
@property (nonatomic, readonly) NSUUID* identifier;
@property (nonatomic, strong) void(^modalEventLoopRunner)(OakCommand*, BOOL* didTerminate);
@property (nonatomic, strong) void(^terminationHandler)(OakCommand*, BOOL normalExit);
@property (nonatomic) BOOL updateHTMLViewAtomically;
@property (nonatomic, readonly) OakHTMLOutputView* htmlOutputView;
- (instancetype)initWithBundleCommand:(bundle_command_t const&)aCommand;
- (void)executeWithInput:(NSFileHandle*)fileHandleForReading variables:(std::map<std::string, std::string> const&)someVariables outputHandler:(void(^)(std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, std::map<std::string, std::string> const& environment))handler;
- (void)terminate;
- (void)closeHTMLOutputView;
@end
