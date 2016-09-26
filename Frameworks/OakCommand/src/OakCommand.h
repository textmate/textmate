#include <command/parser.h>

extern NSString* const OakCommandDidTerminateNotification;
extern NSString* const OakCommandErrorDomain;

NS_ENUM(NSInteger) {
	OakCommandRequirementsMissingError,
	OakCommandAbnormalTerminationError
};

PUBLIC @interface OakCommand : NSObject
@property (nonatomic, weak) NSResponder* firstResponder;
@property (nonatomic, getter = isAsyncCommand, readonly) BOOL asyncCommand;
@property (nonatomic, readonly) NSUUID* identifier;
@property (nonatomic) void(^terminationHandler)(OakCommand*, BOOL normalExit);
- (instancetype)initWithBundleCommand:(bundle_command_t const&)aCommand;
- (void)executeWithInput:(NSFileHandle*)fileHandleForReading variables:(std::map<std::string, std::string> const&)someVariables outputHandler:(void(^)(std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, std::map<std::string, std::string> const& environment))handler;
- (void)waitUntilExit;
- (void)terminate;
@end
