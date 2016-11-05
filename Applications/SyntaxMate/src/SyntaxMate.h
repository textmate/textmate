#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/*!
 * Custom domain for SyntaxMate errors.
 */
extern NSString* const SyntaxMateErrorDomain;

/*!
 * The type of SyntaxMate error.
 *
 * @field SyntaxMateErrorInvalidInput RPC call parameters are invalid.
 * @field SyntaxMateErrorUnknownFileType Path extension cannot be recognized.
 * @field SyntaxMateErrorUnknownTheme Custom theme is not available in the package.
 */
typedef NS_ENUM(NSInteger, SyntaxMateError) {
	SyntaxMateErrorInvalidInput = -1,
	SyntaxMateErrorUnknownFileType = -2,
	SyntaxMateErrorUnknownTheme = -3
};

/*!
 * Syntax highlighting interface for clients.
 */
@protocol SyntaxMate

/*!
 * Parses attributed string with source code.
 *
 * Unfortunately, you cannot pass `NSAttributedString` through XPC, so you should transfer source code as `NSData`.
 *
 * @param sourceCode `NSAttributedString` archived into `NSData` for XPC transfer.
 * @param path File name for automatic selection of the most suitable grammar.
 * @param reply The asynchronous callback with syntax highlighted `NSAttributedString` or error in case of failure.
 */
- (void)highlightSourceCode:(NSData*)sourceCode withPath:(NSString*)path reply:(void (^)(NSData* _Nullable, NSError* _Nullable))reply NS_SWIFT_NAME(highlight(_:path:reply:));

@end

NS_ASSUME_NONNULL_END
