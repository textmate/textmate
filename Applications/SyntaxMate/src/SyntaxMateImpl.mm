#import "SyntaxMateImpl.h"

#import <buffer/buffer.h>
#import <bundles/query.h>
#import <theme/theme.h>
#import <parse/parse.h>
#import <parse/grammar.h>
#import <ns/ns.h>
#import <file/type.h>
#import <OakFoundation/NSString Additions.h>

NSString* const SyntaxMateErrorDomain = @"com.macromates.SyntaxMate.Error";

@implementation SyntaxMateImpl

#pragma mark - Public API

- (void)highlightSourceCode:(NSData*)input withPath:(NSString*)path reply:(void (^)(NSData*, NSError*))reply
{
	if(reply == nil)
		return;
	
	// Validate input or return with error
	if(input.length == 0)
	{
		NSDictionary* info = @{ NSLocalizedDescriptionKey: @"The first parameter must be non-empty piece of data" };
		reply(nil, [NSError errorWithDomain:SyntaxMateErrorDomain code:SyntaxMateErrorInvalidInput userInfo:info]);
		return;
	}
	if(path.length == 0)
	{
		NSDictionary* info = @{ NSLocalizedDescriptionKey: @"The second parameter must be non-empty file name" };
		reply(nil, [NSError errorWithDomain:SyntaxMateErrorDomain code:SyntaxMateErrorInvalidInput userInfo:info]);
		return;
	}
	
	// The input parameter must be `NSAttributedString` archived into `NSData` with secure coding
	NSKeyedUnarchiver* unarchiver = [[NSKeyedUnarchiver alloc] initForReadingWithData:input];
	unarchiver.requiresSecureCoding = YES;
	NSMutableAttributedString* sourceCode = [[unarchiver decodeObjectOfClass:NSAttributedString.class forKey:NSKeyedArchiveRootObjectKey] mutableCopy];
	[unarchiver finishDecoding];
	if(sourceCode == nil)
	{
		NSDictionary* info = @{ NSLocalizedDescriptionKey: @"The first parameter must be of type NSMutableAttributedString archived with flag requiresSecureCoding" };
		reply(nil, [NSError errorWithDomain:SyntaxMateErrorDomain code:SyntaxMateErrorInvalidInput userInfo:info]);
		return;
	}
	
	// Highlight source code using TextMate engine
	NSError* error;
	if(![SyntaxMateImpl addStylesToSourceCode:sourceCode withPath:path error:&error])
	{
		reply(nil, error);
		return;
	}
	
	//	Archive result and send it back to the client
	NSMutableData* output = [[NSMutableData alloc] init];
	NSKeyedArchiver* archiver = [[NSKeyedArchiver alloc] initForWritingWithMutableData:output];
	archiver.requiresSecureCoding = YES;
	[archiver encodeObject:sourceCode forKey:NSKeyedArchiveRootObjectKey];
	[archiver finishEncoding];
	reply(output, nil);
}

#pragma mark - Private API

+ (BOOL)addStylesToSourceCode:(NSMutableAttributedString*)styled withPath:(NSString*)path error:(NSError**)errorPtr
{
	NSParameterAssert(errorPtr);

	// Make sure that path extension is registered in the SyntaxMate bundle
	std::string fileType = file::type_from_path(to_s(path));
	if(fileType == NULL_STR)
	{
		NSDictionary* info = @{ NSLocalizedDescriptionKey: @"The first parameter must be of type NSMutableAttributedString archived with flag requiresSecureCoding" };
		*errorPtr = [NSError errorWithDomain:SyntaxMateErrorDomain code:SyntaxMateErrorUnknownFileType userInfo:info];
		return NO;
	}
	
	//	Make sure that custom Theme is available TODO: Provide read-only access to UIDs via SyntaxMate protocol
	theme_ptr theme = parse_theme(bundles::lookup(std::string("CAB58494-D1A3-4BAD-BBC6-DAED679AD20B")));
	if(!theme)
	{
		NSDictionary* info = @{ NSLocalizedDescriptionKey: @"The first parameter must be of type NSMutableAttributedString archived with flag requiresSecureCoding" };
		*errorPtr = [NSError errorWithDomain:SyntaxMateErrorDomain code:SyntaxMateErrorUnknownTheme userInfo:info];
		return NO;
	}

	// Clear original colors and styles
	NSString* plain = styled.string;
	for(NSString* attr in @[NSForegroundColorAttributeName, NSBackgroundColorAttributeName, NSUnderlineStyleAttributeName, NSStrikethroughStyleAttributeName])
		[styled removeAttribute:attr range:NSMakeRange(0, plain.length)];
	
	// Non-empty file type guarantees that grammar is available in the package
	// Buffer is used because plain std::string is too short for some requests
	ng::buffer_t buffer;
	buffer.insert(0, to_s(plain));
	for(auto item : bundles::query(bundles::kFieldGrammarScope, fileType, scope::wildcard, bundles::kItemTypeGrammar))
	{
		buffer.set_grammar(item);
		break;
	}
	
	// Parse source code by using configured Grammar and Theme
	buffer.wait_for_repair();
	std::map<size_t, scope::scope_t> scopes = buffer.scopes(0, buffer.size());

	// Parsing is done, add styles to the original mutable attributed string
	size_t bufferFrom = 0, stringLocation = 0;
	for(auto pair = scopes.begin(); pair != scopes.end();)
	{
		styles_t styles = theme->styles_for_scope(pair->second);
		size_t bufferTo = ++pair != scopes.end() ? pair->first : buffer.size();
		size_t stringLength = [NSString stringWithCxxString:buffer.substr(bufferFrom, bufferTo)].length;
		NSDictionary* attrs = @{
			NSForegroundColorAttributeName: [NSColor colorWithCGColor:styles.foreground()],
			NSBackgroundColorAttributeName: [NSColor colorWithCGColor:styles.background()],
			NSUnderlineStyleAttributeName: @(styles.underlined() ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
			NSStrikethroughStyleAttributeName: @(styles.strikethrough() ? NSUnderlineStyleSingle : NSUnderlineStyleNone)
		};
		[styled addAttributes:attrs range:NSMakeRange(stringLocation, stringLength)];
		bufferFrom = bufferTo;
		stringLocation += stringLength;
	}
	return YES;
}

@end
