#import "OakFileURLSchemeHandler.h"
#import <UniformTypeIdentifiers/UniformTypeIdentifiers.h>

@implementation OakFileURLSchemeHandler
- (void)webView:(WKWebView*)webView startURLSchemeTask:(id<WKURLSchemeTask>)urlSchemeTask
{
	NSURL* url = urlSchemeTask.request.URL;
	NSString* filePath = url.path;

	// Handle directory -> index.html redirect
	BOOL isDir = NO;
	if([[NSFileManager defaultManager] fileExistsAtPath:filePath isDirectory:&isDir] && isDir)
	{
		NSString* indexPath = [filePath stringByAppendingPathComponent:@"index.html"];
		if([[NSFileManager defaultManager] fileExistsAtPath:indexPath])
			filePath = indexPath;
	}

	NSData* data = [NSData dataWithContentsOfFile:filePath];
	if(!data)
	{
		NSURL* errorURL = [[NSBundle bundleForClass:[self class]] URLForResource:@"error_not_found" withExtension:@"html"];
		NSString* errorHTML = [NSString stringWithContentsOfURL:errorURL encoding:NSUTF8StringEncoding error:nil];
		if(!errorHTML)
			errorHTML = [NSString stringWithFormat:@"<h1>File Not Found</h1><p>%@</p>", filePath];
		data = [errorHTML dataUsingEncoding:NSUTF8StringEncoding];

		NSURLResponse* response = [[NSURLResponse alloc] initWithURL:url
			MIMEType:@"text/html" expectedContentLength:data.length textEncodingName:@"utf-8"];
		[urlSchemeTask didReceiveResponse:response];
		[urlSchemeTask didReceiveData:data];
		[urlSchemeTask didFinish];
		return;
	}

	// Determine MIME type from file extension
	NSString* mimeType = @"application/octet-stream";
	NSString* extension = filePath.pathExtension;
	if(extension.length > 0)
	{
		UTType* utType = [UTType typeWithFilenameExtension:extension];
		if(utType.preferredMIMEType)
			mimeType = utType.preferredMIMEType;
	}

	NSString* textEncodingName = nil;
	if([mimeType hasPrefix:@"text/"] || [mimeType isEqualToString:@"application/javascript"] || [mimeType isEqualToString:@"application/json"])
		textEncodingName = @"utf-8";

	NSDictionary* headers = @{
		@"Access-Control-Allow-Origin": @"*",
		@"Content-Length": [NSString stringWithFormat:@"%lu", (unsigned long)data.length],
		@"Content-Type": textEncodingName ? [NSString stringWithFormat:@"%@; charset=%@", mimeType, textEncodingName] : mimeType,
	};
	NSHTTPURLResponse* response = [[NSHTTPURLResponse alloc] initWithURL:url statusCode:200 HTTPVersion:@"HTTP/1.1" headerFields:headers];
	[urlSchemeTask didReceiveResponse:response];
	[urlSchemeTask didReceiveData:data];
	[urlSchemeTask didFinish];
}

- (void)webView:(WKWebView*)webView stopURLSchemeTask:(id<WKURLSchemeTask>)urlSchemeTask
{
	// File reads are synchronous and fast; nothing to cancel
}
@end
