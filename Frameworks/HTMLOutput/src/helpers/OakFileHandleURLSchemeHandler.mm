#import "OakFileHandleURLSchemeHandler.h"
#import "OakHTMLOutputRequestMetadata.h"
#import <OakSystem/process.h>

@implementation OakFileHandleURLSchemeHandler
{
	NSMutableDictionary<NSValue*, NSNumber*>* _stoppedTasks;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_stoppedTasks = [NSMutableDictionary new];
	}
	return self;
}

- (void)webView:(WKWebView*)webView startURLSchemeTask:(id<WKURLSchemeTask>)urlSchemeTask
{
	NSString* urlString = urlSchemeTask.request.URL.absoluteString;
	OakHTMLOutputRequestMetadata* metadata = [OakHTMLOutputRequestMetadata metadataForURLString:urlString];

	if(!metadata || !metadata.fileHandle)
	{
		NSURLResponse* response = [[NSHTTPURLResponse alloc] initWithURL:urlSchemeTask.request.URL
			statusCode:404 HTTPVersion:@"HTTP/1.1" headerFields:nil];
		[urlSchemeTask didReceiveResponse:response];
		[urlSchemeTask didFinish];
		NSLog(@"No command output for '%@'", urlSchemeTask.request.URL);
		return;
	}

	NSURLResponse* response = [[NSURLResponse alloc] initWithURL:urlSchemeTask.request.URL
		MIMEType:@"text/html" expectedContentLength:-1 textEncodingName:@"utf-8"];
	[urlSchemeTask didReceiveResponse:response];

	NSFileHandle* fileHandle = metadata.fileHandle;
	NSValue* taskKey = [NSValue valueWithNonretainedObject:urlSchemeTask];

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		int len;
		char buf[8192];
		__block BOOL keepRunning = YES;

		@try {
			while(keepRunning && (len = read(fileHandle.fileDescriptor, buf, sizeof(buf))) > 0)
			{
				NSData* rawData = [NSData dataWithBytes:buf length:len];
				NSData* data = RewriteFileURLsInAttributes(rawData);
				dispatch_sync(dispatch_get_main_queue(), ^{
					if(self->_stoppedTasks[taskKey])
					{
						keepRunning = NO;
					}
					else
					{
						if(data.length > 0)
							[urlSchemeTask didReceiveData:data];
					}
				});
			}
		}
		@catch(NSException* e) {
			NSData* data = [[NSString stringWithFormat:@"<p>Exception thrown while reading data: %@.</p>",
				e.reason] dataUsingEncoding:NSUTF8StringEncoding];
			dispatch_sync(dispatch_get_main_queue(), ^{
				if(!self->_stoppedTasks[taskKey])
					[urlSchemeTask didReceiveData:data];
			});
		}

		if(len == -1)
			perror("HTMLOutput: read");

		[fileHandle closeFile];
		dispatch_async(dispatch_get_main_queue(), ^{
			if(!self->_stoppedTasks[taskKey])
				[urlSchemeTask didFinish];
			[self->_stoppedTasks removeObjectForKey:taskKey];
			[OakHTMLOutputRequestMetadata removeMetadataForURLString:urlString];
		});
	});
}

- (void)webView:(WKWebView*)webView stopURLSchemeTask:(id<WKURLSchemeTask>)urlSchemeTask
{
	NSValue* taskKey = [NSValue valueWithNonretainedObject:urlSchemeTask];
	_stoppedTasks[taskKey] = @YES;

	NSString* urlString = urlSchemeTask.request.URL.absoluteString;
	OakHTMLOutputRequestMetadata* metadata = [OakHTMLOutputRequestMetadata metadataForURLString:urlString];
	if(pid_t pid = metadata.processIdentifier.intValue)
		oak::kill_process_group_in_background(pid);
}

static NSData* RewriteFileURLsInAttributes(NSData* data)
{
	NSString* html = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
	if(!html)
		return data;

	// Rewrite file:// in href="..." and src="..." attributes only,
	// but NOT inside txmt:// URLs (which embed file:// as a parameter)
	NSMutableString* result = [html mutableCopy];

	// Match href="file:// or src="file:// but not href="txmt://...file://
	NSRegularExpression* regex = [NSRegularExpression regularExpressionWithPattern:
		@"((?:href|src)\\s*=\\s*[\"'])file://" options:0 error:nil];

	// Work backwards so indices stay valid
	NSArray<NSTextCheckingResult*>* matches = [regex matchesInString:result options:0 range:NSMakeRange(0, result.length)];
	for(NSTextCheckingResult* match in [matches reverseObjectEnumerator])
	{
		NSRange fullRange = match.range;
		NSString* prefix = [result substringWithRange:[match rangeAtIndex:1]];

		// Skip if this is inside a txmt:// URL
		NSUInteger searchStart = fullRange.location > 50 ? fullRange.location - 50 : 0;
		NSRange searchRange = NSMakeRange(searchStart, fullRange.location - searchStart);
		if([result rangeOfString:@"txmt://" options:0 range:searchRange].location != NSNotFound)
			continue;

		[result replaceCharactersInRange:fullRange withString:[prefix stringByAppendingString:@"tm-file://"]];
	}

	return [result dataUsingEncoding:NSUTF8StringEncoding];
}
@end
