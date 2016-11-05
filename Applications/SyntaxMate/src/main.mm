#import "SyntaxMateImpl.h"

#import <settings/settings.h>
#import <buffer/buffer.h>

/// Single instance which creates new SyntaxMateImpl for each request
@interface ConnectionHandler : NSObject <NSXPCListenerDelegate>
@end

#pragma mark

int main (int argc, char const* argv[])
{
	// Parse only SyntaxMate bundle packaged into this XPC Sevice
	std::vector<std::string> paths;
	paths.push_back(std::string(path::join(NSBundle.mainBundle.bundleURL.fileSystemRepresentation, "Contents/Resources")));
	
	// Keep parsed bundle in the local Caches folder
	NSURL* cachesURL = [NSFileManager.defaultManager URLForDirectory:NSCachesDirectory inDomain:NSUserDomainMask appropriateForURL:nil create:YES error:NULL];
	std::string bundlesIndexCachePath([cachesURL URLByAppendingPathComponent:@"TextMateBundlesIndex.binary"].fileSystemRepresentation);
	
	// Convert property lists into fast binaries
	plist::cache_t cache;
	cache.load(bundlesIndexCachePath);
	auto index = create_bundle_index(paths, cache);
	bundles::set_index(index.first, index.second);
	
	// Start listening for incoming requests from the parent bundle
	static ConnectionHandler *delegate = nil;
	delegate = [[ConnectionHandler alloc] init];
	NSXPCListener* listener = [NSXPCListener serviceListener];
	listener.delegate = delegate;
	
	// Listen to incoming connections while the service is running
	[listener resume]; // This method will never return
	return 0;
}

#pragma mark

@implementation ConnectionHandler

- (BOOL)listener:(NSXPCListener*)listener shouldAcceptNewConnection:(NSXPCConnection*)newConnection
{	
	// Declare support for RPC calls using SyntaxMate protocol
	newConnection.exportedInterface = [NSXPCInterface interfaceWithProtocol:@protocol(SyntaxMate)];
	
	// Create new object conforming to the SyntaxMate protocol that will be released once request is completed
	id<SyntaxMate> exportedObject = [[SyntaxMateImpl alloc] init];
	newConnection.exportedObject = exportedObject;

	// Report to macOS that incoming connection is accepted
	[newConnection resume];
	return YES;
}

@end
