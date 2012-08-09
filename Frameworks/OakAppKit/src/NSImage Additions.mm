#import "NSImage Additions.h"

@implementation NSImage (ImageFromBundle)
+ (NSImage*)imageNamed:(NSString*)aName inSameBundleAsClass:(id)aClass
{
	if(!aName)
		return nil;

	NSBundle* bundle = [NSBundle bundleForClass:aClass];
	NSString* name   = [NSString stringWithFormat:@"%@.%@", [bundle bundleIdentifier], aName];

	static NSMutableDictionary* cache = [NSMutableDictionary new];
	if(NSImage* res = [cache objectForKey:name])
		return res;

	NSString* resourcePath = bundle.resourcePath;
	for(NSString* resource in [[NSFileManager defaultManager] contentsOfDirectoryAtPath:resourcePath error:nil])
	{
		static NSSet* imageTypes = [[NSSet alloc] initWithArray:[NSImage imageFileTypes]];
		if([aName isEqualToString:[resource stringByDeletingPathExtension]] && [imageTypes containsObject:[resource pathExtension]])
		{
			NSString* imagePath = [resourcePath stringByAppendingPathComponent:resource];
			if(NSImage* image = [[[NSImage alloc] initWithContentsOfFile:imagePath] autorelease])
			{
				[cache setObject:image forKey:name];
				return image;
			}
		}
	}

	NSBeep();
	NSLog(@"*** could not find image named ‘%@’ in %@", aName, resourcePath);

	return nil;
}

+ (NSImage*)imageWithCGImage:(CGImageRef)cgImage
{
	NSImage* res = nil;
	if(cgImage)
	{
		res = [[NSImage alloc] init];
		NSBitmapImageRep* bitmapRep = [[NSBitmapImageRep alloc] initWithCGImage:cgImage];
		[res addRepresentation:bitmapRep];
		[bitmapRep release];
	}
	return res;
}

// ===================================================
// = Gracefully draw in potentially flipped contexts =
// ===================================================

- (void)drawAdjustedAtPoint:(NSPoint)aPoint fromRect:(NSRect)srcRect operation:(NSCompositingOperation)op fraction:(CGFloat)delta
{
	[self drawAdjustedInRect:(NSRect){ aPoint, [self size] } fromRect:srcRect operation:op fraction:delta];
}

- (void)drawAdjustedInRect:(NSRect)dstRect fromRect:(NSRect)srcRect operation:(NSCompositingOperation)op fraction:(CGFloat)delta
{
	NSGraphicsContext* context = [NSGraphicsContext currentContext];
	if([context isFlipped])
	{
		[context saveGraphicsState];

		NSAffineTransform* transform = [NSAffineTransform transform];
		[transform translateXBy:0 yBy:NSMaxY(dstRect)];
		[transform scaleXBy:1 yBy:-1];
		[transform concat];

		dstRect.origin.y = 0.0; // The transform above places the y-origin right where the image should be drawn.
		[self drawInRect:dstRect fromRect:srcRect operation:op fraction:delta];

		[context restoreGraphicsState];
	}
	else
	{
		[self drawInRect:dstRect fromRect:srcRect operation:op fraction:delta];
	}
}
@end
