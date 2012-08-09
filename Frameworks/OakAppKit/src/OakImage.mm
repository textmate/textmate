#import "OakImage.h"

// ===============================
// = Custom Image Representation =
// ===============================

@interface OakCustomImageRep : NSImageRep
{
	OakImage* image;
}
- (id)initWithImage:(OakImage*)anImage;
@end

@implementation OakCustomImageRep
- (id)initWithImage:(OakImage*)anImage
{
	if((self = [super init]))
	{
		self.size = anImage.size;
		image = anImage;
	}
	return self;
}

- (id)copyWithZone:(NSZone*)zone
{
	OakCustomImageRep* copy = [super copyWithZone:zone];
	copy->image = image;
	return copy;
}

- (void)dealloc
{
	[super dealloc];
}

- (BOOL)draw
{
	NSSize imageSize = image.size;
	NSSize badgeSize = image.badge.size;

	CGFloat x = image.edge == CGRectMinXEdge || image.edge == CGRectMaxYEdge ? 0 : imageSize.width  - badgeSize.width;
	CGFloat y = image.edge == CGRectMinXEdge || image.edge == CGRectMinYEdge ? 0 : imageSize.height - badgeSize.height;

	[image.base drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeCopy fraction:1];
	[image.badge drawAtPoint:NSMakePoint(x, y) fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1];
	return YES;
}
@end

// ============
// = OakImage =
// ============

@implementation OakImage
@synthesize base, badge, edge;

+ (OakImage*)imageWithBase:(NSImage*)imageBase
{
	return [self imageWithBase:imageBase badge:nil edge:CGRectMinXEdge];
}

+ (OakImage*)imageWithBase:(NSImage*)imageBase badge:(NSImage*)badgeImage
{
	return [self imageWithBase:imageBase badge:badgeImage edge:CGRectMinXEdge];
}

+ (OakImage*)imageWithBase:(NSImage*)imageBase badge:(NSImage*)badgeImage edge:(CGRectEdge)badgeEdge
{
	OakImage* res = [[[self alloc] initWithSize:[imageBase size]] autorelease];
	res.base  = imageBase;
	res.badge = badgeImage;
	res.edge  = badgeEdge;
	return res;
}

- (id)init
{
	return [self initWithSize:NSMakeSize(128, 128)];
}

- (id)initWithSize:(NSSize)aSize
{
	if((self = [super initWithSize:aSize]))
		[self addRepresentation:[[[OakCustomImageRep alloc] initWithImage:self] autorelease]];
	return self;
}

- (void)setSize:(NSSize)aSize
{
	for(NSImageRep* imageRep in [self representations])
		[imageRep setSize:aSize];
	[super setSize:aSize];
}

- (void)dealloc
{
	[base release];
	[badge release];
	[super dealloc];
}
@end
