#import "OakImage.h"

// ===============================
// = Custom Image Representation =
// ===============================

@interface OakCustomImageRep : NSImageRep
@property (nonatomic) OakImage* image;
- (id)initWithImage:(OakImage*)anImage;
@end

@implementation OakCustomImageRep
- (id)initWithImage:(OakImage*)anImage
{
	if((self = [super init]))
	{
		self.size  = anImage.size;
		self.image = anImage;
	}
	return self;
}

- (id)copyWithZone:(NSZone*)zone
{
	OakCustomImageRep* copy = [super copyWithZone:zone];
	copy.image = self.image;
	return copy;
}

- (BOOL)draw
{
	NSSize imageSize = self.image.size;
	NSSize badgeSize = self.image.badge.size;

	CGFloat x = self.image.edge == CGRectMinXEdge || self.image.edge == CGRectMaxYEdge ? 0 : imageSize.width  - badgeSize.width;
	CGFloat y = self.image.edge == CGRectMinXEdge || self.image.edge == CGRectMinYEdge ? 0 : imageSize.height - badgeSize.height;

	[self.image.base drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositingOperationCopy fraction:1];
	[self.image.badge drawAtPoint:NSMakePoint(x, y) fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:1];
	return YES;
}
@end

// ============
// = OakImage =
// ============

@implementation OakImage
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
	OakImage* res = [[self alloc] initWithSize:[imageBase size]];
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
		[self addRepresentation:[[OakCustomImageRep alloc] initWithImage:self]];
	return self;
}

- (void)setSize:(NSSize)aSize
{
	for(NSImageRep* imageRep in [self representations])
		[imageRep setSize:aSize];
	[super setSize:aSize];
}
@end
