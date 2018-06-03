#import "StringImage.h"

// ===============================
// = Custom Image Representation =
// ===============================

@interface StringImageRep : NSImageRep
@property (nonatomic) NSString* stringValue;
@end

@implementation StringImageRep
- (id)initWithString:(NSString*)aString
{
	if((self = [super init]))
	{
		_stringValue = aString;
	}
	return self;
}

- (id)init
{
	return [self initWithString:@"?"];
}

- (id)copyWithZone:(NSZone*)zone
{
	StringImageRep* copy = [super copyWithZone:zone];
	copy->_stringValue = _stringValue;
	return copy;
}

- (BOOL)draw
{
	NSDictionary* attributes = @{
		NSFontAttributeName: [NSFont systemFontOfSize:20],
		NSForegroundColorAttributeName: [NSColor textColor],
	};
	NSMutableAttributedString* str = [[NSMutableAttributedString alloc] initWithString:_stringValue attributes:attributes];

	NSSize size = [str size];
	NSRect box = { NSZeroPoint, self.size };

	box = NSIntegralRect(NSInsetRect(box, (NSWidth(box) - size.width)/2, (NSHeight(box) - size.height)/2));
	[str drawInRect:box];

	return YES;
}
@end

// ===============
// = StringImage =
// ===============

@interface StringImage ()
@property (nonatomic) StringImageRep* stringImageRep;
@end

@implementation StringImage
+ (instancetype)imageWithString:(NSString*)aString size:(NSSize)aSize
{
	return [[StringImage alloc] initWithString:aString size:aSize];
}

- (instancetype)init
{
	return [self initWithSize:NSZeroSize];
}

- (instancetype)initWithSize:(NSSize)aSize
{
	return [self initWithRepresentation:[[StringImageRep alloc] init] size:aSize];
}

- (instancetype)initWithString:(NSString*)aString size:(NSSize)aSize
{
	return [self initWithRepresentation:[[StringImageRep alloc] initWithString:aString] size:aSize];
}

- (instancetype)initWithRepresentation:(StringImageRep*)aRepresentation size:(NSSize)aSize
{
	if((self = [super initWithSize:aSize]))
	{
		_stringImageRep = aRepresentation;
		[self addRepresentation:_stringImageRep];
	}
	return self;
}

- (instancetype)copyWithZone:(NSZone*)zone
{
	StringImage* copy = [super copyWithZone:zone];
	copy->_stringImageRep = (StringImageRep*)[[copy representations] firstObject];
	return copy;
}
@end
