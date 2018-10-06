//  Created by Chris Thomas on 2/6/05.
//  Copyright 2005-2007 Chris Thomas. All rights reserved.
//  MIT license.
//
#import "CWStatusStringTransformer.h"

#define RGB8ComponentTransform(component) ((component) == 0 ? 0.0 : 1.0/(255.0/(component)))

#define OneShotNSColorFromTriplet(accessorName,r,g,b) \
static inline NSColor* accessorName(void)\
{\
	static NSColor* color = nil;\
	if(color == nil)\
		color = [NSColor colorWithDeviceRed:RGB8ComponentTransform(r) green:RGB8ComponentTransform(g) blue:RGB8ComponentTransform(b) alpha:1.0];\
	return color;\
}

OneShotNSColorFromTriplet(ForeColorForFileAdded, 0x00, 0xAA, 0x00)
OneShotNSColorFromTriplet(BackColorForFileAdded, 0xBB, 0xFF, 0xB3)

OneShotNSColorFromTriplet(ForeColorForFileModified, 0xEB, 0x64, 0x00)
OneShotNSColorFromTriplet(BackColorForFileModified, 0xF7, 0xE1, 0xAD)

OneShotNSColorFromTriplet(ForeColorForFileDeleted, 0xFF, 0x00, 0x00)
OneShotNSColorFromTriplet(BackColorForFileDeleted, 0xF5, 0xBD, 0xBD)

OneShotNSColorFromTriplet(ForeColorForFileConflict, 0x00, 0x80, 0x80)
OneShotNSColorFromTriplet(BackColorForFileConflict, 0xA3, 0xCE, 0xD0)

OneShotNSColorFromTriplet(ForeColorForFileIgnore, 0x80, 0x00, 0x80)
OneShotNSColorFromTriplet(BackColorForFileIgnore, 0xED, 0xAE, 0xF5)

OneShotNSColorFromTriplet(ForeColorForExternal, 0xFF, 0xFF, 0xFF)
OneShotNSColorFromTriplet(BackColorForExternal, 0x00, 0x00, 0x00)

static inline void ColorsFromStatus(NSString* status, NSColor** foreColor, NSColor** backColor )
{
	if([status isEqualToString:@"M"] || [status isEqualToString:@"G"])
	{
		*foreColor = ForeColorForFileModified();
		*backColor = BackColorForFileModified();
	}
	else if([status isEqualToString:@"X"])
	{
		*foreColor = ForeColorForExternal();
		*backColor = BackColorForExternal();
	}
	else if([status isEqualToString:@"A"])
	{
		*foreColor = ForeColorForFileAdded();
		*backColor = BackColorForFileAdded();
	}
	else if([status isEqualToString:@"D"] || [status isEqualToString:@"R"])
	{
		*foreColor = ForeColorForFileDeleted();
		*backColor = BackColorForFileDeleted();
	}
	else if([status isEqualToString:@"C"] || [status isEqualToString:@"?"])
	{
		*foreColor = ForeColorForFileConflict();
		*backColor = BackColorForFileConflict();
	}
	else if([status isEqualToString:@"I"])
	{
		*foreColor = ForeColorForFileIgnore();
		*backColor = BackColorForFileIgnore();
	}
	else
	{
		*foreColor = [NSColor controlTextColor];
		*backColor = [NSColor controlBackgroundColor];
	}
}

static NSAttributedString* attributedStatusString (NSString* aString)
{
	NSUInteger length                                 = [aString length];
	NSMutableAttributedString* attributedStatusString = [[NSMutableAttributedString alloc] init];
	NSAttributedString* spaceString                   = [[NSAttributedString alloc] initWithString:@" " attributes:nil];

	unichar emSpace   = 0x2003;
	unichar hairSpace = 0x200A;

	for(NSUInteger i = 0; i < length; i++)
	{
		unichar character = [aString characterAtIndex:i];

		NSString* charString;
		NSMutableAttributedString* attributedCharString;
		NSColor* foreColor;
		NSColor* backColor;

		// We pass in underscores for empty multicolumn attributes
		if(character == '_')
			character = emSpace;

		charString = [NSString stringWithCharacters:&character length:1];

		ColorsFromStatus(charString, &foreColor, &backColor);

		NSDictionary* attributes = @{ NSForegroundColorAttributeName: foreColor, NSBackgroundColorAttributeName: backColor };

		attributedCharString = [[NSMutableAttributedString alloc] initWithString:[NSString stringWithFormat:@"%C%@%C", hairSpace, charString, hairSpace] attributes:attributes];

		CGFloat width = [attributedCharString size].width;
		CGFloat desiredWidth = 13.0f;
		if(width < desiredWidth)
		{
			CGFloat hairSpaceWidth = [[[NSAttributedString alloc] initWithString:[NSString stringWithFormat:@"%C", hairSpace] attributes:attributes] size].width;
			CGFloat extraWidth = 0.5f * (desiredWidth - width) + hairSpaceWidth;
			CGFloat scale = logf(extraWidth - (hairSpaceWidth - 1.0f));

			NSMutableDictionary* dict = [NSMutableDictionary dictionary];
			[dict setObject:[NSNumber numberWithFloat:scale] forKey:NSExpansionAttributeName];
			[attributedCharString addAttributes:dict range:NSMakeRange(0, 1)];
			[attributedCharString addAttributes:dict range:NSMakeRange(2, 1)];
		}

		[attributedStatusString appendAttributedString:attributedCharString];
		[attributedStatusString appendAttributedString:spaceString];
	}

	return attributedStatusString;
}

@implementation CWStatusStringTransformer
+ (void)register                        { [NSValueTransformer setValueTransformer:[CWStatusStringTransformer new] forName:@"CWStatusStringTransformer"]; }
+ (Class)transformedValueClass          { return [NSAttributedString class]; }
+ (BOOL)allowsReverseTransformation     { return YES; }
- (id)transformedValue:(id)value        { return attributedStatusString(value); }
- (id)reverseTransformedValue:(id)value { return [value string]; }
@end
