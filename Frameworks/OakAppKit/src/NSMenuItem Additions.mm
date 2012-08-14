#import "NSMenu Additions.h"
#import "OakFileIconImage.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <text/case.h>
#import <text/utf8.h>

extern "C" MenuRef _NSGetCarbonMenu (NSMenu* aMenu);

static void set_legacy_key_equivalent (MenuRef aMenu, UInt16 anIndex, std::string keyStr, NSUInteger nsModifiers)
{
	std::string const uppercaseKeyStr = text::uppercase(keyStr);
	if(keyStr != text::lowercase(keyStr))
		nsModifiers |= NSShiftKeyMask;
	if(keyStr != uppercaseKeyStr)
		keyStr = uppercaseKeyStr;

	UInt8 modifiers = kMenuNoCommandModifier;
	if(nsModifiers & NSShiftKeyMask)     modifiers |= kMenuShiftModifier;
	if(nsModifiers & NSControlKeyMask)   modifiers |= kMenuControlModifier;
	if(nsModifiers & NSAlternateKeyMask) modifiers |= kMenuOptionModifier;
	if(nsModifiers & NSCommandKeyMask)   modifiers &= ~kMenuNoCommandModifier;

	uint32_t keyCode = utf8::to_ch(keyStr);
	if(keyCode == NSDeleteFunctionKey)
	{
		SetMenuItemKeyGlyph(aMenu, anIndex, kMenuDeleteRightGlyph);
	}
	else if(keyCode == NSDeleteCharacter)
	{
		SetMenuItemKeyGlyph(aMenu, anIndex, kMenuDeleteLeftGlyph);
	}
	else
	{
		uint16_t code = 0;
		if(nsModifiers & NSNumericPadKeyMask)
		{
			switch(keyCode)
			{
				case '0': code = 82; break;
				case '1': code = 83; break;
				case '2': code = 84; break;
				case '3': code = 85; break;
				case '4': code = 86; break;
				case '5': code = 87; break;
				case '6': code = 88; break;
				case '7': code = 89; break;
				case '8': code = 91; break;
				case '9': code = 92; break;
				case '=': code = 81; break;
				case '/': code = 75; break;
				case '*': code = 67; break;
				case '+': code = 69; break;
				case '-': code = 78; break;
				case ',': code = 65; break;
				case '.': code = 65; break;
				case NSEnterCharacter: code = 76; break; // could also use kMenuEnterGlyph
			}
		}

		if(code)
		{
			SetMenuItemCommandKey(aMenu, anIndex, true, code);
		}
		else
		{
			if(keyCode > 0x7F)
			{
				NSString* key = [NSString stringWithCxxString:keyStr];
				if([key canBeConvertedToEncoding:NSMacOSRomanStringEncoding])
				{
					if(NSData* data = [key dataUsingEncoding:NSMacOSRomanStringEncoding])
					{
						if([data length] == 1)
							keyCode = *(char const*)[data bytes];
					}
				}
			}

			SetMenuItemCommandKey(aMenu, anIndex, false, keyCode);
		}
	}
	SetMenuItemModifiers(aMenu, anIndex, modifiers);
}

@interface MenuMutableAttributedString : NSMutableAttributedString
{
	NSMutableAttributedString* contents;
	CGSize size;
}
- (void)appendTableCellWithString:(NSString*)string table:(NSTextTable*)table textAlignment:(NSTextAlignment)textAlignment verticalAlignment:(NSTextBlockVerticalAlignment)verticalAlignment font:(NSFont*)font row:(int)row column:(int)column;
- (CGSize)size;
@end

@implementation MenuMutableAttributedString

// Methods to override in subclass

- (id)init
{
	return [self initWithAttributedString:nil];
}

- (id)initWithAttributedString:(NSAttributedString*)attributedString
{
	if(self = [super init])
		contents = attributedString ? [attributedString mutableCopy] : [[NSMutableAttributedString alloc] init];
	return self;
}

- (NSString*)string
{
	return [contents string];
}

- (NSDictionary*)attributesAtIndex:(NSUInteger)location effectiveRange:(NSRange*)range
{
	return [contents attributesAtIndex:location effectiveRange:range];
}

- (void)replaceCharactersInRange:(NSRange)range withString:(NSString*)string
{
	[contents replaceCharactersInRange:range withString:string];
}

- (void)setAttributes:(NSDictionary*)attributes range:(NSRange)range
{
	[contents setAttributes:attributes range:range];
}

- (id)copyWithZone:(NSZone*)zone
{
	MenuMutableAttributedString* copy = [MenuMutableAttributedString allocWithZone:zone];
	copy->contents = [contents copyWithZone:zone];
	copy->size = size;
	return copy;
}

- (void)dealloc
{
	[contents release];
	[super dealloc];
}

// NOTE: AppKit additions produce invalid values here, provide our own implementation

- (NSRect)boundingRectWithSize:(NSSize)aSize options:(NSStringDrawingOptions)options
{
	return NSMakeRect(0, 0, size.width, size.height);
}

// Helper method for adding table cell into the attributed string

- (void)appendTableCellWithString:(NSString*)string table:(NSTextTable*)table textAlignment:(NSTextAlignment)textAlignment verticalAlignment:(NSTextBlockVerticalAlignment)verticalAlignment font:(NSFont*)font row:(int)row column:(int)column;
{
	CGSize stringSize = [string sizeWithAttributes:[NSDictionary dictionaryWithObject:font forKey:NSFontAttributeName]];

	NSTextTableBlock* block = [[[NSTextTableBlock alloc] initWithTable:table startingRow:row rowSpan:1 startingColumn:column columnSpan:1] autorelease];

	if(column > 0)
		[block setContentWidth:stringSize.width type:NSTextBlockAbsoluteValueType];

	block.verticalAlignment = verticalAlignment;

	NSMutableParagraphStyle* paragraphStyle = [[[NSMutableParagraphStyle alloc] init] autorelease];
	[paragraphStyle setTextBlocks:[NSArray arrayWithObjects:block, nil]];
	[paragraphStyle setAlignment:textAlignment];

	string = [string stringByAppendingString:@"\n"];

	NSMutableAttributedString* cellString = [[[NSMutableAttributedString alloc] initWithString:string] autorelease];
	[cellString addAttribute:NSParagraphStyleAttributeName value:paragraphStyle range:NSMakeRange(0, [cellString length])];
	[cellString addAttribute:NSFontAttributeName value:font range:NSMakeRange(0, [cellString length])];

	size.width += stringSize.width;
	if(size.height < stringSize.height)
		size.height = stringSize.height;

	[self appendAttributedString:cellString];
}

- (CGSize)size
{
	return size;
}

@end

@implementation NSMenuItem (FileIcon)
- (void)setIconForFile:(NSString*)path;
{
	NSImage* icon = nil;
	if([[NSFileManager defaultManager] fileExistsAtPath:path])
		icon = [OakFileIconImage fileIconImageWithPath:path size:NSMakeSize(16, 16)];
	else if(NSNotEmptyString([path pathExtension]))
		icon = [[NSWorkspace sharedWorkspace] iconForFileType:[path pathExtension]];
	else
		icon = [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(kUnknownFSObjectIcon)];

	if(icon)
	{
		[icon setSize:NSMakeSize(16, 16)];
		[self setImage:icon];
	}
}

- (void)setKeyEquivalentCxxString:(std::string const&)aKeyEquivalent
{
	if(aKeyEquivalent == NULL_STR || aKeyEquivalent.empty())
	{
		[self setKeyEquivalent:@""];
		[self setKeyEquivalentModifierMask:0];
		return;
	}

	NSUInteger modifiers = 0;

	size_t i = 0;
	while(true)
	{
		if(i+1 >= aKeyEquivalent.size() || !strchr("$^~@#", aKeyEquivalent[i]))
			break;

		switch(aKeyEquivalent[i++])
		{
			case '$': modifiers |= NSShiftKeyMask;      break;
			case '^': modifiers |= NSControlKeyMask;    break;
			case '~': modifiers |= NSAlternateKeyMask;  break;
			case '@': modifiers |= NSCommandKeyMask;    break;
			case '#': modifiers |= NSNumericPadKeyMask; break;
		}
	}

	if(MenuRef menu = _NSGetCarbonMenu([self menu]))
	{
		set_legacy_key_equivalent(menu, [[self menu] indexOfItem:self] + 1, aKeyEquivalent.substr(i), modifiers);
	}
	else
	{
		[self setKeyEquivalent:[NSString stringWithCxxString:aKeyEquivalent.substr(i)]];
		[self setKeyEquivalentModifierMask:modifiers];
	}
}

- (void)setTabTriggerCxxString:(std::string const&)aTabTrigger
{
	if(aTabTrigger == NULL_STR)
		return;

	MenuMutableAttributedString* attributedTitle = [[[MenuMutableAttributedString alloc] init] autorelease];
	NSTextTable* table = [[[NSTextTable alloc] init] autorelease];
	[table setNumberOfColumns:2];

	NSFont* font = self.menu.font ?: [NSFont menuFontOfSize:0];
	[attributedTitle appendTableCellWithString:self.title table:table textAlignment:NSLeftTextAlignment verticalAlignment:NSTextBlockMiddleAlignment font:font row:0 column:0];
	[attributedTitle appendTableCellWithString:[NSString stringWithCxxString:(" "+aTabTrigger+"\u21E5")] table:table textAlignment:NSRightTextAlignment
		verticalAlignment:font.pointSize >= 13 ? NSTextBlockBottomAlignment : NSTextBlockMiddleAlignment
		font:[NSFont menuBarFontOfSize:floor(font.pointSize * 0.85)] row:0 column:1];
	self.attributedTitle = attributedTitle;
}

- (void)setModifiedState:(BOOL)flag
{
	if(MenuRef menu = _NSGetCarbonMenu([self menu]))
	{
		MenuItemIndex itemIndex = [[self menu] indexOfItem:self] + 1;
		SetItemMark(menu, itemIndex, flag ? 0xA5 : noMark);
	}
}
@end
