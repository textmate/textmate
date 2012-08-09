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

	if(MenuRef menu = _NSGetCarbonMenu([self menu]))
	{
		MenuItemIndex itemIndex = [[self menu] indexOfItem:self] + 1;

		ChangeMenuItemAttributes(menu, itemIndex, kMenuItemAttrCustomDraw, 0);
		std::string const& tabTrigger(aTabTrigger + "⇥");
		size_t len = tabTrigger.size();
		SetMenuItemProperty(menu, itemIndex, 'TxMt', 'TbLn', sizeof(size_t), &len);
		SetMenuItemProperty(menu, itemIndex, 'TxMt', 'TbTr', tabTrigger.size(), tabTrigger.data());
	}
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
