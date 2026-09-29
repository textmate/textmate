#import "ns.h"
#import <OakFoundation/src/NSString Additions.h>
#import <oak/oak.h>
#import <oak/debug.h>
#import <text/src/utf8.h>

NSString* to_ns (std::string const& str)
{
	return [NSString stringWithCxxString:str];
}

std::string to_s (NSString* aString)
{
	if(!aString)
		return NULL_STR;

	CFRange range = CFRangeMake(0, CFStringGetLength((CFStringRef)aString));
	CFIndex byteCount;
	CFStringGetBytes((CFStringRef)aString, range, kCFStringEncodingUTF8, 0, false, NULL, 0, &byteCount);
	std::string res(byteCount, '\0');
	CFStringGetBytes((CFStringRef)aString, range, kCFStringEncodingUTF8, 0, false, (UInt8*)&res[0], byteCount, NULL);
	return res;
}

std::string to_s (NSAttributedString* anAttributedString)
{
	return to_s([anAttributedString string]);
}

std::string to_s (NSUUID* identifier)
{
	return to_s(identifier.UUIDString);
}

std::string to_s (NSData* someData)
{
	return someData ? std::string((char const*)[someData bytes], (char const*)[someData bytes] + [someData length]) : NULL_STR;
}

std::string to_s (NSError* anError)
{
	return text::format("Error Domain=%s Code=%ld “%s”", [[anError domain] UTF8String], [anError code], [[anError localizedDescription] UTF8String]);
}

std::string to_s (id someObject)
{
	if([someObject isKindOfClass:[NSString class]])
		return to_s((NSString*)someObject);
	if([someObject isKindOfClass:[NSAttributedString class]])
		return to_s((NSAttributedString*)someObject);
	if([someObject isKindOfClass:[NSData class]])
		return to_s((NSData*)someObject);
	if([someObject isKindOfClass:[NSError class]])
		return to_s((NSError*)someObject);
	if([someObject isKindOfClass:[NSEvent class]])
		return to_s((NSEvent*)someObject);
	return to_s([someObject description]);
}

static std::string string_for (CGKeyCode key, CGEventFlags flags)
{
	CGEventRef event = CGEventCreateKeyboardEvent(NULL, key, true);
	CGEventSetFlags(event, flags);
	std::string const tmp = to_s([[NSEvent eventWithCGEvent:event] characters]);

	std::string res = tmp;
	citerate(str, diacritics::make_range(tmp.data(), tmp.data() + tmp.size()))
		res = std::string(&str, &str + str.length());

	CFRelease(event);
	return res == "" ? NULL_STR : res;
}

static std::string string_for (CGEventFlags flags)
{
	static struct EventFlag_t { CGEventFlags flag; std::string symbol; } const EventFlags[] =
	{
		{ kCGEventFlagMaskNumericPad, "#" },
		{ kCGEventFlagMaskControl,    "^" },
		{ kCGEventFlagMaskAlternate,  "~" },
		{ kCGEventFlagMaskShift,      "$" },
		{ kCGEventFlagMaskCommand,    "@" }
	};

	std::string res = "";
	for(auto const& eventFlag : EventFlags)
		res += (flags & eventFlag.flag) ? eventFlag.symbol : "";
	return res;
}

static bool is_ascii (std::string const& str)
{
	char ch = str.size() == 1 ? str[0] : 0;
	return 0x20 < ch && ch < 0x7F;
}

static char char_for_key_code (CGKeyCode key, bool shift)
{
	static std::map<CGKeyCode, char> const RegularMap =
	{
		{ kVK_ANSI_1, '1' },
		{ kVK_ANSI_2, '2' },
		{ kVK_ANSI_3, '3' },
		{ kVK_ANSI_4, '4' },
		{ kVK_ANSI_5, '5' },
		{ kVK_ANSI_6, '6' },
		{ kVK_ANSI_7, '7' },
		{ kVK_ANSI_8, '8' },
		{ kVK_ANSI_9, '9' },
		{ kVK_ANSI_0, '0' },
		{ kVK_ANSI_A, 'a' },
		{ kVK_ANSI_B, 'b' },
		{ kVK_ANSI_C, 'c' },
		{ kVK_ANSI_D, 'd' },
		{ kVK_ANSI_E, 'e' },
		{ kVK_ANSI_F, 'f' },
		{ kVK_ANSI_G, 'g' },
		{ kVK_ANSI_H, 'h' },
		{ kVK_ANSI_I, 'i' },
		{ kVK_ANSI_J, 'j' },
		{ kVK_ANSI_K, 'k' },
		{ kVK_ANSI_L, 'l' },
		{ kVK_ANSI_M, 'm' },
		{ kVK_ANSI_N, 'n' },
		{ kVK_ANSI_O, 'o' },
		{ kVK_ANSI_P, 'p' },
		{ kVK_ANSI_Q, 'q' },
		{ kVK_ANSI_R, 'r' },
		{ kVK_ANSI_S, 's' },
		{ kVK_ANSI_T, 't' },
		{ kVK_ANSI_U, 'u' },
		{ kVK_ANSI_V, 'v' },
		{ kVK_ANSI_W, 'w' },
		{ kVK_ANSI_X, 'x' },
		{ kVK_ANSI_Y, 'y' },
		{ kVK_ANSI_Z, 'z' },

		{ kVK_ANSI_LeftBracket,  '['  },
		{ kVK_ANSI_RightBracket, ']'  },
		{ kVK_ANSI_Slash,        '/'  },
		{ kVK_ANSI_Backslash,    '\\' },
		{ kVK_ANSI_Comma,        ','  },
		{ kVK_ANSI_Period,       '.'  },
		{ kVK_ANSI_Minus,        '-'  },
		{ kVK_ANSI_Equal,        '='  },
		{ kVK_ANSI_Quote,        '\'' },
		{ kVK_ANSI_Grave,        '`'  },
		{ kVK_ANSI_Semicolon,    ';'  },
	};

	static std::map<CGKeyCode, char> const ShiftedMap =
	{
		{ kVK_ANSI_1, '!' },
		{ kVK_ANSI_2, '@' },
		{ kVK_ANSI_3, '#' },
		{ kVK_ANSI_4, '$' },
		{ kVK_ANSI_5, '%' },
		{ kVK_ANSI_6, '^' },
		{ kVK_ANSI_7, '&' },
		{ kVK_ANSI_8, '*' },
		{ kVK_ANSI_9, '(' },
		{ kVK_ANSI_0, ')' },
		{ kVK_ANSI_A, 'A' },
		{ kVK_ANSI_B, 'B' },
		{ kVK_ANSI_C, 'C' },
		{ kVK_ANSI_D, 'D' },
		{ kVK_ANSI_E, 'E' },
		{ kVK_ANSI_F, 'F' },
		{ kVK_ANSI_G, 'G' },
		{ kVK_ANSI_H, 'H' },
		{ kVK_ANSI_I, 'I' },
		{ kVK_ANSI_J, 'J' },
		{ kVK_ANSI_K, 'K' },
		{ kVK_ANSI_L, 'L' },
		{ kVK_ANSI_M, 'M' },
		{ kVK_ANSI_N, 'N' },
		{ kVK_ANSI_O, 'O' },
		{ kVK_ANSI_P, 'P' },
		{ kVK_ANSI_Q, 'Q' },
		{ kVK_ANSI_R, 'R' },
		{ kVK_ANSI_S, 'S' },
		{ kVK_ANSI_T, 'T' },
		{ kVK_ANSI_U, 'U' },
		{ kVK_ANSI_V, 'V' },
		{ kVK_ANSI_W, 'W' },
		{ kVK_ANSI_X, 'X' },
		{ kVK_ANSI_Y, 'Y' },
		{ kVK_ANSI_Z, 'Z' },

		{ kVK_ANSI_LeftBracket,  '{' },
		{ kVK_ANSI_RightBracket, '}' },
		{ kVK_ANSI_Slash,        '?' },
		{ kVK_ANSI_Backslash,    '|' },
		{ kVK_ANSI_Comma,        '<' },
		{ kVK_ANSI_Period,       '>' },
		{ kVK_ANSI_Minus,        '_' },
		{ kVK_ANSI_Equal,        '+' },
		{ kVK_ANSI_Quote,        '"' },
		{ kVK_ANSI_Grave,        '~' },
		{ kVK_ANSI_Semicolon,    ':' },
	};

	auto const& map = shift ? ShiftedMap : RegularMap;
	auto const it = map.find(key);
	return it != map.end() ? it->second : 0;
}

/*
	The “simple” heuristic is the following:

		Always treat ⌃ as literal modifier
		Remove numpad modifier unless key is among what is on standard numpad (incl. comma)
		if ⌘ changes key (Qwerty-Dvorak ⌘ hybrid):
			if ⌥ is down: treat ⌥ as literal
			if ⇧ is down and (changed) key is not a-z: treat ⇧ as literal else if key is a-z: upcase key string
		else if ⌃ is down and key string is non-ASCII
			ignore keymap and decode virtual key code to get ⌃ + «modifiers» + ASCII key
		else
			If ⌥ is down and character (with flags & ⌥⇧) is non-ASCII or if ⌥ doesn’t affect key string: treat ⌥ as literal
			if ⇧ is down and character (with flags & ⌥⇧) is non-ASCII or if ⇧ doesn’t affect key string, treat ⇧ as literal else if key is a-z: upcase key string
		end if
*/

std::string to_s (NSEvent* anEvent, bool preserveNumPadFlag)
{
	CGEventRef cgEvent = [anEvent CGEvent];
	CGKeyCode key      = (CGKeyCode)[anEvent keyCode];
	CGEventFlags flags = CGEventGetFlags(cgEvent);
	flags &= kCGEventFlagMaskCommand | kCGEventFlagMaskShift | kCGEventFlagMaskAlternate | kCGEventFlagMaskControl | kCGEventFlagMaskNumericPad;

	std::string keyString              = NULL_STR;
	std::string const keyStringNoFlags = string_for(key, 0);
	CGEventFlags newFlags              = flags & (kCGEventFlagMaskControl|kCGEventFlagMaskCommand);

	if(flags & kCGEventFlagMaskNumericPad)
	{
		static std::string const numPadKeys = "0123456789=/*-+.,";
		if(preserveNumPadFlag && numPadKeys.find(keyStringNoFlags) != std::string::npos)
			newFlags |= kCGEventFlagMaskNumericPad;
	}

	std::string const keyStringCommand = string_for(key, kCGEventFlagMaskCommand);
	if((flags & kCGEventFlagMaskCommand) && keyStringNoFlags != keyStringCommand)
	{
		newFlags |= flags & kCGEventFlagMaskAlternate;
		flags    &= ~kCGEventFlagMaskAlternate;

		if(flags & kCGEventFlagMaskShift)
		{
			if(keyStringCommand.size() == 1 && isalpha(keyStringCommand[0]))
			{
				keyString = std::string(1, toupper(keyStringCommand[0]));
			}
			else
			{
				newFlags |= kCGEventFlagMaskShift;
			}
		}
	}
	else
	{
		char ch;
		if((flags & kCGEventFlagMaskControl) && !is_ascii(keyStringNoFlags) && (ch = char_for_key_code(key, flags & kCGEventFlagMaskShift)))
		{
			keyString = std::string(1, ch);
			newFlags |= flags & kCGEventFlagMaskAlternate;
		}
		else
		{
			if(flags & kCGEventFlagMaskAlternate)
			{
				std::string const keyStringAlternate = string_for(key, flags & (kCGEventFlagMaskAlternate|kCGEventFlagMaskShift));
				if(!is_ascii(keyStringAlternate) || keyStringNoFlags == keyStringAlternate)
				{
					newFlags |= kCGEventFlagMaskAlternate;
					flags    &= ~kCGEventFlagMaskAlternate;
				}
			}

			if(flags & kCGEventFlagMaskShift)
			{
				std::string const keyStringShift = string_for(key, flags & (kCGEventFlagMaskAlternate|kCGEventFlagMaskShift));
				if(!is_ascii(keyStringShift) || keyStringNoFlags == keyStringShift)
				{
					newFlags |= kCGEventFlagMaskShift;
					flags    &= ~kCGEventFlagMaskShift;
				}
				else
				{
					keyString = keyStringShift;
				}
			}
		}
	}

	return string_for(newFlags) + (keyString == NULL_STR ? string_for(key, flags & ~kCGEventFlagMaskControl) : keyString);
}
