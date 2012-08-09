#import "ns.h"
#import <oak/oak.h>
#import <oak/debug.h>
#import <text/utf8.h>

OAK_DEBUG_VAR(NSEvent);

std::string to_s (NSString* aString)
{
	return aString ? [aString UTF8String] : NULL_STR;
}

std::string to_s (NSData* someData)
{
	return someData ? std::string((char const*)[someData bytes], (char const*)[someData bytes] + [someData length]) : NULL_STR;
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
	for(size_t i = 0; i < sizeofA(EventFlags); ++i)
		res += (flags & EventFlags[i].flag) ? EventFlags[i].symbol : "";
	return res;
}

static bool is_ascii (std::string const& str)
{
	char ch = str.size() == 1 ? str[0] : 0;
	return 0x20 < ch && ch < 0x7F;
}

/*
	The “simple” heuristic is the following:

		Always treat ⌃ as literal modifier
		Remove numpad modifier unless key is among what is on standard numpad (incl. comma)
		if ⌘ changes key (Qwerty-Dvorak ⌘ hybrid):
			if ⌥ is down: treat ⌥ as literal
			if ⇧ is down and (changed) key is not a-z: treat ⇧ as literal else if key is a-z: upcase key string
		else
			If ⌥ is down and character (with flags & ⌥⇧) is non-ASCII or if ⌥ doesn’t affect key string: treat ⌥ as literal
			if ⇧ is down and character (with flags & ⌥⇧) is non-ASCII or if ⇧ doesn’t affect key string, treat ⇧ as literal else if key is a-z: upcase key string
		end if
*/

std::string to_s (NSEvent* anEvent)
{
	CGEventRef cgEvent = [anEvent CGEvent];
	CGKeyCode key      = (CGKeyCode)[anEvent keyCode];
	CGEventFlags flags = CGEventGetFlags(cgEvent);
	flags &= kCGEventFlagMaskCommand | kCGEventFlagMaskShift | kCGEventFlagMaskAlternate | kCGEventFlagMaskControl | kCGEventFlagMaskNumericPad;

	std::string keyString              = NULL_STR;
	std::string const keyStringNoFlags = string_for(key, 0);
	CGEventFlags newFlags              = flags & (kCGEventFlagMaskControl|kCGEventFlagMaskCommand);
	flags &= ~kCGEventFlagMaskControl;

	if(flags & kCGEventFlagMaskNumericPad)
	{
		static std::string const numPadKeys = "0123456789=/*-+.,";
		if(numPadKeys.find(keyStringNoFlags) != std::string::npos)
			newFlags |= kCGEventFlagMaskNumericPad;
		flags &= ~kCGEventFlagMaskControl;
	}

	std::string const keyStringCommand = string_for(key, kCGEventFlagMaskCommand);
	if((flags & kCGEventFlagMaskCommand) && keyStringNoFlags != keyStringCommand)
	{
		D(DBF_NSEvent, bug("command (⌘) changes key\n"););

		newFlags |= flags & kCGEventFlagMaskAlternate;
		flags    &= ~kCGEventFlagMaskAlternate;

		if(flags & kCGEventFlagMaskShift)
		{
			if(keyStringCommand.size() == 1 && isalpha(keyStringCommand[0]))
			{
				D(DBF_NSEvent, bug("manually upcase key\n"););
				keyString = std::string(1, toupper(keyStringCommand[0]));
			}
			else
			{
				D(DBF_NSEvent, bug("shift (⇧) is literal\n"););
				newFlags |= kCGEventFlagMaskShift;
			}
		}
	}
	else
	{
		if(flags & kCGEventFlagMaskAlternate)
		{
			std::string const keyStringAlternate = string_for(key, flags & (kCGEventFlagMaskAlternate|kCGEventFlagMaskShift));
			if(!is_ascii(keyStringAlternate) || keyStringNoFlags == keyStringAlternate)
			{
				D(DBF_NSEvent, bug("option (⌥) is literal\n"););
				newFlags |= kCGEventFlagMaskAlternate;
				flags    &= ~kCGEventFlagMaskAlternate;
			}
		}

		if(flags & kCGEventFlagMaskShift)
		{
			std::string const keyStringShift = string_for(key, flags & (kCGEventFlagMaskAlternate|kCGEventFlagMaskShift));
			if(!is_ascii(keyStringShift) || keyStringNoFlags == keyStringShift)
			{
				D(DBF_NSEvent, bug("shift (⇧) is literal\n"););
				newFlags |= kCGEventFlagMaskShift;
				flags    &= ~kCGEventFlagMaskShift;
			}
			else
			{
				D(DBF_NSEvent, bug("use NSEvent’s uppercased version\n"););
				keyString = keyStringShift;
			}
		}
	}

	return string_for(newFlags) + (keyString == NULL_STR ? string_for(key, flags) : keyString);
}
