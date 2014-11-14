#import "event.h"
#import "ns.h"
#import <text/utf8.h>
#import <text/case.h>
#import <text/format.h>
#import <OakFoundation/NSString Additions.h>

static std::string glyph_named (std::string const& name)
{
	static struct { std::string name; std::string glyph; } const KeyGlyphs[] =
	{
		{ "pb_enter",     "⌤" },

		{ "left",         "←" },
		{ "up",           "↑" },
		{ "right",        "→" },
		{ "down",         "↓" },

		{ "ib_left",      "⇠" },
		{ "ib_up",        "⇡" },
		{ "ib_right",     "⇢" },
		{ "ib_down",      "⇣" },

		{ "home",         "↖" },
		{ "end",          "↘" },
		{ "return",       "↩" },
		{ "pageup",       "⇞" },
		{ "pagedown",     "⇟" },
		{ "tab",          "⇥" },
		{ "backtab",      "⇤" },
		{ "shift",        "⇧" },
		{ "control",      "⌃" },
		{ "enter",        "⌅" },
		{ "command",      "⌘" },
		{ "modifier",     "⌥" },
		{ "backspace",    "⌫" },
		{ "delete",       "⌦" },
		{ "escape",       "⎋" },
		{ "numlock",      "⌧" },
		{ "help",         "?⃝" },

		{ "space",        "␣" }
	};

	for(size_t i = 0; i != sizeofA(KeyGlyphs); ++i)
	{
		if(name == KeyGlyphs[i].name)
			return KeyGlyphs[i].glyph;
	}

	return "�";
}

static std::string glyphs_for_key (std::string const& key, bool numpad = false)
{
	if(key == "")
		return key;

	static struct { unsigned short code; std::string name; } const Keys[] =
	{
		{ NSUpArrowFunctionKey,          "up"        },
		{ NSDownArrowFunctionKey,        "down"      },
		{ NSLeftArrowFunctionKey,        "left"      },
		{ NSRightArrowFunctionKey,       "right"     },
		{ NSDeleteFunctionKey,           "delete"    },
		{ NSHomeFunctionKey,             "home"      },
		{ NSEndFunctionKey,              "end"       },
		{ NSPageUpFunctionKey,           "pageup"    },
		{ NSPageDownFunctionKey,         "pagedown"  },
		{ NSClearLineFunctionKey,        "numlock",  },
		{ NSHelpFunctionKey,             "help",     },
		{ NSTabCharacter,                "tab"       },
		{ NSCarriageReturnCharacter,     "return"    },
		{ NSEnterCharacter,              "enter"     },
		{ NSBackTabCharacter,            "backtab"   },
		{ '\033',                        "escape"    },
		{ NSDeleteCharacter,             "backspace" },
		{ ' ',                           "space"     },
	};

	std::string res = key;

	bool didMatch = false;
	uint32_t code = utf8::to_ch(key);
	for(size_t i = 0; i < sizeofA(Keys) && !didMatch; ++i)
	{
		if(didMatch = (code == Keys[i].code))
			res = glyph_named(Keys[i].name);
	}

	if(code == 0xA0)
		res = "nbsp";
	else if(NSF1FunctionKey <= code && code <= NSF35FunctionKey)
		res = text::format("F%d", code - NSF1FunctionKey + 1);

	if(numpad)
		res += "\u20E3"; // COMBINING ENCLOSING KEYCAP

	return res;
}

static std::string string_for (NSUInteger flags)
{
	static struct EventFlag_t { NSUInteger flag; std::string symbol; } const EventFlags[] =
	{
		{ NSNumericPadKeyMask, "#" },
		{ NSControlKeyMask,    "^" },
		{ NSAlternateKeyMask,  "~" },
		{ NSShiftKeyMask,      "$" },
		{ NSCommandKeyMask,    "@" },
	};

	std::string res = "";
	for(size_t i = 0; i < sizeofA(EventFlags); ++i)
		res += (flags & EventFlags[i].flag) ? EventFlags[i].symbol : "";
	return res;
}

static NSUInteger ns_flag_for_char (uint32_t ch)
{
	switch(ch)
	{
		case '$': return NSShiftKeyMask;
		case '^': return NSControlKeyMask;
		case '~': return NSAlternateKeyMask;
		case '@': return NSCommandKeyMask;
		case '#': return NSNumericPadKeyMask;
	}
	return 0;
}

static void parse_event_string (std::string const& eventString, std::string& key, NSUInteger& flags, bool legacy = false)
{
	flags = 0;
	if(legacy)
	{
		key = "";
		bool scanningFlags = true, real = true;
		foreach(ch, utf8::make(eventString.data()), utf8::make(eventString.data() + eventString.size()))
		{
			if(scanningFlags = scanningFlags && ns_flag_for_char(*ch) != 0)
				flags |= ns_flag_for_char(*ch);
			else if(real = (!real || *ch != '\\'))
				key.append(&ch, ch.length());
		}
	}
	else
	{
		std::string::size_type i = eventString.find_first_not_of("$^~@#");
		if(i == std::string::npos)
			i = eventString.empty() ? 0 : eventString.size() - 1;

		foreach(ch, eventString.data(), eventString.data() + i)
			flags |= ns_flag_for_char(*ch);
		key = eventString.substr(i);
	}
}

namespace ns
{
	std::string create_event_string (NSString* key, NSUInteger flags)
	{
		return string_for(flags) + to_s(key);
	}

	std::string normalize_event_string (std::string const& eventString, size_t* startOfKey)
	{
		std::string key; NSUInteger flags;
		parse_event_string(eventString, key, flags, true);

		if(utf8::to_ch(key) == NSBackspaceCharacter)
			key = NSDeleteCharacter;
		else if(utf8::to_ch(key) == NSNewlineCharacter)
			key = NSCarriageReturnCharacter;

		std::string modifierString = key.empty() ? "" : string_for(flags);
		if(startOfKey)
			*startOfKey = modifierString.size();
		return modifierString + key;
	}

	std::string glyphs_for_flags (NSUInteger flags)
	{
		std::string res = "";
		if(flags & NSControlKeyMask)
			res += glyph_named("control");
		if(flags & NSAlternateKeyMask)
			res += glyph_named("modifier");
		if(flags & NSShiftKeyMask)
			res += glyph_named("shift");
		if(flags & NSCommandKeyMask)
			res += glyph_named("command");
		return res;
	}

	std::string glyphs_for_event_string (std::string const& eventString, size_t* startOfKey)
	{
		std::string key; NSUInteger flags;
		parse_event_string(eventString, key, flags);

		if((flags & NSShiftKeyMask) == 0)
		{
			std::string const upCased = text::uppercase(key);
			if(key != text::lowercase(key))
				flags |= NSShiftKeyMask;
			else if(key != upCased)
				key = upCased;
		}

		std::string modifierString = glyphs_for_flags(flags);
		if(startOfKey)
			*startOfKey = modifierString.size();

		return modifierString + glyphs_for_key(key, flags & NSNumericPadKeyMask);
	}

} /* ns */

NSAttributedString* OakAttributedStringForEventString (NSString* eventString, NSFont* font)
{
	static NSSet* const FunctionKeys = [NSSet setWithArray:@[ @"F1", @"F2", @"F3", @"F4", @"F5", @"F6",@"F7", @"F8", @"F9", @"F10", @"F11", @"F12", @"F13", @"F14", @"F15" ]];

	size_t keyStartsAt = 0;
	std::string const glyphString = ns::glyphs_for_event_string(to_s(eventString), &keyStartsAt);
	NSString* flags = [NSString stringWithCxxString:glyphString.substr(0, keyStartsAt)];
	NSString* key   = [NSString stringWithCxxString:glyphString.substr(keyStartsAt)];

	NSDictionary* style = @{ NSFontAttributeName : font };
	NSMutableAttributedString* str = [[NSMutableAttributedString alloc] initWithString:[NSString stringWithFormat:@"%@%@\t", flags, key] attributes:style];

	NSRange flagsRange = NSMakeRange(0, [flags length]);
	NSRange keyRange   = NSMakeRange(NSMaxRange(flagsRange), [key length]);

	if(NSGlyphInfo* glyphInfo = [FunctionKeys containsObject:key] ? [NSGlyphInfo glyphInfoWithGlyphName:key forFont:font baseString:key] : nil)
		[str addAttributes:@{ NSGlyphInfoAttributeName : glyphInfo } range:keyRange];

	CGFloat flagsWidth = ceil(std::max<CGFloat>([[str attributedSubstringFromRange:flagsRange] size].width, 1));
	CGFloat keyWidth   = ceil(std::max<CGFloat>([[str attributedSubstringFromRange:keyRange] size].width, [@"⌫" sizeWithAttributes:style].width));

	NSMutableParagraphStyle* pStyle = [NSMutableParagraphStyle new];
	[pStyle setAlignment:NSRightTextAlignment];
	[pStyle setTabStops:@[ [[NSTextTab alloc] initWithType:NSLeftTabStopType location:flagsWidth + keyWidth] ]];
	[str addAttributes:@{ NSParagraphStyleAttributeName : pStyle } range:NSMakeRange(0, [str length])];

	return str;
}
