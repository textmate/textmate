#include "ct.h"
#include "render.h"
#include <cf/cf.h>
#include <text/utf8.h>
#include <text/utf16.h>
#include <text/hexdump.h>
#include <crash/info.h>

namespace ng
{
	// =============
	// = context_t =
	// =============

	context_t::context_t (CGContextRef context, CGImageRef spellingDot, std::function<CGImageRef(double, double)> foldingDotsFactory) : _context(context), _spelling_dot(spellingDot), _folding_dots_create(foldingDotsFactory)
	{
		if(_spelling_dot)
			CFRetain(_spelling_dot);
	}

	context_t::~context_t ()
	{
		if(_spelling_dot)
			CFRelease(_spelling_dot);

		for(auto const& pair : _folding_dots_cache)
		{
			if(pair.second)
				CFRelease(pair.second);
		}
	}

	CGImageRef context_t::folding_dots (double width, double height) const
	{
		if(!_folding_dots_create)
			return NULL;

		auto size = std::make_pair(width, height);
		auto it = _folding_dots_cache.find(size);
		if(it == _folding_dots_cache.end())
			it = _folding_dots_cache.emplace(size, _folding_dots_create(width, height)).first;
		return it->second;
	}

} /* ng */

namespace ct
{
	// =============
	// = metrics_t =
	// =============

	static double read_double_from_defaults (CFStringRef key, double defaultValue)
	{
		if(CFPropertyListRef value = CFPreferencesCopyAppValue(key, kCFPreferencesCurrentApplication))
		{
			// TODO Should coarse strings to numbers.
			if(CFGetTypeID(value) == CFNumberGetTypeID())
				CFNumberGetValue((CFNumberRef)value, kCFNumberDoubleType, &defaultValue);
			CFRelease(value);
		}
		return defaultValue;
	}

	metrics_t::metrics_t (std::string const& fontName, CGFloat fontSize)
	{
		_ascent_delta  = read_double_from_defaults(CFSTR("fontAscentDelta"), 1);
		_leading_delta = read_double_from_defaults(CFSTR("fontLeadingDelta"), 1);

		if(CTFontRef font = CTFontCreateWithName(cf::wrap(fontName), fontSize, NULL))
		{
			_ascent     = CTFontGetAscent(font);
			_descent    = CTFontGetDescent(font);
			_leading    = CTFontGetLeading(font);
			_x_height   = CTFontGetXHeight(font);
			_cap_height = CTFontGetCapHeight(font);

			if(CFMutableAttributedStringRef str = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0))
			{
				CFAttributedStringReplaceString(str, CFRangeMake(0, 0), CFSTR("n"));
				CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTFontAttributeName, font);
				if(CTLineRef line = CTLineCreateWithAttributedString(str))
				{
					_column_width = CTLineGetTypographicBounds(line, NULL, NULL, NULL);
					CFRelease(line);
				}
				CFRelease(str);
			}
			CFRelease(font);
		}
	}

	CGFloat metrics_t::line_height (CGFloat minAscent, CGFloat minDescent, CGFloat minLeading) const
	{
		CGFloat ascent  = std::max(minAscent, _ascent) + _ascent_delta;
		CGFloat descent = std::max(minDescent, _descent);
		CGFloat leading = std::max(minLeading, _leading) + _leading_delta;
		return ceil(ascent + descent + leading);
	}

	// ==========
	// = line_t =
	// ==========

	line_t::line_t (std::string const& text, std::map<size_t, scope::scope_t> const& scopes, theme_ptr const& theme, CGFloat tabSize, ct::metrics_t const& metrics, CGColorRef textColor) : _text(text)
	{
		ASSERT(utf8::is_valid(text.begin(), text.end()));
		ASSERT(scopes.empty() || (--scopes.end())->first <= text.size());

		if(CFMutableAttributedStringRef toDraw = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0))
		{
			for(auto pair = scopes.begin(); pair != scopes.end(); )
			{
				styles_t const& styles = theme->styles_for_scope(pair->second);
				size_t i = pair->first;
				size_t j = ++pair != scopes.end() ? pair->first : text.size();

				if(j < i)
				{
					crash_reporter_info_t info(text::format("bad range: %zu-%zu (at end: %s)", i, j, BSTR(pair == scopes.end())));
					abort();
				}

				if(!utf8::is_valid(text.begin() + i, text.begin() + j))
				{
					crash_reporter_info_t info(text::format("text size: %zu, line is valid utf-8: %s, %zu scope(s): %zu-%zu", text.size(), BSTR(utf8::is_valid(text.begin(), text.end())), scopes.size(), scopes.empty() ? 0 : scopes.begin()->first, scopes.empty() ? 0 : (--scopes.end())->first));
					info << text::format("range %zu-%zu is not UTF-8:\n%s", i, j, text::to_hex(text.begin() + i, text.begin() + j).c_str());
					abort();
				}

				if(CFStringRef cfStr = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*)text.data() + i, j - i, kCFStringEncodingUTF8, false))
				{
					if(CFMutableAttributedStringRef str = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0))
					{
						CFAttributedStringReplaceString(str, CFRangeMake(0, 0), cfStr);
						CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTFontAttributeName, styles.font());
						CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTForegroundColorAttributeName, textColor ?: styles.foreground());
						if(styles.underlined())
							_underlines.push_back(std::make_pair(CFRangeMake(CFAttributedStringGetLength(toDraw), CFAttributedStringGetLength(str)), CGColorPtr(CGColorRetain(styles.foreground()), CGColorRelease)));
						_backgrounds.push_back(std::make_pair(CFRangeMake(CFAttributedStringGetLength(toDraw), CFAttributedStringGetLength(str)), CGColorPtr(CGColorRetain(styles.background()), CGColorRelease)));
						CFAttributedStringReplaceAttributedString(toDraw, CFRangeMake(CFAttributedStringGetLength(toDraw), 0), str);
						CFRelease(str);
					}
					CFRelease(cfStr);
				}
				else
				{
					fprintf(stderr, "%s: failed to create CFString for ‘%.*s’\n", getprogname(), int(j - i), text.data() + i);
				}
			}

			CTLineRef tmpLine = CTLineCreateWithAttributedString(toDraw);
			double tabWidth = tabSize * metrics.column_width();
			double standardTabWidths = 0;
			double newTabWidths = 0;
			size_t j = 0, i = 0;
			std::vector<CTTextTabRef> tabs;
			tabs.push_back(CTTextTabCreate(kCTNaturalTextAlignment, 0, NULL));
			citerate(ch, diacritics::make_range(text.data(), text.data() + text.size()))
			{
				switch(*ch)
				{
					case '\t':
						double x = CTLineGetOffsetForStringIndex(tmpLine, j, NULL);
						double newX = (x - standardTabWidths + newTabWidths);
						double stopLocation = (floor(newX / tabWidth)+1) * tabWidth;
						if (stopLocation - newX < metrics.column_width()*0.5)
							stopLocation += tabWidth;
						newTabWidths += stopLocation - newX;
						standardTabWidths += CTLineGetOffsetForStringIndex(tmpLine, j+1, NULL) - x;
						tabs.push_back(CTTextTabCreate(kCTNaturalTextAlignment, stopLocation, NULL));
						_tabLocations.push_back(i);
						break;
				}
				i += ch.length();
				++j;
			}

			CFArrayRef tabStops = CFArrayCreate(kCFAllocatorDefault, (const void**) (&tabs[0]), tabs.size(), &kCFTypeArrayCallBacks);
			for(CTTextTabRef t : tabs)
				CFRelease(t);

			CTParagraphStyleSetting settings[] = {
				{kCTParagraphStyleSpecifierTabStops, sizeof(CFArrayRef), &tabStops},
				{kCTParagraphStyleSpecifierDefaultTabInterval, sizeof(tabWidth), &tabWidth}
			};
			CTParagraphStyleRef paragraphStyle = CTParagraphStyleCreate(settings, 2);
			CFAttributedStringSetAttribute(toDraw, CFRangeMake(0, CFAttributedStringGetLength(toDraw)), kCTParagraphStyleAttributeName, paragraphStyle);
			CFRelease(paragraphStyle);
			CFRelease(tabStops);
			_line.reset(CTLineCreateWithAttributedString(toDraw), CFRelease);
			CFRelease(toDraw);
		}
	}

	CGFloat line_t::width (CGFloat* ascent, CGFloat* descent, CGFloat* leading) const
	{
		return _line ? CTLineGetTypographicBounds(_line.get(), ascent, descent, leading) : 0;
	}

	size_t line_t::index_for_offset (CGFloat offset) const
	{
		return _line ? utf16::advance(_text.data(), CTLineGetStringIndexForPosition(_line.get(), CGPointMake(offset, 0)), _text.data() + _text.size()) - _text.data() : 0;
	}

	CGFloat line_t::offset_for_index (size_t index) const
	{
		return _line ? CTLineGetOffsetForStringIndex(_line.get(), utf16::distance(_text.begin(), _text.begin() + index), NULL) : 0;
	}

	static void draw_spelling_dot (ng::context_t const& context, CGRect const& rect, bool isFlipped)
	{
		if(CGImageRef spellingDot = context.spelling_dot())
		{
			CGContextSaveGState(context);
			if(isFlipped)
				CGContextConcatCTM(context, CGAffineTransformMake(1, 0, 0, -1, 0, 2 * rect.origin.y + 3));
			for(CGFloat x = rect.origin.x; x < rect.origin.x + rect.size.width - 0.5; x += 4)
				CGContextDrawImage(context, CGRectMake(x, rect.origin.y, 4, 3), spellingDot);
			CGContextRestoreGState(context);
		}
	}

	void line_t::draw_invisible (std::vector<size_t> locations, CGPoint pos, std::string text, styles_t styles, ng::context_t const& context, bool isFlipped) const
	{
		CFMutableAttributedStringRef str = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
		CFAttributedStringReplaceString(str, CFRangeMake(0, 0), cf::wrap(text));
		CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTFontAttributeName, styles.font());
		CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTForegroundColorAttributeName, styles.foreground());
		CTLineRef line = CTLineCreateWithAttributedString(str);
		CFRelease(str);
		CGContextSaveGState(context);
		if(isFlipped)
			CGContextConcatCTM(context, CGAffineTransformMake(1, 0, 0, -1, 0, 2 * pos.y));

		for(auto const& location : locations)
		{
			if (location > 5000) break;
			CGFloat x1 = round(pos.x + offset_for_index(location));
			CGFloat x2 = round(pos.x + offset_for_index(location+1));
			CGFloat x = x2 < x1 ? x1 - CTLineGetTypographicBounds(line, NULL, NULL, NULL) : x1;
			CGContextSetTextPosition(context, x, pos.y);
			CTLineDraw(line, context);
		}
		CGContextRestoreGState(context);
		CFRelease(line);
	}

	void line_t::draw_foreground (CGPoint pos, ng::context_t const& context, bool isFlipped, std::vector< std::pair<size_t, size_t> > const& misspelled, ng::invisibles_t const& invisibles, theme_ptr const& theme) const
	{
		if(!_line)
			return;

		if(invisibles.enabled)
		{
			if(invisibles.tab != "")
				draw_invisible(_tabLocations, pos, invisibles.tab, theme->styles_for_scope("deco.invisible.tab"), context, isFlipped);
		}

		for(auto const& pair : _underlines) // Draw our own underline since CoreText does an awful job <rdar://5845224>
		{
			CGFloat x1 = round(pos.x + CTLineGetOffsetForStringIndex(_line.get(), pair.first.location, NULL));
			CGFloat x2 = round(pos.x + CTLineGetOffsetForStringIndex(_line.get(), pair.first.location + pair.first.length, NULL));
			render::fill_rect(context, pair.second.get(), CGRectMake(x1, pos.y + 1, x2 - x1, 1));
		}

		for(auto const& pair : misspelled)
		{
			CFIndex location = utf16::distance(_text.begin(),               _text.begin() + pair.first);
			CFIndex length   = utf16::distance(_text.begin() + pair.first, _text.begin() + pair.second);
			CGFloat x1 = round(pos.x + CTLineGetOffsetForStringIndex(_line.get(), location, NULL));
			CGFloat x2 = round(pos.x + CTLineGetOffsetForStringIndex(_line.get(), location + length, NULL));
			draw_spelling_dot(context, CGRectMake(x1, pos.y + 1, x2 - x1, 3), isFlipped);
		}

		CGContextSaveGState(context);
		if(isFlipped)
			CGContextConcatCTM(context, CGAffineTransformMake(1, 0, 0, -1, 0, 2 * pos.y));
		CGContextSetTextPosition(context, pos.x, pos.y);
		CTLineDraw(_line.get(), context);
		CGContextRestoreGState(context);
	}

	void line_t::draw_background (CGPoint pos, CGFloat height, ng::context_t const& context, bool isFlipped, CGColorRef currentBackground) const
	{
		if(!_line)
			return;

		for(auto const& pair : _backgrounds)
		{
			if(CFEqual(currentBackground, pair.second.get()))
				continue;

			CGFloat x1 = round(pos.x + CTLineGetOffsetForStringIndex(_line.get(), pair.first.location, NULL));
			CGFloat x2 = round(pos.x + CTLineGetOffsetForStringIndex(_line.get(), pair.first.location + pair.first.length, NULL));
			render::fill_rect(context, pair.second.get(), CGRectMake(x1, pos.y, x2 - x1, height));
		}
	}

} /* ct */
