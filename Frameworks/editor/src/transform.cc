#include "transform.h"
#include "indent.h"
#include <regexp/format_string.h>
#include <oak/oak.h>
#include <oak/server.h>
#include <text/case.h>
#include <text/ctype.h>
#include <text/parse.h>
#include <text/utf8.h>
#include <cf/cf.h>

static size_t count_columns (std::string const& str, size_t tabSize)
{
	size_t col = 0;
	citerate(ch, diacritics::make_range(str.data(), str.data() + str.size()))
		col += (*ch == '\t' ? tabSize - (col % tabSize) : (text::is_east_asian_width(*ch) ? 2 : 1));
	return col;
}

static std::string justify_line (std::string const& str, size_t width, size_t tabSize)
{
	size_t currentWidth = count_columns(str, tabSize);
	if(width <= currentWidth)
		return str;

	size_t numberOfSpaces = std::count(str.begin(), str.end(), ' ');
	size_t extraSpaces    = width - currentWidth;
	size_t spaceCount     = 0;

	std::string res = "";
	iterate(ch, str)
	{
		res.insert(res.end(), 1, *ch);
		if(*ch == ' ')
		{
			size_t from = (extraSpaces * spaceCount + numberOfSpaces/2) / numberOfSpaces;
			size_t to = (extraSpaces * (spaceCount+1) + numberOfSpaces/2) / numberOfSpaces;
			res.insert(res.end(), to-from, ' ');
			++spaceCount;
		}
	}
	return res;
}

namespace transform
{
	std::string null (std::string const& src)
	{
		return "";
	}

	std::string unwrap (std::string src)
	{
		src = format_string::replace(src, "[\\s&&[^\n\xA0]]+\n", "\n");
		src = format_string::replace(src, "(\\A)?\n(\n+)?(\\z)?", "${1:?:${3:?\n:${2:?\n\n: }}}");
		src = format_string::replace(src, "(\\A\\s+)|[\\s&&[^\n\xA0]]+(\\z)?", "${1:?$1:${2:?\n: }}");
		return src;
	}

	std::string capitalize (std::string const& src)
	{
		return format_string::replace(src, "(?m).+", "${0:/capitalize}");
	}

	std::string transpose (std::string const& src)
	{
		std::vector< std::pair<char const*, char const*> > v = text::to_lines(src.data(), src.data() + src.size());
		bool hasNewline = !src.empty() && src[src.size()-1] == '\n';

		std::string res("");
		if(v.size() == 1 || (v.size() == 2 && v.back().first == v.back().second))
		{
			std::deque<char> tmp;
			citerate(it, diacritics::make_range(src.data(), src.data() + src.size() - (hasNewline ? 1 : 0)))
				tmp.insert(tmp.begin(), &it, &it + it.length());

			std::copy(tmp.begin(), tmp.end(), back_inserter(res));
			if(hasNewline)
				res += '\n';
		}
		else
		{
			riterate(it, v)
				res += std::string(it->first, it->second);

			if(!hasNewline)
			{
				res.insert(v.back().second - v.back().first, "\n");
				res.resize(res.size()-1);
			}
		}
		return res;
	}

	// this is copy/paste from string_ranker.cc
	static std::string decompose_string (std::string const& src)
	{
		CFMutableStringRef tmp = CFStringCreateMutableCopy(kCFAllocatorDefault, 0, cf::wrap(src));
		CFStringNormalize(tmp, kCFStringNormalizationFormD);
		std::string const& res = cf::to_s(tmp);
		CFRelease(tmp);
		return res;
	}

	std::string decompose (std::string src)
	{
		src = decompose_string(src);
		return src.empty() ? src : std::string(src.begin(), &--utf8::make(src.end()));
	}

	std::string shift::operator() (std::string const& src) const
	{
		std::string res;
		citerate(it, text::to_lines(src.data(), src.data() + src.size()))
		{
			char const* from = it->first;
			char const* to   = it->second;

			if(!text::is_blank(from, to))
			{
				if(amount > 0)
				{
					res += indent::create(amount, indent.tab_size(), indent.soft_tabs());
				}
				else if(amount < 0)
				{
					for(int col = 0; from != to && col < -amount && text::is_space(*from); ++from)
						col += *from == '\t' ? indent.tab_size() - (col % indent.tab_size()) : 1;
				}
			}

			std::copy(from, to, back_inserter(res));
		}
		return res;
	}

	static std::string fill_string (std::string const& src)
	{
		if(regexp::match_t const& m = regexp::search("\\A( *([*o•·-]) (?=\\S)|\\s{2,})", src.data(), src.data() + src.size()))
			return format_string::replace(src.substr(0, m.end()), "\\S", " ");
		return "";
	}

	static size_t length_excl_whitespace (std::string const& buffer, size_t bol, size_t eol)
	{
		size_t len = eol - bol;
		while(len > 0 && strchr(" \n", buffer[bol + len - 1]))
			--len;
		return len;
	}

	std::string reformat::operator() (std::string const& src) const
	{
		std::string res;
		size_t from = 0;
		std::string const unwrapped = unwrap(src);
		std::string const fillStr = fill_string(unwrapped);
		citerate(offset, text::soft_breaks(unwrapped, wrap, tabSize, fillStr.size()))
		{
			res += unwrapped.substr(from, length_excl_whitespace(unwrapped, from, *offset));
			res += "\n";
			if(*offset != unwrapped.size())
				res += fillStr;
			from = *offset;
		}
		res += unwrapped.substr(from, length_excl_whitespace(unwrapped, from, unwrapped.size()));
		return newline ? res + "\n" : res;
	}

	std::string justify::operator() (std::string const& src) const
	{
		std::string res;
		std::string const unwrapped = unwrap(src);
		citerate(it, text::to_lines(unwrapped.data(), unwrapped.data() + unwrapped.size()))
		{
			size_t from = 0;
			std::string const str = std::string(it->first, it->second);
			citerate(offset, text::soft_breaks(str, wrap, tabSize))
			{
				res += justify_line(unwrapped.substr(from, length_excl_whitespace(str, from, *offset)), wrap, tabSize);
				res += "\n";
				from = *offset;
			}
			res += str.substr(from, length_excl_whitespace(str, from, str.size()));
		}
		return newline ? res + "\n" : res;
	}

	std::string replace::operator() (std::string const& src) const
	{
		return text;
	}

	std::string surround::operator() (std::string const& src) const
	{
		return prefix + src + suffix;
	}

} /* transform */
