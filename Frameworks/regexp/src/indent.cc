#include "indent.h"
#include <text/ctype.h>
#include <text/format.h>
#include <oak/oak.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Indent);

namespace indent
{
	enum type_t
	{
		kIncrease     = 1 << 0,
		kDecrease     = 1 << 1,
		kIncreaseNext = 1 << 2,
		kIgnore       = 1 << 3,
	};
#ifndef NDEBUG
	static std::string to_s (type_t type)
	{
		std::vector<std::string> v;
		if(type & kIgnore)       v.push_back("kIgnore");
		if(type & kDecrease)     v.push_back("kDecrease");
		if(type & kIncrease)     v.push_back("kIncrease");
		if(type & kIncreaseNext) v.push_back("kIncreaseNext");
		return text::join(v, "|");
	}
#endif
	std::string create (size_t size, size_t tabSize, bool softTabs)
	{
		return softTabs ? std::string(size, ' ') : std::string(size / tabSize, '\t') + std::string(size % tabSize, ' ');
	}

	int leading_whitespace (char const* it, char const* last, size_t tabSize)
	{
		int res = 0;
		for(; it != last && text::is_space(*it); ++it)
			res += *it == '\t' ? tabSize - (res % tabSize) : 1;
		return res;
	}

	static size_t classify (std::string const& line, regexp::pattern_t const (&patterns)[4])
	{
		size_t res = 0;
		for(size_t i = 0; i < sizeofA(patterns); ++i)
		{
			if(search(patterns[i], line.data(), line.data() + line.size()))
				res |= 1 << i;
		}

		if(res & kIgnore)
			res = kIgnore;
		else if(res & kIncrease)
			res &= ~kIncreaseNext;

		D(DBF_Indent, bug("%s\n", to_s((type_t)res).c_str()););
		return res;
	}

	static bool is_blank (std::string const& line)                                           { return text::is_blank(line.data(), line.data() + line.size()); }
	static size_t leading_whitespace (std::string const& line, size_t tabSize)               { return leading_whitespace(line.data(), line.data() + line.size(), tabSize); }

	// =========
	// = fsm_t =
	// =========

	bool fsm_t::is_seeded (std::string const& line)
	{
		D(DBF_Indent, bug("%s\n", line.c_str()););

		bool res = true;
		size_t type = classify(line, _patterns);
		if(is_blank(line) || (type & kIgnore))
		{
			res = false;
		}
		else if(++_seen == 1)
		{
			_level       = leading_whitespace(line, _tab_size);
			_carry       = 0;
			_last_type   = type;
			_last_indent = _level;

			if(type & kIncrease)
				_level += _indent_size;
			if(type & kIncreaseNext)
				_carry += _indent_size;

			res = false;
		}
		else if((type & kIncreaseNext) && !(_last_type & (kIncrease | kDecrease)))
		{
			_level = leading_whitespace(line, _tab_size);
			if(_last_type & kIncreaseNext && _level < _last_indent)
				_carry += _last_indent - _level;
			_last_indent = _level;

			res = false;
		}

		D(DBF_Indent, bug("level %zd, carry %zd, is seeded %s\n", _level, _carry, BSTR(res)););
		return res;
	}

	bool fsm_t::is_ignored (std::string const& line) const
	{
		return is_blank(line) || (classify(line, _patterns) & kIgnore);
	}

	size_t fsm_t::scan_line (std::string const& line)
	{
		D(DBF_Indent, bug("%s\n", line.c_str()););
		int type = classify(line, _patterns);
		ssize_t res = _level + _carry;
		if(!(type & kIgnore))
		{
			if(type & (kIncrease | kDecrease))
				_carry = 0;
			if(type & kDecrease)
				_level -= _indent_size;

			res = _level + _carry;

			if(type & kIncrease)
				_level += _indent_size;
			_carry = type & kIncreaseNext ? _carry + _indent_size : 0;
		}
		D(DBF_Indent, bug("indent %zd (level %zd, carry %zd)\n", res < 0 ? 0 : res, _level, _carry););
		return res < 0 ? 0 : res;
	}

} /* indent */
