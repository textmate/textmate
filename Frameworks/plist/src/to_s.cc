#include "plist.h"
#include <oak/oak.h>
#include <text/format.h>
#include <text/hexdump.h>
#include <text/utf8.h>

namespace
{
	struct is_composite : boost::static_visitor<bool>
	{
		bool operator() (bool flag) const                       { return false; }
		bool operator() (int32_t i) const                       { return false; }
		bool operator() (uint64_t i) const                      { return false; }
		bool operator() (std::string const& str) const          { return false; }
		bool operator() (std::vector<char> const& data) const   { return false; }
		bool operator() (oak::date_t const& date) const         { return false; }
		bool operator() (plist::array_t const& array) const     { return true;  }
		bool operator() (plist::dictionary_t const& dict) const { return true;  }
	};

	struct fits_single_line : boost::static_visitor<bool>
	{
		bool operator() (bool flag) const                     { return true; }
		bool operator() (int32_t i) const                     { return true; }
		bool operator() (uint64_t i) const                    { return true; }
		bool operator() (std::string const& str) const        { return true; }
		bool operator() (std::vector<char> const& data) const { return true; }
		bool operator() (oak::date_t const& date) const       { return true; }

		bool operator() (plist::array_t const& array) const
		{
			bool singleLineItems = true;
			bool compositeItems = false;
			for(auto const& it : array)
			{
				singleLineItems = singleLineItems && boost::apply_visitor(*this, it);
				compositeItems = compositeItems || boost::apply_visitor(is_composite(), it);
			}
			return singleLineItems && (array.size() <= 1 || !compositeItems);
		}

		bool operator() (plist::dictionary_t const& dict) const
		{
			bool res = dict.size() <= 1;
			for(auto const& it : dict)
				res = res && boost::apply_visitor(*this, it.second);
			return res;
		}
	};
}

static std::string double_quote_escape (std::string const& ch)
{
	uint32_t const val = utf8::to_ch(ch);
	switch(val)
	{
		case '\0': return "\\000";
		case '\t': return "\\t"  ;
		case '\n': return "\\n"  ;
		case '\f': return "\\f"  ;
		case '\r': return "\\r"  ;
		case '\e': return "\\033";
		case '\\': return "\\\\" ;
		case '"':  return "\\\"" ;

		default:
		{
			static CFCharacterSetRef const illegal_set = CFCharacterSetGetPredefined(kCFCharacterSetIllegal);
			static CFCharacterSetRef const control_set = CFCharacterSetGetPredefined(kCFCharacterSetControl);

			if(0x20 <= val && val <= 0x7E)
				return ch;
			else if(iscntrl(val) || 0x7F < val && CFCharacterSetIsLongCharacterMember(control_set, val))
				return text::format("\\x%02X", val);
			else if(0xE000 <= val && val <= 0xF8FF)
				return text::format("\\U%04X", val);
			else if(0x0F0000 <= val && val <= 0x0FFFFD || 0x100000 <= val && val <= 0x10FFFD)
				return text::format("\\U%06X", val);
			else if(CFCharacterSetIsLongCharacterMember(illegal_set, val))
				return text::format("\\U%04X", val);
		}
	};

	return ch;
}

static std::string single_quote_escape (std::string const& ch)
{
	return ch == "'" ? "''" : ch;
}

static std::string escape (std::string const& str, std::string (*escapeFunction) (std::string const& ch), std::string const& quoteChar = "")
{
	std::string escaped("");
	citerate(it, diacritics::make_range(str.data(), str.data() + str.size()))
		escaped.append(escapeFunction(std::string(&it, &it + it.length())));
	return quoteChar + escaped + quoteChar;
}

static std::string pretty_string (std::string const& str, int flags)
{
	if((flags & plist::kPreferSingleQuotedStrings) == plist::kPreferSingleQuotedStrings && (str.find('\'') == std::string::npos || str != escape(str, &double_quote_escape)))
			return escape(str, &single_quote_escape, "'");
	else	return escape(str, &double_quote_escape, "\"");
}

static std::string pretty_data (std::vector<char> const& data)
{
	std::string res = "<";
	size_t i = 3;
	for(auto const& byte : data)
	{
		if(++i != 4 && i % 4 == 0)
			res.push_back(' ');
		text::int_to_hex(byte, back_inserter(res), 2);
	}
	return res + ">";
}

static std::string pretty_key (std::string const& key, int flags)
{
	bool should_quote = false;
	bool all_digits = true;
	bool first_char = true;
	citerate(it, diacritics::make_range(key.data(), key.data() + key.size()))
	{
		uint32_t const val = utf8::to_ch(std::string(&it, &it + it.length()));
		if(!isdigit(val))
			all_digits = false;

		bool local_should_quote = true;
		struct range_t { char first; char last; } const ranges[] = { { 'a', 'z' }, { 'A', 'Z' }, { '_', '_' } };
		for(size_t i = 0; i < sizeofA(ranges); ++i)
		{
			if(ranges[i].first <= val && val <= ranges[i].last)
				local_should_quote = false;
		}

		// Also allow "".-" when not the first char
		if(!first_char && (val == '.' || val == '-'))
			local_should_quote = false;
		first_char = false;

		should_quote = should_quote || local_should_quote;
	}
	return should_quote && !all_digits ? pretty_string(key, flags) : key;
}

namespace
{
	struct key_less_than_t
	{
		key_less_than_t (std::vector<std::string> const& order)
		{
			for(size_t i = 0; i < order.size(); ++i)
				_key_ranks.emplace(order[i], i);
		}

		bool operator() (std::pair<std::string, plist::any_t> const& lhs, std::pair<std::string, plist::any_t> const& rhs) const
		{
			auto lhsIter = _key_ranks.find(lhs.first);
			auto rhsIter = _key_ranks.find(rhs.first);
			if(lhsIter != _key_ranks.end() && rhsIter != _key_ranks.end())
				return lhsIter->second < rhsIter->second;
			else if(lhsIter != _key_ranks.end())
				return true;
			else if(rhsIter != _key_ranks.end())
				return false;
			else if(is_numeric(lhs.first) && is_numeric(rhs.first))
				return std::stol(lhs.first) < std::stol(rhs.first);
			else
				return lhs.first < rhs.first;
		}

	private:
		static bool is_numeric (std::string const& str)
		{
			return str.find_first_not_of("0123456789") == std::string::npos;
		}

		std::map<std::string, size_t> _key_ranks;
	};

	struct pretty : boost::static_visitor<std::string>
	{
		pretty (int flags, key_less_than_t const& keyCompare, size_t indent = 0, bool single_line = true, bool is_key = false) : flags(flags), key_compare(keyCompare), indent(indent), single_line(single_line), is_key(is_key) { }

		int flags;
		key_less_than_t const& key_compare;
		size_t indent;
		bool single_line;
		bool is_key;

		std::string indent_string () const                           { return std::string(indent, '\t'); }
		std::string operator() (bool flag) const                     { return flag ? ":true" : ":false"; }
		std::string operator() (int32_t i) const                     { return std::to_string(i); }
		std::string operator() (uint64_t i) const                    { return std::to_string(i); }
		std::string operator() (std::string const& str) const        { return pretty_string(str, flags); }
		std::string operator() (std::vector<char> const& data) const { return pretty_data(data); }
		std::string operator() (oak::date_t const& date) const       { return "@" + to_s(date); }

		std::string operator() (plist::array_t const& array) const
		{
			std::string res = "";
			if(array.empty())
			{
				res = " ";
			}
			else if(fits_single_line()(array))
			{
				size_t wrap = 0;
				for(auto const& it : array)
				{
					if(!res.empty())
						res += ", ";

					if(res.size() - wrap > 80)
					{
						res[res.size()-1] = '\n';
						res += indent_string() + '\t';
						wrap = res.size();
					}

					res += boost::apply_visitor(pretty(flags, key_compare), it);
				}

				res = " " + res + " ";
				
				if(res.find('\n') != std::string::npos)
				{
					res[res.size()-1] = '\n';
					res += indent_string();
				}
			}
			else
			{
				for(auto const& it : array)
				{
					if(!res.empty())
						res += "\n";
					res += indent_string() + '\t';
					res += boost::apply_visitor(pretty(flags, key_compare, indent+1, false), it);
					res += ",";
				}
				res = "\n" + res + "\n" + indent_string();
			}
			return "(" + res + ")";
		}

		std::string operator() (plist::dictionary_t const& dict) const
		{
			std::string res = "";
			if(dict.empty())
			{
				res = " ";
			}
			else if(fits_single_line()(dict))
			{
				std::string const& key = pretty_key(dict.begin()->first, flags);
				std::string const& value = boost::apply_visitor(pretty(flags, key_compare), dict.begin()->second);
				res = text::format("%c%s = %s; ", single_line || is_key ? ' ' : '\t', key.c_str(), value.c_str());
			}
			else
			{
				std::vector<std::pair<std::string, plist::any_t>> values(dict.begin(), dict.end());
				std::sort(values.begin(), values.end(), key_compare);

				for(auto const& it : values)
				{
					if(!res.empty())
						res += indent_string();
					std::string const& key = pretty_key(it.first, flags);
					std::string const& value = boost::apply_visitor(pretty(flags, key_compare, indent+1, false, true), it.second);
					res += text::format("\t%s = %s;\n", key.c_str(), value.c_str());
				}
				res = (is_key ? "\n" + indent_string() : "") + res + indent_string();
			}
			return "{" + res + "}";
		}
	};
}

namespace boost
{
	std::string to_s (plist::any_t const& plist, int flags, std::vector<std::string> const& keySortOrder)
	{
		return boost::apply_visitor(pretty(flags, key_less_than_t(keySortOrder)), plist);
	}

} /* boost */
