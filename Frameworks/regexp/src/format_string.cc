#include "format_string.h"
#include "parser.h"
#include "snippet.h"
#include "regexp.h"

#include <oak/oak.h>
#include <oak/compat.h>
#include <oak/server.h>
#include <text/case.h>
#include <text/utf8.h>

OAK_DEBUG_VAR(FormatString);

struct expand_visitor : boost::static_visitor<void>
{
	WATCH_LEAKS(expand_visitor);

	std::map<std::string, std::string> variables;
	snippet::run_command_callback_t* callback;
	std::string res;
	std::vector< std::pair<size_t, parser::case_change::type> > case_changes;

	size_t rank_count;
	std::map<size_t, snippet::field_ptr> fields;
	std::multimap<size_t, snippet::field_ptr> mirrors;
	std::multimap<size_t, snippet::field_ptr> ambiguous;

	expand_visitor (std::map<std::string, std::string> const& variables, snippet::run_command_callback_t* callback) : variables(variables), callback(callback)
	{
		rank_count = 0;
	}

	void traverse (parser::nodes_t const& nodes)
	{
		for(auto const& it : nodes)
			boost::apply_visitor(*this, it);
	}

	void handle_case_changes ()
	{
		case_changes.insert(case_changes.end(), std::make_pair(res.size(), parser::case_change::none));

		std::string tmp;

		char const* data = res.data();
		size_t prev = 0;
		parser::case_change::type style = parser::case_change::none;
		for(auto const& it : case_changes)
		{
			if(prev < it.first)
			{
				bool only_next = style == parser::case_change::upper_next || style == parser::case_change::lower_next;
				char const* to = only_next ? &(diacritics::begin_of(data + prev, data + it.first) + 1) : data + it.first;

				switch(style)
				{
					case parser::case_change::none:             std::copy(data + prev, to, back_inserter(tmp)); break;
					case parser::case_change::upper_next: tmp += text::uppercase(std::string(data + prev, to)); break;
					case parser::case_change::lower_next: tmp += text::lowercase(std::string(data + prev, to)); break;
					case parser::case_change::upper:      tmp += text::uppercase(std::string(data + prev, to)); break;
					case parser::case_change::lower:      tmp += text::lowercase(std::string(data + prev, to)); break;
				}
				std::copy(to, data + it.first, back_inserter(tmp));
			}

			prev = it.first;
			style = it.second;
		}
		tmp.swap(res);
	}

	void replace (std::string const& src, regexp::pattern_t const& ptrn, parser::nodes_t const& format, bool repeat)
	{
		char const* first = src.data();
		char const* last = src.data() + src.size();
		char const* it = first;
		while(regexp::match_t const& m = search(ptrn, first, last, it))
		{
			res.insert(res.end(), it, m.buffer() + m.begin());

			std::map<std::string, std::string> tmp(m.captures());
			tmp.insert(variables.begin(), variables.end());
			tmp.swap(variables);
			traverse(format);
			tmp.swap(variables);

			it = m.buffer() + m.end();
			if(!repeat)
				break;

			if(m.empty())
			{
				if(it == last)
					break;
				res += *it++;
			}
		}
		res.insert(res.end(), it, last);
	}

	std::map<std::string, std::string>::const_iterator variable (std::string const& name) const
	{
		std::map<std::string, std::string>::const_iterator it = variables.find(name);
		return it != variables.end() && it->second != NULL_STR ? it : variables.end();
	}

	void operator() (parser::variable_t const& v)
	{
		std::map<std::string, std::string>::const_iterator it = variable(v.name);
		if(it != variables.end())
			res += it->second;
	}

	void operator() (parser::variable_transform_t const& v)
	{
		expand_visitor tmp(variables, callback);
		tmp.traverse(v.pattern);
		tmp.handle_case_changes();
		auto ptrn = regexp::pattern_t(tmp.res, parser::convert(v.options));

		std::map<std::string, std::string>::const_iterator it = variable(v.name);
		replace(it != variables.end() ? it->second : "", ptrn, v.format, v.options & parser::regexp_options::g);
	}

	void operator() (parser::variable_fallback_t const& v)
	{
		std::map<std::string, std::string>::const_iterator it = variable(v.name);
		if(it != variables.end())
				res += it->second;
		else	traverse(v.fallback);
	}

	void operator() (parser::variable_condition_t const& v)
	{
		std::map<std::string, std::string>::const_iterator it = variable(v.name);
		traverse(it != variables.end() ? v.if_set : v.if_not_set);
	}

	static std::string capitalize (std::string const& src)
	{
		return format_string::replace(text::lowercase(src), "^(\\s+)?(\\w+)|\\b((?!(?:else|from|over|then|when)\\b)\\w{4,}|\\w+\\s*$)", "${1:?$1\\u$2:\\u$0}");
	}

	static std::string asciify (std::string const& src)
	{
		iconv_t cd = iconv_open("ASCII//TRANSLIT", "UTF-8");
		if(cd == (iconv_t)(-1))
			return src; // error

		char const* first = src.data();
		char const* last  = first + src.size();

		std::string buffer(256, ' ');
		size_t buffer_contains = 0;

		while(first != last)
		{
			if(buffer.size() - buffer_contains < 256)
				buffer.resize(buffer.size() * 2);

			char* dst      = &buffer[buffer_contains];
			size_t dstSize = buffer.size() - buffer_contains;
			size_t srcSize = last - first;

			size_t rc = iconv(cd, (char**)&first, &srcSize, &dst, &dstSize);
			if(rc == (size_t)(-1) && errno != E2BIG && (errno != EINVAL || buffer.size() - buffer_contains - dstSize == 0))
				return src; // error

			buffer_contains += buffer.size() - buffer_contains - dstSize;
		}

		iconv_close(cd);

		buffer.resize(buffer_contains);
		return buffer;
	}

	void operator() (parser::variable_change_t const& v)
	{
		std::map<std::string, std::string>::const_iterator it = variable(v.name);
		if(it != variables.end())
		{
			std::string value = it->second;
			if(v.change & parser::transform::kUpcase)
				value = text::uppercase(value);
			if(v.change & parser::transform::kDowncase)
				value = text::lowercase(value);
			if(v.change & parser::transform::kCapitalize)
				value = capitalize(value);
			if(v.change & parser::transform::kAsciify)
				value = asciify(value);
			res += value;
		}
	}

	void operator() (parser::case_change_t const& v)
	{
		case_changes.insert(case_changes.end(), std::make_pair(res.size(), v.type));
	}

	void operator() (parser::text_t const& v)
	{
		res += v.text;
	}

	// =================
	// = Snippet Stuff =
	// =================
	
	void operator() (parser::placeholder_t const& v)
	{
		snippet::pos_t from(res.size(), ++rank_count);
		if(fields.find(v.index) == fields.end())
			traverse(v.content);
		snippet::pos_t to(res.size(), rank_count += 2);
		auto field = std::make_shared<snippet::placeholder_t>(v.index, from, to);
		if(fields.find(v.index) != fields.end())
			mirrors.emplace(v.index, field);
		else if(v.content.empty())
			ambiguous.emplace(v.index, field);
		else
			fields.emplace(v.index, field);
	}

	void operator() (parser::placeholder_transform_t const& v)
	{
		snippet::pos_t pos(res.size(), ++rank_count);
		auto field = std::make_shared<snippet::transform_t>(v.index, pos, snippet::pos_t(res.size(), rank_count += 2), v.pattern, v.format, v.options & parser::regexp_options::g);
		mirrors.emplace(v.index, field);
	}

	void operator() (parser::placeholder_choice_t const& v)
	{
		if(fields.find(v.index) != fields.end())
			return;

		std::vector<std::string> all_choices;
		for(auto const& it : v.choices)
		{
			expand_visitor tmp(variables, callback);
			tmp.traverse(it);
			tmp.handle_case_changes();
			all_choices.push_back(tmp.res);
		}

		snippet::pos_t pos(res.size(), ++rank_count);
		res += all_choices[0];
		auto field = std::make_shared<snippet::choice_t>(v.index, pos, snippet::pos_t(res.size(), rank_count += 2), all_choices);
		fields.emplace(v.index, field);
	}

	void operator() (parser::code_t const& v)
	{
		if(callback)
		{
			std::string const& str = callback->run_command(v.code, variables);
			res.insert(res.end(), str.begin(), !str.empty() && str[str.size()-1] == '\n' ? --str.end() : str.end());
		}
	}
};

namespace format_string
{
	// ===================
	// = format_string_t =
	// ===================
	
	format_string_t::format_string_t (parser::nodes_t const& n)
	{
		nodes = std::make_shared<parser::nodes_t>(n);
	}

	void format_string_t::init (std::string const& str, char const* stopChars)
	{
		D(DBF_FormatString, bug("%s\n", str.c_str()););
		parser::nodes_t const& n = parser::parse_format_string(str, stopChars, &_length);
		nodes = std::make_shared<parser::nodes_t>(n);
	}
	
	std::string format_string_t::expand (std::map<std::string, std::string> const& variables) const
	{
		expand_visitor v(variables, NULL);
		v.traverse(*nodes);
		v.handle_case_changes();
		return v.res;
	}

	// =======
	// = API =
	// =======
	
	std::string replace (std::string const& src, regexp::pattern_t const& ptrn, format_string_t const& format, bool repeat, std::map<std::string, std::string> const& variables)
	{
		D(DBF_FormatString, bug("%s\n", src.c_str()););
		
		expand_visitor v(variables, NULL);
		v.replace(src, ptrn, *format.nodes, repeat);
		v.handle_case_changes();
		return v.res;
	}

	std::string expand (std::string const& format, std::map<std::string, std::string> const& variables)
	{
		return format_string_t(format).expand(variables);
	}

	std::string escape (std::string const& format)
	{
		std::string res = "";
		for(size_t i = 0; i < format.size(); ++i)
		{
			switch(format[i])
			{
				case '\t': res.append("\\t"); break;
				case '\r': res.append("\\r"); break;
				case '\n': res.append("\\n"); break;

				case '$': case '(': case '\\':
				{
					if(format[i] != '\\' || i+1 != format.size() && strchr("\\$(trn", format[i+1]))
						res.append("\\");
				}
				/* continue */

				default:
					res += format[i];
				break;
			}
		}
		return res;
	}

} /* format_string */

namespace snippet
{
	snippet_t parse (std::string const& str, std::map<std::string, std::string> const& variables, std::string const& indentString, text::indent_t const& indent, run_command_callback_t* callback)
	{
		expand_visitor v(variables, callback);
		v.traverse(parser::parse_snippet(str));
		v.handle_case_changes();

		for(auto const& pair : v.ambiguous)
		{
			if(v.fields.find(pair.first) == v.fields.end())
					v.fields.insert(pair);
			else	v.mirrors.insert(pair);
		}

		return snippet_t(v.res, v.fields, v.mirrors, variables, indentString, indent);
	}

} /* snippet */ 
