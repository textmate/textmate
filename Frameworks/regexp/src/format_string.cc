#include "format_string.h"
#include "parser.h"
#include "snippet.h"
#include "regexp.h"

#include <oak/oak.h>
#include <text/case.h>
#include <text/utf8.h>
#include <text/transcode.h>
#include <text/encode.h>
#include <text/format.h>
#include <cf/cf.h>

struct expand_visitor : boost::static_visitor<void>
{
	std::function<std::optional<std::string>(std::string const&)> variable;
	snippet::run_command_callback_t* callback;
	std::string res;
	std::vector< std::pair<size_t, parser::case_change::type> > case_changes;

	size_t rank_count;
	std::map<size_t, snippet::field_ptr> fields;
	std::multimap<size_t, snippet::field_ptr> mirrors;
	std::multimap<size_t, snippet::field_ptr> ambiguous;

	expand_visitor (std::function<std::optional<std::string>(std::string const&)> const& variable, snippet::run_command_callback_t* callback) : variable(variable), callback(callback)
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

			std::set<std::string> eclipsed;
			for(size_t i = 0; i < m.size(); ++i)
			{
				if(!m.did_match(i))
					eclipsed.insert(std::to_string(i));
			}

			auto getVariable = [&](std::string const& name) -> std::optional<std::string> {
				if(eclipsed.find(name) == eclipsed.end())
				{
					auto const captures = m.captures();
					auto const it = captures.find(name);
					return it != captures.end() ? it->second : this->variable(name);
				}
				return std::nullopt;
			};

			expand_visitor tmp(getVariable, callback);
			tmp.traverse(format);
			tmp.handle_case_changes();
			res += tmp.res;

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

	void operator() (parser::variable_t const& v)
	{
		if(auto value = variable(v.name))
			res += *value;
	}

	void operator() (parser::variable_transform_t const& v)
	{
		expand_visitor tmp(variable, callback);
		tmp.traverse(v.pattern);
		tmp.handle_case_changes();
		auto ptrn = regexp::pattern_t(tmp.res, parser::convert(v.options));

		replace(variable(v.name).value_or(std::string()), ptrn, v.format, v.options & parser::regexp_options::g);
	}

	void operator() (parser::variable_fallback_t const& v)
	{
		if(auto value = variable(v.name))
				res += *value;
		else	traverse(v.fallback);
	}

	void operator() (parser::variable_condition_t const& v)
	{
		traverse(variable(v.name).has_value() ? v.if_set : v.if_not_set);
	}

	static std::string capitalize (std::string const& src)
	{
		static regexp::pattern_t const words  = "\\A\\p{^Lower}+\\z|\\b\\p{Upper}\\p{^Upper}+?\\b";
		static regexp::pattern_t const upcase = "^([\\W\\d]*)(\\w[-\\w]*)|\\b((?!(?:else|from|over|then|when)\\b)\\w[-\\w]{3,}|\\w[-\\w]*[\\W\\d]*$)";
		return format_string::replace(format_string::replace(src, words, "${0:/downcase}"), upcase, "${1:?$1\\u$2:\\u$0}");
	}

	static std::string asciify (std::string const& org)
	{
		std::string src = org;
		if(CFMutableStringRef tmp = CFStringCreateMutableCopy(kCFAllocatorDefault, 0, cf::wrap(src)))
		{
			CFStringTransform(tmp, nullptr, kCFStringTransformStripDiacritics, false);
			CFStringTransform(tmp, nullptr, kCFStringTransformStripCombiningMarks, false);
			src = cf::to_s(tmp);
			CFRelease(tmp);
		}

		text::transcode_t transcode("UTF-8", "ASCII//TRANSLIT");
		if(!transcode)
			return src; // error

		std::string buffer;
		transcode(transcode(src.data(), src.data() + src.size(), back_inserter(buffer)));
		return buffer;
	}

	static std::string shell_escape (std::string const& src)
	{
		std::string const special   = "|&;<>()$`\\\" \t\n*?[#˜=%";
		std::string const separator = "'";

		std::string res;

		std::string::size_type bow = 0;
		while(true)
		{
			std::string::size_type eow   = src.find(separator, bow);
			std::string::size_type count = eow == std::string::npos ? eow : eow - bow;
			std::string const word = src.substr(bow, count);

			bool needQuotes = word.find_first_of(special) != std::string::npos;
			if(needQuotes)
				res += "'";
			res += word;
			if(needQuotes)
				res += "'";

			if(eow == std::string::npos)
				break;

			res += "\\'";
			bow = eow + separator.size();
		}
		return res;
	}

	static std::string relative_time (std::string const& src)
	{
		time_t now = time(nullptr);
		for(auto format : { "%F %T %z", "%F %T", "%F", "%T" })
		{
			struct tm bsdTime = *gmtime(&now);
			if(strptime(src.c_str(), format, &bsdTime))
			{
				static char const DateWithoutTZ[] = "YYYY-MM-DD HH:MM:SS";
				bool hasTZ = src.size() > sizeof(DateWithoutTZ);

				double const duration = round(difftime(now, hasTZ ? mktime(&bsdTime) : timegm(&bsdTime)));
				char* tmp = nullptr;

				if(duration < 0)             asprintf(&tmp, "in the future");
				else if(duration < 2)        asprintf(&tmp, "just now"); // TODO Should be ‘today’ when src has no time part
				else if(duration < 60)       asprintf(&tmp, "%.0f seconds ago", duration);
				else if(duration < 90)       asprintf(&tmp, "a minute ago");
				else if(duration < 3570)     asprintf(&tmp, "%.0f minutes ago", duration/60);
				else if(duration < 5400)     asprintf(&tmp, "an hour ago");
				else if(duration < 84600)    asprintf(&tmp, "%.0f hours ago", duration/(60*60));
				else if(duration < 129600)   asprintf(&tmp, "a day ago");
				else if(duration < 561600)   asprintf(&tmp, "%.0f days ago", duration/(24*60*60));
				else if(duration < 1036800)  asprintf(&tmp, "a week ago");
				else if(duration < 2419200)  asprintf(&tmp, "%.0f weeks ago", duration/(7*24*60*60));
				else if(duration < 3952800)  asprintf(&tmp, "a month ago");
				else if(duration < 30304800) asprintf(&tmp, "%.0f months ago", duration/(30.5*24*60*60));
				else if(duration < 47304000) asprintf(&tmp, "a year ago");
				else asprintf(&tmp, "%.0f years ago", duration/(365*24*60*60));

				std::string res = tmp;
				free(tmp);
				return res;
			}
		}
		return src;
	}

	static std::string format_number (std::string const& src)
	{
		return format_string::replace(src, "(\\d+)(\\.\\d+)?", "${1/\\d{1,3}(?=\\d{3}+(?!\\d))/$0,/g}${2}");
	}

	static std::string format_duration (std::string const& src)
	{
		try {
			size_t seconds = round(std::stod(src)); // This may throw an exception
			struct { char const* singular; char const* plural; size_t amount; bool include; } units[] = {
				{ "day",    "days",    (seconds / 60 / 60 / 24), true },
				{ "hour",   "hours",   (seconds / 60 / 60) % 24, true },
				{ "minute", "minutes", (seconds / 60) % 60,      true },
				{ "second", "seconds", (seconds) % 60,           seconds < 10 * 60 }, // Include seconds for durations up to 10 minutes
			};

			std::vector<std::string> v;
			for(auto const& unit : units)
			{
				if(unit.amount && unit.include)
					v.emplace_back(text::format("%zu %s", unit.amount, unit.amount == 1 ? unit.singular : unit.plural));
			}
			return text::join(v, ", ");
		}
		catch (...) {
		}
		return src;
	}

	void operator() (parser::variable_change_t const& v)
	{
		if(auto optionalValue = variable(v.name))
		{
			std::string value = *optionalValue;
			if(v.change & parser::transform::kUpcase)
				value = text::uppercase(value);
			if(v.change & parser::transform::kDowncase)
				value = text::lowercase(value);
			if(v.change & parser::transform::kCapitalize)
				value = capitalize(value);
			if(v.change & parser::transform::kAsciify)
				value = asciify(value);
			if(v.change & parser::transform::kUrlEncode)
				value = encode::url_part(value);
			if(v.change & parser::transform::kShellEscape)
				value = shell_escape(value);
			if(v.change & parser::transform::kRelative)
				value = relative_time(value);
			if(v.change & parser::transform::kNumber)
				value = format_number(value);
			if(v.change & parser::transform::kDuration)
				value = format_duration(value);

			if(v.change & parser::transform::kDirname)
			{
				char buf[MAXPATHLEN];
				value = dirname_r(value.c_str(), buf) ?: value;
			}

			if(v.change & parser::transform::kBasename)
			{
				char buf[MAXPATHLEN];
				value = basename_r(value.c_str(), buf) ?: value;
			}

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
			expand_visitor tmp(variable, callback);
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
			std::string const& str = callback->run_command(v.code);
			res.insert(res.end(), str.begin(), !str.empty() && str.back() == '\n' ? --str.end() : str.end());
		}
	}
};

namespace format_string
{
	// ===================
	// = format_string_t =
	// ===================

	format_string_t::format_string_t (parser::nodes_t const& n) : nodes(std::make_shared<parser::nodes_t>(n)) { }

	void format_string_t::init (std::string const& str, char const* stopChars)
	{
		parser::nodes_t const& n = parser::parse_format_string(str, stopChars, &_length);
		nodes = std::make_shared<parser::nodes_t>(n);
	}

	std::string format_string_t::expand (std::function<std::optional<std::string>(std::string const&)> const& getVariable) const
	{
		expand_visitor v(getVariable, nullptr);
		v.traverse(*nodes);
		v.handle_case_changes();
		return v.res;
	}

	// =======
	// = API =
	// =======

	std::string replace (std::string const& src, regexp::pattern_t const& ptrn, format_string_t const& format, bool repeat, std::map<std::string, std::string> const& variables)
	{
		auto getVariable = [&variables](std::string const& name) -> std::optional<std::string> {
			auto it = variables.find(name);
			return it != variables.end() ? it->second : std::optional<std::string>();
		};

		expand_visitor v(getVariable, nullptr);
		v.replace(src, ptrn, *format.nodes, repeat);
		v.handle_case_changes();
		return v.res;
	}

	std::string expand (std::string const& format, std::function<std::optional<std::string>(std::string const&)> const& getVariable)
	{
		if(format.find_first_of("$(\\") == std::string::npos)
			return format;
		return format_string_t(format).expand(getVariable);
	}

	std::string expand (std::string const& format, std::map<std::string, std::string> const& variables)
	{
		auto getVariable = [&variables](std::string const& name) -> std::optional<std::string> {
			auto it = variables.find(name);
			return it != variables.end() ? it->second : std::optional<std::string>();
		};
		return expand(format, getVariable);
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
		auto getVariable = [&variables](std::string const& name) -> std::optional<std::string> {
			auto it = variables.find(name);
			return it != variables.end() ? it->second : std::optional<std::string>();
		};

		expand_visitor v(getVariable, callback);
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
