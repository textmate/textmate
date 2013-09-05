#include "meta_data.h"
#include <regexp/format_string.h>
#include <bundles/bundles.h>
#include <oak/oak.h>
#include <text/ctype.h>
#include <oak/duration.h>

namespace
{
	struct transform_t
	{
		transform_t (std::string const& src) : src(src)
		{
			char const* it = src.data();
			char const* last = it + src.size();
			while(it != last)
			{
				if(text::is_space(*it) || *it == '\n')
				{
					++it;
				}
				else if(parse_char(it, last, '#'))
				{
					while(!parse_char(it, last, '\n') && it != last)
						++it;
				}
				else if(parse_char(it, last, 's') && parse_char(it, last, '/'))
				{
					std::string regexp;
					while(it != last && *it != '/')
					{
						if(*it == '\\' && it + 1 != last)
							regexp += *it++;
						regexp += *it++;
					}

					if(!parse_char(it, last, '/'))
					{
						fprintf(stderr, "malformed symbol transformation at offset %td (expected ‘/’): %s\n", it - src.data(), src.c_str());
						return;
					}

					format_string::format_string_t format(std::string(it, last), "/");
					if(format.length() == 0)
					{
						fprintf(stderr, "malformed symbol transformation at offset %td (expected /format string/): %s\n", it - src.data(), src.c_str());
						return;
					}

					it += format.length();

					std::string options;
					while(it != last && 'a' <= *it && *it <= 'z')
						options += *it++;

					parse_char(it, last, ';'); // semi-colon is optional, so we do not treat it as an error

					records.push_back((record_t){ regexp::pattern_t(regexp, options), format, options.find('g') != std::string::npos });
				}
				else
				{
					fprintf(stderr, "malformed symbol transformation at offset %td (expected ‘s’, ‘#’, or space, found %c (0x%02x)): %s\n", it - src.data(), *it, *it, src.c_str());
					return;
				}
			}
		}

		std::string expand (std::string const& str) const
		{
			static regexp::pattern_t newline("\n");
			std::string res = replace(str, newline, format_string::format_string_t(" "));
			iterate(it, records)
				res = replace(res, it->regexp, it->format, it->repeat);
			res = replace(res, newline, format_string::format_string_t("↵"));

			return res;
		}
	private:
		static bool parse_char (char const*& it, char const* last, char ch)
		{
			return it != last && *it == ch ? (++it, true) : false;
		}

		struct record_t
		{
			regexp::pattern_t regexp;
			format_string::format_string_t format;
			bool repeat;
		};

		std::string src;
		std::vector<record_t> records;
	};
}

namespace ng
{
	void symbols_t::replace (buffer_t* buffer, size_t from, size_t to, std::string const& str) { _symbols.replace(from, to, str.size()); }

	void symbols_t::did_parse (buffer_t const* buffer, size_t from, size_t to)
	{
		_symbols.remove(_symbols.lower_bound(from), _symbols.lower_bound(to));

		std::set<scope::scope_t> all_scopes;
		foreach(it, buffer->_scopes.lower_bound(from), buffer->_scopes.lower_bound(to))
			all_scopes.insert(all_scopes.end(), it->second);

		std::map<scope::scope_t, transform_t> transforms;
		iterate(it, all_scopes)
		{
			if(plist::is_true(bundles::value_for_setting("showInSymbolList", *it)))
			{
				plist::any_t const& symbolTransformationValue = bundles::value_for_setting("symbolTransformation", *it);
				std::string const* symbolTransformation = boost::get<std::string>(&symbolTransformationValue);
				transforms.emplace(*it, transform_t(symbolTransformation ? *symbolTransformation : ""));
			}
		}

		size_t beginOfSymbol = 0;
		bool inSymbol = false;
		transform_t* transform = NULL;
		foreach(it, buffer->_scopes.lower_bound(from), buffer->_scopes.lower_bound(to))
		{
			std::map<scope::scope_t, transform_t>::iterator transformIt = transforms.find(it->second);
			if(transformIt != transforms.end())
			{
				if(!inSymbol)
					beginOfSymbol = it->first;
				transform = &transformIt->second;
				inSymbol  = true;
			}
			else if(inSymbol)
			{
				_symbols.set(beginOfSymbol, transform->expand(buffer->substr(beginOfSymbol, it->first)));
				inSymbol = false;
			}
		}

		if(inSymbol)
			_symbols.set(beginOfSymbol, transform->expand(buffer->substr(beginOfSymbol, to)));
	}

	std::map<size_t, std::string> symbols_t::symbols (buffer_t const* buffer) const
	{
		std::map<size_t, std::string> res;
		iterate(it, _symbols)
			res.insert(*it);
		return res;
	}

	std::string symbols_t::symbol_at (buffer_t const* buffer, size_t i) const
	{
		tree_t::iterator it = _symbols.upper_bound(i);
		if(it == _symbols.begin())
			return NULL_STR;
		return (--it)->second;
	}

} /* ng */
