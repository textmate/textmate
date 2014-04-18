#include "parse_glob.h"
#include "parser_base.h"
#include <text/utf8.h>

// \«char»         -- Literal «char»
// ?               -- Match one character
// *               -- Match zero or more characters¹
// **              -- Match zero or more path components
// {«a»,«b»,«c»}   -- Match «a» or «b» or «c»
// [«a»«b»«c»]     -- Match an «a», «b» or «c» character
// [«a»-«b»]       -- Match one character in the range «a»-«b»
// [^«a»-«b»]      -- Match one character not in the range «a»-«b»

namespace
{
	struct node_t
	{
		enum type { kText, kCharClass, kOptional, kRoot, kGroup, kOr, kAny, kAnyRecursive, kExclude };

		node_t (type t, node_t* left = nullptr, node_t* right = nullptr) : _type(t), _left(left), _right(right) { }
		node_t (type t, std::string const& text, node_t* left = nullptr, node_t* right = nullptr) : _type(t), _text(text), _left(left), _right(right) { }

		~node_t ()
		{
			delete _left;
			delete _right;
		}

		std::string excludes () const
		{
			if(!this || _type != kExclude)
				return { };

			std::string const left  = _left->to_regexp(true);
			std::string const right = _right->excludes();
			return right.empty() ? left : right + "|" + left;
		}

		std::string to_regexp (bool matchDotFiles) const
		{
			if(!this)
				return { };

			std::string res;
			switch(_type)
			{
				case kText:
				{
					for(auto const& ch : _text)
					{
						if(strchr("[](){}\\|.?*+^$", ch))
							res += '\\';
						res += ch;
					}
				}
				break;

				case kCharClass:    res = "[" + _text + "]";                                                break;
				case kOptional:     res = ".";                                                              break;
				case kGroup:        res = "(?:" + _left->to_regexp(matchDotFiles) + ")";                    break;
				case kOr:           res = (_left ? _left->to_regexp(matchDotFiles) + "|" : std::string());  break;
				case kAny:          res = matchDotFiles ? "[^/]*" : "(?:(?!\\.)|(?<!^|/))[^/]*";            break;
				case kAnyRecursive: res = matchDotFiles ? "(?:.*" + _text + ")?" : "(?:(?:(?!\\.)|(?<!^|/))[^/]*(?:/(?!\\.)[^/]*)*" + _text + ")?"; break;
			}

			return res + (_right ? _right->to_regexp(matchDotFiles) : std::string());
		}

		type _type;
		std::string _text;
		node_t* _left;
		node_t* _right;
	};

	struct parse_glob_t : parser_base_t
	{
		parse_glob_t (std::string const& str) : parser_base_t(str) { }
		node_t* parse (char const* stopChars = "", bool enableExcludes = true);

	private:
		bool parse_escape ();
		bool parse_optional ();
		bool parse_any_recursive ();
		bool parse_any ();
		bool parse_brace_expansion ();
		bool parse_character_class ();
		bool parse_exclude ();
		bool parse_text ();

		bool add_node (node_t* node)
		{
			*_last = node;
			_last = &node->_right;
			return true;
		}

		node_t* _root;
		node_t** _last;
	};

	node_t* parse_glob_t::parse (char const* stopChars, bool enableExcludes)
	{
		_root = nullptr;
		_last = &_root;

		while(it != last && !strchr(stopChars, *it))
		{
			if(false
				|| parse_escape()
				|| parse_optional()
				|| parse_any_recursive()
				|| parse_any()
				|| parse_brace_expansion()
				|| parse_character_class()
				|| enableExcludes && parse_exclude()
				|| parse_text())
				continue;

			delete _root;
			_root = nullptr;
		}

		return std::exchange(_root, nullptr);
	}

	bool parse_glob_t::parse_escape ()
	{
		char const* backtrack = it;
		if(parse_char("\\") && it != last)
		{
			if(!strchr("trn", *it))
				return parse_text();

			switch(*it++)
			{
				case 't': return add_node(new node_t(node_t::kText, "\t"));
				case 'r': return add_node(new node_t(node_t::kText, "\r"));
				case 'n': return add_node(new node_t(node_t::kText, "\n"));
			}
		}
		return it = backtrack, false;
	}

	bool parse_glob_t::parse_optional ()
	{
		if(parse_char("?"))
			return add_node(new node_t(node_t::kOptional));
		return false;
	}

	bool parse_glob_t::parse_any ()
	{
		if(parse_char("*"))
			return add_node(new node_t(node_t::kAny));
		return false;
	}

	bool parse_glob_t::parse_any_recursive ()
	{
		char const* backtrack = it;
		if(parse_char("*") && parse_char("*"))
			return add_node(new node_t(node_t::kAnyRecursive, parse_char("/") ? "/" : ""));
		return it = backtrack, false;
	}

	bool parse_glob_t::parse_brace_expansion ()
	{
		char const* backtrack = it;
		node_t* oldRoot = _root;
		node_t** oldLast = _last;

		if(parse_char("{"))
		{
			bool hasComma = false;
			node_t* localRoot = nullptr;
			while(true)
			{
				localRoot = new node_t(node_t::kOr, localRoot, parse(",}", false));
				if(parse_char("}"))
				{
					_root = oldRoot;
					_last = oldLast;
					if(hasComma)
						return add_node(new node_t(node_t::kGroup, localRoot));

					delete localRoot;

					char const* parseTo = std::exchange(it, backtrack);
					while(it != parseTo && (parse_escape() || parse_text()))
						;
					return it != parseTo ? ((it = backtrack), false) : true;
				}
				else if(parse_char(","))
				{
					hasComma = true;
				}
				else
				{
					break;
				}
			}
			delete localRoot;
		}

		_root = oldRoot;
		_last = oldLast;
		return (it = backtrack), false;
	}

	bool parse_glob_t::parse_character_class ()
	{
		char const* backtrack = it;
		std::string group;
		if(parse_char("[") && parse_until("]", group))
			return add_node(new node_t(node_t::kCharClass, group));
		return (it = backtrack), false;
	}

	bool parse_glob_t::parse_exclude ()
	{
		char const* backtrack = it;
		if((parse_char("~") || parse_char("!")) && it != last)
		{
			_root = new node_t(node_t::kExclude, nullptr, _root);
			_last = &_root->_left;
			return true;
		}
		return (it = backtrack), false;
	}

	bool parse_glob_t::parse_text ()
	{
		char const* backtrack = it;
		if(it != last)
		{
			size_t len = utf8::multibyte<char>::length(*it);
			while(it != last)
			{
				++it;
				if(--len == 0)
					return add_node(new node_t(node_t::kText, std::string(backtrack, it)));
			}
		}
		return (it = backtrack), false;
	}
}

// =================
// = API Functions =
// =================

std::string convert_glob_to_regexp (std::string const& str, bool matchDotFiles)
{
	std::string res = NULL_STR;
	if(node_t* root = parse_glob_t(str).parse())
	{
		std::string const incldues = root->to_regexp(matchDotFiles);
		std::string const excludes = root->excludes();

		res  = "^";

		res += !excludes.empty() ? "(?!(?:.*/)?(?:" + excludes + ")$)" : "";
		res += !incldues.empty() ? "(?:.*/)?(?:" + incldues + ")" : ".*";

		res += "$";

		delete root;
	}
	return res;
}
