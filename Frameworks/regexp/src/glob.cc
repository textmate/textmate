#include "glob.h"
#include "format_string.h"
#include "parser_base.h"
#include <oak/oak.h>
#include <text/format.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Glob);
OAK_DEBUG_VAR(Glob_Parser);

/*
	We do a simple transformation of the glob into a regexp using these rules:

		\\.              → $0
		\*\* /?          → ([^/.].*(?=/|$) /? )?
		\*               → ([^/.][^/]*)?
		\?               → .
		\[.*?\]          → $0
		[\\|[().?*+{^$]  → \\$0
*/

namespace path
{
	void glob_t::setup (std::string const& glob)
	{
		static regexp::pattern_t const glob_matcher = "(?:"
			"(\\\\.)"                     "|"
			"(\\*\\*(/?))"                "|"
			"(\\*)"                       "|"
			"(\\?)"                       "|"
			"(\\[.*?\\])"                 "|"
			"([\\\\|\\[().?*+{^$])"       ")"
		;

		static std::string const glob_formater = ""
			"${1}"
			"${2:+([^/.].*(?=/|$)$3)?}"
			"${4:+([^/.][^/]*)?}"
			"${5:+.}"
			"${6}"
			"${7:+\\\\$7}"
		;

		_negate = glob.empty() ? false : glob[0] == '!';

		std::vector<std::string> expanded;
		citerate(str, expand_braces(_negate ? glob.substr(1) : glob))
			expanded.push_back(format_string::replace(*str, glob_matcher, glob_formater));

		std::string ptrn = "^(.*/)?(" + text::join(expanded, "|") + ")$";
		_compiled = regexp::pattern_t(ptrn);
		D(DBF_Glob, bug("%s → %s\n", glob.c_str(), ptrn.c_str()););
	}

	bool glob_t::does_match (std::string const& filename) const
	{
		bool res = _negate ^ (bool)regexp::search(_compiled, filename.data(), filename.data() + filename.size());
		D(DBF_Glob, bug("%s → %s\n", filename.c_str(), BSTR(res)););
		return res;
	}

	std::string to_s (glob_t const& glob)
	{
		return to_s(glob._compiled);
	}

	// ===================
	// = Brace Expansion =
	// ===================

	namespace
	{
		struct node_t
		{
			node_t (std::string const& str = "") : text(str), left(NULL), right(NULL) { }

			std::string text;
			node_t* left;
			node_t* right;
		};
#if 0
		static std::string to_s (node_t const* node, size_t level = 0)
		{
			return node == NULL ? "" :
				to_s(node->right, level + 1) +
				std::string(6*level, ' ') +
				"(" + node->text + ")\n" +
				to_s(node->left, level + 1);
		}
#endif
		static std::vector<std::string> expand (node_t const* node)
		{
			if(!node)
				return std::vector<std::string>(1, "");

			std::vector<std::string> heads, tails = expand(node->left);
			for(node_t* n = node->right; n; n = n->right)
			{
				std::vector<std::string> const& suffixes = expand(n->left);
				heads.insert(heads.end(), suffixes.begin(), suffixes.end());
			}

			if(heads.empty())
				heads.push_back("");

			std::vector<std::string> res;
			iterate(head, heads)
			{
				iterate(tail, tails)
					res.push_back(*head + node->text + *tail);
			}

			return res;
		}

		struct parse_braces_t : parser_base_t
		{
			parse_braces_t (std::string const& str) : parser_base_t(str) { }

			node_t* parse ()
			{
				D(DBF_Glob_Parser, bug("‘%s’\n", std::string(it, last).c_str()););

				node_t* root = new node_t;
				node_t* cur = root;

				std::string upto;
				while(parse_until("{\\", upto))
				{
					--it;
					if(node_t* n = parse_braces())
					{
						cur = cur->left = new node_t(upto);
						cur = cur->left = n;
					}
					else if(node_t* n = parse_escape())
					{
						cur = cur->left = new node_t(upto);
						cur = cur->left = n;
					}
					else
					{
						D(DBF_Glob_Parser, bug("false positive ‘%c’\n", *it););
						cur = cur->left = new node_t(upto + std::string(1, *it));
						++it;
					}
				}

				if(it != last)
					cur = cur->left = new node_t(std::string(it, last));

				return root;
			}

			node_t* parse_escape ()
			{
				char const* backtrack = it;
				if(!parse_char("\\"))
					return NULL;

				D(DBF_Glob_Parser, bug("‘%s’\n", std::string(it-1, last).c_str()););
				if(parse_char("{,}\\"))
					return new node_t(std::string(1, it[-1]));

				D(DBF_Glob_Parser, bug("*** backtrack\n"););
				return (it = backtrack), (node_t*)NULL;
			}

			node_t* parse_braces ()
			{
				char const* backtrack = it;
				if(!parse_char("{"))
					return NULL;

				D(DBF_Glob_Parser, bug("‘%s’\n", std::string(it-1, last).c_str()););

				node_t* root   = new node_t;
				node_t* branch = root->right = new node_t;
				node_t* cur    = branch;

				std::string upto;
				while(parse_until("{,}\\", upto))
				{
					--it;
					if(node_t* n = parse_braces())
					{
						cur = cur->left = new node_t(upto);
						cur = cur->left = n;
					}
					else if(parse_char(","))
					{
						cur = cur->left = new node_t(upto);
						cur = branch = branch->right = new node_t;
					}
					else if(node_t* n = parse_escape())
					{
						cur = cur->left = new node_t(upto);
						cur = cur->left = n;
					}
					else if(parse_char("}"))
					{
						cur = cur->left = new node_t(upto);
						return root->right->right ? root : ((it = backtrack), (node_t*)NULL);
					}
					else
					{
						D(DBF_Glob_Parser, bug("false positive ‘%c’\n", *it););
						cur = cur->left = new node_t(upto + std::string(1, *it));
						++it;
					}
				}
				D(DBF_Glob_Parser, bug("*** backtrack\n"););
				return (it = backtrack), (node_t*)NULL;
			}
		};
	}

	std::vector<std::string> expand_braces (std::string const& glob)
	{
		return expand(parse_braces_t(glob).parse());
	}

} /* path */
