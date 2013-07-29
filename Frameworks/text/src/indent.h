#ifndef TEXT_INDENT_H_VYHC46E4
#define TEXT_INDENT_H_VYHC46E4

#include <oak/misc.h>

namespace text
{
	struct PUBLIC indent_t
	{
		indent_t (size_t tabSize = 8, size_t indentSize = SIZE_T_MAX, bool softTabs = false, bool tabFollowsIndent = true) : _tab_size(tabSize), _indent_size(indentSize), _soft_tabs(softTabs), _tab_follows_indent(tabFollowsIndent)
		{
			if(indentSize == SIZE_T_MAX)
				_indent_size = _tab_size;
		}

		size_t tab_size () const                 { return _tab_size;           }
		size_t indent_size () const              { return _indent_size;        }
		bool soft_tabs () const                  { return _soft_tabs;          }
		bool tab_follows_indent () const         { return _tab_follows_indent; }

		void set_tab_size (size_t tabSize)       { _tab_size = tabSize;        if(_tab_follows_indent) _indent_size = tabSize; }
		void set_indent_size (size_t indentSize) { _indent_size = indentSize;  if(_tab_follows_indent) _tab_size = indentSize; }
		void set_soft_tabs (bool softTabs)       { _soft_tabs = softTabs;      }
		void set_tab_follows_indent (bool flag)  { _tab_follows_indent = flag; }

		std::string create (size_t atColumn = 0, size_t units = 1) const;

		bool operator== (indent_t const& rhs) const
		{
			return _tab_size == rhs._tab_size && _indent_size == rhs._indent_size && _soft_tabs == rhs._soft_tabs && _tab_follows_indent == rhs._tab_follows_indent;
		}

		bool operator!= (indent_t const& rhs) const
		{
			return _tab_size != rhs._tab_size || _indent_size != rhs._indent_size || _soft_tabs != rhs._soft_tabs || _tab_follows_indent != rhs._tab_follows_indent;
		}

	private:
		size_t _tab_size;
		size_t _indent_size;
		bool _soft_tabs;
		bool _tab_follows_indent;
	};

} /* text */

#endif /* end of include guard: TEXT_INDENT_H_VYHC46E4 */
