#include "indent.h"

namespace text
{
	std::optional<bool> detect_soft_tabs (std::string const& text)
	{
		size_t tabs = 0, spaces = 0;
		for(size_t line = 0; line < text.size(); )
		{
			size_t first = line;
			while(line < text.size() && text[line] != '\n' && text[line] != '\r')
				++line;

			size_t content = first;
			while(content < line && (text[content] == ' ' || text[content] == '\t'))
				++content;
			if(content < line && content != first)
				text[first] == '\t' ? ++tabs : ++spaces;

			while(line < text.size() && (text[line] == '\n' || text[line] == '\r'))
				++line;
		}

		if(tabs == spaces)
			return std::nullopt;
		return spaces > tabs;
	}

	std::string indent_t::create (size_t atColumn, size_t units) const
	{
		size_t baseColumn    = atColumn - (atColumn % indent_size());
		size_t desiredColumn = baseColumn + units * indent_size();

		if(soft_tabs())
		{
			return std::string(desiredColumn - atColumn, ' ');
		}
		else if(indent_size() == tab_size())
		{
			return std::string(units, '\t');
		}
		else
		{
			size_t desiredBase = desiredColumn - (desiredColumn % tab_size());
			if(desiredBase <= atColumn)
				return std::string(desiredColumn - atColumn, ' ');
			return std::string(desiredBase / tab_size() - baseColumn / tab_size(), '\t') + std::string(desiredColumn - desiredBase, ' ');
		}
	}

} /* text */
