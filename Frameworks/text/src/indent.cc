#include "indent.h"

namespace text
{
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
