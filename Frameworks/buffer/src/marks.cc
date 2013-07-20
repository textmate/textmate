#include "meta_data.h"
#include <oak/oak.h>

namespace ng
{
	void marks_t::replace (buffer_t* buffer, size_t from, size_t to, std::string const& str)
	{
		iterate(m, _marks)
		{
			tree_t::iterator it = m->second.upper_bound(to);
			std::string preserveMark = it != m->second.begin() && from < (--it)->first && it->first <= to ? it->second : NULL_STR;
			m->second.replace(from, to, str.size(), false);
			if(preserveMark != NULL_STR)
				m->second.set(from + str.size(), preserveMark);
		}
	}

	void marks_t::set (size_t index, std::string const& markType)
	{
		_marks[markType].set(index, markType);
	}

	void marks_t::remove (size_t index, std::string const& markType)
	{
		_marks[markType].remove(index);
	}

	void marks_t::remove_all (std::string const& markType)
	{
		_marks.erase(markType);
	}

	std::string marks_t::get (size_t index, std::string const& markType) const
	{
		ASSERT(_marks.find(markType) != _marks.end());
		ASSERT(_marks.find(markType)->second.find(index) != _marks.find(markType)->second.end());
		return _marks.find(markType)->second.find(index)->second;
	}

	std::pair<size_t, std::string> marks_t::next (size_t index, std::string const& markType) const
	{
		std::map<std::string, tree_t>::const_iterator m = _marks.find(markType);
		if(m == _marks.end())
			return std::pair<size_t, std::string>(0, NULL_STR);

		if(m->second.size() == 0)
			return std::pair<size_t, std::string>(0, NULL_STR);
		else if(m->second.size() == 1)
			return *m->second.begin();

		tree_t::iterator it = m->second.upper_bound(index);
		return *(it == m->second.end() ? m->second.begin() : it);
	}

	std::pair<size_t, std::string> marks_t::prev (size_t index, std::string const& markType) const
	{
		std::map< std::string, tree_t>::const_iterator m = _marks.find(markType);
		if(m == _marks.end())
			return std::pair<size_t, std::string>(0, NULL_STR);

		if(m->second.size() == 0)
			return std::pair<size_t, std::string>(0, NULL_STR);
		else if(m->second.size() == 1)
			return *m->second.begin();

		tree_t::iterator it = m->second.lower_bound(index);
		return *(--(it == m->second.begin() ? m->second.end() : it));
	}

	std::map<size_t, std::string> marks_t::get_range (size_t from, size_t to, std::string const& markType) const
	{
		ASSERT_LE(from, to);
		std::map<size_t, std::string> res;

		if(markType == NULL_STR)
		{
			iterate(m, _marks)
			{
				foreach(it, m->second.lower_bound(from), m->second.upper_bound(to))
					res[it->first - from] = it->second;
			}
		}
		else
		{
			std::map< std::string, tree_t>::const_iterator m = _marks.find(markType);
			if(m != _marks.end())
			{
				foreach(it, m->second.lower_bound(from), m->second.upper_bound(to))
					res[it->first - from] = it->second;
			}
		}
		return res;
	}

} /* ng */
