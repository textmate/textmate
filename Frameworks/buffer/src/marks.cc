#include "meta_data.h"
#include <oak/oak.h>

namespace ng
{
	void marks_t::replace (buffer_t* buffer, size_t from, size_t to, size_t len)
	{
		for(auto& m : _marks)
		{
			tree_t::iterator it = m.second.upper_bound(to);
			std::string preserveMark = it != m.second.begin() && from < (--it)->first && it->first <= to ? it->second : NULL_STR;
			m.second.replace(from, to, len, false);
			if(preserveMark != NULL_STR)
				m.second.set(from + len, preserveMark);
		}
	}

	void marks_t::set (size_t index, std::string const& markType, std::string const& value)
	{
		_marks[markType].set(index, value);
	}

	void marks_t::remove (size_t index, std::string const& markType)
	{
		_marks[markType].remove(index);
	}

	void marks_t::remove_all (std::string const& markType)
	{
		if(!markType.empty() && markType.back() == '/')
				oak::erase_descendent_keys(_marks, markType);
		else	_marks.erase(markType);
	}

	std::string marks_t::get (size_t index, std::string const& markType) const
	{
		ASSERT(_marks.find(markType) != _marks.end());
		ASSERT(_marks.find(markType)->second.find(index) != _marks.find(markType)->second.end());
		return _marks.find(markType)->second.find(index)->second;
	}

	std::pair<size_t, std::string> marks_t::next (size_t index, std::string const& markType) const
	{
		auto from = markType == NULL_STR ? _marks.begin() : _marks.lower_bound(markType);
		auto to   = markType == NULL_STR ? _marks.end()   : _marks.upper_bound(markType);

		std::vector<std::pair<size_t, std::string>> candidates;
		for(auto m = from; m != to; ++m)
		{
			if(m->second.empty())
				continue;

			auto it = m->second.upper_bound(index);
			candidates.push_back(it == m->second.end() ? *(m->second.begin()) : *it);
		}

		if(candidates.empty())
			return std::make_pair(0, NULL_STR);
		else if(candidates.size() == 1)
			return candidates.front();

		std::sort(candidates.begin(), candidates.end());
		auto it = std::upper_bound(candidates.begin(), candidates.end(), index, [](size_t index, std::pair<size_t, std::string> const& mark){ return mark.first > index; });

		return it == candidates.end() ? candidates.front() : *it;
	}

	std::pair<size_t, std::string> marks_t::prev (size_t index, std::string const& markType) const
	{
		auto from = markType == NULL_STR ? _marks.begin() : _marks.lower_bound(markType);
		auto to   = markType == NULL_STR ? _marks.end()   : _marks.upper_bound(markType);

		std::vector<std::pair<size_t, std::string>> candidates;
		for(auto m = from; m != to; ++m)
		{
			if(m->second.empty())
				continue;

			auto it = m->second.lower_bound(index);
			candidates.push_back(*(--(it == m->second.begin() ? m->second.end() : it)));
		}

		if(candidates.empty())
			return std::make_pair(0, NULL_STR);

		std::sort(candidates.begin(), candidates.end());
		auto it = std::lower_bound(candidates.begin(), candidates.end(), index, [](std::pair<size_t, std::string> const& mark, size_t index){ return mark.first < index; });

		return *(--(it == candidates.begin() ? candidates.end() : it));
	}

	std::multimap<size_t, std::pair<std::string, std::string>> marks_t::get_range (size_t from, size_t to) const
	{
		ASSERT_LE(from, to);
		std::multimap<size_t, std::pair<std::string, std::string>> res;
		for(auto const& m : _marks)
		{
			foreach(it, m.second.lower_bound(from), m.second.upper_bound(to))
				res.emplace(it->first, std::make_pair(m.first, it->second));
		}
		return res;
	}

	std::map<size_t, std::string> marks_t::get_range (size_t from, size_t to, std::string const& markType) const
	{
		ASSERT_LE(from, to);
		std::map<size_t, std::string> res;
		std::map<std::string, tree_t>::const_iterator m = _marks.find(markType);
		if(m != _marks.end())
			std::copy(m->second.lower_bound(from), m->second.upper_bound(to), std::inserter(res, res.end()));
		return res;
	}

} /* ng */
