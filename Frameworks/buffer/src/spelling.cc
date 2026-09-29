#include "meta_data.h"
#include <bundles/src/bundles.h>
#include <oak/oak.h>
#include <text/src/my_ctype.h>
#include <oak/duration.h>
#include <ns/src/spellcheck.h>
#include <oak/debug.h>

namespace ng
{
	bool spelling_t::misspelled_at (size_t i) const
	{
		tree_t::iterator it = _misspellings.upper_bound(i);
		return it != _misspellings.begin() ? (--it)->second : false;
	}

	std::pair<size_t, size_t> spelling_t::next_misspelling (size_t from) const
	{
		tree_t::iterator it = _misspellings.upper_bound(from);
		if(it == _misspellings.end())
			it = _misspellings.begin();
		else if(!it->second && it != _misspellings.begin())
			--it;

		if(it != _misspellings.end() && it->second)
		{
			auto from = it;
			if(++it != _misspellings.end() && !it->second)
				return { from->first, it->first };
		}
		return { 0, 0 };
	}

	void spelling_t::did_parse (buffer_t const* buffer, size_t from, size_t to)
	{
		auto first = buffer->_scopes.lower_bound(from);
		auto last  = buffer->_scopes.upper_bound(to);
		if(first != buffer->_scopes.begin() && from < first->first)
			--from;

		std::set<scope::scope_t> enabled, disabled;
		for(auto pair = first; pair != last; ++pair)
		{
			if(enabled.find(pair->second) != enabled.end() || disabled.find(pair->second) != disabled.end())
				continue;
			bundles::item_ptr spellCheckingItem;
			plist::any_t const& spellCheckingValue = bundles::value_for_setting("spellChecking", pair->second, &spellCheckingItem);
			if(spellCheckingItem && !plist::is_true(spellCheckingValue))
					disabled.insert(pair->second);
			else	enabled.insert(pair->second);
		}

		std::vector< std::pair<size_t, size_t> > ranges;
		while(first != last)
		{
			scope::scope_t scope = first->second;
			size_t i = std::max<ssize_t>(from, first->first);
			size_t j = ++first == last ? to : first->first;

			if(enabled.find(scope) != enabled.end())
			{
				if(ranges.empty() || ranges.back().second != i)
						ranges.emplace_back(i, j);
				else	ranges.back().second = j;
			}
		}

		// TODO We should delay checking to the misspellings function. This way we won’t spell check entire document on load etc.
		auto fromIter = _misspellings.lower_bound(from);
		auto toIter   = _misspellings.upper_bound(to);
		if(fromIter != toIter && fromIter->first >= to && fromIter->second)
			fromIter = toIter;
		_misspellings.remove(fromIter, toIter);

		if(_disabled)
			return;

		size_t revision = buffer->revision();
		for(auto const& r : ranges)
		{
			std::string const& text = buffer->substr(r.first, r.second);
			for(auto const& range : ns::spellcheck(text.data(), text.data() + text.size(), buffer->spelling_language(), buffer->spelling_tag()))
			{
				if(revision != buffer->revision())
				{
					os_log_error(OS_LOG_DEFAULT, "Buffer has changed after ns::spellcheck()");
					break;
				}

				_misspellings.set(r.first + range.first, true);
				_misspellings.set(r.first + range.last,  false);
			}
		}
	}

	void spelling_t::replace (buffer_t* buffer, size_t from, size_t to, size_t len)
	{
		// TODO We need to keep a list of dirty ranges to minimize work done in did_parse — these ranges should be extended to “surrounding words”, e.g. if user inserts space into “sho‸rtcut” we need to re-check “sho” even though it is not included in the changed range.
		_misspellings.replace(from, to, len);
	}

	std::map<size_t, bool> spelling_t::misspellings (buffer_t const* buffer, size_t from, size_t to) const
	{
		ASSERT_LE(from, to);
		std::map<size_t, bool> res;
		foreach(it, _misspellings.lower_bound(from), _misspellings.lower_bound(to))
			res[it->first < from ? 0 : it->first - from] = it->second;
		if(!res.empty() && res.begin()->second == false)
			res[0] = true;
		return res;
	}

	void spelling_t::recheck (buffer_t const* buffer, size_t from, size_t to)
	{
		did_parse(buffer, from, to);
	}

} /* ng */
