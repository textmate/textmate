#include "buffer.h"
#include <oak/oak.h>

OAK_DEBUG_VAR(Buffer_Pairs);

namespace ng
{
	pairs_t::pairs_t () : _rank(0)
	{
	}

	void pairs_t::replace (buffer_t* buffer, size_t from, size_t to, std::string const& str)
	{
		if(from < to)
		{
			std::set<size_t> ranksToRemove;
			foreach(it, _pairs.lower_bound(from), _pairs.lower_bound(to))
			{
				size_t rank = it->second;
				ranksToRemove.insert(rank & ~1);
				ranksToRemove.insert(rank |  1);
			}

			if(!ranksToRemove.empty())
			{
				std::set<ssize_t> indicesToRemove;
				for(auto const& it : _pairs)
				{
					if(ranksToRemove.find(it.second) != ranksToRemove.end())
						indicesToRemove.insert(it.first);
				}

				for(auto const& index : indicesToRemove)
					_pairs.remove(index);
			}
		}
		_pairs.replace(from, to, str.size());
	}

	void pairs_t::add_pair (size_t firstIndex, size_t lastIndex)
	{
		_pairs.set(firstIndex, _rank++);
		_pairs.set(lastIndex,  _rank++);
	}

	void pairs_t::remove (size_t index)
	{
		size_t other = counterpart(index);
		_pairs.remove(index);
		_pairs.remove(other);
	}

	bool pairs_t::is_paired (size_t index) const
	{
		return _pairs.find(index) != _pairs.end();
	}

	bool pairs_t::is_first (size_t index) const
	{
		tree_t::iterator it = _pairs.find(index);
		return it != _pairs.end() && (it->second & 1) == 0;
	}

	bool pairs_t::is_last (size_t index) const
	{
		tree_t::iterator it = _pairs.find(index);
		return it != _pairs.end() && (it->second & 1) == 1;
	}

	size_t pairs_t::counterpart (size_t index) const
	{
		ASSERT(_pairs.find(index) != _pairs.end());
		size_t rank = _pairs.find(index)->second;
		D(DBF_Buffer_Pairs, bug("%zu, rank %zu\n", index, rank););
		for(auto const& it : _pairs)
		{
			D(DBF_Buffer_Pairs, bug("%zd, rank %zu → %s\n", it.first, it.second, BSTR((it.second & ~1) == (rank & ~1) && it.second != rank)););
			if((it.second & ~1) == (rank & ~1) && it.second != rank)
				return it.first;
		}
		ASSERT(false);
		return index;
	}

} /* ng */
