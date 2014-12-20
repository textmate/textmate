#include "diff.h"
#include <algorithm>
#include <iostream>
#include <set>

namespace diff
{
	typedef std::map<cache_t::string_id_t, size_t> unique_map_t;

	static int comp_abs (size_t key, size_t const& offset, size_t const& node)
	{
		 return key < offset ? -1 : (key == offset ? 0 : +1);
	}
	static void dump (cache_t::tree_t& tree, std::map<cache_t::string_id_t, std::string >& inverse)
	{
		fprintf(stderr,"dump:\n");
		for(auto tree_row : tree)
		{
			//fprintf(stderr,"[%zu<%s>, ", tree_row.offset, tree_row.value->_id.to_s().c_str() );
			std::cerr << "[" << tree_row.offset << "<" << tree_row.value->_id.to_s() << ">, ";
			auto iter = tree_row.value->_previous;
			while(iter != tree.end())
			{
				//fprintf(stderr,"%zu<%s>, ", iter->offset, iter->value->_id.to_s().c_str() );
				std::cerr << iter->offset << "<"<< iter->value->_id.to_s()<< ">" <<", ";
				iter = iter->value->_previous;
			}
			//fprintf(stderr,"]%s \n", inverse.find(tree_row.value->_id)->second.c_str() );
			std::cerr << "]" << inverse.find(tree_row.value->_id)->second << std::endl;
		}
	}

	void dump (cache_t& cache, bool both)
	{
		fprintf(stderr,"dump: %s\n", both ? "both" : "left");

		std::map<cache_t::string_id_t, std::string > inverse;
		for(auto item : cache.stringToId)
			inverse.insert(std::make_pair(item.second, item.first));
		dump(cache.line_position_to_id_A, inverse);
		if(both)
			dump(cache.line_position_to_id_B, inverse);
	}

	static bool unlink (cache_t& cache, cache_t::tree_t::iterator& removePosition, cache_t::top_link_t& previous)
	{
		auto stringId = removePosition->value->_id;
		auto foundLink = previous.find(stringId)->second;
		ASSERT(previous.find(stringId) != previous.end());

		auto successor = foundLink;
		while(foundLink != removePosition)
		{
			successor = foundLink;
			foundLink = foundLink->value->_previous;
		}

		if(successor != foundLink)
			successor->value->_previous = foundLink->value->_previous;
		else
		{
			previous.erase(stringId);
			return true;
		}
		return false;
	}

	static void remove (cache_t& cache, cache_t::tree_t& tree, size_t offset, std::vector<std::string> const& lines, cache_t::top_link_t& previous, cache_t::top_link_t& previousOther)
	{
		auto removePosition = tree.find(offset, comp_abs);
		for(auto& line : lines)
		{
			bool last = unlink(cache, removePosition, previous);
			if(last && previousOther.find(removePosition->value->_id) == previousOther.end())
				cache.stringToId.erase(line);

			auto tmp = removePosition;
			++removePosition;
			tree.erase(tmp);
		}
	}

	void remove (cache_t& cache, size_t offset, std::vector<std::string> const& lines, bool file1)
	{
		remove(cache, file1 ? cache.line_position_to_id_A : cache.line_position_to_id_B, offset, lines, file1 ? cache.previousA : cache.previousB, file1 ? cache.previousB : cache.previousA);
	}

	static cache_t::tree_t::iterator link (cache_t& cache, cache_t::tree_t::iterator& newLink, cache_t::top_link_t& previous)
	{
		const auto end = newLink->value->_previous;
		auto stringId = newLink->value->_id;
		auto stringIter = previous.find(stringId);
		if(stringIter == previous.end()) {
			previous.emplace(stringId, newLink);
			return newLink;
		}
		auto successor = end;
		auto foundLink = stringIter->second;

		while(foundLink != end && foundLink.refresh()->offset > newLink->offset)
		{
			successor = foundLink;
			foundLink = foundLink->value->_previous;
		}

		newLink->value->_previous = foundLink;
		if(successor != end)
		{
			successor->value->_previous = newLink;
		}
		else
		{
			stringIter->second = newLink;
		}
		return newLink;
	}

	static void insert (cache_t& cache, cache_t::tree_t& tree, size_t offset, std::vector<std::string> const& lines, cache_t::top_link_t& previous)
	{
		auto insertPosition = tree.lower_bound(offset, comp_abs);
		bool atEnd = insertPosition == tree.end();
		for(auto& line : lines)
		{
			auto alreadyThere = cache.stringToId.find(line);
			if(alreadyThere == cache.stringToId.end())
				alreadyThere = cache.stringToId.emplace(line, cache.identity++).first;
			insertPosition = tree.insert(insertPosition, 1, std::make_unique<cache_t::string_node_t>(tree.end(), alreadyThere->second));
			insertPosition = ++link(cache, insertPosition, previous);
		}
	}

	void insert (cache_t& cache, size_t offset, std::vector<std::string> const& lines, bool file1)
	{
		insert(cache, file1 ? cache.line_position_to_id_A : cache.line_position_to_id_B, offset, lines, file1 ? cache.previousA : cache.previousB);
	}

	static void setup (cache_t& cache, std::vector<std::string> const& linesA, std::vector<std::string> const& linesB)
	{
		insert(cache, cache.line_position_to_id_A, 0, linesA, cache.previousA);
		insert(cache, cache.line_position_to_id_B, 0, linesB, cache.previousB);
	}

	auto intersectMaps(unique_map_t& left, unique_map_t& right)
	{
		std::map<size_t, std::pair<size_t, cache_t::string_id_t> > result;
		unique_map_t::const_iterator il = left.begin();
		unique_map_t::const_iterator ir = right.begin();
		while (il != left.end() && ir != right.end())
		{
			if (il->first < ir->first)
				++il;
			else if (ir->first < il->first)
				++ir;
			else
			{
				result.insert(std::make_pair(il->second, std::make_pair(ir->second, ir->first)));
				++il;
				++ir;
			}
		}
		return result;
	}

	static unique_map_t lone_elements_in_range(cache_t::tree_t& positions, std::set<cache_t::string_id_t>& condemned,size_t low, size_t high)
	{
		size_t range = high - low;
		auto lineId = positions.lower_bound(high, comp_abs);
		auto end = positions.end();
		unique_map_t uniques;
		while(range-- > 0)
		{
			--lineId;
			auto foundLink = lineId->value->_previous;
			if(condemned.find(lineId->value->_id) == condemned.end() && (foundLink == end || foundLink.refresh()->offset < low))
				uniques[lineId->value->_id] = lineId->offset;
			else
				condemned.insert(lineId->value->_id);
		}
		return uniques;
	}

	static std::vector<diff::position_t> unique (cache_t& cache, size_t lowA, size_t lowB, size_t highA, size_t highB)
	{
		std::set<cache_t::string_id_t> condemned;
		auto a = lone_elements_in_range(cache.line_position_to_id_A, condemned, lowA, highA);
		auto b = lone_elements_in_range(cache.line_position_to_id_B, condemned, lowB, highB);
		auto b_to_a = intersectMaps(b, a);

		// patience sort
		std::map<size_t, diff::position_t> backpointer;
		std::vector<diff::position_t> topOfPiles;
		// use SIZE_MAX as backpointer to leftmost values
		// values increase with index increase except for first
		topOfPiles.emplace_back(SIZE_MAX, SIZE_MAX);
		for(auto it : b_to_a)
		{
			auto insert_at = std::upper_bound(topOfPiles.begin()+1, topOfPiles.end(), it.second.first, [](size_t const key, diff::position_t const& position){ return key < position.a_pos;});

			if(insert_at == topOfPiles.end())
				insert_at = topOfPiles.insert(insert_at, diff::position_t{it.second.first, it.first});
			else
				*insert_at = diff::position_t{it.second.first, it.first};
			backpointer[it.first]=*(--insert_at);
		}

		// using topOfPiles as result vector
		auto& item = topOfPiles.back();
		size_t index = topOfPiles.size() - 1;
		while(item.b_pos != SIZE_MAX)
			item = backpointer[(topOfPiles[--index] = item).b_pos];
		topOfPiles.resize(topOfPiles.size() - 1);
		return topOfPiles;
	}

	static void recurse (cache_t& cache, size_t lowA, size_t lowB, size_t highA, size_t highB, std::vector<diff::position_t >& matches)
	{
		if(lowA == highA || lowB == highB)
			return;
		auto uniques = unique(cache, lowA, lowB, highA, highB);
		if(uniques.size() > 0)
		{
			// matches are treated as range markers, we need an end marker to close the range the last match opened
			// e.g. [lastMatch, highA)
			uniques.emplace_back(highA, highB);
			// and a begin marker
			diff::position_t previousPosition{lowA, lowB};
			for(auto& pos : uniques)
			{
				recurse(cache, previousPosition.a_pos, previousPosition.b_pos, pos.a_pos, pos.b_pos, matches);
				previousPosition = pos;
				matches.push_back(previousPosition++);
			}
			// pop off the last match, which corresponded to the end position.
			matches.pop_back();
			return;
		}

		auto lineA = cache.line_position_to_id_A.lower_bound(lowA, comp_abs);
		auto lineB = cache.line_position_to_id_B.lower_bound(lowB, comp_abs);
		if (lineA->value->_id == lineB->value->_id)
		{			
			// find match from beginning
			while(lineA->offset < highA && lineB->offset < highB && lineA->value->_id == lineB->value->_id)
			{
				matches.emplace_back(lineA->offset, lineB->offset);
				++lineA;
				++lineB;
			}
			recurse(cache, lineA->offset, lineB->offset, highA, highB, matches);
			return;
		}

		lineA = cache.line_position_to_id_A.lower_bound(highA, comp_abs);
		lineB = cache.line_position_to_id_B.lower_bound(highB, comp_abs);
		size_t equalCount = 0;
		bool overshot = true;
		while((--lineA)->value->_id == (--lineB)->value->_id)
		{
			++equalCount;
			if(lineA->offset == lowA || lineB->offset == lowB)
			{
				overshot = false;
				break;
			}
		}
		if(equalCount > 0)
		{
			highA = lineA->offset;
			highB = lineB->offset;

			if(overshot)
			{
				++highA;
				++highB;
			}
			recurse(cache, lowA, lowB, highA, highB, matches);
			// add end matches

			while(equalCount--) {
				matches.emplace_back(highA++, highB++);
			}
		}
	}

	std::vector<diff::position_t> diff (std::vector<std::string> const& linesA, std::vector<std::string> const& linesB)
	{
		cache_t cache;
		setup(cache, linesA, linesB);
		std::vector<diff::position_t> matches;
		recurse(cache, 0, 0, linesA.size(), linesB.size(), matches);
		return matches;
	}

	cache_t updateable_diff (std::vector<std::string> const& linesA, std::vector<std::string> const& linesB)
	{
		cache_t cache;
		setup(cache, linesA, linesB);
		std::vector<diff::position_t> matches;
		return cache;
	}

	std::vector<position_t> update (cache_t& cache)
	{
		//dump(cache);
		std::vector<diff::position_t> matches;
		recurse(cache, 0, 0, cache.line_position_to_id_A.aggregated(), cache.line_position_to_id_B.aggregated(), matches);
		cache.start = diff::unset;
		cache.stop = size(cache);

		return matches;
	}

	position_t size (cache_t& cache)
	{
		return diff::position_t{cache.line_position_to_id_A.aggregated(), cache.line_position_to_id_B.aggregated()};
	}

}
