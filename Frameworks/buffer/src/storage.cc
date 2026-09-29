#include "storage.h"
#include <text/src/format.h>

namespace ng
{
	namespace detail
	{
		// ============
		// = memory_t =
		// ============

		template <typename _InputIter>
		memory_t::memory_t (_InputIter first, _InputIter last) : _helper(std::make_shared<helper_t>(first, last)), _offset(0) { }

		template <typename _InputIter>
		void memory_t::insert (size_t pos, _InputIter first, _InputIter last)
		{
			ASSERT_EQ(_offset + pos, _helper->size());
			ASSERT_LE(std::distance(first, last), _helper->available());
			_helper->append(first, last);
		}

		// =============
		// = storage_t =
		// =============

		static int comp_abs (size_t pos, size_t const& offset, size_t const& node) { return pos < offset ? -1 : (pos == offset ? 0 : +1); }

		storage_t::tree_t::iterator storage_t::find_pos (size_t pos) const
		{
			auto it = _tree.upper_bound(pos, &comp_abs);
			if(it != _tree.begin())
				--it;
			return it;
		}

		storage_t::tree_t::iterator storage_t::split_at (tree_t::iterator it, size_t pos)
		{
			ASSERT_LE(pos, it->key);

			if(pos == 0)
				return it;
			else if(pos == it->key)
				return ++it;

			size_t suffixLen = it->key - pos;
			memory_t suffix = it->value.subset(pos);

			it->key = pos;
			_tree.update_key(it);

			return _tree.insert(++it, suffixLen, suffix);
		}

		void storage_t::insert (size_t pos, char const* data, size_t length)
		{
			ASSERT_LE(pos, size());
			if(length == 0)
				return;

			auto it = find_pos(pos);
			if(it != _tree.end() && it->offset < pos)
				it = split_at(it, pos - it->offset);

			if(it != _tree.begin())
			{
				auto tmp = it;
				--tmp;
				if(tmp->key == tmp->value.size() && length <= tmp->value.available())
				{
					tmp->value.insert(tmp->key, data, data + length);
					tmp->key += length;
					_tree.update_key(tmp);
					return;
				}
			}

			_tree.insert(it, length, memory_t(data, data + length));
		}

		void storage_t::erase (size_t first, size_t last)
		{
			ASSERT_LE(first, last); ASSERT_LE(last, size());

			auto from = find_pos(first);
			from = from != _tree.end() ? split_at(from, first - from->offset) : from;

			auto to = find_pos(last);
			to = to != _tree.end() ? split_at(to, last - to->offset) : to;

			_tree.erase(from, to);
		}

		char storage_t::operator[] (size_t i) const
		{
			ASSERT_LE(i, size());
			auto it = find_pos(i);
			return it->value.bytes()[i - it->offset];
		}

		bool storage_t::operator== (storage_t const& rhs) const
		{
			if(size() != rhs.size())
				return false;

			auto lhsIter = _tree.begin(), lhsEnd = _tree.end();
			auto rhsIter = rhs._tree.begin(), rhsEnd = rhs._tree.end();

			size_t lhsOffset = 0, rhsOffset = 0;
			while(lhsIter != lhsEnd && rhsIter != rhsEnd)
			{
				size_t size = std::min(lhsIter->key - lhsOffset, rhsIter->key - rhsOffset);
				if(!std::equal(lhsIter->value.bytes() + lhsOffset, lhsIter->value.bytes() + lhsOffset + size, rhsIter->value.bytes() + rhsOffset))
					return false;

				lhsOffset += size;
				rhsOffset += size;

				if(lhsOffset == lhsIter->key)
				{
					lhsOffset = 0;
					++lhsIter;
				}

				if(rhsOffset == rhsIter->key)
				{
					rhsOffset = 0;
					++rhsIter;
				}
			}

			return lhsIter == lhsEnd && rhsIter == rhsEnd;
		}

		std::string storage_t::substr (size_t first, size_t last) const
		{
			ASSERT_LE(first, last); ASSERT_LE(last, size());
			std::string res = "";

			auto from = find_pos(first);
			auto to = _tree.upper_bound(last, &comp_abs);

			for(auto it = from; it != to; ++it)
			{
				size_t i = std::max(it->offset, first) - it->offset;
				size_t j = std::min(it->offset + it->key, last) - it->offset;
				res.insert(res.end(), it->value.bytes() + i, it->value.bytes() + j);
			}

			return res;
		}

	} /* detail */

} /* ng */
