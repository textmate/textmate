#include "storage.h"
#include <crash/info.h>
#include <text/format.h>

namespace ng
{
	namespace detail
	{
		// ============
		// = memory_t =
		// ============

		template <typename _InputIter>
		memory_t::memory_t (_InputIter first, _InputIter last) : _offset(0)
		{
			_helper = std::make_shared<helper_t>(std::distance(first, last));
			std::copy(first, last, _helper->bytes());
		}

		template <typename _InputIter>
		void memory_t::insert (size_t pos, _InputIter first, _InputIter last)
		{
			ASSERT_EQ(_offset + pos, _helper->size());
			std::copy(first, last, _helper->bytes() + _offset + pos);
			_helper->grow(std::distance(first, last));
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
			crash_reporter_info_t crashInfo(text::format("storage insert %zu, size %zu", pos, size()));
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
				if(tmp->key == tmp->value.size() && length <= tmp->value.free())
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
			crash_reporter_info_t crashInfo(text::format("storage erase %zu-%zu, size %zu", first, last, size()));
			ASSERT_LE(first, last); ASSERT_LE(last, size());

			auto from = find_pos(first);
			from = from != _tree.end() ? split_at(from, first - from->offset) : from;

			auto to = find_pos(last);
			to = to != _tree.end() ? split_at(to, last - to->offset) : to;

			_tree.erase(from, to);
		}

		char storage_t::operator[] (size_t i) const
		{
			crash_reporter_info_t crashInfo(text::format("storage access %zu, size %zu", i, size()));
			ASSERT_LE(i, size());
			auto it = find_pos(i);
			return it->value.bytes()[i - it->offset];
		}

		std::string storage_t::substr (size_t first, size_t last) const
		{
			crash_reporter_info_t crashInfo(text::format("storage access %zu-%zu, size %zu", first, last, size()));
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