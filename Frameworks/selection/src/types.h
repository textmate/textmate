#ifndef TYPES_H_BY7A88L9
#define TYPES_H_BY7A88L9

#include <oak/oak.h>
#include <oak/debug.h>

namespace ng
{
	struct index_t
	{
		index_t () : index(SIZE_T_MAX), carry(SIZE_T_MAX) { }
		index_t (size_t index, size_t carry = 0) : index(index), carry(carry) { }

		explicit operator bool () const            { return index != SIZE_T_MAX; }
		bool operator== (index_t const& rhs) const { return index == rhs.index && carry == rhs.carry; }
		bool operator!= (index_t const& rhs) const { return !(*this == rhs); }
		bool operator< (index_t const& rhs) const  { return index < rhs.index || index == rhs.index && carry < rhs.carry; }
		bool operator<= (index_t const& rhs) const { return *this < rhs || *this == rhs; }
		index_t operator+ (ssize_t i) const        { return index_t(index + i, carry); }

		size_t index;
		size_t carry;
	};

	struct range_t
	{
		range_t () : columnar(false), freehanded(false), unanchored(false) { }
		range_t (index_t const& first, index_t const& last = index_t(), bool columnar = false, bool freehanded = false, bool unanchored = false, bool color = false) : first(first), last(last ?: first), columnar(columnar), freehanded(last ? freehanded : first.carry != 0), unanchored(unanchored), color(color) { }

		index_t& min ()                            { return first < last ? first : last;  }
		index_t& max ()                            { return first < last ? last  : first; }
		index_t const& min () const                { return first < last ? first : last;  }
		index_t const& max () const                { return first < last ? last  : first; }
		range_t sorted () const                    { return range_t(min(), max(), columnar, freehanded, unanchored, color); }
		bool empty () const                        { return !last || (freehanded ? first == last : first.index == last.index); }
		explicit operator bool () const            { return first ? true : false; }
		bool operator== (range_t const& tmp) const { auto lhs = normalized(), rhs = tmp.normalized(); return lhs.first == rhs.first && lhs.last == rhs.last && lhs.columnar == rhs.columnar && lhs.freehanded == rhs.freehanded; }
		bool operator!= (range_t const& tmp) const { auto lhs = normalized(), rhs = tmp.normalized(); return lhs.first != rhs.first || lhs.last != rhs.last || lhs.columnar != rhs.columnar || lhs.freehanded != rhs.freehanded; }
		bool operator< (range_t const& tmp) const  { auto lhs = normalized(), rhs = tmp.normalized(); return lhs.first < rhs.first || lhs.first == rhs.first && lhs.last < rhs.last; }
		range_t operator+ (ssize_t i) const        { return range_t(first + i, last + i, columnar, freehanded, unanchored, color); }

		range_t& operator= (index_t const& rhs)    { first = last = rhs; return *this; }

		index_t first, last;
		bool columnar, freehanded, unanchored;
		bool color = false;

		range_t normalized () const
		{
			bool strip = !columnar && !freehanded;
			return range_t(strip ? min().index : min(), strip ? max().index : max(), columnar, freehanded, unanchored, color);
		}
	};

	struct ranges_t
	{
		typedef range_t                              value_type;
		typedef std::vector<range_t>::iterator       iterator;
		typedef std::vector<range_t>::const_iterator const_iterator;

		ranges_t () { }
		ranges_t (index_t const& first) : ranges(1, range_t(first, first)) { }
		ranges_t (range_t const& range) : ranges(1, range) { }
		ranges_t (std::initializer_list<range_t> const& list) : ranges(list) { }

		bool operator== (ranges_t const& rhs) const { return ranges == rhs.ranges; }
		bool operator!= (ranges_t const& rhs) const { return ranges != rhs.ranges; }

		bool empty () const                     { return ranges.empty(); }
		size_t size () const                    { return ranges.size(); }
		explicit operator bool () const         { return !empty(); }
		void push_back (range_t const& r)       { ranges.push_back(r); }
		void push_back (index_t const& index)   { push_back(range_t(index, index)); }

		iterator begin ()                       { return ranges.begin(); }
		iterator end ()                         { return ranges.end(); }
		const_iterator begin () const           { return ranges.begin(); }
		const_iterator end () const             { return ranges.end(); }

		range_t& first ()                       { ASSERT(!empty()); return ranges.front(); }
		range_t const& first () const           { ASSERT(!empty()); return ranges.front(); }
		range_t& last ()                        { ASSERT(!empty()); return ranges.back(); }
		range_t const& last () const            { ASSERT(!empty()); return ranges.back(); }

		ranges_t sorted () const
		{
			std::vector<range_t> tmp(ranges);
			std::sort(tmp.begin(), tmp.end());
			return tmp;
		}

	private:
		ranges_t (std::vector<range_t> const& ranges) : ranges(ranges) { }
		std::vector<range_t> ranges;
	};

	std::string to_s (index_t const& index);
	std::string to_s (range_t const& range);
	std::string to_s (ranges_t const& ranges);

} /* ng */

#endif /* end of include guard: TYPES_H_BY7A88L9 */
