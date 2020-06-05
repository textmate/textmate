#ifndef TEXT_TYPES_H_KMS7GJE8
#define TEXT_TYPES_H_KMS7GJE8

#include "format.h"
#include <oak/debug.h>
#include <oak/oak.h>

namespace text
{
	struct pos_t
	{
		size_t line, column, offset;

		pos_t () : line(0), column(0), offset(0) { }
		pos_t (size_t line, size_t column, size_t offset = 0) : line(line), column(column), offset(offset) { }

		pos_t (std::string const& str)
		{
			if(str != NULL_STR)
			{
				size_t pos[3] = { 1, 1, 0 };
				if(1 == sscanf(str.c_str(), "%zu:%zu+%zu", &pos[0], &pos[1], &pos[2]))
					sscanf(str.c_str(), "%zu+%zu", &pos[0], &pos[2]);
				*this = pos_t(pos[0]-1, pos[1]-1, pos[2]);
			}
			else
			{
				*this = pos_t::undefined;
			}
		}

		pos_t strip_offset () const                  { return pos_t(line, column); }

		bool operator== (pos_t const& rhs) const     { return line == rhs.line && column == rhs.column && offset == rhs.offset; }
		bool operator< (pos_t const& rhs) const      { return std::make_tuple(line, column, offset) < std::make_tuple(rhs.line, rhs.column, rhs.offset); }
		bool operator!= (pos_t const& rhs) const     { return !(*this == rhs); }
		bool operator> (pos_t const& rhs) const      { return !(*this < rhs || *this == rhs); }
		bool operator>= (pos_t const& rhs) const     { return !(*this < rhs); }
		bool operator<= (pos_t const& rhs) const     { return !(*this > rhs); }

		pos_t operator+ (ssize_t dist) const         { return pos_t(line, column + dist); }
		pos_t operator- (ssize_t dist) const         { ASSERT(column >= dist); return pos_t(line, column - dist); }

		operator std::string () const                { return std::to_string(line+1) + column_str() + offset_str(); }
		explicit operator bool () const              { return *this != undefined; }

		static pos_t zero;
		static pos_t undefined;

	private:
		std::string column_str () const              { return column ? text::format(":%zu", column+1) : ""; }
		std::string offset_str () const              { return offset ? text::format("+%zu", offset)   : ""; }
	};

	struct range_t
	{
		range_t (pos_t const& from = pos_t(), pos_t const& to = pos_t::undefined, bool columnar = false) : from(from), to(to == pos_t::undefined ? from : to), columnar(columnar) { }

		range_t (std::string const& str)
		{
			if(str != NULL_STR)
			{
				std::string::size_type n = str.find_first_of("-x");
				from     = pos_t(str.substr(0, n));
				to       = pos_t(str.substr(n == std::string::npos ? 0 : n+1));
				columnar = n != std::string::npos && str[n] == 'x';
			}
			else
			{
				*this = range_t::undefined;
			}
		}

		pos_t const& min () const                    { return std::min(from, to); }
		pos_t const& max () const                    { return std::max(from, to); }
		pos_t& min ()                                { return to < from ? to : from; }
		pos_t& max ()                                { return to < from ? from : to; }
		bool empty () const                          { return from == to; }
		bool is_undefined () const                   { return *this == undefined; }
		range_t reversed () const                    { return range_t(to, from, columnar); }
		range_t normalized () const                  { return range_t(min(), max(), columnar); }
		void clear ()                                { from = to; }

		range_t strip_offset () const                { return range_t(from.strip_offset(), to.strip_offset(), columnar); }

		bool operator== (range_t const& rhs) const   { return min() == rhs.min() && max() == rhs.max() && columnar == rhs.columnar; }
		bool operator!= (range_t const& rhs) const   { return !(*this == rhs); }
		bool operator< (range_t const& rhs) const    { return min() < rhs.min() || (min() == rhs.min() && max() < rhs.max()); }

		operator std::string () const                { return empty() ? std::string(from) : std::string(from) + (columnar ? "x" : "-") + std::string(to); }

		pos_t from, to;
		bool columnar;

		static range_t undefined;
	};

	struct selection_t
	{
		typedef std::vector<range_t>::iterator        iterator;
		typedef std::vector<range_t>::reference       reference;
		typedef std::vector<range_t>::const_iterator  const_iterator;
		typedef std::vector<range_t>::const_reference const_reference;

		selection_t ()                                        { }
		selection_t (pos_t const& pos) : ranges(1, pos)       { }
		selection_t (range_t const& range) : ranges(1, range) { }

		selection_t (std::string const& str)
		{
			std::string::size_type from = 0, to;
			do {
				to = str.find('&', from);
				ranges.push_back(str.substr(from, to == std::string::npos ? to : to - from));
				from = to == std::string::npos ? to : to+1;
			} while(to != std::string::npos);

			if(empty())
				ranges.push_back(range_t(0));
		}

		operator std::string () const                    { return text::join(ranges, "&"); }

		bool empty () const                              { return ranges.empty(); }
		size_t size () const                             { return ranges.size(); }
		void clear ()                                    { ranges.clear(); }
		void add (range_t const& range)                  { ranges.push_back(range); }
		void push_back (range_t const& range)            { ranges.push_back(range); }
		range_t& last ()                                 { ASSERT(!empty()); return ranges.back(); }
		range_t const& last () const                     { ASSERT(!empty()); return ranges.back(); }

		bool operator== (selection_t const& rhs) const   { return ranges == rhs.ranges; }
		bool operator!= (selection_t const& rhs) const   { return !(*this == rhs); }

		iterator begin ()                                { return ranges.begin(); }
		iterator end ()                                  { return ranges.end(); }

		const_iterator begin () const                    { return ranges.begin(); }
		const_iterator end () const                      { return ranges.end(); }

	private:
		std::vector<range_t> ranges;
	};

} /* text */

#endif /* end of include guard: TEXT_TYPES_H_KMS7GJE8 */
