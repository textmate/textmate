#ifndef SNIPPET_H_2OAJB31U
#define SNIPPET_H_2OAJB31U

#include "format_string.h"
#include "regexp.h"
#include <text/src/indent.h>
#include <oak/debug.h>

namespace snippet
{
	struct pos_t
	{
		pos_t (size_t offset = 0, size_t rank = 0) : offset(offset), rank(rank) { }
		size_t offset, rank;

		bool operator<  (pos_t const& rhs) const { return offset < rhs.offset || (offset == rhs.offset && rank < rhs.rank); }
		bool operator== (pos_t const& rhs) const { return offset == rhs.offset && rank == rhs.rank; }

		pos_t operator+ (ssize_t dist) const { return pos_t(offset + dist, rank); }
		pos_t operator- (ssize_t dist) const { return pos_t(offset - dist, rank); }
	};

	struct range_t
	{
		range_t (pos_t const& from, pos_t const& to) : from(from), to(to) { }
		pos_t from, to;

		bool contains (pos_t const& pos) const   { return from < pos && pos < to; }
		bool contains (range_t const& rhs) const { return from < rhs.from && rhs.to < to; }
		size_t size () const { return to.offset - from.offset; }
		std::string to_s (std::string const& str) const { return str.substr(from.offset, to.offset - from.offset); }

		bool operator<  (range_t const& rhs) const { return from < rhs.from || (from == rhs.from && to < rhs.to); }
		bool operator== (range_t const& rhs) const { return from == rhs.from && to == rhs.to; }
		bool operator!= (range_t const& rhs) const { return !(*this == rhs); }

		range_t operator+ (ssize_t dist) const { return range_t(from + dist, to + dist); }
		range_t operator- (ssize_t dist) const { return range_t(from - dist, to - dist); }
	};

	struct placeholder_t
	{
		placeholder_t (size_t index, pos_t const& from, pos_t const& to) : index(index), range(from, to) { }
		virtual ~placeholder_t () { }

		size_t index;
		range_t range;

		virtual std::string transform (std::string const& src, std::map<std::string, std::string> const& variables) const { return src; }
		virtual std::vector<std::string> const& choices () const { static std::vector<std::string> const empty; return empty; }
	};

	struct transform_t : placeholder_t
	{
		transform_t (size_t index, pos_t const& from, pos_t const& to, regexp::pattern_t const& pattern, format_string::format_string_t const& format, bool repeat) : placeholder_t(index, from, to), pattern(pattern), format(format), repeat(repeat) { }

		virtual std::string transform (std::string const& src, std::map<std::string, std::string> const& variables) const { return replace(src, pattern, format, repeat, variables); }

		regexp::pattern_t pattern;
		format_string::format_string_t format;
		bool repeat;
	};

	struct choice_t : placeholder_t
	{
		choice_t (size_t index, pos_t const& from, pos_t const& to, std::vector<std::string> const& choices) : placeholder_t(index, from, to), _choices(choices) { }
		virtual std::vector<std::string> const& choices () const { return _choices; }

	private:
		std::vector<std::string> _choices;
	};

	typedef std::shared_ptr<placeholder_t> field_ptr;

	struct snippet_t
	{
		snippet_t (std::string const& text, std::map<size_t, field_ptr> const& fields, std::multimap<size_t, field_ptr> const& mirrors, std::map<std::string, std::string> const& variables, std::string const& indent_string, text::indent_t const& indent);

		typedef std::vector< std::pair<range_t, std::string> > replacements_t;
		replacements_t replace (range_t range, std::string const& str);

		std::string text;
		std::map<size_t, field_ptr> fields;
		std::multimap<size_t, field_ptr> mirrors;
		std::map<std::string, std::string> variables;
		std::string indent_string;
		text::indent_t indent_info;

		size_t current_field;

	private:
		void setup ();
		void update_mirrors (std::set<size_t> const& forFields = std::set<size_t>());
		replacements_t replace_helper (size_t n, range_t const& range, std::string const& replacement);
	};

	struct stack_t
	{
		void push (snippet::snippet_t const& snippet, snippet::range_t const& range);
		std::vector< std::pair<snippet::range_t, std::string> > replace (snippet::range_t range, std::string replacement);
		snippet::range_t current () const;
		std::vector<std::string> const& choices () const;
		void drop_for_pos (snippet::pos_t pos);

		bool next ();
		bool previous ();
		bool in_last_placeholder () const;

		void clear ()       { records.clear(); }
		bool empty () const { return records.empty(); }

	private:
		struct record_t
		{
			record_t (snippet::snippet_t const& snippet, size_t caret = 0) : snippet(snippet), caret(caret) { }

			snippet::snippet_t snippet;
			size_t caret;
		};

		std::vector<record_t> records;
	};

	struct run_command_callback_t
	{
		virtual ~run_command_callback_t () { }
		virtual std::string run_command (std::string const& cmd) = 0;
	};

	snippet_t parse (std::string const& str, std::map<std::string, std::string> const& variables, std::string const& indentString, text::indent_t const& indent, run_command_callback_t* callback);

} /* snippet */

#endif /* end of include guard: SNIPPET_H_2OAJB31U */
