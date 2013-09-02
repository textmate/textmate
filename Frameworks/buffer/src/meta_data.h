#ifndef BUFFER_META_DATA_H_Z0JQSBGY
#define BUFFER_META_DATA_H_Z0JQSBGY

#include "buffer.h"

namespace ng
{
	struct spelling_t : meta_data_t
	{
		void check_spelling (buffer_t const* buffer);
		std::map<size_t, bool> misspellings (buffer_t const* buffer, size_t from, size_t to) const;

		bool misspelled_at (size_t i) const;
		std::pair<size_t, size_t> next_misspelling (size_t from) const;
		void recheck (buffer_t const* buffer, size_t from, size_t to);

	private:
		void replace (buffer_t* buffer, size_t from, size_t to, std::string const& str);
		void did_parse (buffer_t const* buffer, size_t from, size_t to);

		typedef indexed_map_t<bool> tree_t;
		tree_t _misspellings;    // true = misspelled, false = proper
	};

	struct symbols_t : meta_data_t
	{
		std::map<size_t, std::string> symbols (buffer_t const* buffer) const;
		std::string symbol_at (buffer_t const* buffer, size_t i) const;

	private:
		void replace (buffer_t* buffer, size_t from, size_t to, std::string const& str);
		void did_parse (buffer_t const* buffer, size_t from, size_t to);

		typedef indexed_map_t<std::string> tree_t;
		tree_t _symbols;
	};

	struct marks_t : meta_data_t
	{
		void set (size_t index, std::string const& markType);
		void remove (size_t index, std::string const& markType);
		void remove_all (std::string const& markType);
		std::string get (size_t index, std::string const& markType) const;
		std::map<size_t, std::string> get_range (size_t from, size_t to, std::string const& markType) const;

		std::pair<size_t, std::string> next (size_t index, std::string const& markType) const;
		std::pair<size_t, std::string> prev (size_t index, std::string const& markType) const;

	private:
		void replace (buffer_t* buffer, size_t from, size_t to, std::string const& str);
		using meta_data_t::did_parse;

		typedef indexed_map_t<std::string> tree_t;
		std::map<std::string, tree_t> _marks;
	};

} /* ng */

#endif /* end of include guard: BUFFER_META_DATA_H_Z0JQSBGY */
