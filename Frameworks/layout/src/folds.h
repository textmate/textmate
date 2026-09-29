#ifndef NG_BUFFER_FOLDS_H_3NU0J3FS
#define NG_BUFFER_FOLDS_H_3NU0J3FS

#include <buffer/src/buffer.h>
#include <regexp/src/regexp.h>
#include <oak/basic_tree.h>

namespace ng
{
	struct folds_t : callback_t
	{
		folds_t (buffer_t& buffer);
		~folds_t ();

		std::string folded_as_string () const;
		void set_folded_as_string (std::string const& str);

		bool has_folded (size_t n) const;
		bool has_start_marker (size_t n) const;
		bool has_stop_marker (size_t n) const;
		indexed_map_t<bool> const& folded () const;

		void fold (size_t from, size_t to);
		bool unfold (size_t from, size_t to);
		std::vector< std::pair<size_t, size_t> > remove_enclosing (size_t from, size_t to);

		std::pair<size_t, size_t> toggle_at_line (size_t n, bool recursive);
		std::vector< std::pair<size_t, size_t> > toggle_all_at_level (size_t level);

		void will_replace (size_t from, size_t to, char const* buf, size_t len);
		void did_parse (size_t from, size_t to);

	private:
		struct value_t
		{
			int indent = 0;

			bool start_marker : 1;
			bool stop_marker : 1;
			bool indent_start_marker : 1;
			bool ignore_line : 1;
			bool empty_line : 1;
		};

		static int key_comp (size_t key, size_t offset, size_t node)                              { return key < offset + node ? -1 : (key == offset + node ? 0 : +1); }
		static std::string info_to_s (oak::basic_tree_t<size_t, value_t>::value_type const& info) { return text::format("%zu: %zu + %zu", info.offset + info.key, info.offset, info.key); }

		void set_folded (std::vector< std::pair<size_t, size_t> > const& newFoldings);
		std::vector< std::pair<size_t, size_t> > foldable_ranges () const;
		std::pair<size_t, size_t> foldable_range_at_line (size_t n) const;
		value_t info_for (size_t n) const;

		// ===============
		// = Member Data =
		// ===============

		buffer_t& _buffer;

		mutable indexed_map_t<value_t> _levels;
		std::vector< std::pair<size_t, size_t> > _folded;
		indexed_map_t<bool> _legacy;
	};

} /* ng */

#endif /* end of include guard: NG_BUFFER_FOLDS_H_3NU0J3FS */
