#ifndef NG_UNDO_H_MZSRPG3
#define NG_UNDO_H_MZSRPG3

#include <buffer/src/buffer.h>
#include <selection/src/selection.h>

namespace ng
{
	struct undo_manager_t
	{
		undo_manager_t (buffer_t& buffer);
		~undo_manager_t ();

		bool can_undo () const;
		bool can_redo () const;

		void begin_undo_group (ranges_t const& ranges);
		void end_undo_group (ranges_t const& ranges, bool force = false);
		bool in_undo_group () const;

		ranges_t undo ();
		ranges_t redo ();

	private:
		void will_replace (size_t from, size_t to, char const* buf, size_t len);

		struct buffer_callback_t : callback_t
		{
			buffer_callback_t (undo_manager_t& undo_manager) : undo_manager(undo_manager) { }

			void will_replace (size_t from, size_t to, char const* buf, size_t len) { undo_manager.will_replace(from, to, buf, len); }
		private:
			undo_manager_t& undo_manager;
		};

		struct record_t
		{
			record_t (size_t pos, std::string const& before, std::string const& after, ranges_t const& selection, size_t revision) : pos(pos), before(before), after(after), pre_selection(selection), pre_revision(revision) { }
			size_t pos;
			std::string before;
			std::string after;
			ranges_t pre_selection;
			size_t pre_revision;
			ranges_t post_selection;
			size_t post_revision;
		};

		bool should_merge (record_t const& r, record_t const& t);

		buffer_t& _buffer;
		buffer_callback_t _buffer_callback;
		std::vector<record_t> _records;
		size_t _index = 0;
		size_t _nesting_count = 0;
		ranges_t _pre_selection;
		size_t _pre_revision;
		size_t _changes;
	};

} /* ng */

#endif /* end of include guard: NG_UNDO_H_MZSRPG3 */
