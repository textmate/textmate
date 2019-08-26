#include "undo.h"

namespace ng
{
	undo_manager_t::undo_manager_t (buffer_t& buffer) : _buffer(buffer), _buffer_callback(*this)
	{
		_buffer.add_callback(&_buffer_callback);
	}

	undo_manager_t::~undo_manager_t ()
	{
		_buffer.remove_callback(&_buffer_callback);
	}

	bool undo_manager_t::can_undo () const      { return _index != 0;               }
	bool undo_manager_t::can_redo () const      { return _index != _records.size(); }
	bool undo_manager_t::in_undo_group () const { return _nesting_count != 0;       }

	void undo_manager_t::begin_undo_group (ranges_t const& ranges)
	{
		if(++_nesting_count == 1)
		{
			_pre_selection = ranges;
			_pre_revision = _buffer.revision();
			_changes = 0;
		}
	}

	void undo_manager_t::end_undo_group (ranges_t const& ranges, bool force)
	{
		if((--_nesting_count == 0 || force) && _changes)
		{
			_records[_index-1].post_selection = ranges;
			_records[_index-1].post_revision = _buffer.bump_revision();
			_changes = 0;
		}
	}

	bool undo_manager_t::should_merge (record_t const& r, record_t const& t)
	{
		bool rWasInsertEvent = r.before.size() == 0 && r.after.size() != 0;
		bool rWasEraseEvent  = r.before.size() != 0 && r.after.size() == 0;
		bool tWasInsertEvent = t.before.size() == 0 && t.after.size() != 0;
		bool tWasEraseEvent  = t.before.size() != 0 && t.after.size() == 0;
		if(rWasInsertEvent != tWasInsertEvent || rWasEraseEvent != tWasEraseEvent)
			return false;

		bool rWasAllWhitespace = (rWasInsertEvent ? r.after : r.before).find_first_not_of(" \n\t") == std::string::npos;
		bool rWasNoWhitespace  = (rWasInsertEvent ? r.after : r.before).find_first_of(" \n\t") == std::string::npos;
		bool tWasAllWhitespace = (tWasInsertEvent ? t.after : t.before).find_first_not_of(" \n\t") == std::string::npos;
		bool tWasNoWhitespace  = (tWasInsertEvent ? t.after : t.before).find_first_of(" \n\t") == std::string::npos;

		return (rWasAllWhitespace || rWasNoWhitespace) && rWasAllWhitespace == tWasAllWhitespace && rWasNoWhitespace == tWasNoWhitespace;
	}

	ranges_t undo_manager_t::undo ()
	{
		ASSERT(can_undo());

		_buffer.remove_callback(&_buffer_callback);
		ranges_t res;
		size_t rev;
		while(res.empty())
		{
			ASSERT(_index != 0);
			record_t const& r = _records[--_index];
			_buffer.replace(r.pos, r.pos + r.after.size(), r.before);
			res = r.pre_selection;
			rev = r.pre_revision;

			if(_index != 0 && _index != _records.size()-1 && res == _records[_index-1].post_selection && should_merge(r, _records[_index-1]))
				res = ranges_t();
		}
		_buffer.set_revision(rev);
		_buffer.add_callback(&_buffer_callback);
		_changes = 0;
		return res;
	}

	ranges_t undo_manager_t::redo ()
	{
		ASSERT(can_redo());

		_buffer.remove_callback(&_buffer_callback);
		ranges_t res;
		size_t rev;
		while(res.empty())
		{
			ASSERT(_index != _records.size());
			record_t const& r = _records[_index++];
			_buffer.replace(r.pos, r.pos + r.before.size(), r.after);
			res = r.post_selection;
			rev = r.post_revision;

			if(_index != _records.size() && res == _records[_index].pre_selection && should_merge(r, _records[_index]))
				res = ranges_t();
		}
		_buffer.set_revision(rev);
		_buffer.add_callback(&_buffer_callback);
		_changes = 0;
		return res;
	}

	void undo_manager_t::will_replace (size_t from, size_t to, char const* buf, size_t len)
	{
		_records.erase(_records.begin() + _index, _records.end());
		_records.emplace_back(from, _buffer.substr(from, to), std::string(buf, len), _pre_selection, _pre_revision);
		_pre_selection = ranges_t();
		++_changes;
		++_index;
	}

} /* ng */
