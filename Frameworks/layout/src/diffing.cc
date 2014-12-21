#include "layout.h"

namespace ng
{
	void static insert_in_diff (size_t start_line, size_t end_line, ng::buffer_t const& buffer, diff::cache_t&  data, bool file1)
	{
		for(size_t line = start_line; line < end_line; line++)
		{
			std::vector<std::string> lines;
			lines.push_back(buffer.substr(buffer.begin(line), buffer.end(line)));
			diff::insert(data, line, lines, file1);
		}
	}

	void static remove_from_diff (size_t start_line, size_t end_line, ng::buffer_t const& buffer, diff::cache_t& data, bool file1)
	{
		for(size_t line = start_line; line < end_line; line++)
		{
			std::vector<std::string> lines;
			lines.push_back(buffer.substr(buffer.begin(line), buffer.end(line)));
			diff::remove(data, start_line, lines, file1);
		}
	}

	void layout_t::initiate_diff (std::shared_ptr<ng::buffer_t> diffBuffer)
	{
		if(!diffBuffer)
		{
			bool found = false;
			CGFloat height = 0.0;

			// clear diff remove rows
			size_t diffIndex = _rows.aggregated()._diff_index;
			auto iter = _rows.find(diffIndex, &row_diff_comp);
			while(iter != _rows.end() && iter->value.is_diff_row())
			{
				height = iter->offset._height;
				found = true;

				diffIndex = iter->offset._diff_index;
				_rows.erase(iter);
				iter = _rows.find(diffIndex, &row_diff_comp);
			}

			if(found)
			{
				auto rowIter = _rows.upper_bound(height, &row_y_comp);
				if(rowIter != _rows.begin())
					--rowIter;
				refresh_row(rowIter, true);
			}

			// clear add
			auto result = diff::update(_diff_model->_diff_data);
			result.push_back(diff::size(_diff_model->_diff_data));
			auto equal = result.begin();

			auto previous = diff::position_t{};
			auto add = _rows.begin();

			while(equal != result.end())
			{

				// UnMark Add
				for(size_t index = previous.a_pos; index < equal->a_pos ; index++)
				{
					if(index == previous.a_pos)
						add = _rows.lower_bound(_buffer.end(index), &row_key_comp);
					else
						++add;
					if(!found)
					{
						found = true;
						height = add->offset._height;
					}
					clear_diff_status(add);
					add->value.did_update_scopes(0, add->value.length(), _buffer, 0);
				}
				previous = *equal;
				previous++;
				++equal;
			}
			_diff_model.reset();

			return;
		}

		struct parser_callback_t : ng::callback_t
		{
			parser_callback_t (layout_t& layout) : layout(layout) { }
			void did_parse (size_t from, size_t to)
			{
				layout.did_update_diff_scope(from, to);
			}

		private:
			layout_t& layout;
		};
		_diff_model = std::make_shared<diff_t>(diffBuffer, new parser_callback_t(*this));

		diffBuffer->wait_for_repair();

		insert_in_diff(0, diffBuffer->lines(), *diffBuffer, _diff_model->_diff_data, false);
		insert_in_diff(0, _buffer.lines(), _buffer, _diff_model->_diff_data, true);
		//diff::dump(_diff_model->_diff_data);
		cached_paragraphs_t cache{};
		add_new_diff_layout(0, _buffer.size(), cache);
	}

	void layout_t::remove_diff_data (size_t from, size_t to, char const* buf, size_t len)
	{
		if(!_diff_model)
			return;
		// store old values, so we know the correct range in remove_old_diff_layout
		auto astart = _diff_model->_diff_data.start.a_pos;
		auto aend = _diff_model->_diff_data.stop.a_pos;
		_diff_model->_diff_begin = astart == SIZE_MAX ? 0 : _buffer.begin(astart);
		auto size = diff::size(_diff_model->_diff_data);
		_diff_model->_diff_end = aend == size.a_pos ? SIZE_MAX : _buffer.begin(aend);

		if(from == to && len == 0)
			return;

		auto fromLine = _buffer.convert(from).line;
		auto toLine = _buffer.convert(to).line + 1;

		remove_from_diff(fromLine, toLine, _buffer, _diff_model->_diff_data, true);
	}

	void layout_t::add_diff_data (size_t from, size_t to, char const* buf, size_t len)
	{
		if(!_diff_model)
			return;

		if(from == to && len == 0)
			return;

		auto fromLine = _buffer.convert(from).line;
		auto toLine = _buffer.convert(from + len).line + 1;

		insert_in_diff(fromLine, toLine, _buffer, _diff_model->_diff_data, true);
	}

	void layout_t::did_update_diff_scope (size_t from, size_t to)
	{
		auto iter = _rows.lower_bound(from, &row_diff_comp);
		if(iter == _rows.end() || !iter->value.is_diff_row())
			return;

		auto iter_end = _rows.lower_bound(to, &row_diff_comp);
		while(iter != iter_end)
		{
			size_t index = diff_index(iter);
			size_t length = index + iter->value.length();
			if(index >= to)
				return;
			iter->value.did_update_scopes(index, length, *_diff_model->_diff_buffer, index);
			iter = _rows.lower_bound(length, &row_diff_comp);
		}
	}

	cached_paragraphs_t layout_t::remove_old_diff_layout (size_t from, size_t to) 
	{
		cached_paragraphs_t paragraphs;
		if(!_diff_model)
			return paragraphs;
		// as long as we don't allow edits to original file. No need to remove the 'diff-added' status

		auto iter_start = _rows.lower_bound(_diff_model->_diff_begin, &row_offset_comp);
		auto iter_stop = _rows.lower_bound(_diff_model->_diff_end, &row_offset_comp);
		size_t start_diff_index = iter_start->offset._diff_index;
		size_t end_diff_index = _rows.aggregated()._diff_index;
		if(iter_stop != _rows.end())
		{
			end_diff_index = iter_stop->offset._diff_index;
		}

		auto iter_diff = _rows.lower_bound(end_diff_index, &row_diff_comp);
		if(iter_diff == _rows.end())
			return paragraphs;

		size_t diff_decrease = 0;
		// zero index is ambigious, so must check is_diff_row
		while(iter_diff != _rows.end() && start_diff_index <= iter_diff->offset._diff_index && iter_diff->value.is_diff_row())
		{
			size_t offset = iter_diff->offset._diff_index;
			diff_decrease += iter_diff->key._diff_index;
			paragraphs.emplace_front(iter_diff->value, offset + iter_diff->key._diff_index, iter_diff->offset._height);
			refresh_row(iter_diff, true);
			_rows.erase(iter_diff);
			iter_diff = _rows.find(offset, &row_diff_comp);
		}
		if(iter_stop != _rows.end() && diff_decrease > 0) {
			auto next_diff = _rows.upper_bound(end_diff_index - diff_decrease, &row_diff_comp);

			if(next_diff != _rows.end() )
			{
				next_diff->key._diff_index += diff_decrease;
				_rows.update_key(next_diff);
			}
		}
		return paragraphs;
	}

	bool layout_t::add_new_diff_layout (size_t from, size_t to, cached_paragraphs_t& paragraphs)
	{
		bool fullRefresh = false;
		if(!_diff_model)
			return fullRefresh;

		auto result = diff::update(_diff_model->_diff_data);
		auto previous = _diff_model->_diff_data.start;
		auto end = _diff_model->_diff_data.stop;
		if(end == diff::size(_diff_model->_diff_data))
			result.push_back(end);
		auto equal = result.begin();
		if(previous != diff::unset)
			equal = std::lower_bound(result.begin(), result.end(), previous.a_pos, [](diff::position_t const& position, size_t const key ){ return position.a_pos < key;});
		previous++;

		auto upper_bound = std::lower_bound(result.begin(), result.end(), end.a_pos, [](diff::position_t const& position, size_t const key ){ return position.a_pos < key;});
		++upper_bound;

		size_t diff_increase = 0;
		bool found = false;
		CGFloat height = 0.0;
		while(equal != upper_bound)
		{
			auto add = _rows.lower_bound(_buffer.end(previous.a_pos), &row_key_comp);
			auto row = add;

			// Mark Add
			for(size_t index = previous.a_pos; index < equal->a_pos ; index++)
			{
				add->value.set_diff_status(paragraph_t::kDiffStatusAdded);
				add->value.did_update_scopes(0, add->value.length(), _buffer, 0);

				++add;
				fullRefresh = true;
			}

			// Insert Delete
			size_t previous_offset = row->offset._diff_index;
			for(size_t index = previous.b_pos; index < equal->b_pos ; index++)
			{
				size_t pos     = _diff_model->_diff_buffer->begin(index);
				size_t end     = _diff_model->_diff_buffer->end(index);

				auto diff_row = _rows.insert(row, row_key_t(0, default_line_height()));
				while(paragraphs.size() > 0 && paragraphs.front().index < pos)
				{
					if(!found)
					{
						found = true;
						height = paragraphs.front().height;
					}
					paragraphs.pop_front();
				}
				if(paragraphs.size() > 0 && paragraphs.front().index == pos)
				{
					diff_row->value = paragraphs.front().paragraph;
					if(!found && paragraphs.front().height != diff_row->offset._height)
					{
						found = true;
						height = paragraphs.front().height;
					}
					paragraphs.pop_front();
				}
				else
				{
					diff_row->value.set_diff_status(paragraph_t::kDiffStatusRemoved);
					diff_row->value.insert(pos, end - pos, *_diff_model->_diff_buffer, pos);

					if(!found)
					{
						found = true;
						height = diff_row->offset._height;
					}
				}
				diff_increase += pos - previous_offset;
				diff_row->key._diff_index = pos - previous_offset;
				previous_offset = pos;
				update_row(diff_row);
				fullRefresh = true;
			}
			previous = *equal;
			previous++;
			++equal;
		}

		if(found)
		{
			auto rowIter = _rows.upper_bound(height, &row_y_comp);
			if(rowIter != _rows.begin())
				--rowIter;
			refresh_row(rowIter, true);
		}
		// Update diff outside the "edit"-range, 
		if(equal != result.end())
		{
			auto diff_value = _rows.lower_bound(_buffer.begin(equal->a_pos), &row_offset_comp)->offset._diff_index;
			auto next_diff = _rows.upper_bound(diff_value, &row_diff_comp);
			if(next_diff != _rows.end() )
			{
				next_diff->key._diff_index -= next_diff->offset._diff_index - diff_increase;
				_rows.update_key(next_diff);
			}
		}

		return fullRefresh;
	}

	size_t layout_t::diff_index (row_tree_t::iterator& rowIter) const
	{
		return rowIter->offset._diff_index + rowIter->key._diff_index;
	}

	void layout_t::clear_diff_status (row_tree_t::iterator& rowIter)
	{
		rowIter->value.set_diff_status(paragraph_t::kDiffStatusNone);
	}

}