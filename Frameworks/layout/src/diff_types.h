#ifndef DIFF_TYPES_H_8UQTEJED
#define DIFF_TYPES_H_8UQTEJED

#include <diff/diff.h>
#include "paragraph.h"
#include <list>

namespace ng
{
	struct diff_t
	{
		diff_t (std::shared_ptr<buffer_t> diffBuffer, ng::callback_t* callback) : _diff_buffer(diffBuffer), _buffer_callback(callback)
		{
			_diff_buffer->add_callback(_buffer_callback);
		}

		~diff_t ()
		{
			_diff_buffer->remove_callback(_buffer_callback);
			delete _buffer_callback;
		}
		std::shared_ptr<buffer_t> _diff_buffer;
		diff::cache_t _diff_data;
		ng::callback_t* _buffer_callback;
		size_t _diff_begin = 0;
		size_t _diff_end = 0;
	};

	struct offset_paragraph_t
	{
		offset_paragraph_t (paragraph_t paragraph, size_t index, CGFloat height) : paragraph(paragraph), index(index), height(height) {}
		paragraph_t paragraph;
		size_t index;
		CGFloat height;
	};

	typedef std::list<offset_paragraph_t> cached_paragraphs_t;
	typedef std::shared_ptr<diff_t> diff_ptr;
}

#endif /* end of include guard: DIFF_TYPES_H_8UQTEJED */
