#ifndef SCAN_PATH_H_RRYJZFKK
#define SCAN_PATH_H_RRYJZFKK

#include <oak/debug.h>
#include <text/types.h>
#include <document/document.h>

namespace find
{
	extern std::string const kSearchOpenFiles;

	struct match_t
	{
		WATCH_LEAKS(find::match_t);

		match_t () { }
		match_t (document::document_ptr document, uint32_t crc32, size_t first, size_t last, text::range_t const& range, std::map<std::string, std::string> const& captures) : document(document), crc32(crc32), first(first), last(last), range(range), captures(captures) { }

		size_t line_number () const { return range.from.line; }

		document::document_ptr document;
		uint32_t crc32;
		size_t first, last;
		text::range_t range;
		std::map<std::string, std::string> captures;
		std::string excerpt;
		size_t excerpt_offset = 0;
		std::string newlines = "\n";
		bool truncate_head = false;
		bool truncate_tail = false;
	};

} /* find */

#endif /* end of include guard: SCAN_PATH_H_RRYJZFKK */
