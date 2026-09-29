#ifndef ADD_TO_BUFFER_H_U7OGWZ60
#define ADD_TO_BUFFER_H_U7OGWZ60

#include <text/src/utf8.h>

static std::pair<std::string::iterator, std::string::iterator> add_bytes_to_utf8_buffer (std::string& buf, char const* first, char const* last, bool clearPrevious)
{
	auto from = utf8::find_safe_end(buf.begin(), buf.end());
	if(clearPrevious)
		from = buf.erase(buf.begin(), from);

	size_t fromOffset = from - buf.begin();
	buf.insert(buf.end(), first, last);
	from = buf.begin() + fromOffset;

	auto to = utf8::find_safe_end(from, buf.end());
	to = buf.erase(utf8::remove_malformed(from, to), to);
	from = buf.begin() + fromOffset;

	return { from, to };
}

#endif /* end of include guard: ADD_TO_BUFFER_H_U7OGWZ60 */
