#include "clipboard.h"
#include <text/utf8.h>
#include <text/hexdump.h>
#include <text/format.h>

std::string const kClipboardOptionIndent    = "indent";
std::string const kClipboardOptionComplete  = "complete";
std::string const kClipboardOptionFragments = "fragments";
std::string const kClipboardOptionColumnar  = "columnar";

clipboard_t::entry_t::entry_t (std::string const& content, std::map<std::string, std::string> const& options) : _content(content), _options(options)
{
	if(!utf8::is_valid(_content.begin(), _content.end()))
	{
		std::string const prefix = "*** malformed UTF-8 data on clipboard:\n";
		_content = prefix + text::to_hex(_content.begin(), _content.end()) + "\n";
	}
}

static std::map<std::string, std::string> create_clipboard_options (std::string const& indent, bool complete, size_t fragments, bool columnar)
{
	std::map<std::string, std::string> res;
	if(indent != NULL_STR) res[kClipboardOptionIndent]    = indent;
	if(complete)           res[kClipboardOptionComplete]  = "1";
	if(fragments > 1)      res[kClipboardOptionFragments] = std::to_string(fragments);
	if(columnar)           res[kClipboardOptionColumnar]  = "1";
	return res;
}

clipboard_t::entry_t::entry_t (std::vector<std::string> const& contents, std::string const& indent, bool complete, bool columnar) : entry_t(text::join(contents, "\n"), create_clipboard_options(indent, complete, contents.size(), columnar))
{
}

struct simple_clipboard_t : clipboard_t
{
	simple_clipboard_t () : _index(0) { }

	bool empty () const               { return _stack.empty(); }

	entry_ptr previous ()             { return _index == 0               ? entry_ptr() : _stack[--_index]; }
	entry_ptr current () const        { return _index == _stack.size()   ? entry_ptr() : _stack[  _index]; }
	entry_ptr next ()                 { return _index+1 >= _stack.size() ? entry_ptr() : _stack[++_index]; }

	void push_back (entry_ptr entry)
	{
		_stack.push_back(entry);
		_index = _stack.size()-1;
	}

private:
	std::vector<entry_ptr> _stack;
	size_t _index;
};

clipboard_ptr create_simple_clipboard ()
{
	return std::make_shared<simple_clipboard_t>();
}
