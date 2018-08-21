#include "clipboard.h"
#include <text/utf8.h>
#include <text/hexdump.h>

clipboard_t::entry_t::entry_t (std::string const& content, std::map<std::string, std::string> const& options) : _content(content), _options(options)
{
	if(!utf8::is_valid(_content.begin(), _content.end()))
	{
		std::string const prefix = "*** malformed UTF-8 data on clipboard:\n";
		_content = prefix + text::to_hex(_content.begin(), _content.end()) + "\n";
	}
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
