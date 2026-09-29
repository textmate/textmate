#include "clipboard.h"
#include <text/src/utf8.h>
#include <text/src/hexdump.h>
#include <text/src/format.h>

std::string const kClipboardOptionIndent    = "indent";
std::string const kClipboardOptionComplete  = "complete";
std::string const kClipboardOptionColumnar  = "columnar";

clipboard_t::entry_t::entry_t (std::string const& content, std::map<std::string, std::string> const& options) : _contents(1, content), _options(options)
{
}

clipboard_t::entry_t::entry_t (std::vector<std::string> const& contents, std::map<std::string, std::string> const& options) : _contents(contents), _options(options)
{
}

static std::map<std::string, std::string> create_clipboard_options (std::string const& indent, bool complete, bool columnar)
{
	std::map<std::string, std::string> res;
	if(indent != NULL_STR) res[kClipboardOptionIndent]    = indent;
	if(complete)           res[kClipboardOptionComplete]  = "1";
	if(columnar)           res[kClipboardOptionColumnar]  = "1";
	return res;
}

clipboard_t::entry_t::entry_t (std::vector<std::string> const& contents, std::string const& indent, bool complete, bool columnar) : entry_t(contents, create_clipboard_options(indent, complete, columnar))
{
}

std::string clipboard_t::entry_t::content () const
{
	std::string res = _contents.size() == 1 ? _contents.back() : text::join(_contents, "\n");
	if(!utf8::is_valid(res.begin(), res.end()))
	{
		std::string const prefix = "*** malformed UTF-8 data on clipboard:\n";
		res = prefix + text::to_hex(res.begin(), res.end()) + "\n";
	}
	return res;
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
