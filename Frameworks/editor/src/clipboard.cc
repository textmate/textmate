#include "clipboard.h"

std::map<std::string, std::string> const& clipboard_t::entry_t::options () const
{
	static std::map<std::string, std::string> const dummy;
	return dummy;
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
