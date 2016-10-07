#include "info.h"
#include <oak/debug.h>

/* CrashReporter info */
char const* __crashreporter_info__ = nullptr;
asm(".desc ___crashreporter_info__, 0x10");

namespace
{
	struct stack_t
	{
		void push (std::string const& str)
		{
			_stack.push_back(str);
			update();
		}

		void pop ()
		{
			_stack.pop_back();
			update();
		}

		void assign (std::string const& str)
		{
			_stack.back() = str;
			update();
		}

		void append (std::string const& str)
		{
			_stack.back().append("\n");
			_stack.back().append(str);
			update();
		}

	private:
		void update ()
		{
			__crashreporter_info__ = nullptr;

			bool first = true;
			_description.clear();
			for(auto const& str : _stack)
			{
				if(!std::exchange(first, false))
					_description.append("\n");
				_description.append(str);
			}

			if(!_description.empty())
				__crashreporter_info__ = _description.c_str();
		}

		std::vector<std::string> _stack;
		std::string _description;
	};

	static stack_t& stack ()
	{
		thread_local stack_t stack;
		return stack;
	}
}

crash_reporter_info_t::crash_reporter_info_t (std::string const& str)
{
	stack().push(str);
}

crash_reporter_info_t::crash_reporter_info_t (char const* format, ...)
{
	char* tmp = nullptr;

	va_list ap;
	va_start(ap, format);
	vasprintf(&tmp, format, ap);
	va_end(ap);

	if(tmp)
	{
		stack().push(tmp);
		free(tmp);
	}
}

crash_reporter_info_t::~crash_reporter_info_t ()
{
	stack().pop();
}

crash_reporter_info_t& crash_reporter_info_t::operator= (std::string const& str)
{
	stack().assign(str);
	return *this;
}

crash_reporter_info_t& crash_reporter_info_t::operator<< (std::string const& str)
{
	stack().append(str);
	return *this;
}
