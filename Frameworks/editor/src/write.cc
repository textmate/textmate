#include "write.h"
#include <text/utf8.h>
#include <text/ctype.h>
#include <oak/server.h>

namespace
{
	struct write_t
	{
		struct request_t { int fd; std::string str; };

		WATCH_LEAKS(write_t);

		write_t (int fd, std::string const& str);
		virtual ~write_t ();

		static int handle_request (write_t::request_t const& request);
		void handle_reply (int error);

	private:
		size_t client_key;
	};

	static oak::server_t<write_t>& server ()
	{
		static oak::server_t<write_t> instance;
		return instance;
	}

	write_t::write_t (int fd, std::string const& str)
	{
		client_key = server().register_client(this);
		int newFd = dup(fd);
		fcntl(newFd, F_SETFD, FD_CLOEXEC);
		server().send_request(client_key, (request_t){ newFd, str });
	}

	write_t::~write_t ()
	{
		server().unregister_client(client_key);
	}

	int write_t::handle_request (write_t::request_t const& request)
	{
		bool success = write(request.fd, request.str.data(), request.str.size()) == request.str.size();
		close(request.fd);
		return success ? 0 : errno;
	}

	void write_t::handle_reply (int error)
	{
		delete this;
	}
}

namespace ng
{
	static size_t count_columns (buffer_t const& buffer, index_t caret, size_t tabSize)
	{
		std::string const str = buffer.substr(buffer.begin(buffer.convert(caret.index).line), caret.index);
		size_t len = 0;
		citerate(ch, diacritics::make_range(str.data(), str.data() + str.size()))
			len += *ch == '\t' ? tabSize - (len % tabSize) : (text::is_east_asian_width(*ch) ? 2 : 1);
		return len + caret.carry;
	}

	text::range_t write_unit_to_fd (buffer_t const& buffer, range_t const& range, size_t tabSize, int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection) // TODO Move write_unit_to_fd to command framework.
	{
		input::type actualUnit = unit == input::selection && range.empty() ? fallbackUnit : unit;
		*inputWasSelection = actualUnit == input::selection;

		range_t r;
		switch(actualUnit)
		{
			case input::character:        r = extend_if_empty(buffer, range, kSelectionExtendRight).last();        break;
			case input::word:             r = extend_if_empty(buffer, range, kSelectionExtendToWord).last();       break;
			case input::line:             r = extend_if_empty(buffer, range, kSelectionExtendToLineExclLF).last(); break;
			case input::scope:            r = select_scope(buffer, range, scopeSelector).last();                   break;
			case input::selection:        r = range;                                                               break;
			case input::entire_document:  r = extend(buffer, range, kSelectionExtendToAll).last();                 break;
		};

		if(!r.empty())
		{
			if(format == input_format::xml)
					new write_t(fd, to_xml(buffer, r.min().index, r.max().index));
			else	new write_t(fd, buffer.substr(r.min().index, r.max().index));
		}
		close(fd);

		if(r && actualUnit != input::entire_document)
		{
			text::pos_t const& pos = buffer.convert(r.min().index);
			variables.insert(std::make_pair("TM_INPUT_START_LINE",       text::format("%zu", pos.line + 1)));
			variables.insert(std::make_pair("TM_INPUT_START_LINE_INDEX", text::format("%zu", pos.column)));
			variables.insert(std::make_pair("TM_INPUT_START_COLUMN",     text::format("%zu", count_columns(buffer, r.min(), tabSize) + 1)));
		}
		return text::range_t(buffer.convert(r.min().index), buffer.convert(r.max().index));
	}

} /* ng */
