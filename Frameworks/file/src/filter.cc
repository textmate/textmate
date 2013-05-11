#include "filter.h"
#include "path_info.h"
#include <command/parser.h>
#include <command/runner.h>
#include <text/utf8.h>
#include <regexp/regexp.h>
#include <oak/server.h>
#include <oak/debug.h>

static std::vector<bundles::item_ptr> binary_filters (std::string const& event, std::string const& pathAttributes, io::bytes_ptr content)
{
	std::string contentAsString = "";
	size_t contentMatchSize = 256;
	foreach(ch, content->begin(), content->end())
	{
		contentAsString += utf8::to_s(*ch);
		if(--contentMatchSize == 0)
			break;
	}

	std::multimap<ssize_t, bundles::item_ptr> ordering;
	citerate(item, bundles::query(bundles::kFieldSemanticClass, event, pathAttributes, bundles::kItemTypeCommand))
	{
		citerate(pattern, (*item)->values_for_field(bundles::kFieldContentMatch))
		{
			if(regexp::match_t const& m = regexp::search(*pattern, contentAsString))
				ordering.insert(std::make_pair(-m.end(), *item));
		}
	}
	return ordering.empty() ? std::vector<bundles::item_ptr>() : std::vector<bundles::item_ptr>(1, ordering.begin()->second);
}

static std::vector<bundles::item_ptr> path_filters (std::string const& event, std::string const& pathAttributes)
{
	return bundles::query(bundles::kFieldSemanticClass, event, pathAttributes, bundles::kItemTypeCommand);
}

// ==================
// = Threaded Write =
// ==================

namespace filter
{
	struct write_t
	{
		struct request_t { int fd; io::bytes_ptr bytes; };

		WATCH_LEAKS(write_t);

		write_t (int fd, io::bytes_ptr const& bytes);
		virtual ~write_t ();

		static int handle_request (write_t::request_t const& request);
		void handle_reply (int error);

	private:
		size_t _client_key;
	};

	static oak::server_t<write_t>& write_server ()
	{
		static oak::server_t<write_t> server;
		return server;
	}

	write_t::write_t (int fd, io::bytes_ptr const& bytes)
	{
		_client_key = write_server().register_client(this);
		int newFd = dup(fd);
		fcntl(newFd, F_SETFD, FD_CLOEXEC);
		write_server().send_request(_client_key, (request_t){ newFd, bytes });
	}

	write_t::~write_t ()
	{
		write_server().unregister_client(_client_key);
	}

	int write_t::handle_request (write_t::request_t const& request)
	{
		bool success = write(request.fd, request.bytes->get(), request.bytes->size()) == request.bytes->size();
		close(request.fd);
		return success ? 0 : errno;
	}

	void write_t::handle_reply (int error)
	{
		delete this;
	}
	
} /* filter */

// ==========================
// = Filter Runner Delegate =
// ==========================

namespace
{
	struct event_delegate_t : command::delegate_t
	{
		event_delegate_t (io::bytes_ptr input, filter::callback_ptr context) : _input(input), _context(context) { }

		bool accept_html_data (command::runner_ptr runner, char const* data, size_t len)   { return fprintf(stderr, "html: %.*s", (int)len, data), false; }
		void show_document (std::string const& str)                                        { fprintf(stderr, "document: %s\n", str.c_str()); }
		void show_tool_tip (std::string const& str)                                        { fprintf(stderr, "tool tip: %s\n", str.c_str()); }
		void show_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err) { _context->filter_error(command, rc, out, err); }

		text::range_t write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection);
		bool accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, text::range_t inputRange, std::map<std::string, std::string> const& environment);

	private:
		io::bytes_ptr _input;
		filter::callback_ptr _context;
	};

	text::range_t event_delegate_t::write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection)
	{
		if(unit != input::entire_document || format != input_format::text)
			return fprintf(stderr, "*** write unit to fd: unhandled unit/format: %d/%d\n", unit, format), text::range_t::undefined;

		new filter::write_t(fd, _input);
		return text::range_t::undefined;
	}

	bool event_delegate_t::accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, text::range_t inputRange, std::map<std::string, std::string> const& environment)
	{
		if(placement != output::replace_document || format != output_format::text)
			return fprintf(stderr, "*** unhandled placement/format (%d/%d): %s\n", placement, format, out.c_str()), false;

		_context->set_content(io::bytes_ptr(new io::bytes_t(out)));
		return true;
	}
}

namespace filter
{
	std::string const kBundleEventBinaryImport  = "callback.document.binary-import";
	std::string const kBundleEventBinaryExport  = "callback.document.binary-export";
	std::string const kBundleEventTextImport    = "callback.document.import";
	std::string const kBundleEventTextExport    = "callback.document.export";

	std::vector<bundles::item_ptr> find (std::string const& path, io::bytes_ptr content, std::string const& pathAttributes, std::string const& event)
	{
		if(event == kBundleEventBinaryImport)
			return binary_filters(event, pathAttributes, content);
		else if(event == kBundleEventBinaryExport || event == kBundleEventTextImport || event == kBundleEventTextExport)
			return path_filters(event, pathAttributes);
		return std::vector<bundles::item_ptr>();
	}

	void run (bundles::item_ptr filter, std::string const& path, io::bytes_ptr content, callback_ptr context)
	{
		std::map<std::string, std::string> variables = file::path_variables(path);
		command::runner_ptr runner = command::runner(parse_command(filter), ng::buffer_t(), ng::ranges_t(), bundles::scope_variables(variables << filter->bundle_variables(), file::path_attributes(path)), command::delegate_ptr(new event_delegate_t(content, context)));
		runner->launch();
	}

} /* filter */
