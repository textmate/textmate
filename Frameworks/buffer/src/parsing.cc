#include "buffer.h"
#include <oak/server.h>

OAK_DEBUG_VAR(Buffer_Parsing);

namespace ng
{
	struct buffer_parser_t
	{
		WATCH_LEAKS(buffer_parser_t);

		struct request_t
		{
			parse::grammar_ptr grammar;
			parse::stack_ptr state;
			std::string line;
			std::pair<size_t, size_t> range;
			size_t batch_start;
			size_t limit_redraw;
		};

		struct result_t
		{
			parse::stack_ptr state;
			std::map<size_t, scope::scope_t> scopes;
			std::pair<size_t, size_t> range;
			size_t batch_start;
			size_t limit_redraw;
		};

		buffer_parser_t (buffer_t& buffer, parse::stack_ptr const& parserState, std::string const& line, std::pair<size_t, size_t> const& range, size_t const& batch_start, size_t limit_redraw);
		~buffer_parser_t ();
		static result_t handle_request (request_t const& request);
		void handle_reply (result_t const& result);

	private:
		size_t _client_key;
		buffer_t& _buffer;
		size_t _revision;
	};

	static oak::server_t<buffer_parser_t>& server ()
	{
		static oak::server_t<buffer_parser_t> server;
		return server;
	}

	// ===================
	// = buffer_parser_t =
	// ===================

	buffer_parser_t::buffer_parser_t (buffer_t& buffer, parse::stack_ptr const& parserState, std::string const& line, std::pair<size_t, size_t> const& range, size_t const& batch_start, size_t limit_redraw) : _buffer(buffer)
	{
		_client_key = server().register_client(this);
		_revision = buffer.revision();
		server().send_request(_client_key, (request_t){ buffer.grammar(), parserState, line, range, batch_start, limit_redraw });
	}

	buffer_parser_t::~buffer_parser_t ()
	{
		server().unregister_client(_client_key);
	}

	buffer_parser_t::result_t buffer_parser_t::handle_request (request_t const& request)
	{
		std::lock_guard<std::mutex> lock(request.grammar->mutex());

		result_t result;
		result.state = parse::parse(request.line.data(), request.line.data() + request.line.size(), request.state, result.scopes, request.range.first == 0);
		result.range = request.range;
		result.batch_start = request.batch_start;
		result.limit_redraw = request.limit_redraw;
		return result;
	}

	void buffer_parser_t::handle_reply (result_t const& result)
	{
		if(_buffer.revision() == _revision)
				_buffer.update_scopes(result.limit_redraw, result.batch_start, result.range, result.scopes, result.state);
		else	_buffer.initiate_repair();
	}

	// ============
	// = buffer_t =
	// ============

	void buffer_t::initiate_repair (size_t limit_redraw, size_t batch_start)
	{
		if(!_dirty.empty() && !_parser_states.empty())
		{
			size_t n    = convert(_dirty.begin()->first).line;
			size_t from = begin(n);
			size_t to   = end(n);
			auto state  = from == 0 ? _parser_states.begin() : _parser_states.find(from);
			D(DBF_Buffer_Parsing, bug("line %zu dirty, offset %zu → %zu-%zu\n", n, _dirty.begin()->first, from, to););
			if(state != _parser_states.end())
					parser = std::make_shared<buffer_parser_t>(*this, state->second, substr(from, to), std::make_pair(from, to), batch_start ==-1? from : batch_start, limit_redraw);
			else	fprintf(stderr, "no parser state for %zu-%zu (%p)\n%s\n%s\n", from, to, this, substr(0, size()).c_str(), to_s(*this).c_str());
		}
	}

	void buffer_t::update_scopes (size_t limit_redraw, size_t const& batch_start, std::pair<size_t, size_t> const& range, std::map<size_t, scope::scope_t> const& newScopes, parse::stack_ptr parserState)
	{
		bool atEOF = convert(range.first).line+1 == lines();
		D(DBF_Buffer_Parsing, bug("did parse %zu-%zu (revision %zu), at EOL %s\n", range.first, range.second, revision(), BSTR(atEOF)););

		_scopes.remove(_scopes.lower_bound(range.first), atEOF ? _scopes.end() : _scopes.lower_bound(range.second));
		iterate(pair, newScopes)
		{
			if(range.first + pair->first < range.second || atEOF)
				_scopes.set(range.first + pair->first, pair->second);
		}

		_dirty.remove(_dirty.lower_bound(range.first), atEOF ? _dirty.end() : _dirty.lower_bound(range.second));
		bool next_line_needs_update = false;
		if((_parser_states.find(range.second) == _parser_states.end() || !parse::equal(parserState, (_parser_states.find(range.second)->second))))
		{
			_parser_states.set(range.second, parserState);
			if(!atEOF)
			{
				next_line_needs_update = true;
				_dirty.set(range.second, true);
			}
		}

		parser.reset();
		if(!next_line_needs_update || limit_redraw == 0)
		{
			did_parse(batch_start, range.second);
			initiate_repair(10);
		}
		else
		{
			initiate_repair(limit_redraw == 0 ? 0 : limit_redraw-1, batch_start);
		}
	}

	void buffer_t::wait_for_repair ()
	{
		if(!grammar())
			return;

		parser.reset();
		std::lock_guard<std::mutex> lock(grammar()->mutex());
		while(!_dirty.empty() && !_parser_states.empty())
		{
			size_t n    = convert(_dirty.begin()->first).line;
			size_t from = begin(n);
			size_t to   = end(n);
			auto state  = from == 0 ? _parser_states.begin() : _parser_states.find(from);
			if(state == _parser_states.end())
			{
				fprintf(stderr, "*** no parser state for %zu-%zu (%p)\n%s\n", from, to, this, substr(0, size()).c_str());
				break;
			}

			std::string const line = substr(from, to);
			std::map<size_t, scope::scope_t> newScopes;
			auto newState = parse::parse(line.data(), line.data() + line.size(), state->second, newScopes, from == 0);
			update_scopes(0, from, std::make_pair(from, to), newScopes, newState);
		}
	}

} /* ng */