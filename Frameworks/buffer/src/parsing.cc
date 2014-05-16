#include "buffer.h"

OAK_DEBUG_VAR(Buffer_Parsing);

namespace ng
{
	// ===================
	// = buffer_parser_t =
	// ===================

	struct result_t
	{
		parse::stack_ptr state;
		std::map<size_t, scope::scope_t> scopes;
	};

	result_t handle_request (parse::grammar_ptr grammar, parse::stack_ptr state, std::string const& line, std::pair<size_t, size_t> range, size_t batch_start, size_t limit_redraw)
	{
		std::lock_guard<std::mutex> lock(grammar->mutex());

		result_t result;
		result.state = parse::parse(line.data(), line.data() + line.size(), state, result.scopes, range.first == 0);
		return result;
	}

	// ============
	// = buffer_t =
	// ============

	void buffer_t::initiate_repair (size_t limit_redraw, size_t batch_start)
	{
		if(!_dirty.empty() && !_parser_states.empty())
		{
			size_t n       = convert(_dirty.begin()->first).line;
			size_t from    = begin(n);
			size_t to      = end(n);
			auto stateIter = from == 0 ? _parser_states.begin() : _parser_states.find(from);
			D(DBF_Buffer_Parsing, bug("line %zu dirty, offset %zu → %zu-%zu\n", n, _dirty.begin()->first, from, to););
			if(stateIter != _parser_states.end())
			{
				if(batch_start == -1)
					batch_start = from;

				auto grammarRef = grammar();
				auto state      = stateIter->second;
				auto line       = substr(from, to);

				if(!pthread_main_np()) // ng::buffer_t is used from a test running in a global dispatch queue
				{
					result_t result = handle_request(grammarRef, state, line, { from, to }, batch_start, limit_redraw);
					update_scopes(limit_redraw, batch_start, { from, to }, result.scopes, result.state);
					return;
				}

				size_t bufferRev = revision();
				auto bufferRef   = parser_reference();

				dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
					result_t result = handle_request(grammarRef, state, line, { from, to }, batch_start, limit_redraw);
					dispatch_async(dispatch_get_main_queue(), ^{
						if(bufferRef.lock())
						{
							if(bufferRev == revision())
									update_scopes(limit_redraw, batch_start, { from, to }, result.scopes, result.state);
							else	initiate_repair();
						}
					});
				});
			}
			else
			{
				fprintf(stderr, "no parser state for %zu-%zu (%p)\n%s\n%s\n", from, to, this, substr(0, size()).c_str(), to_s(*this).c_str());
			}
		}
	}

	void buffer_t::update_scopes (size_t limit_redraw, size_t const& batch_start, std::pair<size_t, size_t> const& range, std::map<size_t, scope::scope_t> const& newScopes, parse::stack_ptr parserState)
	{
		bool atEOF = convert(range.first).line+1 == lines();
		D(DBF_Buffer_Parsing, bug("did parse %zu-%zu (revision %zu), at EOL %s\n", range.first, range.second, revision(), BSTR(atEOF)););

		_scopes.remove(_scopes.lower_bound(range.first), atEOF ? _scopes.end() : _scopes.lower_bound(range.second));
		for(auto const& pair : newScopes)
		{
			if(range.first + pair.first < range.second || atEOF)
				_scopes.set(range.first + pair.first, pair.second);
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

		_parser_reference.reset();
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

		_parser_reference.reset();
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
