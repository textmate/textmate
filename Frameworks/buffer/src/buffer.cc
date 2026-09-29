#include "buffer.h"
#include "meta_data.h"
#include <oak/oak.h>
#include <text/src/utf8.h>
#include <text/src/parse.h>
#include <regexp/src/format_string.h>
#include <parse/src/grammar.h>

namespace ng
{
	buffer_t::buffer_t () : _grammar_callback(*this), _revision(0), _next_revision(1), _spelling_language("")
	{
		_meta_data.push_back((_symbols = std::make_shared<symbols_t>()).get());
		_meta_data.push_back((_marks = std::make_shared<marks_t>()).get());
		_meta_data.push_back((_pairs = std::make_shared<pairs_t>()).get());
		_scopes.set(-1, "text");
	}

	buffer_t::buffer_t (char const* str) : buffer_t()
	{
		insert(0, str, strlen(str));
	}

	buffer_t::~buffer_t ()
	{
		if(_grammar)
			_grammar->remove_callback(&_grammar_callback);
	}

	size_t buffer_t::size () const
	{
		return _storage.size();
	}

	char buffer_t::at (size_t i) const
	{
		return _storage.substr(i, i+1)[0];
	}

	uint32_t buffer_t::code_point (size_t& i, size_t& len) const
	{
		len = 1;
		char ch = at(i);
		if(!utf8::multibyte<char>::partial(ch))
			return ch;

		while(!utf8::multibyte<char>::is_start(ch))
		{
			ASSERT(0 < i);
			ch = at(--i);
		}

		len = utf8::multibyte<char>::length(ch);
		return utf8::to_ch(_storage.substr(i, i + len));
	}

	static bool is_non_base (uint32_t ch)
	{
		static CFCharacterSetRef const NonBaseSet = CFCharacterSetGetPredefined(kCFCharacterSetNonBase);
		return 0xFF < ch && CFCharacterSetIsLongCharacterMember(NonBaseSet, ch);
	}

	size_t buffer_t::sanitize_index (size_t i) const
	{
		if(size() <= i)
			return size();
		while(utf8::multibyte<char>::partial(at(i)) && !utf8::multibyte<char>::is_start(at(i)))
			--i;
		uint32_t codePoint = utf8::to_ch(_storage.substr(i, i + (utf8::multibyte<char>::is_start(at(i)) ? utf8::multibyte<char>::length(at(i)) : 1)));
		return is_non_base(codePoint) ? sanitize_index(i-1) : i;
	}

	std::string buffer_t::operator[] (size_t i) const
	{
		if(i == size())
			return "";

		size_t from     = i;
		size_t totalLen = 1;
		uint32_t ch     = code_point(from, totalLen);

		while(from + totalLen < size())
		{
			size_t next = from + totalLen, nextLen;
			if(is_non_base(code_point(next, nextLen)))
					totalLen += nextLen;
			else	break;
		}

		while(is_non_base(ch))
		{
			ASSERT(from);
			size_t len;
			ch = code_point(--from, len);
			totalLen += len;
		}

		return _storage.substr(from, from + totalLen);
	}

	std::string buffer_t::substr (size_t from, size_t to) const
	{
		return _storage.substr(from, to);
	}

	bool buffer_t::visit_data (std::function<void(char const*, size_t, size_t, bool*)> const& f) const
	{
		size_t offset = 0;
		for(auto memory : _storage)
		{
			bool stop = false;
			f(memory.data(), offset, memory.size(), &stop);
			if(stop)
				return true;
			offset += memory.size();
		}
		return false;
	}

	bool buffer_t::operator== (buffer_t const& rhs) const
	{
		return _storage == rhs._storage;
	}

	size_t buffer_t::replace (size_t from, size_t to, char const* buf, size_t len)
	{
		size_t shrinkLeft  = 0;
		size_t shrinkRight = 0;

		while(from + shrinkLeft < to && shrinkLeft < len && _storage[from + shrinkLeft] == buf[shrinkLeft])
			++shrinkLeft;

		if(shrinkLeft < len)
		{
			while(shrinkLeft && utf8::multibyte<char>::partial(buf[shrinkLeft]) && !utf8::multibyte<char>::is_start(buf[shrinkLeft]))
				--shrinkLeft;
		}

		while(from + shrinkLeft < to - shrinkRight && len - shrinkLeft - shrinkRight && _storage[to - shrinkRight - 1] == buf[len - shrinkRight - 1])
			++shrinkRight;

		while(shrinkRight && utf8::multibyte<char>::partial(buf[len - shrinkRight]) && !utf8::multibyte<char>::is_start(buf[len - shrinkRight]))
			--shrinkRight;

		if(shrinkLeft == 0 && shrinkRight == 0)
			return actual_replace(from, to, buf, len);

		actual_replace(from + shrinkLeft, to - shrinkRight, buf + shrinkLeft, len - shrinkLeft - shrinkRight);
		return from + len;
	}

	size_t buffer_t::actual_replace (size_t from, size_t to, char const* buf, size_t len)
	{
		ASSERT_LE(from, to); ASSERT_LE(to, size());
		_callbacks(&callback_t::will_replace, from, to, buf, len);

		_storage.erase(from, to);
		_storage.insert(from, buf, len);

		_dirty.replace(from, to, len, false);
		_dirty.set(from, true);

		_hardlines.replace(from, to, len);
		auto scopeIter = _scopes.upper_bound(to);
		scope::scope_t preserveScope = scopeIter != _scopes.begin() && from < (--scopeIter)->first && scopeIter->first <= to ? scopeIter->second : scope::scope_t();
		_scopes.replace(from, to, len);
		if(preserveScope)
			_scopes.set(from + len, preserveScope);
		_parser_states.replace(from, to, len, false);

		for(size_t i = 0; i < len; ++i)
		{
			if(buf[i] == '\n')
				_hardlines.set(from + i, true);
		}

		for(auto const& hook : _meta_data)
			hook->replace(this, from, to, len);

		_callbacks(&callback_t::did_replace, from, to, buf, len);
		return from + len;
	}

	bool buffer_t::set_grammar (bundles::item_ptr const& grammarItem)
	{
		if(_grammar)
			_grammar->remove_callback(&_grammar_callback);
		_grammar.reset();
		_scopes.clear();
		_parser_states.clear();
		_dirty.clear();

		std::string rootScope = NULL_STR;
		plist::get_key_path(grammarItem->plist(), bundles::kFieldGrammarScope, rootScope);
		_scopes.set(-1, rootScope);

		if(parse::grammar_ptr grammar = parse::parse_grammar(grammarItem))
		{
			_grammar = grammar;
			_grammar->add_callback(&_grammar_callback);
			_parser_states.set(-1, grammar->seed());
			_dirty.set(0, true);

			initiate_repair(10);

			return true;
		}
		return false;
	}

	void buffer_t::grammar_did_change ()
	{
		set_grammar(bundles::lookup(_grammar->uuid()));
	}

	scope::context_t buffer_t::scope (size_t i, bool includeDynamic) const
	{
		if(_scopes.empty())
			return scope::context_t();

		auto first = _scopes.lower_bound(i);
		auto last  = _scopes.find(i);
		if(first != _scopes.begin())
			--first;
		if(last == _scopes.end())
			last = first;

		scope::scope_t left  = first->second;
		scope::scope_t right = last->second;

		if(_spelling && includeDynamic)
		{
			if(i && _spelling->misspelled_at(i-1))
				left.push_scope("dyn.misspelled");
			if(i+i < size() && _spelling->misspelled_at(i))
				right.push_scope("dyn.misspelled");
		}

		return scope::context_t(left, right);
	}

	std::map<size_t, scope::scope_t> buffer_t::scopes (size_t from, size_t to) const
	{
		ASSERT_LE(from, to); ASSERT_LE(to, size());
		std::map<size_t, scope::scope_t> scopes;
		auto first = _scopes.upper_bound(from);
		auto last  = _scopes.upper_bound(to);
		if(first != _scopes.begin() && (first == _scopes.end() || (ssize_t)from < first->first))
		{
			auto it = first;
			scopes.emplace(0, (--it)->second);
		}
		std::transform(first, last, std::inserter(scopes, scopes.end()), [&from](std::pair<ssize_t, scope::scope_t> const& pair){
			return std::make_pair(pair.first - from, pair.second);
		});

		std::map<size_t, bool> const m = misspellings(from, to);

		auto scopeIter       = scopes.begin();
		auto spellingIter    = m.begin();
		scope::scope_t scope = scopeIter->second;
		bool misspelled      = false;

		while(spellingIter != m.end() || (misspelled && scopeIter != scopes.end()))
		{
			if(spellingIter == m.end())
			{
				scopeIter->second.push_scope("dyn.misspelled");
				++scopeIter;
			}
			else if(scopeIter == scopes.end() || spellingIter->first < scopeIter->first)
			{
				if((misspelled = spellingIter->second) && scope.back() != "dyn.misspelled")
					scope.push_scope("dyn.misspelled");
				else if(scope.back() == "dyn.misspelled")
					scope.pop_scope();
				scopes.emplace(spellingIter->first, scope);
				++spellingIter;
			}
			else
			{
				if(scopeIter->first == spellingIter->first)
				{
					misspelled = spellingIter->second;
					++spellingIter;
				}

				if(misspelled)
					scopeIter->second.push_scope("dyn.misspelled");
				scope = scopeIter->second;
				++scopeIter;
			}
		}

		return scopes;
	}

	// =============
	// = Meta data =
	// =============

	std::map<size_t, std::string> buffer_t::symbols () const    { return _symbols->symbols(this);      }
	std::string buffer_t::symbol_at (size_t i) const            { return _symbols->symbol_at(this, i); }

	// ==================
	// = Spell Checking =
	// ==================

	bool buffer_t::live_spelling () const                                          { return _spelling ? true : false; }
	std::string const& buffer_t::spelling_language () const                        { return _spelling_language; }
	std::map<size_t, bool> buffer_t::misspellings (size_t from, size_t to) const   { return _spelling ? _spelling->misspellings(this, from, to) : std::map<size_t, bool>(); }
	std::pair<size_t, size_t> buffer_t::next_misspelling (size_t from) const       { return _spelling ? _spelling->next_misspelling(from) : std::pair<size_t, size_t>(0, 0); }
	ns::spelling_tag_t buffer_t::spelling_tag () const                             { return _spelling_tag; }
	void buffer_t::recheck_spelling (size_t from, size_t to)                       { if(_spelling) _spelling->recheck(this, from, to); }

	void buffer_t::set_live_spelling (bool flag)
	{
		remove_meta_data(_spelling.get());
		_spelling.reset(flag ? new spelling_t : nullptr);
		add_meta_data(_spelling.get());

		if(flag && _spelling)
			_spelling->recheck(this, 0, size());
	}

	void buffer_t::set_spelling_language (std::string const& lang)
	{
		if(lang != _spelling_language)
		{
			_spelling_language = lang;
			if(_spelling)
				_spelling->recheck(this, 0, size());
		}
	}

	// =========
	// = Marks =
	// =========

	void buffer_t::set_mark (size_t index, std::string const& markType, std::string const& value)                 { return _marks->set(index, markType, value); }
	void buffer_t::remove_mark (size_t index, std::string const& markType)                                        { return _marks->remove(index, markType); }
	void buffer_t::remove_all_marks (std::string const& markType)                                                 { return _marks->remove_all(markType); }
	std::string buffer_t::get_mark (size_t index, std::string const& markType) const                              { return _marks->get(index, markType); }
	std::multimap<size_t, std::pair<std::string, std::string>> buffer_t::get_marks (size_t from, size_t to) const { return _marks->get_range(from, to); }
	std::map<size_t, std::string> buffer_t::get_marks (size_t from, size_t to, std::string const& markType) const { return _marks->get_range(from, to, markType); }
	std::pair<size_t, std::string> buffer_t::next_mark (size_t index, std::string const& markType) const          { return _marks->next(index, markType); }
	std::pair<size_t, std::string> buffer_t::prev_mark (size_t index, std::string const& markType) const          { return _marks->prev(index, markType); }

	// ========
	// = to_s =
	// ========

	std::string to_s (buffer_t const& buf, size_t from, size_t to)
	{
		to = to != SIZE_T_MAX ? to : buf.size();
		std::string res = "";

		auto first = buf._scopes.upper_bound(from);
		auto last  = buf._scopes.lower_bound(to);
		if(first != buf._scopes.begin())
			--first;

		scope::scope_t lastScope;
		for(auto it = first; it != last; )
		{
			res.append(xml_difference(lastScope, it->second, "«", "»"));
			size_t pos = std::max<ssize_t>(from, it->first);
			lastScope = it->second;
			++it;
			res.append(buf.substr(pos, it == last ? to : it->first));
		}

		return res + xml_difference(lastScope, scope::scope_t(), "«", "»");
	}

	std::string buffer_t::xml_substr (size_t from, size_t to) const
	{
		to = to != SIZE_T_MAX ? to : size();
		std::string res = "";

		auto first = _scopes.upper_bound(from);
		auto last  = _scopes.lower_bound(to);
		if(first != _scopes.begin())
			--first;

		scope::scope_t lastScope;
		for(auto it = first; it != last; )
		{
			res.append(xml_difference(lastScope, it->second, "<", ">"));
			size_t pos = std::max<ssize_t>(from, it->first);
			lastScope = it->second;
			++it;
			res.append(format_string::replace(substr(pos, it == last ? to : it->first), "(<)|&", "&${1:?lt:amp};"));
		}
		return res + xml_difference(lastScope, scope::scope_t(), "<", ">");
	}

} /* ng */
