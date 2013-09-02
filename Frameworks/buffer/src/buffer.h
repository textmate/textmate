#ifndef COMPOSITE_H_BOKD8YWS
#define COMPOSITE_H_BOKD8YWS

#include "indexed_map.h"
#include "storage.h"
#include <oak/callbacks.h>
#include <text/types.h>
#include <text/indent.h>
#include <scope/scope.h>
#include <parse/parse.h>
#include <parse/grammar.h>
#include <bundles/bundles.h>
#include <regexp/regexp.h>
#include <oak/debug.h>
#include <ns/spellcheck.h>

namespace ng
{
	struct buffer_t;

	struct PUBLIC meta_data_t
	{
		virtual ~meta_data_t ()               { }
		virtual void replace (buffer_t* buffer, size_t from, size_t to, std::string const& str) { }
		virtual void did_parse (buffer_t const* buffer, size_t from, size_t to)                 { }
	};

	struct PUBLIC pairs_t : meta_data_t
	{
		pairs_t ();

		void add_pair (size_t firstIndex, size_t lastIndex);
		void remove (size_t index);
		bool is_first (size_t index) const;
		bool is_last (size_t index) const;
		size_t counterpart (size_t index) const;

	private:
		void replace (buffer_t* buffer, size_t from, size_t to, std::string const& str);
		using meta_data_t::did_parse;

		bool is_paired (size_t index) const;

		size_t _rank;
		typedef indexed_map_t<size_t> tree_t;
		tree_t _pairs;
	};

	struct PUBLIC callback_t
	{
		WATCH_LEAKS(ng::callback_t);

		virtual ~callback_t ()                                                     { }
		virtual void did_parse (size_t from, size_t to)                            { }
		virtual void will_replace (size_t from, size_t to, std::string const& str) { }
		virtual void did_replace (size_t from, size_t to, std::string const& str)  { }
	};

	struct spelling_t;
	struct symbols_t;
	struct marks_t;
	struct lines_t;
	struct buffer_parser_t;
	typedef std::shared_ptr<buffer_parser_t> buffer_parser_ptr;

	struct PUBLIC buffer_t
	{
		WATCH_LEAKS(ng::buffer_t);

		buffer_t (char const* str = NULL);
		buffer_t (buffer_t const& rhs) = delete;
		buffer_t& operator= (buffer_t const& rhs) = delete;
		~buffer_t ();

		size_t size () const;
		bool empty () const                    { return size() == 0; }
		size_t revision () const               { return _revision; }
		size_t next_revision () const          { return _next_revision; }
		size_t bump_revision ()                { set_revision(_next_revision++); return _revision; }
		void set_revision (size_t newRevision) { ASSERT_LT(newRevision, _next_revision); _revision = newRevision; initiate_repair(20); }

		char at (size_t i) const;
		std::string operator[] (size_t i) const;
		std::string substr (size_t from, size_t to) const;

		bool operator== (buffer_t const& rhs) const;

		size_t replace (size_t from, size_t to, std::string const& str);

		size_t insert (size_t i, std::string const& str)   { return replace(i, i, str);    }
		size_t erase (size_t from, size_t to)              { return replace(from, to, ""); }

		typedef detail::storage_t::iterator iterator;

		iterator begin () const                      { return _storage.begin(); }
		iterator end () const                        { return _storage.end();   }

		size_t begin (size_t n) const                { ASSERT_LT(n, lines()); return n   ==       0 ?      0 : _hardlines.nth(n-1)->first + 1; }
		size_t eol (size_t n) const                  { ASSERT_LT(n, lines()); return n+1 == lines() ? size() : _hardlines.nth(n)->first;       }
		size_t end (size_t n) const                  { ASSERT_LT(n, lines()); return n+1 == lines() ? size() : _hardlines.nth(n)->first + 1;   }
		size_t lines () const                        { return _hardlines.size() + 1;                                            }

		size_t sanitize_index (size_t i) const;

		size_t convert (text::pos_t const& p) const  { return begin(p.line) + p.column;                                         }
		text::pos_t convert (size_t i) const         { return text::pos_t(_hardlines.lower_bound(i).index(), i - begin(_hardlines.lower_bound(i).index())); }

		text::indent_t& indent ()                         { return _indent; }
		text::indent_t const& indent () const             { return _indent; }

		bool set_grammar (bundles::item_ptr const& grammarItem);
		parse::grammar_ptr grammar () const { return _grammar; }

		scope::context_t scope (size_t i, bool includeDynamic = true) const;
		std::map<size_t, scope::scope_t> scopes (size_t from, size_t to) const;

		std::map<size_t, std::string> symbols () const;
		std::string symbol_at (size_t i) const;

		void set_live_spelling (bool flag);
		bool live_spelling () const;
		void set_spelling_language (std::string const& lang);
		std::string const& spelling_language () const;
		std::map<size_t, bool> misspellings (size_t from, size_t to) const;
		std::pair<size_t, size_t> next_misspelling (size_t from) const;
		ns::spelling_tag_t spelling_tag () const;
		void recheck_spelling (size_t from, size_t to);

		pairs_t& pairs ()              { return *_pairs.get(); }
		pairs_t const& pairs () const  { return *_pairs.get(); }

		void set_mark (size_t index, std::string const& markType);
		void remove_mark (size_t index, std::string const& markType);
		void remove_all_marks (std::string const& markType);
		std::string get_mark (size_t index, std::string const& markType) const;
		std::map<size_t, std::string> get_marks (size_t from, size_t to, std::string const& markType = NULL_STR) const;
		std::pair<size_t, std::string> next_mark (size_t index, std::string const& markType = NULL_STR) const;
		std::pair<size_t, std::string> prev_mark (size_t index, std::string const& markType = NULL_STR) const;

		void wait_for_repair ();

		// ============
		// = Callback =
		// ============

		void add_callback (callback_t* callback)     { _callbacks.add(callback); }
		void remove_callback (callback_t* callback)  { _callbacks.remove(callback); }

	private:
		void did_parse (size_t first, size_t last)
		{
			iterate(hook, _meta_data)
				(*hook)->did_parse(this, first, last);
			_callbacks(&callback_t::did_parse, first, last);
		}

		oak::callbacks_t<callback_t> _callbacks;
		std::vector<meta_data_t*> _meta_data;

		// ====================
		// = Grammar Callback =
		// ====================

		void grammar_did_change ();

		struct grammar_callback_t : parse::grammar_t::callback_t
		{
			grammar_callback_t (buffer_t& buffer) : _buffer(buffer) { }
			void grammar_did_change ()                              { _buffer.grammar_did_change(); }
		private:
			buffer_t& _buffer;
		};

		parse::grammar_ptr _grammar;
		grammar_callback_t _grammar_callback;

		// ============

		void add_meta_data (meta_data_t* hook)      { if(hook) _meta_data.push_back(hook); }
		void remove_meta_data (meta_data_t* hook)   { if(hook) _meta_data.erase(std::find(_meta_data.begin(), _meta_data.end(), hook)); }

		size_t actual_replace (size_t from, size_t to, std::string const& str);

		uint32_t code_point (size_t& i, size_t& len) const;
		friend std::string to_s (buffer_t const& buf, size_t first, size_t last);
		friend std::string to_xml (buffer_t const& buf, size_t first, size_t last);

		friend struct buffer_parser_t;
		buffer_parser_ptr parser;
		text::indent_t _indent;
		void initiate_repair (size_t limit_redraw = 0, size_t super_from = -1);
		void update_scopes (size_t limit_redraw, size_t const& super_range,std::pair<size_t, size_t> const& range, std::map<size_t, scope::scope_t> const& newScopes, parse::stack_ptr parserState);

		size_t _revision, _next_revision;
		std::string _spelling_language;
		ns::spelling_tag_t _spelling_tag;

		detail::storage_t                _storage;
		indexed_map_t<bool>              _hardlines;
		indexed_map_t<bool>              _dirty;
		indexed_map_t<scope::scope_t>    _scopes;
		indexed_map_t<parse::stack_ptr>  _parser_states;

		std::shared_ptr<spelling_t> _spelling;
		std::shared_ptr<symbols_t>  _symbols;
		std::shared_ptr<marks_t>    _marks;
		std::shared_ptr<pairs_t>    _pairs;

		friend struct spelling_t; // _scopes
		friend struct symbols_t;  // _scopes
	};

	PUBLIC std::string to_s (buffer_t const& buf, size_t first = 0, size_t last = SIZE_T_MAX);
	PUBLIC std::string to_xml (buffer_t const& buf, size_t first = 0, size_t last = SIZE_T_MAX);

} /* ng */

#endif /* end of include guard: COMPOSITE_H_BOKD8YWS */
