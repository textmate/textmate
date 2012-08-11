#ifndef EDITOR_H_GWYNAZT0
#define EDITOR_H_GWYNAZT0

#include "clipboard.h"
#include "snippets.h"
#include <oak/callbacks.h>
#include <regexp/find.h>
#include <command/parser.h>
#include <command/runner.h>
#include <document/document.h>
#include <layout/layout.h>

namespace ng
{
	enum action_t
	{
		kMoveBackward,
		kMoveBackwardAndModifySelection,
		kMoveDown,
		kMoveDownAndModifySelection,
		kMoveForward,
		kMoveForwardAndModifySelection,
		kMoveParagraphBackwardAndModifySelection,
		kMoveParagraphForwardAndModifySelection,
		kMoveSubWordLeft,
		kMoveSubWordLeftAndModifySelection,
		kMoveSubWordRight,
		kMoveSubWordRightAndModifySelection,
		kMoveToBeginningOfColumn,
		kMoveToBeginningOfColumnAndModifySelection,
		kMoveToBeginningOfDocument,
		kMoveToBeginningOfDocumentAndModifySelection,
		kMoveToBeginningOfLine,
		kMoveToBeginningOfLineAndModifySelection,
		kMoveToBeginningOfParagraph,
		kMoveToBeginningOfParagraphAndModifySelection,
		kMoveToBeginningOfBlock,
		kMoveToBeginningOfBlockAndModifySelection,
		kMoveToEndOfColumn,
		kMoveToEndOfColumnAndModifySelection,
		kMoveToEndOfDocument,
		kMoveToEndOfDocumentAndModifySelection,
		kMoveToEndOfLine,
		kMoveToEndOfLineAndModifySelection,
		kMoveToEndOfParagraph,
		kMoveToEndOfParagraphAndModifySelection,
		kMoveToEndOfBlock,
		kMoveToEndOfBlockAndModifySelection,
		kMoveUp,
		kMoveUpAndModifySelection,
		kMoveWordBackward,
		kMoveWordBackwardAndModifySelection,
		kMoveWordForward,
		kMoveWordForwardAndModifySelection,

		kPageDown,
		kPageDownAndModifySelection,
		kPageUp,
		kPageUpAndModifySelection,

		kSelectAll,
		kSelectCurrentScope,
		kSelectBlock,
		kSelectHardLine,
		kSelectLine,
		kSelectParagraph,
		kSelectWord,
		kToggleColumnSelection,

		kFindNext,
		kFindPrevious,
		kFindAll,
		kFindAllInSelection,

		kReplace,
		kReplaceAll,
		kReplaceAllInSelection,

		kReplaceAndFind,

		kDeleteBackward,
		kDeleteForward,
		kDeleteSubWordLeft,
		kDeleteSubWordRight,
		kDeleteToBeginningOfLine,
		kDeleteToBeginningOfParagraph,
		kDeleteToEndOfLine,
		kDeleteToEndOfParagraph,
		kDeleteWordBackward,
		kDeleteWordForward,
		kDeleteBackwardByDecomposingPreviousCharacter,
		kDeleteSelection,

		kCut,
		kCopy,
		kCopySelectionToFindPboard,
		kCopySelectionToReplacePboard,
		kPaste,
		kPastePrevious,
		kPasteNext,
		kPasteWithoutReindent,
		kYank,

		kCapitalizeWord,
		kChangeCaseOfLetter,
		kChangeCaseOfWord,
		kLowercaseWord,
		kReformatText,
		kReformatTextAndJustify,
		kShiftLeft,
		kShiftRight,
		kTranspose,
		kTransposeWords,
		kUnwrapText,
		kUppercaseWord,

		kSetMark,
		kDeleteToMark,
		kSelectToMark,
		kSwapWithMark,

		kComplete,
		kNextCompletion,
		kPreviousCompletion,

		kInsertBacktab,
		kInsertTab,
		kInsertTabIgnoringFieldEditor,
		kInsertNewline,
		kInsertNewlineIgnoringFieldEditor,

		kIndent,

		kMoveSelectionUp,
		kMoveSelectionDown,
		kMoveSelectionLeft,
		kMoveSelectionRight,

		kNop
	};

	PUBLIC action_t to_action (std::string const& sel);

	struct PUBLIC editor_t
	{
		editor_t ();
		editor_t (buffer_t& buffer);
		editor_t (document::document_ptr document);

		void perform (action_t action, layout_t const* layout = NULL);

		void insert (std::string const& str, bool selectInsertion = false);
		void insert_with_pairing (std::string const& str);
		void move_selection_to (ng::index_t const& index, bool selectInsertion = true);
		ranges_t replace (std::string const& searchFor, std::string const& replaceWith, find::options_t options = find::none, bool searchOnlySelection = false);
		void delete_tab_trigger (std::string const& str);

		void macro_dispatch (plist::dictionary_t const& args, std::map<std::string, std::string> const& variables);
		void find_dispatch (plist::dictionary_t const& args);
		void snippet_dispatch (plist::dictionary_t const& args, std::map<std::string, std::string> const& variables);
		void execute_dispatch (plist::dictionary_t const& args, std::map<std::string, std::string> const& variables);

		scope::context_t scope () const;
		std::map<std::string, std::string> variables (std::map<std::string, std::string> map) const;

		std::vector<std::string> const& choices () const;
		std::string placeholder_content (ng::range_t* placeholderSelection = NULL) const;
		void set_placeholder_content (std::string const& str, size_t selectFrom);

		ranges_t ranges () const                                              { return _selections; }
		void set_selections (ranges_t const& r)                               { _selections = r; }
		bool has_selection () const                                           { return not_empty(_buffer, _selections); }
		std::string as_string (size_t from = 0, size_t to = SIZE_T_MAX) const { return _buffer.substr(from, to != SIZE_T_MAX ? to : _buffer.size()); }

		void perform_replacements (std::multimap<text::range_t, std::string> const& replacements);
		bool handle_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, text::range_t input_range, std::map<std::string, std::string> environment);

		// ==============
		// = Clipboards =
		// ==============

		clipboard_ptr clipboard () const              { ASSERT(_clipboard);           return _clipboard; }
		clipboard_ptr find_clipboard () const         { ASSERT(_find_clipboard);      return _find_clipboard; }
		clipboard_ptr replace_clipboard () const      { ASSERT(_replace_clipboard);   return _replace_clipboard; }
		clipboard_ptr yank_line_clipboard () const    { ASSERT(_yank_line_clipboard); return _yank_line_clipboard; }

		void set_clipboard (clipboard_ptr cb)           { _clipboard = cb; }
		void set_find_clipboard (clipboard_ptr cb)      { _find_clipboard = cb; }
		void set_replace_clipboard (clipboard_ptr cb)   { _replace_clipboard = cb; }
		void set_yank_line_clipboard (clipboard_ptr cb) { _yank_line_clipboard = cb; }

	private:
		void setup ();
		friend struct indent_helper_t;

		static size_t visual_distance (ng::buffer_t const& buffer, index_t first, index_t last);
		static index_t visual_advance (ng::buffer_t const& buffer, index_t caret, size_t distance);

		static ng::ranges_t insert_tab_with_indent (ng::buffer_t& buffer, ng::ranges_t const& selections, snippet_controller_t& snippets);
		static ng::ranges_t insert_newline_with_indent (ng::buffer_t& buffer, ng::ranges_t const& selections, snippet_controller_t& snippets);

		static clipboard_t::entry_ptr copy (ng::buffer_t const& buffer, ng::ranges_t const& selections);
		static ng::ranges_t paste (ng::buffer_t& buffer, ng::ranges_t const& selections, snippet_controller_t& snippets, clipboard_t::entry_ptr entry);

		// ==============
		// = Completion =
		// ==============

		struct completion_info_t
		{
			size_t revision () const                                           { return _revision; }
			void set_revision (size_t rev)                                     { _revision = rev;  }

			ng::ranges_t const& ranges () const                                { return _ranges;      }
			void set_ranges (ng::ranges_t const& ranges)                       { _ranges = ranges; }

			ng::ranges_t const& prefix_ranges () const                         { return _prefix_ranges;   }
			void set_prefix_ranges (ng::ranges_t const& prefixRanges)          { _prefix_ranges = prefixRanges; }

			std::vector<std::string> const& suggestions () const               { return _suggestions; }
			void set_suggestions (std::vector<std::string> const& suggestions) { _suggestions = suggestions; _index = _suggestions.size(); }

			std::string const& current () const                                { ASSERT_LT(_index, _suggestions.size()); return _suggestions[_index]; }

			void advance ()                                                    { if(++_index >= _suggestions.size()) _index = 0;  }
			void retreat ()                                                    { if(--_index < 0) _index = _suggestions.size()-1; }

      private:
			size_t _revision = 0;
			ng::ranges_t _ranges;

			ng::ranges_t _prefix_ranges;
			std::vector<std::string> _suggestions;

			ssize_t _index = 0;
		};

		std::vector<std::string> completions (size_t bow, size_t eow, std::string const& prefix, std::string const& suffix);
		bool setup_completion ();
		void next_completion ();
		void previous_completion ();

		// ============
		// = Snippets =
		// ============

		void snippet (std::string const& str, std::map<std::string, std::string> const& variables);
		ranges_t snippet (size_t from, size_t to, std::string const& str, std::map<std::string, std::string> const& variables);

		void find (std::string const& searchFor, find::options_t options = find::none, bool searchOnlySelection = false);
		ranges_t replace (std::multimap<range_t, std::string> const& replacements, bool selectInsertions = false);
		void move_selection (int deltaX, int deltaY);

		// ===============
		// = Member data =
		// ===============

		buffer_t& _buffer;
		ng::ranges_t _selections = ranges_t(0);
		snippet_controller_t _snippets;

		completion_info_t _completion_info;

		clipboard_ptr _clipboard;
		clipboard_ptr _find_clipboard;
		clipboard_ptr _replace_clipboard;
		clipboard_ptr _yank_line_clipboard;

		document::document_ptr _document;
	};

	typedef std::tr1::shared_ptr<editor_t> editor_ptr;

	PUBLIC editor_ptr editor_for_document (document::document_ptr document);

} /* ng */

#endif /* end of include guard: EDITOR_H_GWYNAZT0 */
