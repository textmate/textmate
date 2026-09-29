#ifndef EDITOR_H_GWYNAZT0
#define EDITOR_H_GWYNAZT0

#include "clipboard.h"
#include "snippets.h"
#include <oak/callbacks.h>
#include <regexp/src/find.h>
#include <command/src/parser.h>
#include <selection/src/selection.h>

namespace ng
{
	find::options_t convert (std::map<std::string, std::string> const& options);

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
		kMoveToBeginningOfIndentedLine,
		kMoveToBeginningOfIndentedLineAndModifySelection,
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
		kMoveToEndOfIndentedLine,
		kMoveToEndOfIndentedLineAndModifySelection,
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
		kDeselectLast,

		kFindNext,
		kFindPrevious,
		kFindNextAndModifySelection,
		kFindPreviousAndModifySelection,
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
		kDeleteToBeginningOfIndentedLine,
		kDeleteToBeginningOfLine,
		kDeleteToBeginningOfParagraph,
		kDeleteToEndOfIndentedLine,
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
		kCopySelectionToYankPboard,
		kAppendSelectionToYankPboard,
		kPrependSelectionToYankPboard,
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

	action_t to_action (std::string const& sel);

	struct editor_delegate_t
	{
		virtual ~editor_delegate_t () { }
		virtual std::map<std::string, std::string> variables_for_bundle_item (bundles::item_ptr item) = 0;
	};

	enum indent_correction_t { kIndentCorrectNever, kIndentCorrectNonEmptyLines, kIndentCorrectAlways };

	struct editor_t
	{
		editor_t ();
		editor_t (buffer_t& buffer);

		editor_delegate_t* delegate () const            { return _delegate; }
		void set_delegate (editor_delegate_t* delegate) { _delegate = delegate; }

		void perform (action_t action, layout_movement_t const* layout = NULL, indent_correction_t indentCorrections = kIndentCorrectAlways, std::string const& scopeAttributes = NULL_STR);

		bool disallow_tab_expansion () const;

		void insert (std::string const& str, bool selectInsertion = false);
		void insert_with_pairing (std::string const& str, indent_correction_t indentCorrections, bool autoPairing, std::string const& scopeAttributes = NULL_STR);
		void move_selection_to (ng::index_t const& index, bool selectInsertion = true);
		ranges_t replace_all (std::string const& searchFor, std::string const& replaceWith, find::options_t options = find::none, bool searchOnlySelection = false);
		void delete_tab_trigger (std::string const& str);

		void macro_dispatch (plist::dictionary_t const& args, std::map<std::string, std::string> const& variables, std::function<void(bundle_command_t const&, ng::buffer_api_t const&, ng::ranges_t const&, std::map<std::string, std::string> const&)> const& runner);
		void find_dispatch (plist::dictionary_t const& args);
		void snippet_dispatch (plist::dictionary_t const& args, std::map<std::string, std::string> const& variables);
		void execute_dispatch (plist::dictionary_t const& args, std::map<std::string, std::string> const& variables, std::function<void(bundle_command_t const&, ng::buffer_api_t const&, ng::ranges_t const&, std::map<std::string, std::string> const&)> const& runner);

		scope::context_t scope (std::string const& scopeAttributes) const;
		std::map<std::string, std::string> editor_variables (std::string const& scopeAttributes) const;
		std::map<std::string, std::string> const& match_captures () const { return _match_captures; }

		std::vector<std::string> const& choices () const;
		std::string placeholder_content (ng::range_t* placeholderSelection = NULL) const;
		void set_placeholder_content (std::string const& str, size_t selectFrom);

		ranges_t ranges () const                                              { return _selections; }
		void set_selections (ranges_t const& r)                               { _selections = r; }
		bool has_selection () const                                           { return not_empty(_buffer, _selections); }
		std::string as_string (size_t from = 0, size_t to = SIZE_T_MAX) const { return _buffer.substr(from, to != SIZE_T_MAX ? to : _buffer.size()); }
		void sanitize_selection ();

		void perform_replacements (std::multimap<std::pair<size_t, size_t>, std::string> const& replacements);
		bool handle_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, ng::ranges_t const& inputRanges, std::map<std::string, std::string> const& environment);

		void clear_snippets ();

		// ==============
		// = Clipboards =
		// ==============

		clipboard_ptr clipboard () const              { ASSERT(_clipboard);           return _clipboard; }
		clipboard_ptr find_clipboard () const         { ASSERT(_find_clipboard);      return _find_clipboard; }
		clipboard_ptr replace_clipboard () const      { ASSERT(_replace_clipboard);   return _replace_clipboard; }
		clipboard_ptr yank_clipboard () const         { ASSERT(_yank_clipboard); return _yank_clipboard; }

		void set_clipboard (clipboard_ptr cb)         { _clipboard = cb; }
		void set_find_clipboard (clipboard_ptr cb)    { _find_clipboard = cb; }
		void set_replace_clipboard (clipboard_ptr cb) { _replace_clipboard = cb; }
		void set_yank_clipboard (clipboard_ptr cb)    { _yank_clipboard = cb; }

	private:
		void setup ();
		friend struct indent_helper_t;

		static size_t visual_distance (ng::buffer_t const& buffer, index_t first, index_t last, bool eastAsianWidth = true);
		static index_t visual_advance (ng::buffer_t const& buffer, index_t caret, size_t distance, bool eastAsianWidth = true);

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
			void recede ()                                                     { if(--_index < 0) _index = _suggestions.size()-1; }

		private:
			size_t _revision = 0;
			ng::ranges_t _ranges;

			ng::ranges_t _prefix_ranges;
			std::vector<std::string> _suggestions;

			ssize_t _index = 0;
		};

		std::vector<std::string> completions (size_t bow, size_t eow, std::string const& prefix, std::string const& suffix, std::string const& scopeAttributes);
		bool setup_completion (std::string const& scopeAttributes);
		void next_completion (std::string const& scopeAttributes);
		void previous_completion (std::string const& scopeAttributes);

		// ============
		// = Snippets =
		// ============

		void snippet (std::string const& str, std::map<std::string, std::string> const& variables, bool disableIndent = false);
		ranges_t snippet (size_t from, size_t to, std::string const& str, std::map<std::string, std::string> const& variables, bool disableIndent);

		void find (std::string const& searchFor, find::options_t options = find::none, bool searchOnlySelection = false);
		void set_match_captures (std::map<std::string, std::string> const& match_captures) { _match_captures = match_captures; }
		ranges_t replace (std::multimap<range_t, std::string> const& replacements, bool selectInsertions = false);
		void move_selection (int deltaX, int deltaY);

		// ===============
		// = Member data =
		// ===============

		buffer_t& _buffer;
		ng::ranges_t _selections = ranges_t(0);
		snippet_controller_t _snippets;

		completion_info_t _completion_info;
		std::map<std::string, std::string> _match_captures;

		clipboard_ptr _clipboard;
		clipboard_ptr _find_clipboard;
		clipboard_ptr _replace_clipboard;
		clipboard_ptr _yank_clipboard;
		bool _extend_yank_clipboard = false;

		editor_delegate_t* _delegate = NULL;
	};

} /* ng */

#endif /* end of include guard: EDITOR_H_GWYNAZT0 */
