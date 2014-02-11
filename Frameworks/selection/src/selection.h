#ifndef SELECTION_H_EKADE2RF
#define SELECTION_H_EKADE2RF

#include "types.h"
#include <regexp/find.h>
#include <text/types.h>
#include <oak/misc.h>

enum move_unit_type { kSelectionMoveLeft, kSelectionMoveRight, kSelectionMoveFreehandedLeft, kSelectionMoveFreehandedRight, kSelectionMoveUp, kSelectionMoveDown, kSelectionMoveToBeginOfSelection, kSelectionMoveToEndOfSelection, kSelectionMoveToBeginOfSubWord, kSelectionMoveToEndOfSubWord, kSelectionMoveToBeginOfWord, kSelectionMoveToEndOfWord, kSelectionMoveToBeginOfSoftLine, kSelectionMoveToEndOfSoftLine, kSelectionMoveToBeginOfIndentedLine, kSelectionMoveToEndOfIndentedLine, kSelectionMoveToBeginOfLine, kSelectionMoveToEndOfLine, kSelectionMoveToBeginOfParagraph, kSelectionMoveToEndOfParagraph, kSelectionMoveToBeginOfHardParagraph, kSelectionMoveToEndOfHardParagraph, kSelectionMoveToBeginOfTypingPair, kSelectionMoveToEndOfTypingPair, kSelectionMoveToBeginOfColumn, kSelectionMoveToEndOfColumn, kSelectionMovePageUp, kSelectionMovePageDown, kSelectionMoveToBeginOfDocument, kSelectionMoveToEndOfDocument, kSelectionMoveNowhere };
enum select_unit_type { kSelectionExtendLeft, kSelectionExtendRight, kSelectionExtendFreehandedLeft, kSelectionExtendFreehandedRight, kSelectionExtendUp, kSelectionExtendDown, kSelectionExtendToBeginOfSubWord, kSelectionExtendToEndOfSubWord, kSelectionExtendToBeginOfWord, kSelectionExtendToEndOfWord, kSelectionExtendToBeginOfSoftLine, kSelectionExtendToEndOfSoftLine, kSelectionExtendToBeginOfIndentedLine, kSelectionExtendToEndOfIndentedLine, kSelectionExtendToBeginOfLine, kSelectionExtendToEndOfLine, kSelectionExtendToBeginOfParagraph, kSelectionExtendToEndOfParagraph, kSelectionExtendToBeginOfTypingPair, kSelectionExtendToEndOfTypingPair, kSelectionExtendToBeginOfColumn, kSelectionExtendToEndOfColumn, kSelectionExtendPageUp, kSelectionExtendPageDown, kSelectionExtendToBeginOfDocument, kSelectionExtendToEndOfDocument, kSelectionExtendToWord, kSelectionExtendToScope, kSelectionExtendToSoftLine, kSelectionExtendToLineExclLF, kSelectionExtendToLine, kSelectionExtendToParagraph, kSelectionExtendToTypingPair, kSelectionExtendToAll };

namespace scope { struct context_t; struct selector_t; }

namespace ng
{
	struct buffer_t;

	struct layout_movement_t
	{
		virtual index_t index_left_of (index_t const& index) const = 0;
		virtual index_t index_right_of (index_t const& index) const = 0;
		virtual index_t index_above (index_t const& index) const = 0;
		virtual index_t index_below (index_t const& index) const = 0;
		virtual index_t index_at_bol_for (index_t const& index) const = 0;
		virtual index_t index_at_eol_for (index_t const& index) const = 0;
		virtual index_t page_up_for (index_t const& index) const = 0;
		virtual index_t page_down_for (index_t const& index) const = 0;
	};

	PUBLIC ranges_t convert (buffer_t const& buffer, text::selection_t const& selection);
	PUBLIC bool not_empty (buffer_t const& buffer, ranges_t const& selection);
	PUBLIC bool multiline (buffer_t const& buffer, ranges_t const& selection);
	PUBLIC ranges_t move (buffer_t const& buffer, ranges_t const& selection, move_unit_type const unit, layout_movement_t const* layout = NULL);
	PUBLIC ranges_t extend (buffer_t const& buffer, ranges_t const& selection, select_unit_type const unit, layout_movement_t const* layout = NULL);
	PUBLIC ranges_t extend_if_empty (buffer_t const& buffer, ranges_t const& selection, select_unit_type const unit, layout_movement_t const* layout = NULL);
	PUBLIC ranges_t select_scope (buffer_t const& buffer, ranges_t const& selection, scope::selector_t const& scopeSelector);
	PUBLIC ranges_t toggle_columnar (ranges_t const& selection);
	PUBLIC scope::context_t scope (buffer_t const& buffer, ranges_t const& selection, std::string const& extraAttributes = NULL_STR);
	PUBLIC ranges_t highlight_ranges_for_movement (buffer_t const& buffer, ranges_t const& oldSelection, ranges_t const& newSelection);
	PUBLIC std::map< range_t, std::map<std::string, std::string> > find (buffer_t const& buffer, ranges_t const& selection, std::string const& searchFor, find::options_t options, ranges_t const& searchRanges = ranges_t(), bool* didWrap = nullptr);
	PUBLIC std::map< range_t, std::map<std::string, std::string> > find_all (buffer_t const& buffer, std::string const& searchFor, find::options_t options, ranges_t const& searchRanges = ranges_t());
	PUBLIC range_t word_at (buffer_t const& buffer, range_t const& range);
	PUBLIC ranges_t all_words (buffer_t const& buffer);

	PUBLIC ranges_t dissect_columnar (buffer_t const& buffer, ranges_t const& selection);

	PUBLIC extern std::string const kCharacterClassWord;
	PUBLIC extern std::string const kCharacterClassSpace;
	PUBLIC extern std::string const kCharacterClassOther;
	PUBLIC extern std::string const kCharacterClassUnknown;

	PUBLIC std::string character_class (buffer_t const& buffer, size_t index);

	PUBLIC ranges_t from_string (buffer_t const& buffer, std::string const& str);
	PUBLIC std::string to_s (buffer_t const& buffer, ranges_t const& ranges);

} /* ng */

#endif /* end of include guard: SELECTION_H_EKADE2RF */
