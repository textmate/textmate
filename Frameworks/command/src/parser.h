#ifndef COMMAND_PARSER_H_NXZKS6AZ
#define COMMAND_PARSER_H_NXZKS6AZ

#include <plist/plist.h>
#include <plist/uuid.h>
#include <scope/scope.h>
#include <bundles/bundles.h>

namespace pre_exec      { enum type { nop = 0, save_document, save_project }; }
namespace input         { enum type { selection = 0, entire_document, scope, line, word, character, nothing }; }
namespace input_format  { enum type { text = 0, xml }; }
namespace output        { enum type { replace_input = 0, replace_document, at_caret, after_input, new_window, tool_tip, discard, replace_selection }; }
namespace output_format { enum type { text = 0, snippet, html, completion_list, snippet_no_auto_indent }; }
namespace output_caret  { enum type { after_output = 0, select_output, interpolate_by_char, interpolate_by_line, heuristic }; }
namespace output_reuse  { enum type { reuse_available = 0, reuse_none, reuse_busy, abort_and_reuse_busy }; }
namespace auto_refresh  { enum type { never = 0, on_document_change, on_document_save }; }

#ifndef NDEBUG
inline char const* to_s (input::type const& input)
{
	switch(input)
	{
		case input::character:        return "character";
		case input::word:             return "word";
		case input::line:             return "line";
		case input::scope:            return "scope";
		case input::selection:        return "selection";
		case input::entire_document:  return "document";
	};
	return "undefined";
}

inline char const* to_s (output_caret::type const& caret)
{
	switch(caret)
	{
		case output_caret::after_output:        return "after output";        break;
		case output_caret::select_output:       return "select output";       break;
		case output_caret::interpolate_by_char: return "interpolate by char"; break;
		case output_caret::interpolate_by_line: return "interpolate by line"; break;
		case output_caret::heuristic:           return "heuristic";           break;
	};
	return "undefined";
}
#endif

struct PUBLIC bundle_command_t
{
	std::string name;
	oak::uuid_t uuid;
	scope::selector_t scope_selector;
	std::string command;

	pre_exec::type pre_exec;

	input::type input;
	input::type input_fallback;
	input_format::type input_format;

	output::type output;
	output_format::type output_format;
	output_caret::type output_caret;
	output_reuse::type output_reuse;

	auto_refresh::type auto_refresh;

	bool auto_scroll_output;
	bool disable_output_auto_indent;
};

PUBLIC bundle_command_t parse_command (bundles::item_ptr bundleItem);
PUBLIC bundle_command_t parse_command (plist::dictionary_t const& plist);
PUBLIC bundle_command_t parse_drag_command (bundles::item_ptr bundleItem);
PUBLIC plist::dictionary_t convert_command_from_v1 (plist::dictionary_t plist);

#endif /* end of include guard: COMMAND_PARSER_H_NXZKS6AZ */
