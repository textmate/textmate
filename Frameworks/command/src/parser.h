#ifndef COMMAND_PARSER_H_NXZKS6AZ
#define COMMAND_PARSER_H_NXZKS6AZ

#include <plist/src/plist.h>
#include <plist/src/uuid.h>
#include <scope/src/scope.h>
#include <bundles/src/bundles.h>

namespace pre_exec      { enum type { nop = 0, save_document, save_project }; }
namespace input         { enum type { selection = 0, entire_document, scope, line, word, character, nothing }; }
namespace input_format  { enum type { text = 0, xml }; }
namespace output        { enum type { replace_input = 0, replace_document, at_caret, after_input, new_window, tool_tip, discard, replace_selection }; }
namespace output_format { enum type { text = 0, snippet, html, completion_list, snippet_no_auto_indent }; }
namespace output_caret  { enum type { after_output = 0, select_output, interpolate_by_char, interpolate_by_line, heuristic }; }
namespace output_reuse  { enum type { reuse_available = 0, reuse_none, reuse_busy, abort_and_reuse_busy }; }
namespace auto_refresh  { enum type { never = 0, on_document_change = (1 << 0), on_document_save = (1 << 1), on_document_close = (1 << 2) }; }

struct bundle_command_t
{
	std::string name = NULL_STR;
	oak::uuid_t uuid;
	scope::selector_t scope_selector;
	std::string command;

	pre_exec::type pre_exec           = pre_exec::nop;

	input::type input                 = input::selection;
	input::type input_fallback        = input::entire_document;
	input_format::type input_format   = input_format::text;

	output::type output               = output::replace_input;
	output_format::type output_format = output_format::text;
	output_caret::type output_caret   = output_caret::after_output;
	output_reuse::type output_reuse   = output_reuse::reuse_available;

	int auto_refresh                  = auto_refresh::never;

	bool auto_scroll_output           = false;
	bool disable_output_auto_indent   = false;
	bool disable_javascript_api       = false;
};

bundle_command_t parse_command (bundles::item_ptr bundleItem);
bundle_command_t parse_command (plist::dictionary_t const& plist);
bundle_command_t parse_drag_command (bundles::item_ptr bundleItem);
plist::dictionary_t convert_command_from_v1 (plist::dictionary_t plist);

#endif /* end of include guard: COMMAND_PARSER_H_NXZKS6AZ */
