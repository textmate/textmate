#include "parser.h"
#include <bundles/bundles.h>

static size_t index_of (size_t i, std::string const& needle)
{
	return 0;
}

template <typename... Args>
static size_t index_of (size_t i, std::string const& needle, char const* head, Args... tail)
{
	return needle == head ? i : index_of(i+1, needle, tail...);
}

template <typename T, typename... Args>
void parse (plist::dictionary_t const& plist, std::string const& key, T* out, Args... list)
{
	std::string value;
	if(plist::get_key_path(plist, key, value))
		*out = T(index_of(0, value, list...));
}

plist::dictionary_t convert_command_from_v1 (plist::dictionary_t plist)
{
	int32_t version = 0;
	if(plist::get_key_path(plist, "version", version) && version != 1)
		return plist;

	/*
		replaceSelectedText || replaceSelection || insertAsText
		 → placement: replace input
		 → caret: if hasSelection && input is selection: select output
		 → caret: if !hasSelection && fallback input is line, scope, or document: interpolate by char
		 → caret: else: after output

		replaceDocument
		 → placement: replace document
		 → caret: interpolate (line number)

		afterSelectedText, with input set to “selection or «unit»”
		 → placement: after input
		 → caret: after output

		afterSelectedText, with input set to “none” or “document”
		 → placement: after selection (at caret)
		 → caret: after output

		insertAsSnippet, with input set to “selection or «unit»”
		 → replace input

		insertAsSnippet, with input set to “none” or “document”
		 → replace selection
	*/

	static struct { char const* oldOutput; std::string const output; std::string const format; std::string const caret; } conversionMap[] =
	{
		{ "discard",                "discard",             "text",    NULL_STR            },
		{ "replaceSelectedText",    "replaceInput",        "text",    "heuristic"         },
		{ "replaceSelection",       "replaceInput",        "text",    "heuristic"         },
		{ "insertAsText",           "replaceInput",        "text",    "heuristic"         },
		{ "replaceDocument",        "replaceDocument",     "text",    "interpolateByLine" },
		{ "afterSelectedText",      "afterInput",          "text",    "afterOutput"       },
		{ "insertAsSnippet",        "replaceInput",        "snippet", NULL_STR            },
		{ "showAsHTML",             "newWindow",           "html",    NULL_STR            },
		{ "showAsTooltip",          "toolTip",             "text",    NULL_STR            },
		{ "openAsNewDocument",      "newWindow",           "text",    NULL_STR            },
	};

	std::string oldOutput;
	if(plist::get_key_path(plist, "output", oldOutput))
	{
		for(auto const& mapping : conversionMap)
		{
			if(oldOutput == mapping.oldOutput)
			{
				plist["outputLocation"] = mapping.output;
				plist["outputFormat"]   = mapping.format;
				if(mapping.caret != NULL_STR)
					plist["outputCaret"] = mapping.caret;

				std::string input;
				if(oldOutput == "afterSelectedText" && plist::get_key_path(plist, "input", input) && input != "selection")
					plist["outputLocation"] = std::string("atCaret");
				else if(oldOutput == "insertAsSnippet" && plist::get_key_path(plist, "input", input) && input != "selection")
					plist["outputLocation"] = std::string("replaceSelection");

				break;
			}
		}
	}

	bool flag;
	plist["autoScrollOutput"] = plist::get_key_path(plist, "dontFollowNewOutput", flag) && !flag || plist::get_key_path(plist, "autoScrollOutput", flag) && flag;

	return plist;
}

static void setup_fields (plist::dictionary_t const& plist, bundle_command_t& res)
{
	std::string scopeSelectorString, preExecString, inputString, inputFormatString, inputFallbackString, outputFormatString, outputLocationString, outputCaretString, outputReuseString, autoRefreshString;

	plist::get_key_path(plist, "name", res.name);
	plist::get_key_path(plist, "uuid", res.uuid);
	plist::get_key_path(plist, "command", res.command);
	if(plist::get_key_path(plist, "scope", scopeSelectorString))
		res.scope_selector = scopeSelectorString;

	parse(plist, "beforeRunningCommand", &res.pre_exec,       "nop", "saveActiveFile", "saveModifiedFiles");
	parse(plist, "inputFormat",          &res.input_format,   "text", "xml");
	parse(plist, "input",                &res.input,          "selection", "document", "scope", "line", "word", "character", "none");
	parse(plist, "fallbackInput",        &res.input_fallback, "selection", "document", "scope", "line", "word", "character", "none");
	parse(plist, "outputFormat",         &res.output_format,  "text", "snippet", "html", "completionList");
	parse(plist, "outputLocation",       &res.output,         "replaceInput", "replaceDocument", "atCaret", "afterInput", "newWindow", "toolTip", "discard", "replaceSelection");
	parse(plist, "outputCaret",          &res.output_caret,   "afterOutput", "selectOutput", "interpolateByChar", "interpolateByLine", "heuristic");
	parse(plist, "outputReuse",          &res.output_reuse,   "reuseAvailable", "reuseNone", "reuseBusy", "reuseBusyAutoAbort");
	parse(plist, "autoRefresh",          &res.auto_refresh,   "newer", "onDocumentChange", "onDocumentSave");

	plist::get_key_path(plist, "autoScrollOutput", res.auto_scroll_output);
	plist::get_key_path(plist, "disableOutputAutoIndent", res.disable_output_auto_indent);
}

bundle_command_t parse_command (plist::dictionary_t const& plist)
{
	bundle_command_t res = { };
	res.input_fallback = input::entire_document;
	setup_fields(plist, res);
	return res;
}

bundle_command_t parse_command (bundles::item_ptr bundleItem)
{
	return parse_command(convert_command_from_v1(bundleItem->plist()));
}

bundle_command_t parse_drag_command (bundles::item_ptr bundleItem)
{
	bundle_command_t res = { };
	res.input         = res.input_fallback = input::nothing;
	res.output        = output::at_caret;
	res.output_format = output_format::snippet;

	plist::dictionary_t const& plist = bundleItem->plist();
	setup_fields(plist, res);
	return res;
}
