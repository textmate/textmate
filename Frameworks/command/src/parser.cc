#include "parser.h"
#include <bundles/bundles.h>

template <typename T>
T pick (std::string const& value, ...)
{
	va_list ap;
	va_start(ap, value);
	size_t i = 0;
	while(char const* str = va_arg(ap, char const*))
	{
		if(value == str)
			return T(i);
		++i;
	}
	va_end(ap);
	return T(0);
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
		for(size_t i = 0; i < sizeofA(conversionMap); ++i)
		{
			if(oldOutput == conversionMap[i].oldOutput)
			{
				plist["outputLocation"] = conversionMap[i].output;
				plist["outputFormat"]   = conversionMap[i].format;
				if(conversionMap[i].caret != NULL_STR)
					plist["outputCaret"] = conversionMap[i].caret;

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

static std::vector<std::string> convert_locations (plist::array_t const& array)
{
	std::vector<std::string> res;
	iterate(it, array)
	{
		if(std::string const* str = boost::get<std::string>(&*it))
			res.push_back(*str);
	}
	return res;
}

static std::vector<bundle_command_t::shell_command_t> convert_requirements (plist::array_t const& array)
{
	std::vector<bundle_command_t::shell_command_t> res;
	iterate(info, array)
	{
		std::string command, variable = NULL_STR; plist::array_t locations;
		if(plist::get_key_path(*info, "command", command))
		{
			plist::get_key_path(*info, "variable", variable);
			if(plist::get_key_path(*info, "locations", locations))
					res.push_back(bundle_command_t::shell_command_t(command, variable, convert_locations(locations)));
			else	res.push_back(bundle_command_t::shell_command_t(command));
		}
	}
	return res;
}

static void setup_fields (plist::dictionary_t const& plist, bundle_command_t& res)
{
	std::string scopeSelectorString, preExecString, inputString, inputFormatString, inputFallbackString, outputFormatString, outputLocationString, outputCaretString;

	plist::get_key_path(plist, "name", res.name);
	plist::get_key_path(plist, "uuid", res.uuid);
	plist::get_key_path(plist, "command", res.command);
	if(plist::get_key_path(plist, "scope", scopeSelectorString))
		res.scope_selector = scopeSelectorString;

	if(plist::get_key_path(plist, "beforeRunningCommand", preExecString))
		res.pre_exec = pick<pre_exec::type>(preExecString, "nop", "saveActiveFile", "saveModifiedFiles", NULL);

	if(plist::get_key_path(plist, "inputFormat", inputFormatString))
		res.input_format = pick<input_format::type>(inputFormatString, "text", "xml", NULL);

	if(plist::get_key_path(plist, "input", inputString))
		res.input = pick<input::type>(inputString, "selection", "document", "scope", "line", "word", "character", "none", NULL);

	if(plist::get_key_path(plist, "fallbackInput", inputFallbackString))
		res.input_fallback = pick<input::type>(inputFallbackString, "selection", "document", "scope", "line", "word", "character", "none", NULL);

	if(plist::get_key_path(plist, "outputFormat", outputFormatString))
		res.output_format = pick<output_format::type>(outputFormatString, "text", "snippet", "html", "completionList", NULL);

	if(plist::get_key_path(plist, "outputLocation", outputLocationString))
		res.output = pick<output::type>(outputLocationString, "replaceInput", "replaceDocument", "atCaret", "afterInput", "newWindow", "toolTip", "discard", "replaceSelection", NULL);

	if(plist::get_key_path(plist, "outputCaret", outputCaretString))
		res.output_caret = pick<output_caret::type>(outputCaretString, "afterOutput", "selectOutput", "interpolateByChar", "interpolateByLine", "heuristic", NULL);

	plist::array_t requiredCommands;
	if(plist::get_key_path(plist, "requiredCommands", requiredCommands))
		res.requirements = convert_requirements(requiredCommands);

	plist::get_key_path(plist, "autoScrollOutput", res.auto_scroll_output);
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
