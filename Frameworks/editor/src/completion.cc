#include "editor.h"
#include "write.h"
#include <text/utf8.h>
#include <text/classification.h>
#include <text/ctype.h>
#include <text/tokenize.h>
#include <bundles/bundles.h>
#include <command/runner.h>

template <typename _OutputIter>
_OutputIter words_with_prefix_and_suffix (ng::buffer_t const& buffer, std::string const& prefix, std::string const& suffix, std::string const& excludeWord, _OutputIter out)
{
	citerate(pair, ng::find_all(buffer, prefix.size() < suffix.size() ? suffix : prefix, find::none))
	{
		ng::range_t range = ng::extend(buffer, pair->first, kSelectionExtendToWord).last();
		size_t bow = range.min().index, eow = range.max().index;
		if(prefix.size() < (eow - bow) && prefix == buffer.substr(bow, bow + prefix.size()) && suffix.size() < (eow - bow) && suffix == buffer.substr(eow - suffix.size(), eow) && excludeWord != buffer.substr(bow, eow))
			*out++ = std::make_pair(bow, buffer.substr(bow, eow));
	}
	return out;
}

namespace ng
{
	struct completion_command_delegate_t : command::delegate_t
	{
		completion_command_delegate_t (ng::buffer_t const& buffer, ng::ranges_t const& ranges) : buffer(buffer), ranges(ranges), result(NULL_STR) { }

		ng::range_t write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection)
		{
			return ng::write_unit_to_fd(buffer, ranges.last(), buffer.indent().tab_size(), fd, unit, fallbackUnit, format, scopeSelector, variables, inputWasSelection);
		}

		bool accept_html_data (command::runner_ptr runner, char const* data, size_t len) { return fprintf(stderr, "html: %.*s", (int)len, data), false; }

		void show_document (std::string const& str) { fprintf(stderr, "document: %s\n", str.c_str()); }
		void show_tool_tip (std::string const& str) { fprintf(stderr, "tool tip: %s\n", str.c_str()); }
		void show_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err) { fprintf(stderr, "error: %s%s\n", out.c_str(), err.c_str()); }

		bool accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, ng::range_t inputRange, std::map<std::string, std::string> const& environment)
		{
			if(placement == output::replace_selection && format == output_format::completion_list)
				result = out;
			return true;
		}

		ng::buffer_t const& buffer;
		ng::ranges_t ranges;
		std::string result;
	};

	typedef std::shared_ptr<completion_command_delegate_t> completion_command_delegate_ptr;

	std::vector<std::string> editor_t::completions (size_t bow, size_t eow, std::string const& prefix, std::string const& suffix, std::string const& scopeAttributes)
	{
		std::string const currentWord = _buffer.substr(bow, eow);
		scope::context_t const scope = this->scope(scopeAttributes);

		std::vector< std::pair<size_t, std::string> > tmp;
		std::vector<std::string> commandResult;

		// ====================================
		// = Run Potential Completion Command =
		// ====================================

		bundles::item_ptr item;
		plist::any_t value = bundles::value_for_setting("completionCommand", scope, &item);
		if(std::string const* str = boost::get<std::string>(&value))
		{
			bundle_command_t cmd;
			cmd.command       = *str;
			cmd.name          = item->name();
			cmd.uuid          = item->uuid();
			cmd.input         = input::entire_document;
			cmd.input_format  = input_format::text;
			cmd.output        = output::replace_selection;
			cmd.output_format = output_format::completion_list;

			std::map<std::string, std::string> env;
			if(_delegate)
			{
				env = _delegate->variables_for_bundle_item(item);
			}
			else
			{
				env << oak::basic_environment() << editor_variables(scopeAttributes) << item->bundle_variables();
				env = bundles::scope_variables(env, this->scope(scopeAttributes));
				env = variables_for_path(env, NULL_STR, this->scope(scopeAttributes).right);
			}
			env["TM_CURRENT_WORD"] = prefix;

			bundles::required_command_t failedRequirement;
			if(missing_requirement(item, env, &failedRequirement))
			{
				fprintf(stderr, "Failed running “%s” due to missing dependency “%s”.\n", cmd.name.c_str(), failedRequirement.command.c_str());
			}
			else
			{
				auto delegate = std::make_shared<completion_command_delegate_t>(_buffer, _selections);
				command::runner_ptr runner = command::runner(cmd, _buffer, _selections, env, delegate);
				runner->launch();
				runner->wait();

				if(delegate->result != NULL_STR)
				{
					for(auto line : text::tokenize(delegate->result.begin(), delegate->result.end(), '\n'))
					{
						line.erase(utf8::remove_malformed(line.begin(), line.end()), line.end());
						if(!line.empty())
							commandResult.push_back(line);
					}
				}
			}
		}

		for(ssize_t i = 0; i < commandResult.size(); ++i)
			tmp.push_back(std::make_pair(-commandResult.size() + i, commandResult[i]));

		// =============================
		// = Collect Words from Buffer =
		// =============================

		if(!plist::is_true(bundles::value_for_setting("disableDefaultCompletion", scope, &item)))
		{
			size_t cnt = tmp.size();
			words_with_prefix_and_suffix(_buffer, prefix, suffix, currentWord, back_inserter(tmp));
			if(cnt == tmp.size())
				words_with_prefix_and_suffix(_buffer, prefix, "", currentWord, back_inserter(tmp));
		}

		// ===============================================
		// = Add Fallback Values from Bundle Preferences =
		// ===============================================

		plist::any_t completionsValue = bundles::value_for_setting("completions", scope, &item);
		if(plist::array_t const* completions = boost::get<plist::array_t>(&completionsValue))
		{
			for(size_t i = 0; i < completions->size(); ++i)
			{
				if(std::string const* word = boost::get<std::string>(&(*completions)[i]))
					tmp.push_back(std::make_pair(SSIZE_MAX - completions->size() + i, *word));
			}
		}

		// ===============================================

		std::map<std::string, ssize_t> ranked;
		iterate(pair, tmp)
		{
			std::string word = pair->second;

			bool hasPrefix = prefix.empty() || (prefix.size() < word.size() && word.find(prefix) == 0);
			bool hasSuffix = hasPrefix && !suffix.empty() && suffix.size() < word.size() && word.find(suffix, word.size() - suffix.size()) == word.size() - suffix.size();
			if(!hasPrefix || word == currentWord)
				continue;

			ssize_t rank = bow <= pair->first ? pair->first - bow : bow - (pair->first + word.size());
			word = word.substr(prefix.size(), word.size() - prefix.size() - (hasSuffix ? suffix.size() : 0));

			auto it = ranked.find(word);
			if(it != ranked.end())
					it->second = std::min(rank, it->second);
			else	ranked.emplace(word, rank);
		}

		std::map<ssize_t, std::string> ordered;
		iterate(pair, ranked)
			ordered.emplace(pair->second, pair->first);

		std::vector<std::string> res;
		std::transform(ordered.begin(), ordered.end(), back_inserter(res), [](std::pair<ssize_t, std::string> const& p){ return p.second; });
		return res;
	}

	bool editor_t::setup_completion (std::string const& scopeAttributes)
	{
		completion_info_t& info = _completion_info;
		if(info.revision() != _buffer.revision() || info.ranges() != _selections)
		{
			info.set_revision(_buffer.revision());
			info.set_ranges(_selections);

			info.set_prefix_ranges(dissect_columnar(_buffer, _selections));
			ng::range_t r = info.prefix_ranges().last();
			size_t from = r.min().index, to = r.max().index;
			r = ng::extend(_buffer, r, kSelectionExtendToWord).last();
			size_t bow = r.min().index, eow = r.max().index;

			info.set_suggestions(completions(bow, eow, _buffer.substr(bow, from), _buffer.substr(to, eow), scopeAttributes));
		}
		return !info.suggestions().empty();
	}

	void editor_t::next_completion (std::string const& scopeAttributes)
	{
		if(setup_completion(scopeAttributes))
		{
			completion_info_t& info = _completion_info;
			info.advance();

			std::multimap<range_t, std::string> insertions;
			citerate(range, info.prefix_ranges())
				insertions.emplace(*range, info.current());
			info.set_prefix_ranges(this->replace(insertions, true));
			info.set_revision(_buffer.next_revision());
			info.set_ranges(ng::move(_buffer, info.prefix_ranges(), kSelectionMoveToEndOfSelection));
			_selections = info.ranges();
		}
	}

	void editor_t::previous_completion (std::string const& scopeAttributes)
	{
		if(setup_completion(scopeAttributes))
		{
			completion_info_t& info = _completion_info;
			info.retreat();

			std::multimap<range_t, std::string> insertions;
			citerate(range, info.prefix_ranges())
				insertions.emplace(*range, info.current());
			info.set_prefix_ranges(this->replace(insertions, true));
			info.set_revision(_buffer.next_revision());
			info.set_ranges(ng::move(_buffer, info.prefix_ranges(), kSelectionMoveToEndOfSelection));
			_selections = info.ranges();
		}
	}

} /* ng */
