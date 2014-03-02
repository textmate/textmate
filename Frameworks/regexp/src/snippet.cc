#include "snippet.h"
#include <oak/oak.h>
#include <oak/dependency_graph.h>
#include <text/parse.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Snippet);
OAK_DEBUG_VAR(Snippet_Stack);

namespace snippet
{
	snippet_t::snippet_t (std::string const& text, std::map<size_t, field_ptr> const& fields, std::multimap<size_t, field_ptr> const& mirrors, std::map<std::string, std::string> const& variables, std::string const& indent_string, text::indent_t const& indent) : text(text), fields(fields), mirrors(mirrors), variables(variables), indent_string(indent_string), indent_info(indent)
	{
		setup();
	}

	static std::string tabs_to_spaces (std::string const& src, std::string tabString)
	{
		if(tabString == "\t")
			return src;

		std::string dst;
		size_t lastIndex = 0;
		for(size_t i = 0; i < src.size(); ++i)
		{
			if(src[i] == '\t')
			{
				dst.insert(dst.end(), src.begin() + lastIndex, src.begin() + i);
				dst.append(tabString);
				lastIndex = i + 1;
			}
		}
		dst.insert(dst.end(), src.begin() + lastIndex, src.end());
		return dst;
	}

	static void replace (range_t range, std::string const& str, std::string& buffer, std::map<size_t, field_ptr>& fields, std::multimap<size_t, field_ptr>& mirrors)
	{
		buffer.replace(range.from.offset, range.size(), str);

		std::vector<pos_t*> tmp;
		for(auto const& it : fields)   { tmp.push_back(&it.second->range.from); tmp.push_back(&it.second->range.to); }
		for(auto const& it : mirrors)  { tmp.push_back(&it.second->range.from); tmp.push_back(&it.second->range.to); }

		for(auto const& it : tmp)
		{
			if(range.contains(*it))
				it->offset = range.from.offset;
			else if(range.from < *it)
				it->offset = it->offset + str.size() - range.size();
		}
	}

	static oak::dependency_graph build_graph (std::map<size_t, field_ptr> const& fields, std::multimap<size_t, field_ptr> const& mirrors)
	{
		oak::dependency_graph graph;
		for(auto const& field : fields)
		{
			graph.add_node(field.first);

			for(auto const& otherField : fields)
			{
				if(field.second->range.contains(otherField.second->range))
					graph.add_edge(field.first, otherField.first);
			}

			for(auto const& mirror : mirrors)
			{
				if(field.second->range.contains(mirror.second->range))
					graph.add_edge(field.first, mirror.first);
			}
		}
		return graph;
	}

	void snippet_t::update_mirrors (std::set<size_t> const& forFields)
	{
		oak::dependency_graph const& graph = build_graph(fields, mirrors);
		for(auto const& node : graph.topological_order())
		{
			if(!forFields.empty() && forFields.find(node) == forFields.end())
				continue;

			std::string const& src = fields[node]->range.to_s(text);
			D(DBF_Snippet, bug("update mirrors of %zu (%s)\n", node, src.c_str()););

			foreach(mirror, mirrors.lower_bound(node), mirrors.upper_bound(node))
			{
				std::string str = mirror->second->transform(src, variables);
				str = tabs_to_spaces(str, indent_info.create());
				str = format_string::replace(str, regexp::pattern_t("(?<=\n)(?!$)"), indent_string);
				D(DBF_Snippet, bug(" → %s\n", str.c_str()););
				snippet::replace(mirror->second->range, str, text, fields, mirrors);
			}
		}
	}

	static void indent (std::string& buffer, std::string const& indent_string, std::map<size_t, field_ptr>& fields, std::multimap<size_t, field_ptr>& mirrors)
	{
		std::vector<pos_t*> positions;
		for(auto const& it : fields)   { positions.push_back(&it.second->range.from); positions.push_back(&it.second->range.to); }
		for(auto const& it : mirrors)  { positions.push_back(&it.second->range.from); positions.push_back(&it.second->range.to); }

		char const* data = buffer.data();
		size_t size = buffer.size();

		std::vector< std::pair<char const*, char const*> > lines = text::to_lines(data, data + size);
		if(!lines.empty())
			lines.erase(lines.begin());

		riterate(pair, lines)
		{
			size_t offset = pair->first - data;
			for(auto const& pos : positions)
			{
				if(offset <= pos->offset)
					pos->offset += indent_string.size();
			}
			D(DBF_Snippet, bug("indent line: %s", buffer.substr(offset, pair->second - pair->first).c_str()););
			buffer.insert(buffer.begin() + offset, indent_string.begin(), indent_string.end());
		}

		D(DBF_Snippet, bug("new snippet:\n%s\n", buffer.c_str()););
	}

	static void tabs_to_spaces (std::string& buffer, std::string const& tabString, std::map<size_t, field_ptr>& fields, std::multimap<size_t, field_ptr>& mirrors)
	{
		if(tabString == "\t")
			return;

		std::vector<pos_t*> positions;
		for(auto const& it : fields)   { positions.push_back(&it.second->range.from); positions.push_back(&it.second->range.to); }
		for(auto const& it : mirrors)  { positions.push_back(&it.second->range.from); positions.push_back(&it.second->range.to); }

		char const* data = buffer.data();
		size_t size = buffer.size();

		std::vector<size_t> tabStops;
		for(size_t i = 0; i < size; ++i)
		{
			if(data[i] == '\t')
				tabStops.push_back(i);
		}

		std::string newBuffer;
		size_t lastIndex = 0;
		for(auto const& index : tabStops)
		{
			newBuffer.insert(newBuffer.end(), data + lastIndex, data + index);
			newBuffer.append(tabString);
			lastIndex = index + 1;
		}
		newBuffer.insert(newBuffer.end(), data + lastIndex, data + size);

		for(auto const& pos : positions)
			pos->offset += (tabString.size() - 1) * std::distance(tabStops.begin(), std::lower_bound(tabStops.begin(), tabStops.end(), pos->offset));
		buffer.swap(newBuffer);

		D(DBF_Snippet, bug("new snippet:\n%s\n", buffer.c_str()););
	}

	void snippet_t::setup ()
	{
		std::set<size_t> known_fields, known_mirrors;
		std::transform(fields.begin(), fields.end(), inserter(known_fields, known_fields.end()), [](std::pair<size_t, field_ptr> const& p){ return p.first; });
		std::transform(mirrors.begin(), mirrors.end(), inserter(known_mirrors, known_mirrors.end()), [](std::pair<size_t, field_ptr> const& p){ return p.first; });
		std::vector<size_t> to_move;
		set_difference(known_mirrors.begin(), known_mirrors.end(), known_fields.begin(), known_fields.end(), back_inserter(to_move));
		for(auto const& it : to_move)
		{
			fields.insert(*mirrors.lower_bound(it));
			mirrors.erase(mirrors.lower_bound(it));
		}
		
		if(fields.find(0) == fields.end())
			fields[0] = std::make_shared<placeholder_t>(0, pos_t(text.size(), SIZE_T_MAX-2), pos_t(text.size(), SIZE_T_MAX));

		tabs_to_spaces(text, indent_info.create(), fields, mirrors);
		indent(text, indent_string, fields, mirrors);
		update_mirrors();

		current_field = fields.size() > 1 ? (++fields.begin())->first : 0;
	}

	snippet_t::replacements_t snippet_t::replace_helper (size_t n, range_t const& range, std::string const& replacement)
	{
		oak::dependency_graph const& graph = build_graph(fields, mirrors);
		std::set<size_t> dirty = graph.touch(n);

		replacements_t updated;
		for(auto const& node : graph.topological_order())
		{
			if(dirty.find(node) != dirty.end())
			{
				foreach(mirror, mirrors.lower_bound(node), mirrors.upper_bound(node))
					updated.push_back(std::make_pair(mirror->second->range, std::string()));
			}
		}

		snippet::replace(range, replacement, text, fields, mirrors);
		update_mirrors(dirty);

		size_t i = 0;
		for(auto const& node : graph.topological_order())
		{
			if(dirty.find(node) != dirty.end())
			{
				foreach(mirror, mirrors.lower_bound(node), mirrors.upper_bound(node))
					updated[i++].second = mirror->second->range.to_s(text);
			}
		}

		std::sort(updated.begin(), updated.end());
		D(DBF_Snippet, for(auto const& it : updated) bug("%2zu-%2zu: %s\n", it.first.from.offset, it.first.to.offset, it.second.c_str()););
		return updated;
	}

	snippet_t::replacements_t snippet_t::replace (range_t range, std::string const& str)
	{
		/*
			${1:bla} — ${2:$1 $3} — ${3:$1} — ${4:$2} — ${5:$2 $1}

			• drop mirrors which are inside active field
			• replace range with string
			• cascade (update mirrors)
			• drop fields in active field    # we do this after cascading only to get their mirrors zapped
			• drop mirrors of dropped fields
		*/

		ASSERT(fields.find(current_field) != fields.end());
		range_t const& currentField = fields[current_field]->range;
		range.from.rank = range.to.rank = currentField.from.rank+1;
		ASSERTF(currentField.contains(range), "%zu (%zu) < %zu (%zu) && %zu (%zu) < %zu (%zu)", currentField.from.offset, currentField.from.rank, range.from.offset, range.from.rank, range.to.offset, range.to.rank, currentField.to.offset, currentField.to.rank);

		std::vector<std::multimap<size_t, field_ptr>::iterator> mirrors_to_remove;
		iterate(mirror, mirrors)
		{
			if(currentField.contains(mirror->second->range))
			{
				D(DBF_Snippet, bug("remove mirror: %zu (%zu-%zu, rank %zu-%zu)\n", mirror->first, mirror->second->range.from.offset, mirror->second->range.to.offset, mirror->second->range.from.rank, mirror->second->range.to.rank););
				mirrors_to_remove.push_back(mirror);
			}
		}

		for(auto const& mirror : mirrors_to_remove)
			mirrors.erase(mirror);

		D(DBF_Snippet, bug("replace range %zu (%zu) - %zu (%zu) with ‘%s’\n", currentField.from.offset, currentField.from.rank, currentField.to.offset, currentField.to.rank, str.c_str()););
		replacements_t const& res = replace_helper(current_field, range, str);
		D(DBF_Snippet, bug("new current field: %zu (%zu) - %zu (%zu)\n", currentField.from.offset, currentField.from.rank, currentField.to.offset, currentField.to.rank););


		std::vector<size_t> fields_to_remove;
		for(auto const& field : fields)
		{
			if(currentField.contains(field.second->range))
				fields_to_remove.push_back(field.first);
		}

		for(auto const& field : fields_to_remove)
		{
			D(DBF_Snippet, bug("remove placeholder: %zu\n", field););
			fields.erase(fields.find(field));
			mirrors.erase(mirrors.lower_bound(field), mirrors.upper_bound(field));
		}

		D(DBF_Snippet, bug("Fields:\n");  for(auto const& field : fields)   bug("\t%zu) %zu (%zu) - %zu (%zu)\n", field.first,  field.second->range.from.offset,  field.second->range.from.rank,  field.second->range.to.offset,  field.second->range.to.rank););
		D(DBF_Snippet, bug("Mirrors:\n"); for(auto const& mirror : mirrors) bug("\t%zu) %zu (%zu) - %zu (%zu)\n", mirror.first, mirror.second->range.from.offset, mirror.second->range.from.rank, mirror.second->range.to.offset, mirror.second->range.to.rank););

		return res;
	}

	// =================
	// = Snippet Stack =
	// =================

	void stack_t::push (snippet::snippet_t const& snippet, snippet::range_t const& range)
	{
		D(DBF_Snippet_Stack, bug("%s — %zu-%zu\n", snippet.text.c_str(), range.from.offset, range.to.offset););
		if(!records.empty())
		{
			records.back().caret = range.from.offset - current().from.offset;
			D(DBF_Snippet_Stack, bug("field offset for parent snippet: %zu\n", records.back().caret););
		}
		records.push_back(snippet);
	}

	std::vector< std::pair<snippet::range_t, std::string> > stack_t::replace (snippet::range_t range, std::string replacement)
	{
		std::vector< std::pair<snippet::range_t, std::string> > res(1, std::make_pair(range, replacement));

		drop_for_pos(range.from);
		drop_for_pos(range.to);
		if(records.empty())
			return res;

		std::vector<size_t> offsets(1, 0);
		for(auto& record : records)
			offsets.push_back(offsets.back() + record.snippet.fields[record.snippet.current_field]->range.from.offset + record.caret);

		D(DBF_Snippet_Stack, bug("offsets:\n"); for(auto const& offset : offsets) bug("\t%zu\n", offset););

		riterate(record, records)
		{
			snippet::snippet_t& s = record->snippet;
			offsets.pop_back();
			size_t oldLen = s.text.size();

			range_t local = range - offsets.back();
			local.from.rank = local.to.rank = s.fields[s.current_field]->range.from.rank + 1;

			std::vector< std::pair<snippet::range_t, std::string> > prepend;
			for(auto const& it : s.replace(local, replacement))
			{
				if(it.first.from < local.from)
						prepend.push_back(std::make_pair(it.first + offsets.back(), it.second));
				else	res.push_back(std::make_pair(it.first + offsets.back(), it.second));
			}
			res.insert(res.begin(), prepend.begin(), prepend.end());

			replacement = s.text;
			range = range_t(offsets.back(), offsets.back() + oldLen);
		}
		return res;
	}

	snippet::range_t stack_t::current () const
	{
		if(records.empty())
			return snippet::range_t(0, 0);

		size_t offset = 0;
		for(size_t i = 0; i < records.size()-1; ++i)
		{
			std::map<size_t, snippet::field_ptr>::const_iterator field = records[i].snippet.fields.find(records[i].snippet.current_field);
			ASSERT(field != records[i].snippet.fields.end());
			offset += field->second->range.from.offset + records[i].caret;
		}

		std::map<size_t, snippet::field_ptr>::const_iterator field = records.back().snippet.fields.find(records.back().snippet.current_field);
		ASSERT(field != records.back().snippet.fields.end());
		snippet::range_t range = field->second->range;
		D(DBF_Snippet_Stack, bug("%zu-%zu + %zu\n", range.from.offset, range.to.offset, offset););
		return range + offset;
	}

	bool stack_t::in_last_placeholder () const
	{
		return !records.empty() && records.back().snippet.current_field == 0;
	}

	std::vector<std::string> const& stack_t::choices () const
	{
		static std::vector<std::string> const empty;
		if(records.empty())
			return empty;

		snippet::snippet_t const& s = records.back().snippet;
		std::map<size_t, field_ptr>::const_iterator field = s.fields.find(s.current_field);
		return field != s.fields.end() ? field->second->choices() : empty;
	}

	void stack_t::drop_for_pos (snippet::pos_t pos)
	{
		while(!records.empty())
		{
			if(records.back().snippet.current_field == 0)
			{
				records.pop_back();
				continue;
			}

			pos.rank = current().from.rank+1;
			if(current().contains(pos))
				return;
			D(DBF_Snippet_Stack, bug("%zu outside field %zu-%zu\n", pos.offset, current().from.offset, current().to.offset););
			records.pop_back();
		}
	}

	bool stack_t::next ()
	{
		while(!records.empty())
		{
			snippet::snippet_t& s = records.back().snippet;
			size_t n = s.current_field;
			if(n != 0)
			{
				snippet::range_t currentFieldRange = s.fields[n]->range;
				while(true)
				{
					std::map<size_t, snippet::field_ptr>::iterator field = s.fields.find(n);
					if(++field == s.fields.end())
						field = s.fields.begin();

					if(field->second->range != currentFieldRange)
						return s.current_field = field->first, true;

					if((n = field->first) == 0)
						break;
				}
			}
			records.pop_back();
		}
		return false;
	}

	bool stack_t::previous ()
	{
		while(!records.empty())
		{
			snippet::snippet_t& s = records.back().snippet;
			if(s.current_field == 0)
			{
				if(s.fields.size() > 1)
				{
					s.current_field = (--s.fields.end())->first;
					return true;
				}
				records.pop_back();
				continue;
			}
			else
			{
				std::map<size_t, snippet::field_ptr>::iterator field = s.fields.find(s.current_field);
				if(--field == s.fields.begin())
				{
					if(records.size() == 1)
						return false;
					records.pop_back();
				}
				else
				{
					s.current_field = field->first;
					return true;
				}
			}
		}
		return false;
	}

} /* snippet */
