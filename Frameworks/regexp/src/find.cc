#include "find.h"
#include "private.h"
#include <Onigmo/oniguruma.h>
#include <text/utf8.h>
#include <cf/cf.h>

namespace find
{
	options_t operator| (options_t lhs, options_t rhs)
	{
		return options_t((unsigned)lhs | (unsigned)rhs);
	}

	options_t operator^ (options_t lhs, options_t rhs)
	{
		return options_t((unsigned)lhs ^ (unsigned)rhs);
	}

	options_t operator& (options_t lhs, options_t rhs)
	{
		return options_t((unsigned)lhs & (unsigned)rhs);
	}

	options_t& operator|= (options_t& lhs, options_t rhs)
	{
		return lhs = options_t((unsigned)lhs | (unsigned)rhs);
	}

	options_t& operator&= (options_t& lhs, unsigned rhs)
	{
		return lhs = options_t((unsigned)lhs & rhs);
	}

	// =================================
	// = Base class for find algorithm =
	// =================================

	struct find_implementation_t
	{
		friend struct find_t;
	protected:
		find_implementation_t () : skip_first(0), skip_last(0)  { }
		virtual ~find_implementation_t ()                       { }
		virtual std::pair<ssize_t, ssize_t> match (char const* buf, ssize_t len, std::map<std::string, std::string>* captures) = 0;

		ssize_t skip_first, skip_last;
	};

	// ========================
	// = Plain text searching =
	// ========================

	// ==============
	// = DFA Helper =
	// ==============

	struct dfa_node_t;
	typedef std::shared_ptr<dfa_node_t> dfa_node_ptr;

	struct dfa_node_t
	{
		dfa_node_t (char byte, std::vector<dfa_node_ptr> const& children) : children(children), byte(byte) { }

		void breadth_first_dispose ()
		{
			std::vector<std::vector<dfa_node_ptr>*> allChildren(1, &children);
			std::set<dfa_node_t*> seen{this};

			size_t lastPos = 0;
			while(lastPos < allChildren.size())
			{
				size_t oldPos = lastPos;
				lastPos = allChildren.size();
				for(size_t i = oldPos; i < lastPos; ++i)
				{
					for(dfa_node_ptr child : *allChildren[i])
					{
						if(seen.insert(child.get()).second)
							allChildren.push_back(&child->children);
					}
				}
			}

			riterate(it, allChildren)
				(*it)->clear();
		}

		bool does_match (char needle) const                   { return needle == byte; }
		std::vector<dfa_node_ptr> const& descend () const     { return children; }

		char get_byte () const                                { return byte; }
		bool can_merge (dfa_node_ptr const& rhs) const        { return byte == rhs->byte; }
		dfa_node_ptr merge (dfa_node_ptr const& rhs) const
		{
			std::vector<dfa_node_ptr> merged = children, tmp;

			for(auto const& newChildIter : rhs->children)
			{
				dfa_node_ptr new_node = newChildIter;

				for(auto& it : merged)
				{
					if(it->can_merge(new_node))
					{
						it = it->merge(new_node);
						new_node.reset();
						break;
					}
				}

				if(new_node)
					tmp.push_back(new_node);
			}

			merged.insert(merged.end(), tmp.begin(), tmp.end());
			return std::make_shared<dfa_node_t>(byte, merged);
		}

	private:
		std::vector<dfa_node_ptr> children;
		char byte;
	};

	// =======================
	// = Find implementation =
	// =======================

	static void all_variations (CFStringRef src, options_t options, std::vector<std::string>& out)
	{
		CFMutableStringRef strings[6];
		for(auto& it : strings)
			it = CFStringCreateMutableCopy(kCFAllocatorDefault, 0, src);

		CFStringNormalize(strings[0], kCFStringNormalizationFormC);
		CFStringNormalize(strings[1], kCFStringNormalizationFormC);
		CFStringNormalize(strings[2], kCFStringNormalizationFormD);
		CFStringNormalize(strings[3], kCFStringNormalizationFormD);

		if(options & ignore_case)
		{
			iterate(it, strings)
			{
				CFStringLowercase(  *it, nullptr);
				CFStringUppercase(*++it, nullptr);
			}
		}

		std::set<std::string> res;
		for(auto const& it : strings)
		{
			res.insert(res.end(), cf::to_s(it));
			CFRelease(it);
		}

		for(auto const& it : res)
		{
			if(!it.empty())
				out.push_back(it);
		}
	}

	static bool is_whitespace (uint32_t ch)
	{
		static CFCharacterSetRef const whitespace_set = CFCharacterSetGetPredefined(kCFCharacterSetWhitespaceAndNewline);
		return ch < 0x80 ? isspace(ch) : CFCharacterSetIsLongCharacterMember(whitespace_set, ch);
	}

	struct regular_find_t : find_implementation_t
	{
		regular_find_t (std::string const& str, options_t options) : options(options)
		{
			std::vector< std::vector<std::string> > matrix;
			citerate(it, diacritics::make_range(str.data(), str.data() + str.size()))
			{
				if((options & ignore_whitespace) && is_whitespace(*it))
					continue;

				if(CFStringRef tmp = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*)&it, it.length(), kCFStringEncodingUTF8, false))
				{
					matrix.push_back(std::vector<std::string>());
					all_variations(tmp, options, matrix.back());
					CFRelease(tmp);
				}
			}

			if(options & backwards)
			{
				std::reverse(matrix.begin(), matrix.end());
				for(auto& rowIter : matrix)
				{
					for(auto& colIter : rowIter)
						std::reverse(colIter.begin(), colIter.end());
				}
			}

			riterate(rowIter, matrix)
			{
				std::vector<dfa_node_ptr> tmp;
				for(auto const& colIter : *rowIter)
				{
					if(dfa_node_ptr new_node = node_from_string(colIter, children))
					{
						for(auto& it : tmp)
						{
							if(it->can_merge(new_node))
							{
								it = it->merge(new_node);
								new_node.reset();
								break;
							}
						}

						if(new_node)
							tmp.push_back(new_node);
					}
				}
				tmp.swap(children);
			}

			current_node = &children;
		}

		~regular_find_t ()
		{
			// Ensure non-recursive dispose of nodes to avoid blowing the stack
			for(dfa_node_ptr node : children)
				node->breadth_first_dispose();
		}

		std::pair<ssize_t, ssize_t> match (char const* buf, ssize_t len, std::map<std::string, std::string>* captures)
		{
			if(current_node->empty()) // previous call found a match
			{
				current_node = &children;
				match_data.clear();
			}

			ssize_t start      = options & backwards ? len-1 :   0;
			ssize_t limit      = options & backwards ?    -1 : len;
			ssize_t increment  = options & backwards ?    -1 :  +1;

			for(ssize_t i = start; i != limit; i += increment)
			{
				if((options & ignore_whitespace) && isspace(buf[i]))
				{
					if(!match_data.empty())
						match_data.push_back(buf[i]);
					continue;
				}

				bool did_match = false;
				for(auto const& nodeIter : *current_node)
				{
					if(nodeIter->does_match(buf[i]))
					{
						match_data.push_back(buf[i]);
						current_node = &nodeIter->descend();
						did_match = true;
						break;
					}
				}

				if(did_match && current_node->empty())
					return options & backwards ? std::make_pair(i, i + (ssize_t)match_data.size()) : std::make_pair(i + 1 - (ssize_t)match_data.size(), i + 1);

				if(!did_match && !match_data.empty())
				{
					current_node = &children;

					std::vector<char> tmp;
					tmp.swap(match_data);
					if(tmp.size() > 1)
					{
						if(options & backwards)
						{
							std::reverse(tmp.begin(), tmp.end());
							match(&tmp[0], tmp.size() - 1, captures);
						}
						else
						{
							match(&tmp[0] + 1, tmp.size() - 1, captures);
						}
					}

					i -= increment;
				}
			}
			return { len+1, len };
		}

	private:
		std::vector<dfa_node_ptr> children;
		std::vector<dfa_node_ptr> const* current_node;
		std::vector<char> match_data;
		options_t options;

		dfa_node_ptr node_from_string (std::string const& str, std::vector<dfa_node_ptr> children) const
		{
			std::vector<dfa_node_ptr> tmp;
			riterate(it, str)
			{
				tmp.swap(children);
				children.clear();
				children.push_back(std::make_shared<dfa_node_t>(*it, tmp));
			}
			return children.empty() ? dfa_node_ptr() : children.front();
		}
	};

	// ====================
	// = Regexp searching =
	// ====================

	struct regexp_find_t : find_implementation_t
	{
		regexp_find_t (std::string const& str, options_t options) : options(options)
		{
			did_start_searching = false;
			last_beg = -1;
			last_end = 0;

			OnigErrorInfo einfo;
			int r = onig_new(&compiled_pattern, (OnigUChar const*)str.data(), (OnigUChar const*)str.data() + str.size(), (options & ignore_case ? ONIG_OPTION_IGNORECASE : 0) | ONIG_OPTION_CAPTURE_GROUP, ONIG_ENCODING_UTF8, ONIG_SYNTAX_DEFAULT, &einfo);
			if(r != ONIG_NORMAL)
			{
				OnigUChar s[ONIG_MAX_ERROR_MESSAGE_LEN];
				onig_error_code_to_str(s, r, &einfo);
				os_log_error(OS_LOG_DEFAULT, "regexp_find_t: %{public}s (%{public}s)", s, str.c_str());

				if(compiled_pattern)
				{
					onig_free(compiled_pattern);
					compiled_pattern = nullptr;
				}
			}
		}

		~regexp_find_t ()
		{
			if(compiled_pattern)
				onig_free(compiled_pattern);
		}

		std::pair<ssize_t, ssize_t> match (char const* buf, ssize_t len, std::map<std::string, std::string>* captures)
		{
			std::pair<ssize_t, ssize_t> res(len+1, len);
			if(!compiled_pattern)
				return res;

			if(buf == nullptr)
			{
				// FIXME for an empty range, this returns a result — this also affects the non-regexp search
				if(!did_start_searching)
				{
					did_start_searching = true;
					if(options & backwards)
					{
						std::reverse(buffer.begin(), buffer.end());
						std::swap(skip_first, skip_last);
					}

					last_end = skip_first;
					// fprintf(stderr, "buffer (skip %zd, %zd):\n%.*s\n", skip_first, skip_last, (int)buffer.size(), &buffer[0]);
				}

				if(last_beg == buffer.size()) // last match was zero-width and end-of-buffer
					return res;
				else if(last_beg == last_end && last_end < buffer.size()) // last match was zero-width, so advance one character to not repeat it
					last_end += utf8::multibyte<char>::is_start(buffer[last_end]) ? std::min(utf8::multibyte<char>::length(buffer[last_end]), buffer.size() - last_end) : 1;

				OnigUChar const* first = (OnigUChar const*)&buffer[0];
				OnigUChar const* last = first + buffer.size();

				OnigUChar const* range_start = first + last_end;
				OnigUChar const* range_stop = last - skip_last;
				if(options & backwards)
					std::swap(range_start, range_stop);

				// fprintf(stderr, "allowed range: %d-%d\n", range_start - first, range_stop - first);

				OnigOptionType flags = ONIG_OPTION_NONE;
				if(options & find::not_bol)
				{
					flags |= ONIG_OPTION_NOTBOL;
					options = options ^ find::not_bol;
				}

				if(options & find::not_eol)
				{
					flags |= ONIG_OPTION_NOTEOL;
					options = options ^ find::not_eol;
				}

				int r;
				OnigRegion* region = onig_region_new();
				if(ONIG_MISMATCH != (r = onig_search(compiled_pattern, first, last, range_start, range_stop, region, flags)))
				{
					// fprintf(stderr, "match: %d-%d\n", region->beg[0], region->end[0]);
					res = std::pair<ssize_t, ssize_t>(region->beg[0], region->end[0]);
					res.first -= buffer_size;
					res.second -= buffer_size;

					if(captures)
						*captures = extract_captures(first, region, compiled_pattern);
				}

				last_beg = region->beg[0];
				last_end = region->end[0];

				onig_region_free(region, 1);
			}
			else
			{
				size_t cappedLen = std::min<size_t>(buffer_size + len, (options & filesize_limit) ? 5 * SQ(1024) : buffer_size + len) - buffer_size;
				buffer.insert(buffer.end(), buf, buf + cappedLen);
				if(options & backwards)
					std::reverse(buffer.end() - cappedLen, buffer.end());
				buffer_size += len;
			}
			return res;
		}

	private:
		OnigRegex compiled_pattern;
		options_t options;
		std::vector<char> buffer;
		ssize_t buffer_size = 0;
		int last_beg, last_end;
		bool did_start_searching;
	};

	// =================
	// = Find_t facade =
	// =================

	find_t::find_t (std::string const& str, options_t options)
	{
		if(options & regular_expression)
				pimpl = std::make_shared<regexp_find_t>(str, options);
		else	pimpl = std::make_shared<regular_find_t>(str, options);
	}

	void find_t::each_match (char const* buf, size_t len, bool moreToCome, std::function<void(std::pair<size_t, size_t> const&, std::map<std::string, std::string> const&)> const& f)
	{
		each_match(buf, len, moreToCome, [&f](std::pair<size_t, size_t> const& match, std::map<std::string, std::string> const& captures, bool*){ f(match, captures); });
	}

	void find_t::each_match (char const* buf, size_t len, bool moreToCome, std::function<void(std::pair<size_t, size_t> const&, std::map<std::string, std::string> const&, bool*)> const& f)
	{
		bool stop = false;
		for(size_t offset = 0; offset < len && !stop; )
		{
			std::map<std::string, std::string> captures;
			std::pair<ssize_t, ssize_t> const& m = pimpl->match(buf + offset, len - offset, &captures);
			if(m.first <= m.second)
				f(std::make_pair(_offset + offset + m.first, _offset + offset + m.second), captures, &stop);
			offset += m.second;
		}

		_offset += len;

		if(!moreToCome) // Reached end-of-buffer
		{
			std::map<std::string, std::string> captures;
			std::pair<ssize_t, ssize_t> m = pimpl->match(nullptr, 0, &captures);
			while(m.first <= m.second && !stop)
			{
				f(std::make_pair(_offset + m.first, _offset + m.second), captures, &stop);
				captures.clear();
				m = pimpl->match(nullptr, 0, &captures);
			}
		}
	}

} /* find */
