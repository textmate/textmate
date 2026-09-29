#ifndef SNIPPETS_H_UC2RJ8ET
#define SNIPPETS_H_UC2RJ8ET

#include <regexp/src/snippet.h>
#include <buffer/src/buffer.h>
#include <selection/src/selection.h>

namespace ng
{
	struct snippet_controller_t
	{
		void push (snippet::snippet_t const& snippet, ng::range_t const& range);
		std::vector< std::pair<ng::range_t, std::string> > replace (size_t from, size_t to, std::string const& replacement);
		ng::range_t current () const;
		std::vector<std::string> const& choices () const;
		void drop_for_pos (size_t pos);

		bool next ()        { return stack.next(); }
		bool previous ()    { return stack.previous(); }

		void clear ()       { return stack.clear(); }
		bool empty () const { return stack.empty(); }

		bool in_last_placeholder () const { return stack.in_last_placeholder(); }

	private:
		size_t anchor;
		snippet::stack_t stack;
	};

} /* ng */

#endif /* end of include guard: SNIPPETS_H_UC2RJ8ET */
