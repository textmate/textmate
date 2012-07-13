#ifndef COMPRESSED_H_N3X9XQ2B
#define COMPRESSED_H_N3X9XQ2B

#include <oak/debug.h>

namespace scope
{
	namespace compile
	{
		struct analyze_t;
	}

	namespace compressed
	{
		struct path_t;

		struct any_t
		{
			virtual ~any_t () { }
			virtual bool does_match (path_t const& lhs, path_t const& rhs, double* rank) const = 0;
		};

		typedef std::tr1::shared_ptr<any_t> any_ptr;

		struct scope_t
		{
			scope_t () : anchor_to_previous(false) { }
			scope_t (int data, int mask, int number, bool anchor_to_previous) : data(data), mask(mask), number(number), anchor_to_previous(anchor_to_previous) { }

			int data;
			int mask;
			int number;
			
			bool anchor_to_previous;
			bool operator== (scope_t const& rhs) const { return data == rhs.data && number == rhs.number; }
			bool operator!= (scope_t const& rhs) const { return !(*this == rhs); }
			bool operator< (scope_t const& rhs) const  { return data < rhs.data || data == rhs.data && number < rhs.number; }
			
		};

		struct path_t : any_t
		{
			WATCH_LEAKS(path_t);

			path_t () : anchor_to_bol(false), anchor_to_eol(false) { }
			std::vector<scope_t> scopes;
			bool anchor_to_bol;
			bool anchor_to_eol;

			bool does_match (path_t const& lhs, path_t const& rhs, double* rank) const;
		};

		struct expression_t
		{
			expression_t (char op) : op((op_t)op), negate(false) { }
			enum op_t { op_none, op_or = '|', op_and = '&', op_minus = '-' } op;
			bool negate;
			any_ptr selector;

			bool does_match (path_t const& lhs, path_t const& rhs, double* rank) const;
		};

		struct composite_t
		{
			std::vector<expression_t> expressions;

			bool does_match (path_t const& lhs, path_t const& rhs, double* rank) const;
		};

		struct selector_t
		{
			WATCH_LEAKS(selector_t);

			std::vector<composite_t> composites;

			bool does_match (path_t const& lhs, path_t const& rhs, double* rank) const;
		};

		struct group_t : any_t
		{
			WATCH_LEAKS(group_t);

			selector_t selector;

			bool does_match (path_t const& lhs, path_t const& rhs, double* rank) const;
		};

		struct filter_t : any_t
		{
			WATCH_LEAKS(filter_t);

			filter_t () : filter(unset) { }
			enum side_t { unset, left = 'L', right = 'R', both = 'B' } filter;
			any_ptr selector;

			bool does_match (path_t const& lhs, path_t const& rhs, double* rank) const;
		};

	} /* types */

} /* scope */

#endif /* end of include guard: COMPRESSED_H_N3X9XQ2B */
