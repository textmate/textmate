#ifndef SCOPE_SELECTOR_H_WZ1A8GIC
#define SCOPE_SELECTOR_H_WZ1A8GIC

#include <oak/misc.h>
#include <oak/debug.h>

namespace scope
{
	namespace compile
	{
		class compiler_factory_t;
	}

	namespace types
	{
		struct path_t;
		struct selector_t;
		typedef std::shared_ptr<path_t> path_ptr;
		typedef std::shared_ptr<selector_t> selector_ptr;

	} /* types */

	struct PUBLIC scope_t
	{
		WATCH_LEAKS(scope_t);

		scope_t ();
		scope_t (char const* scope);
		scope_t (std::string const& scope);

		bool has_prefix (scope_t const& rhs) const;

		scope_t append (std::string const& atom) const;
		scope_t parent () const;

		bool operator== (scope_t const& rhs) const;
		bool operator!= (scope_t const& rhs) const;
		bool operator< (scope_t const& rhs) const;

		explicit operator bool () const;

	private:
		void setup (std::string const& str);

		friend struct selector_t;
		friend scope::scope_t shared_prefix (scope_t const& a, scope_t const& b);
		friend std::string xml_difference (scope_t const& from, scope_t const& to, std::string const& open, std::string const& close);
		friend std::string to_s (scope_t const& s);
		types::path_ptr path;
	};

	struct PUBLIC context_t
	{
		context_t () { }
		context_t (char const* str) : left(str), right(str) { }
		context_t (std::string const& str) : left(str), right(str) { }
		context_t (scope_t const& actual) : left(actual), right(actual) { }
		context_t (scope_t const& left, scope_t const& right) : left(left), right(right) { }

		bool operator== (context_t const& rhs) const { return left == rhs.left && right == rhs.right; }
		bool operator!= (context_t const& rhs) const { return !(*this == rhs); }
		bool operator< (context_t const& rhs) const  { return left < rhs.left || left == rhs.left && right < rhs.right; }

		scope_t left, right;
	};

	PUBLIC extern scope_t wildcard;

	PUBLIC scope_t shared_prefix (scope_t const& a, scope_t const& b);
	PUBLIC std::string xml_difference (scope_t const& from, scope_t const& to, std::string const& open = "<", std::string const& close = ">");
	PUBLIC std::string to_s (scope_t const& s);
	PUBLIC std::string to_s (context_t const& s);

	struct PUBLIC selector_t
	{
		WATCH_LEAKS(selector_t);

		selector_t ();
		selector_t (char const* str);
		selector_t (std::string const& str);

		bool does_match (context_t const& scope, double* rank = NULL) const;

	private:
		void setup (std::string const& str);

		friend class compile::compiler_factory_t;
		friend std::string to_s (selector_t const& s);
		types::selector_ptr selector;
	};

	PUBLIC std::string to_s (selector_t const& s);

} /* scope */

#endif /* end of include guard: SCOPE_SELECTOR_H_WZ1A8GIC */
