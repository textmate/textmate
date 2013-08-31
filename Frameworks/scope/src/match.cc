#include "types.h"
#include "parse.h"
#include "scope.h"
#include <oak/oak.h>

struct dummy_t
{
	dummy_t (char const* f) : f(f) { fprintf(stderr, "%s\n", f); }
	~dummy_t ()                    { fprintf(stderr, "~%s\n", f); }
	char const* f;
};

// #define ENTER dummy_t _dummy(__FUNCTION__)
#define ENTER

namespace scope
{
	namespace types
	{
		static bool prefix_match (std::string const& lhs, std::string const& rhs)
		{
			ENTER;
			if(lhs.size() > rhs.size())
				return false;

			auto lhsIter = lhs.begin();
			auto rhsIter = rhs.begin();

			while(lhsIter != lhs.end() && rhsIter != rhs.end())
			{
				if(*lhsIter == *rhsIter)
				{
					++lhsIter;
					++rhsIter;
				}
				else if(*lhsIter == '*')
				{
					++lhsIter;
					while(rhsIter != rhs.end() && *rhsIter != '.')
						++rhsIter;
				}
				else
				{
					return false;
				}
			}
			return lhsIter == lhs.end() && (rhsIter == rhs.end() || *rhsIter == '.');
		}

		static bool is_auxiliary_scope (std::string const& scope)
		{
			return (scope.size() > 5 && strncmp(scope.data(), "attr.", 5) == 0) || (scope.size() > 4 && strncmp(scope.data(), "dyn.", 4) == 0);
		}

		bool path_t::does_match (scope::scope_t const& unused, scope::scope_t const& scope, double* rank) const
		{
			auto node    = scope.node;
			auto sel     = this->scopes.rbegin();
			double score = 0;

			decltype(node) btNode = nullptr;
			auto btSelector = this->scopes.rend();
			double btScore  = 0;

			double power = 0;

			if(this->anchor_to_eol)
			{
				while(node && is_auxiliary_scope(node->atoms))
				{
					if(rank)
						power += std::count(node->atoms.begin(), node->atoms.end(), '.') + 1;
					node = node->parent;
				}
				btSelector = sel;
			}

			while(node && sel != this->scopes.rend())
			{
				if(rank)
					power += std::count(node->atoms.begin(), node->atoms.end(), '.') + 1;

				bool isRedundantNonBOLMatch = this->anchor_to_bol && node->parent && sel+1 == this->scopes.rend();
				if(!isRedundantNonBOLMatch && prefix_match(sel->atoms, node->atoms))
				{
					if(sel->anchor_to_previous)
					{
						if(btSelector == this->scopes.rend())
						{
							btNode     = node;
							btSelector = sel;
							btScore    = score;
						}
					}
					else if(btSelector != this->scopes.rend())
					{
						btSelector = this->scopes.rend();
					}

					if(rank)
					{
						size_t len = std::count(sel->atoms.begin(), sel->atoms.end(), '.') + 1;
						while(len-- != 0)
							score += 1 / exp2(power - len);
					}

					++sel;
				}
				else if(btSelector != this->scopes.rend())
				{
					if(!btNode)
						break;

					node  = btNode;
					sel   = btSelector;
					score = btScore;

					btSelector = this->scopes.rend();
				}

				node = node->parent;
			}

			if(rank)
				*rank = sel == this->scopes.rend() ? score : 0;

			return sel == this->scopes.rend();
		}

		bool composite_t::does_match (scope::scope_t const& lhs, scope::scope_t const& rhs, double* rank) const
		{
			ENTER;
			bool res = false;
			if(rank)
			{
				double r, sum = 0;
				iterate(expr, expressions)
				{
					expression_t::op_t op = expr->op;

					bool local = expr->selector->does_match(lhs, rhs, &r);
					if(local)
						sum = std::max(r, sum);

					if(expr->negate)
						local = !local;

					switch(op)
					{
						case expression_t::op_none:  res = local;         break;
						case expression_t::op_or:    res = res || local;  break;
						case expression_t::op_and:   res = res && local;  break;
						case expression_t::op_minus: res = res && !local; break;
					}
				}

				if(res)
					*rank = sum;

				return res;
			}

			iterate(expr, expressions)
			{
				expression_t::op_t op = expr->op;
				if(res && op == expression_t::op_or) // skip ORs when we already have a true value
					continue;
				else if(!res && op == expression_t::op_and) // skip ANDs when we have a false value
					continue;
				else if(!res && op == expression_t::op_minus) // skip intersection when we have a false value
					continue;

				bool local = expr->selector->does_match(lhs, rhs, rank);
				if(expr->negate)
					local = !local;

				switch(op)
				{
					case expression_t::op_none:  res = local;         break;
					case expression_t::op_or:    res = res || local;  break;
					case expression_t::op_and:   res = res && local;  break;
					case expression_t::op_minus: res = res && !local; break;
				}
			}
			return res;
		}

		bool selector_t::does_match (scope::scope_t const& lhs, scope::scope_t const& rhs, double* rank) const
		{
			ENTER;
			if(rank)
			{
				bool res = false;
				double r, sum = 0;
				iterate(composite, composites)
				{
					if(composite->does_match(lhs, rhs, &r))
					{
						sum = std::max(r, sum);
						res = true;
					}
				}
				if(res)
					*rank = sum;
				return res;
			}

			iterate(composite, composites)
			{
				if(composite->does_match(lhs, rhs, rank))
					return true;
			}
			return false;
		}

		bool group_t::does_match (scope::scope_t const& lhs, scope::scope_t const& rhs, double* rank) const
		{
			ENTER;
			return selector.does_match(lhs, rhs, rank);
		}

		bool filter_t::does_match (scope::scope_t const& lhs, scope::scope_t const& rhs, double* rank) const
		{
			ENTER;
			if(filter == both && rank)
			{
				double r1, r2;
				if(selector->does_match(lhs, lhs, &r1) && selector->does_match(rhs, rhs, &r2))
				{
					*rank = std::max(r1, r2);
					return true;
				}
				return false;
			}

			switch(filter)
			{
				case left:  return selector->does_match(lhs, lhs, rank);
				case right: return selector->does_match(rhs, rhs, rank);
				case both:  return selector->does_match(lhs, lhs, rank) && selector->does_match(rhs, rhs, rank);
			}
			return false;
		}

	} /* types */
	
} /* scope */ 
