#ifndef OAK_ASSERT_H_NMHC4G3U
#define OAK_ASSERT_H_NMHC4G3U

#ifdef NDEBUG
#define ASSERT(expr)
#define ASSERT_LT(lhs, rhs)
#define ASSERT_LE(lhs, rhs)
#define ASSERT_GT(lhs, rhs)
#define ASSERT_GE(lhs, rhs)
#define ASSERT_EQ(lhs, rhs)
#define ASSERT_NE(lhs, rhs)
#define ASSERTF(expr, format, args...)
#else
#include "OakDebugLog.h"
std::string OakStackDump (int linesToSkip = 1);
void OakBadAssertion (char const* name, char const* format = NULL, ...);
void OakPrintBadAssertion (char const* lhs, char const* op, char const* rhs, std::string const& realLHS, char const* realOp, std::string const& realRHS, char const* file, int line);

namespace oak
{
	std::string to_s (bool value);
	std::string to_s (int value);
	std::string to_s (size_t value);
	std::string to_s (ssize_t value);
	std::string to_s (double value);
	std::string to_s (char const* value);
	std::string to_s (std::string const& value);
#if 0
	template <typename X, typename Y>
	std::string to_s (std::pair<X, Y> const& pair)
	{
		return "[ " + to_s(pair.first) + ", " + to_s(pair.second) + " ]";
	}

	template <typename T>
	std::string to_s (T const& v)
	{
		bool first = true;
		std::string res = "( ";
		for(typename T::const_iterator it = v.begin(); it != v.end(); ++it)
		{
			if(!std::exchange(first, false))
				res += ", ";
			res += to_s(*it);
		}
		return res + " )";
	}
#endif
}

#define ASSERT(expr)                   if(!(expr)) OakBadAssertion(#expr);
#define ASSERT_LT(lhs, rhs)            if(!((lhs) < (rhs)))  OakPrintBadAssertion(#lhs, "<",  #rhs, oak::to_s(lhs), ">=", oak::to_s(rhs), __FILE__, __LINE__)
#define ASSERT_LE(lhs, rhs)            if(!((lhs) <= (rhs))) OakPrintBadAssertion(#lhs, "<=", #rhs, oak::to_s(lhs), ">",  oak::to_s(rhs), __FILE__, __LINE__)
#define ASSERT_GT(lhs, rhs)            if(!((lhs) > (rhs)))  OakPrintBadAssertion(#lhs, ">",  #rhs, oak::to_s(lhs), "<=", oak::to_s(rhs), __FILE__, __LINE__)
#define ASSERT_GE(lhs, rhs)            if(!((lhs) >= (rhs))) OakPrintBadAssertion(#lhs, ">=", #rhs, oak::to_s(lhs), "<",  oak::to_s(rhs), __FILE__, __LINE__)
#define ASSERT_EQ(lhs, rhs)            if(!((lhs) == (rhs))) OakPrintBadAssertion(#lhs, "==", #rhs, oak::to_s(lhs), "!=", oak::to_s(rhs), __FILE__, __LINE__)
#define ASSERT_NE(lhs, rhs)            if(!((lhs) != (rhs))) OakPrintBadAssertion(#lhs, "!=", #rhs, oak::to_s(lhs), "==", oak::to_s(rhs), __FILE__, __LINE__)
#define ASSERTF(expr, format, args...) if(!(expr)) { OakBadAssertion(#expr, format, ## args); }
#endif

#endif /* end of include guard: OAK_ASSERT_H_NMHC4G3U */
