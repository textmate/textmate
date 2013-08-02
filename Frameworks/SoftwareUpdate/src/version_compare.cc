#include "version_compare.h"

static bool is_numeric (std::string const& str)
{
	return str.find_first_not_of("0123456789") == std::string::npos;
}

static std::vector<std::string> components (std::string const& str)
{
	std::vector<std::string> res;

	for(size_t from = 0; from < str.size(); )
	{
		size_t to = str.find_first_of(".-+", from);
		if(to != std::string::npos)
		{
			res.push_back(str.substr(from, to - from));
			res.push_back(str.substr(to, 1));
			from = to + 1;
		}
		else
		{
			res.push_back(str.substr(from));
			break;
		}
	}

	return res;
}

static std::vector<std::string> strip_trailing_zeroes (std::vector<std::string> const& src)
{
	auto last = std::find_if_not(src.begin(), src.end(), [](std::string const& str){ return is_numeric(str) || str == "."; });
	auto from = last;

	while(last != src.begin() && is_numeric(*(last-1)) && std::stol(*(last-1)) == 0 && --last != src.begin())
		--last;

	std::vector<std::string> res;
	res.insert(res.end(), src.begin(), last);
	res.insert(res.end(), from, src.end());
	return res;
}

bool version_less (std::string const& lhs, std::string const& rhs)
{
	if(lhs == NULL_STR)
		return rhs != NULL_STR;
	else if(rhs == NULL_STR)
		return false;

	auto lhsV = strip_trailing_zeroes(components(lhs));
	auto rhsV = strip_trailing_zeroes(components(rhs));

	auto lhsIt = lhsV.begin();
	auto rhsIt = rhsV.begin();

	while(lhsIt != lhsV.end() && rhsIt != rhsV.end())
	{
		bool numberCompare = is_numeric(*lhsIt) && is_numeric(*rhsIt);
		if(*lhsIt != *rhsIt && (!numberCompare || std::stol(*lhsIt) != std::stol(*rhsIt)))
		{
			if(numberCompare)
				return std::stol(*lhsIt) < std::stol(*rhsIt);
			else if(lhsIt->find_first_not_of(".-+") == std::string::npos)
				return *lhsIt == "-" || (*lhsIt == "+" && *rhsIt == ".");
			return *lhsIt < *rhsIt;
		}
		else if(*lhsIt == "+")
		{
			return false;
		}

		++lhsIt;
		++rhsIt;
	}

	return lhsIt != lhsV.end() ? *lhsIt == "-" : (rhsIt != rhsV.end() && *rhsIt == ".");
}
