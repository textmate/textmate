#include "ranker.h"
#include "case.h"
#include <oak/debug.h>

OAK_DEBUG_VAR(Ranker);

static bool is_subset (std::string const& needle, std::string const& haystack)
{
	std::string::size_type n = 0, m = 0;
	while(n < needle.size() && m < haystack.size())
	{
		if(needle[n] == haystack[m] || toupper(needle[n]) == haystack[m])
			++n;
		++m;
	}
	D(DBF_Ranker, bug("‘%s’ ⊂ ‘%s’: %s\n", needle.c_str(), haystack.c_str(), BSTR(n == needle.size())););
	return n == needle.size();
}

#ifndef NDEBUG
static void print_matrix (size_t* matrix, size_t n, size_t m, std::string const& rowLabel, std::string const& colLabel)
{
	fprintf(stderr, "   |");
	for(size_t j = 0; j < m; ++j)
		fprintf(stderr, "%3c", colLabel[j]);
	fprintf(stderr, "\n");

	fprintf(stderr, "---+");
	for(size_t j = 0; j < m; ++j)
		fprintf(stderr, "---");
	fprintf(stderr, "\n");

	for(size_t i = 0; i < n; ++i)
	{
		fprintf(stderr, " %c |", rowLabel[i]);
		for(size_t j = 0; j < m; ++j)
		{
			fprintf(stderr, "%3zu", matrix[i*m + j]);
		}
		fprintf(stderr, "\n");
	}
	fprintf(stderr, "\n");
}
#endif

static double calculate_rank (std::string const& lhs, std::string const& rhs, std::vector< std::pair<size_t, size_t> >* out)
{
	size_t const n = lhs.size();
	size_t const m = rhs.size();
	size_t matrix[n][m], first[n], last[n];
	bool capitals[m];
	bzero(matrix, sizeof(matrix));
	std::fill_n(&first[0], n, m);
	std::fill_n(&last[0],  n, 0);

	bool at_bow = true;
	for(size_t j = 0; j < m; ++j)
	{
		char ch = rhs[j];
		capitals[j] = at_bow && isalnum(ch) || isupper(ch);
		at_bow = !isalnum(ch) && ch != '\'' && ch != '.';
	}

	for(size_t i = 0; i < n; ++i)
	{
		size_t j = i == 0 ? 0 : first[i-1] + 1;
		for(; j < m; ++j)
		{
			if(tolower(lhs[i]) == tolower(rhs[j]))
			{
				matrix[i][j] = i == 0 || j == 0 ? 1 : matrix[i-1][j-1] + 1;
				first[i]     = std::min(j, first[i]);
				last[i]      = std::max(j+1, last[i]);
			}
		}
	}

	for(ssize_t i = n-1; i > 0; --i)
	{
		size_t bound = last[i]-1;
		if(bound < last[i-1])
		{
			while(first[i-1] < bound && matrix[i-1][bound-1] == 0)
				--bound;
			last[i-1] = bound;
		}
	}

	for(ssize_t i = n-1; i > 0; --i)
	{
		for(size_t j = first[i]; j < last[i]; ++j)
		{
			if(matrix[i][j] && matrix[i-1][j-1])
				matrix[i-1][j-1] = matrix[i][j];
		}
	}

	for(size_t i = 0; i < n; ++i)
	{
		for(size_t j = first[i]; j < last[i]; ++j)
		{
			if(matrix[i][j] > 1 && i+1 < n && j+1 < m)
				matrix[i+1][j+1] = matrix[i][j] - 1;
		}
	}

	D(DBF_Ranker, print_matrix(&matrix[0][0], n, m, lhs, rhs););

	// =========================
	// = Greedy walk of Matrix =
	// =========================

	size_t capitalsTouched = 0; // 0-n
	size_t substrings = 0;      // 1-n
	size_t prefixSize = 0;      // 0-m

	size_t i = 0;
	while(i < n)
	{
		size_t bestJIndex = 0;
		size_t bestJLength = 0;
		for(size_t j = first[i]; j < last[i]; ++j)
		{
			if(matrix[i][j] && capitals[j])
			{
				bestJIndex = j;
				bestJLength = matrix[i][j];

				for(size_t k = j; k < j + bestJLength; ++k)
					capitalsTouched += capitals[k] ? 1 : 0;

				break;
			}
			else if(bestJLength < matrix[i][j])
			{
				bestJIndex = j;
				bestJLength = matrix[i][j];
			}
		}

		if(i == 0)
			prefixSize = bestJIndex;

		size_t len = 0;
		bool foundCapital = false;
		do {

			++i; ++len;
			first[i] = std::max(bestJIndex + len, first[i]);
			if(len < bestJLength && n < 4)
			{
				if(capitals[first[i]])
					continue;

				for(size_t j = first[i]; j < last[i] && !foundCapital; ++j)
				{
					if(matrix[i][j] && capitals[j])
						foundCapital = true;
				}
			}

		} while(len < bestJLength && !foundCapital);

		if(out)
			out->push_back(std::make_pair(bestJIndex, bestJIndex + len));

		++substrings;
	}

	// ================================
	// = Calculate rank based on walk =
	// ================================

	size_t totalCapitals = std::count(&capitals[0], &capitals[0] + m, true);
	double score = 0.0;
	double denom = n*(n+1) + 1;
	if(n == capitalsTouched)
	{
		score = (denom - 1) / denom;
	}
	else
	{
		double subtract = substrings * n + (n - capitalsTouched);
		score = (denom - subtract) / denom;
	}
	score += (m - prefixSize) / (double)m / (2*denom);
	score += capitalsTouched / (double)totalCapitals / (4*denom);
	score += n / (double)m / (8*denom);

	D(DBF_Ranker, bug("‘%s’ ⊂ ‘%s’: %.3f\n", lhs.c_str(), rhs.c_str(), score););
	return score;
}

namespace oak
{
	std::string normalize_filter (std::string const& filter)
	{
		std::string res = "";
		for(auto const& ch : text::lowercase(filter))
		{
			if(ch != ' ')
				res += ch;
		}
		return res;
	}

	double rank (std::string const& filter, std::string const& candidate, std::vector< std::pair<size_t, size_t> >* out)
	{
		if(filter.empty())
			return 1;
		else if(!is_subset(filter, candidate))
			return 0;
		else if(filter == candidate)
		{
			if(out)
				out->push_back(std::make_pair(0, filter.size()));
			return 1;
		}
		return calculate_rank(filter, candidate, out);
	}

} /* oak */
