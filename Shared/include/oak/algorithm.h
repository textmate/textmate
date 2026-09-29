#ifndef OAK_ALGORITHM_H_E3HYH9S3
#define OAK_ALGORITHM_H_E3HYH9S3

#include <math.h>
#include <random>

namespace oak
{
	template <typename _InputIter, typename _ValueT>
	bool contains (_InputIter const& first, _InputIter const& last, _ValueT const& value)
	{
		return std::find(first, last, value) != last;
	}

	template <typename _InputIter1, typename _InputIter2>
	bool has_prefix (_InputIter1 srcFirst, _InputIter1 const& srcLast, _InputIter2 prefixFirst, _InputIter2 const& prefixLast)
	{
		while(srcFirst != srcLast && prefixFirst != prefixLast)
		{
			if(*srcFirst != *prefixFirst)
				return false;
			++srcFirst, ++prefixFirst;
		}
		return prefixFirst == prefixLast;
	}

	template <int size>
	bool has_prefix (std::string const& str, char const(&prefix)[size])
	{
		// String literals include a trailing zero byte which we skip
		return size ? str.compare(0, size-1, prefix) == 0 : true;
	}

	template <typename _InputIter1, typename _InputIter2, typename _InputIter3, typename _OutputIter>
	_OutputIter replace_copy (_InputIter1 it, _InputIter1 const& srcLast, _InputIter2 const& findFirst, _InputIter2 const& findLast, _InputIter3 const& replaceFirst, _InputIter3 const& replaceLast, _OutputIter out)
	{
		while(it != srcLast)
		{
			_InputIter1 const& next = std::search(it, srcLast, findFirst, findLast);
			out = std::copy(it, next, out);
			if((it = next) != srcLast)
			{
				out = std::copy(replaceFirst, replaceLast, out);
				std::advance(it, std::distance(findFirst, findLast));
			}
		}
		return out;
	}

	template <typename _Map>
	void erase_descendent_keys (_Map& map, std::string const& path)
	{
		auto from = map.upper_bound(path);
		auto to = from;
		while(to != map.end() && oak::has_prefix(to->first.begin(), to->first.end(), path.begin(), path.end()))
			++to;
		map.erase(from, to);
	}

	inline double slow_in_out (double t)
	{
		if(t < 1.0)
			t = 1.0 / (1.0 + exp((-t*12.0)+6.0));
		return std::min(t, 1.0);
	}

	template <typename _InputIter>
	void random_shuffle (_InputIter first, _InputIter last)
	{
		static std::mt19937 g(std::random_device{}());
		std::shuffle(first, last, g);
	}
};

template <typename _SrcKeyT, typename _SrcValueT, typename _DstKeyT, typename _DstValueT>
std::map<_SrcKeyT, _SrcValueT>& operator<< (std::map<_DstKeyT, _DstValueT>& dst, std::map<_SrcKeyT, _SrcValueT> const& src)
{
	dst.insert(src.begin(), src.end());
	return dst;
}

#endif /* end of include guard: OAK_ALGORITHM_H_E3HYH9S3 */
