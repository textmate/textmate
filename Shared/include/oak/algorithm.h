#ifndef OAK_ALGORITHM_H_E3HYH9S3
#define OAK_ALGORITHM_H_E3HYH9S3

namespace oak
{
	template <typename _InputIter, typename _ValueT>
	bool contains (_InputIter const& first, _InputIter const& last, _ValueT const& value)
	{
		return std::find(first, last, value) != last;
	}

	template <typename _ValueT>
	_ValueT cap (_ValueT min, _ValueT cur, _ValueT max)
	{
		return std::max(min, std::min(cur, max));
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

	template <typename _BidirectionalIterator>
	std::reverse_iterator<_BidirectionalIterator> rev_iter (_BidirectionalIterator it)
	{
		return std::reverse_iterator<_BidirectionalIterator>(it);
	}

	inline double slow_in_out (double t)
	{
		if(t < 1.0)
			t = 1.0 / (1.0 + exp((-t*12.0)+6.0));
		return std::min(t, 1.0);
	}
};

template <typename _SrcKeyT, typename _SrcValueT, typename _DstKeyT, typename _DstValueT>
std::map<_SrcKeyT, _SrcValueT>& operator<< (std::map<_DstKeyT, _DstValueT>& dst, std::map<_SrcKeyT, _SrcValueT> const& src)
{
	dst.insert(src.begin(), src.end());
	return dst;
}

#endif /* end of include guard: OAK_ALGORITHM_H_E3HYH9S3 */
