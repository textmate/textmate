#ifndef CGRECT_H_MTNVMRAF
#define CGRECT_H_MTNVMRAF

#include <oak/oak.h>
#include <oak/debug.h>

struct CGRectLessThan
{
	bool operator() (CGRect const& lhs, CGRect const& rhs) const
	{
		return
			lhs.origin.y    < rhs.origin.y    || lhs.origin.y    == rhs.origin.y    && (
			lhs.size.height < rhs.size.height || lhs.size.height == rhs.size.height && (
			lhs.origin.x    < rhs.origin.x    || lhs.origin.x    == rhs.origin.x    && (
			lhs.size.width  < rhs.size.width  || lhs.size.width  == rhs.size.width  && (
		false))));
	}
};

template <typename _OutputIter>
_OutputIter OakRectDifference (CGRect const& r, CGRect s, _OutputIter out)
{
	s = CGRectIntersection(r, s);
	if(CGRectIsEmpty(s))
	{
		*out++ = r;
		return out;
	}

	CGFloat x0 = CGRectGetMinX(r), x3 = CGRectGetMaxX(r);
	CGFloat y0 = CGRectGetMinY(r), y3 = CGRectGetMaxY(r);
	CGFloat x1 = CGRectGetMinX(s), x2 = CGRectGetMaxX(s);
	CGFloat y1 = CGRectGetMinY(s), y2 = CGRectGetMaxY(s);

	ASSERT(x0 <= x1 && x1 <= x2 && x2 <= x3 && y0 <= y1 && y1 <= y2 && y2 <= y3);

	CGRect r0 = CGRectMake(x0, y0, x1 - x0, y3 - y0);
	CGRect r1 = CGRectMake(x2, y0, x3 - x2, y3 - y0);
	CGRect r2 = CGRectMake(x1, y0, x2 - x1, y1 - y0);
	CGRect r3 = CGRectMake(x1, y2, x2 - x1, y3 - y2);

	CGRect const rects[] = { r0, r1, r2, r3 };
	for(CGRect const& rect : rects)
	{
		if(!CGRectIsEmpty(rect))
			*out++ = rect;
	}

	return out;
}

template <typename _OutputIter>
_OutputIter OakRectDifference (std::vector<CGRect> const& rList, std::vector<CGRect> const& sList, _OutputIter out)
{
	for(auto const& r : rList)
	{
		std::vector<CGRect> tList(1, r);
		for(auto const& s : sList)
		{
			std::vector<CGRect> tmp;
			for(auto const& t : tList)
				OakRectDifference(t, s, back_inserter(tmp));
			tList.swap(tmp);
		}
		out = std::copy(tList.begin(), tList.end(), out);
	}
	return out;
}

template <typename _OutputIter>
_OutputIter OakRectSymmetricDifference (std::vector<CGRect> const& rList, std::vector<CGRect> const& sList, _OutputIter out)
{
	out = OakRectDifference(rList, sList, out);
	out = OakRectDifference(sList, rList, out);
	return out;
}

#endif /* end of include guard: CGRECT_H_MTNVMRAF */
