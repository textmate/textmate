#ifndef OAK_ITERATOR_MACROS_H_1SQFE1YN
#define OAK_ITERATOR_MACROS_H_1SQFE1YN

// The essential macro is foreeach, rforeach doing a reverse iteration. Example:
// 	
// 	std::vector<char> v;
// 	foreach(it, v.begin(), v.end())
// 		cout << *it;
// 
// The iterate/riterate macros are taking a container instead, and calls beginof/endof on the container. Example:
// 
// 	std::vector<char> v;
// 	iterate(it, v)
// 		cout << *it;
// 
// The citerate macro is like iterate but will use a temporary variable for the result of the expression passed in as the container, take this example:
// 
// 	std::vector<char> create_vector () { return std::vector<char>(32, 'a'); }
// 
// 	…later…
// 
// 		citerate(it, create_vector())
// 			cout << *it;
// 
// Using the regular iterate macro is not safe since we would call create_vector() twice and (likely) get incompatible begin/end iterators.

#include "stl_iterator_constructors.h"

#ifndef foreach
#define foreach(v,f,l) for(decltype(f) v = (f), _end = (l); v != _end; ++v)
#endif

#ifndef rforeach
#define rforeach(v,f,l) for(decltype(f) v = (l), _begin = (f); v-- != _begin; )
#endif

#ifdef iterate
#undef iterate
#endif
#define iterate(v,c) foreach(v, beginof(c), endof(c))

#ifndef uiterate
#define uiterate(v,c,u) \
decltype(c) u = (c); foreach(v, beginof(u), endof(u))
#endif

#define OAK_UNIQUE          __COUNTER__
#define OAK_MERGE_IMPL(a,c) a ## c
#define OAK_MERGE(a,c)      OAK_MERGE_IMPL(a,c)

#ifndef citerate
#define citerate(v,c) uiterate(v,c,OAK_MERGE(_tmp_,OAK_UNIQUE))
#endif

#ifndef riterate
#define riterate(v,c) foreach(v, rbeginof(c), rendof(c))
#endif

#endif /* end of include guard: OAK_ITERATOR_MACROS_H_1SQFE1YN */
