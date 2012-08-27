#ifndef PRELUDE_CC_PCH_U5CKEP2N
#define PRELUDE_CC_PCH_U5CKEP2N

#ifndef __has_feature
  #define __has_feature(x) 0 // Compatibility with non-clang compilers.
#endif

#if __has_feature(cxx_explicit_conversions) // Constant was introduced in clang r138741, actual feature has been there longer.
  #define EXPLICIT explicit
#else
  #define EXPLICIT
  #warning "No explicit conversion operators."
#endif

#define __STDC_LIMIT_MACROS

#include "prelude.c"
#include "prelude-mac.h"

#include <algorithm>
#include <deque>
#include <functional>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <vector>
#include <memory>
#include <numeric>
#include <thread>
#include <boost/optional.hpp>
#include <boost/variant.hpp>
#include <boost/bimap/bimap.hpp>
#include <boost/bimap/multiset_of.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>

#endif /* end of include guard: PRELUDE_CC_PCH_U5CKEP2N */
