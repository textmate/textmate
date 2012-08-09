#ifndef OAK_STL_ITERATOR_CONSTRUCTORS_H_2CBGJHQM
#define OAK_STL_ITERATOR_CONSTRUCTORS_H_2CBGJHQM

/* =============================================== */
/* = beginof/endof functions to create iterators = */
/* =============================================== */

inline char const* beginof (char const* cStr)								{ return cStr; }
inline char const* endof (char const* cStr)									{ return cStr + strlen(cStr); }
template <typename T, int N> T* beginof (T (&a)[N])						{ return a; }
template <typename T, int N> T* endof (T (&a)[N])							{ return a + N; }
template <typename T, int N, int M> T (*beginof(T (&m)[N][M]))[M]		{ return m; }
template <typename T, int N, int M> T (*endof(T (&m)[N][M]))[M]		{ return m + N; }
template <class T> typename T::const_iterator beginof (T const& c)	{ return c.begin(); }
template <class T> typename T::const_iterator endof (T const& c)		{ return c.end(); }
template <class T> typename T::iterator beginof (T& c)					{ return c.begin(); }
template <class T> typename T::iterator endof (T& c)						{ return c.end(); }
template <typename T> T beginof (std::pair<T, T> const& p)						{ return p.first; }
template <typename T> T endof (std::pair<T, T> const& p)							{ return p.second; }

/* ================================================= */
/* = rbeginof/rendof functions to create iterators = */
/* = wrapped in a std::reverse_iterator            = */
/* ================================================= */

inline std::reverse_iterator<char const*> rbeginof (char const* cStr)								{ return std::reverse_iterator<char const*>(endof(cStr)); }
inline std::reverse_iterator<char const*> rendof (char const* cStr)									{ return std::reverse_iterator<char const*>(beginof(cStr)); }
template <typename T, int N> std::reverse_iterator<T*> rbeginof (T (&a)[N])						{ return std::reverse_iterator<T*>(endof(a)); }
template <typename T, int N> std::reverse_iterator<T*> rendof (T (&a)[N])							{ return std::reverse_iterator<T*>(beginof(a)); }
template <typename T, int N, int M> std::reverse_iterator<T(*)[M]> rbeginof (T (&m)[N][M])	{ return std::reverse_iterator<T(*)[M]>(endof(m)); }
template <typename T, int N, int M> std::reverse_iterator<T(*)[M]> rendof (T (&m)[N][M])		{ return std::reverse_iterator<T(*)[M]>(beginof(m)); }
template <class T> typename T::const_reverse_iterator rbeginof (T const& c)						{ return c.rbegin(); }
template <class T> typename T::const_reverse_iterator rendof (T const& c)							{ return c.rend(); }
template <class T> typename T::reverse_iterator rbeginof (T& c)										{ return c.rbegin(); }
template <class T> typename T::reverse_iterator rendof (T& c)											{ return c.rend(); }

#endif /* end of include guard: OAK_STL_ITERATOR_CONSTRUCTORS_H_2CBGJHQM */
