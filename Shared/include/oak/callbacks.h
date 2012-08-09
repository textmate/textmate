#ifndef OAK_CALLBACKS_H_UQZK6SYT
#define OAK_CALLBACKS_H_UQZK6SYT

#include "oak.h"
#include "debug.h"

namespace oak
{
	template <typename T, bool AllowNonEmpty = false>
	struct callbacks_t
	{
		typedef typename std::vector<T*>::const_iterator iterator;

		callbacks_t ()                                  { }
		callbacks_t (callbacks_t const& rhs)            { } // intentionally skip copy of rhs._callbacks
		callbacks_t& operator= (callbacks_t const& rhs) { _callbacks.clear(); return *this; }
		~callbacks_t ()                                 { ASSERT(_callbacks.empty() || AllowNonEmpty); }

		void swap (callbacks_t& rhs)                    { _callbacks.swap(rhs._callbacks); }

		void add (T* callback)                          { ASSERTF(std::find(_callbacks.begin(), _callbacks.end(), callback) == _callbacks.end(), "%p", callback); _callbacks.push_back(callback); }
		void remove (T* callback)                       { ASSERTF(std::find(_callbacks.begin(), _callbacks.end(), callback) != _callbacks.end(), "%p", callback); _callbacks.erase(std::find(_callbacks.begin(), _callbacks.end(), callback)); }

		template <typename M> void operator () (M fun) const                                                                                                 { citerate(cb, dup()) (*cb->*fun)();           }
		template <typename M, typename A> void operator () (M fun, A const& a) const                                                                         { citerate(cb, dup()) (*cb->*fun)(a);          }
		template <typename M, typename A, typename B> void operator () (M fun, A const& a, B const& b) const                                                 { citerate(cb, dup()) (*cb->*fun)(a, b);       }
		template <typename M, typename A, typename B, typename C> void operator () (M fun, A const& a, B const& b, C const& c) const                         { citerate(cb, dup()) (*cb->*fun)(a, b, c);    }
		template <typename M, typename A, typename B, typename C, typename D> void operator () (M fun, A const& a, B const& b, C const& c, D const& d) const { citerate(cb, dup()) (*cb->*fun)(a, b, c, d); }

		iterator begin () const                         { return _callbacks.begin(); }
		iterator end () const                           { return _callbacks.end(); }

	private:
		std::vector<T*> dup () const                    { return _callbacks; }
		std::vector<T*> _callbacks;
	};

} /* oak */

#endif /* end of include guard: OAK_CALLBACKS_H_UQZK6SYT */
