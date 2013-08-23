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

		void add (T* callback)                          { std::lock_guard<std::mutex> lock(_mutex); ASSERTF(std::find(_callbacks.begin(), _callbacks.end(), callback) == _callbacks.end(), "%p", callback); _callbacks.push_back(callback); }
		void remove (T* callback)                       { std::lock_guard<std::mutex> lock(_mutex); ASSERTF(std::find(_callbacks.begin(), _callbacks.end(), callback) != _callbacks.end(), "%p", callback); _callbacks.erase(std::find(_callbacks.begin(), _callbacks.end(), callback)); }

		template <typename M, typename... Args> void operator () (M fun, Args... args) const { for(auto const& cb : dup()) (cb->*fun)(args...); }

		iterator begin () const                         { return _callbacks.begin(); }
		iterator end () const                           { return _callbacks.end(); }

	private:
		std::vector<T*> dup () const                    { std::lock_guard<std::mutex> lock(_mutex); return _callbacks; }
		std::vector<T*> _callbacks;
		mutable std::mutex _mutex;
	};

} /* oak */

#endif /* end of include guard: OAK_CALLBACKS_H_UQZK6SYT */
