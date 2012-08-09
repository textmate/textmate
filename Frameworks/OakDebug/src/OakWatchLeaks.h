#ifndef OAK_WATCH_LEAKS_H_9QHBFWF9
#define OAK_WATCH_LEAKS_H_9QHBFWF9

#ifdef NDEBUG
#define WATCH_LEAKS(type)
#define OBJC_WATCH_LEAKS(type)
#else

namespace oak_debug
{
	void PUBLIC increase (char const* name);
	void PUBLIC decrease (char const* name);

	struct PUBLIC watch_leaks_t
	{
		char const* class_name;

		watch_leaks_t (char const* class_name) : class_name(class_name)        { oak_debug::increase(class_name); }
		watch_leaks_t (watch_leaks_t const& rhs) : class_name(rhs.class_name)  { oak_debug::increase(class_name); }
		~watch_leaks_t ()                                                      { oak_debug::decrease(class_name); }
	};

} /* oak_debug */

#define OWL_PASTE(x, y)    x##y
#define _WATCH_LEAKS(suffix, type) \
	struct OWL_PASTE(watch_leaks_, suffix) : oak_debug::watch_leaks_t \
	{ \
		OWL_PASTE(watch_leaks_, suffix) () : watch_leaks_t( #type ) {} \
	} _instance_counter_helper;

#define WATCH_LEAKS(type)     _WATCH_LEAKS(__LINE__, type)
#define OBJC_WATCH_LEAKS(type) _WATCH_LEAKS(type, type)
#endif

#endif /* end of include guard: OAK_WATCH_LEAKS_H_9QHBFWF9 */
