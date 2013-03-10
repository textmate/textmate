#ifndef OAK_COCOASTL_H_MGEOT7R7
#define OAK_COCOASTL_H_MGEOT7R7

/* ============================= */
/* = Objective-C smart pointer = */
/* ============================= */

template <typename T = NSObject*>
struct objc_ptr
{
	typedef objc_ptr self;
	T M_object;
	objc_ptr () : M_object(nil)						{ reset(nil); }
	objc_ptr (T object) : M_object(nil)				{ reset(object); }
	objc_ptr (self const& rhs) : M_object(nil)	{ reset(rhs); }
	~objc_ptr ()											{ reset(nil); }
	self& operator= (self const& rhs)				{ reset(rhs); return *this; }
	self& operator= (T object)							{ reset(object); return *this; }
	operator T () const									{ return M_object; }
	T get () const											{ return M_object; }
	T operator-> () const								{ return M_object; }

	void reset (T object)
	{
		M_object = object;
	}
};

#endif /* end of include guard: OAK_COCOASTL_H_MGEOT7R7 */
