#ifndef OAK_COCOASTL_H_MGEOT7R7
#define OAK_COCOASTL_H_MGEOT7R7

#import "misc.h"
#import "iterator_macros.h"

/* =================================== */
/* = Reference counted memory buffer = */
/* = This is a private class!        = */
/* =================================== */

template <typename T> void OakDeleteArray (T* array)		{ delete[] array; }

template <typename T, void(*F)(T*) = OakDeleteArray>
struct OakSharedBuffer
{
	struct impl
	{
		impl () : M_retainCount(1)					{ M_buffer = NULL; }
		~impl ()											{ F(M_buffer); }

		T* M_buffer;
		size_t M_retainCount;
	};

	typedef OakSharedBuffer self;
	mutable impl* M_pimpl;

	void retain ()										{ ++M_pimpl->M_retainCount; }
	void release ()									{ if(M_pimpl && --M_pimpl->M_retainCount == 0) delete M_pimpl; }
	size_t retain_count () const					{ return M_pimpl ? M_pimpl->M_retainCount : 1; }
	impl* get_pimpl () const						{ if(M_pimpl == NULL) M_pimpl = new impl(); return M_pimpl; }

	OakSharedBuffer ()								{ M_pimpl = NULL; }
	OakSharedBuffer (T* buf)						{ M_pimpl = NULL; set(buf); }
	OakSharedBuffer (self const& rhs)			{ M_pimpl = rhs.get_pimpl(); retain(); }
	~OakSharedBuffer ()								{ release(); }
	self& operator= (self const& rhs)			{ if(this != &rhs) { this->~self(); new (this) self(rhs); } return *this; }
	T* get () const									{ return M_pimpl ? M_pimpl->M_buffer : NULL; }
	void set (T* buf)									{ if(M_pimpl) F(M_pimpl->M_buffer); get_pimpl()->M_buffer = buf; }
	T& operator* ()									{ assert(M_pimpl); assert(M_pimpl->M_buffer); return *M_pimpl->M_buffer; }
	T& operator[] (int i)							{ assert(M_pimpl); assert(M_pimpl->M_buffer); return M_pimpl->M_buffer[i]; }
	T* operator-> ()									{ assert(M_pimpl); assert(M_pimpl->M_buffer); return M_pimpl->M_buffer; }
};

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
		if(M_object != object)
		{
			[M_object release];
			M_object = object ? [object retain] : nil;
		}
	}
};

/* ========================================== */
/* = Generic superclass for iterators which = */
/* = need to copy values from the container = */
/* ========================================== */

template <typename C, typename T, T*(*F)(C)>
struct OakBufferIterator : public std::iterator<std::random_access_iterator_tag, T>
{
	typedef OakBufferIterator self;

	objc_ptr<C> M_container;
	mutable OakSharedBuffer<T> M_buffer;
	size_t M_index;

	OakBufferIterator () { }
	OakBufferIterator (C container, size_t index) : M_container(container), M_index(index) { }
	OakBufferIterator (C container, size_t index, OakSharedBuffer<T> const& buffer) : M_container(container), M_buffer(buffer), M_index(index) { }

	self& operator++ ()								{ M_index++; return *this; }
	self& operator-- ()								{ M_index--; return *this; }
	self operator++ (int)							{ self tmp(*this); M_index++; return tmp; }
	self operator-- (int)							{ self tmp(*this); M_index--; return tmp; }
	self operator+ (int i) const					{ return self(M_container, M_index + i, M_buffer); }
	self operator- (int i) const					{ return self(M_container, M_index - i, M_buffer); }
	self& operator+= (int i)						{ M_index += i; return *this; }
	self& operator-= (int i)						{ M_index -= i; return *this; }
	int operator- (self const& rhs) const		{ return M_index - rhs.M_index; }

	bool operator== (self const& rhs) const	{ return M_index == rhs.M_index && M_container.get() == rhs.M_container.get(); }
	bool operator!= (self const& rhs) const	{ return M_index != rhs.M_index || M_container.get() != rhs.M_container.get(); }
	bool operator< (self const& rhs) const		{ return M_index < rhs.M_index; }

	void acquire () const							{ if(M_buffer.get() == NULL) M_buffer.set(F(M_container)); }

	T& operator* () const							{ acquire(); return M_buffer[M_index]; }
	T& operator[] (int i) const					{ acquire(); return M_buffer[M_index + i]; }
};

inline NSUInteger const* OakIndexSetIteratorInit (NSIndexSet* indexSet)
{
	NSUInteger size = [indexSet count];
	NSUInteger* res = new NSUInteger[size];
	NSRange range = NSMakeRange([indexSet firstIndex], [indexSet lastIndex]-[indexSet firstIndex] + 1);
	[indexSet getIndexes:res maxCount:size inIndexRange:&range];
	return res;
}

typedef OakBufferIterator<NSIndexSet*, NSUInteger const, OakIndexSetIteratorInit> OakIndexSetIterator;

/* =============================================== */
/* = beginof/endof functions to create iterators = */
/* =============================================== */

inline OakIndexSetIterator beginof (NSIndexSet* anIndexSet)				{ return OakIndexSetIterator(anIndexSet, 0); }
inline OakIndexSetIterator endof (NSIndexSet* anIndexSet)				{ return OakIndexSetIterator(anIndexSet, [anIndexSet count]); }

#endif /* end of include guard: OAK_COCOASTL_H_MGEOT7R7 */
