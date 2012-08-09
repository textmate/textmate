#ifndef OAK_COCOASTL_H_MGEOT7R7
#define OAK_COCOASTL_H_MGEOT7R7

#import "misc.h"
#import "iterator_macros.h"
#import <text/utf8.h>

@interface NSObject (compare)
- (NSComparisonResult)compare:(NSObject*)otherObject;
@end

struct OakArrayIteratorRef; // forward declaration

/* =================================== */
/* = Reference counted memory buffer = */
/* = This is a private class!        = */
/* =================================== */

template <typename T> void OakDeleteArray (T* array)		{ delete[] array; }
template <typename T> void OakDeleteObject (T* object)	{ delete object; }

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

/* ============================ */
/* = operator<< for NSObjects = */
/* ============================ */

namespace std
{
	template <typename _CharT, typename _Traits>
	basic_ostream<_CharT, _Traits>& operator<< (basic_ostream<_CharT, _Traits>& out, id obj)
	{
		return out << ([[obj description] UTF8String] ?: "<nil>");
	}
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
	objc_ptr (OakArrayIteratorRef const& ref);
	~objc_ptr ()											{ reset(nil); }
	self& operator= (self const& rhs)				{ reset(rhs); return *this; }
	self& operator= (T object)							{ reset(object); return *this; }
	operator T () const									{ return M_object; }
	T get () const											{ return M_object; }
	T operator-> () const								{ return M_object; }

	bool operator== (T rhs) const						{ return (M_object == rhs) || [M_object isEqual:rhs] == YES; }
	bool operator!= (T rhs) const						{ return !(*this == rhs); }
	bool operator< (T rhs) const						{ return [M_object compare:rhs] == NSOrderedAscending; }

	bool operator> (T rhs) const						{ return !(*this < rhs); }
	bool operator<= (T rhs) const						{ return *this == rhs || *this < rhs; }
	bool operator>= (T rhs) const						{ return *this == rhs || !(*this < rhs); }

	void reset (T object)
	{
		if(M_object != object)
		{
			[M_object release];
			M_object = object ? [object retain] : nil;
		}
	}
};

/* ========================= */
/* = back_inserter support = */
/* ========================= */

template <typename C, typename T, int N, typename S>
struct OakGenericInsert : public S
{
	typedef OakGenericInsert self;

	objc_ptr<C> M_container;
	struct buffer_node { T mem[N]; size_t cur; };
	OakSharedBuffer<buffer_node, OakDeleteObject> M_buf;

	OakGenericInsert (C container) : M_container(container), M_buf(new buffer_node) { M_buf->cur = 0; }
	~OakGenericInsert () { if(M_buf.retain_count() == 1) this->overflow(M_container.get(), M_buf->mem, M_buf->cur); }

	void insert (T value)
	{
		if(M_buf->cur == N)
			M_buf->cur = overflow(M_container.get(), M_buf->mem, M_buf->cur);
		M_buf->mem[M_buf->cur++] = value;
	};
};

template <typename C>
struct OakAddObjectInsert
{
	objc_ptr<C> M_container;
	OakAddObjectInsert (C container) : M_container(container) { }
	void insert (id value) { [M_container.get() addObject:value]; }
};

struct OakInsertInNSIndexSet
{
	objc_ptr<NSMutableIndexSet*> M_container;
	OakInsertInNSIndexSet (NSMutableIndexSet* indexSet) : M_container(indexSet) { }
	size_t insert (NSUInteger i) { [M_container.get() addIndex:i]; return 0; }
};

template <typename T>
struct OakInsertInNSData
{
	size_t overflow (NSMutableData* data, T* bytes, size_t len)
	{
		[data appendBytes:bytes length:sizeof(T) * len];
		return 0;
	}
};

struct OakInsertInNSString
{
	size_t overflow (NSMutableString* str, char* bytes, size_t len)
	{
		char* last = utf8::find_safe_end(bytes, bytes + len);

		CFStringAppendCString((CFMutableStringRef)str, std::string(bytes, last).c_str(), kCFStringEncodingUTF8);
		std::copy(last, bytes + len, bytes);
		return (bytes + len) - last;
	}
};

template <typename C, typename T, typename A>
struct OakGenericInsertIterator : public std::iterator<std::output_iterator_tag, T>, A
{
	typedef OakGenericInsertIterator self;
	OakGenericInsertIterator (C container) : A(container)		{ }
	C container () const													{ return A::M_container; }
	self& operator++ ()													{ return *this; }
	self& operator++ (int)												{ return *this; }
	self& operator* ()													{ return *this; }
	self& operator= (T value)											{ A::insert(value); return *this; }
};

template <typename T> OakGenericInsertIterator<NSMutableData*, T, OakGenericInsert<NSMutableData*, T, 256, OakInsertInNSData<T> > > back_inserter (NSMutableData* data) { return data; }
inline OakGenericInsertIterator<NSMutableData*, char, OakGenericInsert<NSMutableData*, char, 256, OakInsertInNSData<char> > > back_inserter (NSMutableData* data) { return data; }
inline OakGenericInsertIterator<NSMutableString*, char, OakGenericInsert<NSMutableString*, char, 256, OakInsertInNSString> > back_inserter (NSMutableString* str) { return str; }
inline OakGenericInsertIterator<NSMutableArray*, id, OakAddObjectInsert<NSMutableArray*> > back_inserter (NSMutableArray* array) { return array; }
inline OakGenericInsertIterator<NSMutableSet*, id, OakAddObjectInsert<NSMutableSet*> > back_inserter (NSMutableSet* set) { return set; }
inline OakGenericInsertIterator<NSMutableIndexSet*, NSUInteger, OakInsertInNSIndexSet> back_inserter (NSMutableIndexSet* indexSet) { return indexSet; }

/* ============================ */
/* = NSArray iterator support = */
/* ============================ */

struct OakArrayIteratorRef
{
	typedef OakArrayIteratorRef self;
	objc_ptr<NSArray*> M_array;
	NSUInteger M_index;
	OakArrayIteratorRef (NSArray* array, NSUInteger index) : M_array(array), M_index(index) { }

	NSArray* array () const							{ return M_array.get(); }
	NSMutableArray* mutableArray () const		{ assert([array() isKindOfClass:[NSMutableArray class]]); return (NSMutableArray*)array(); }
	id value () const									{ return [array() objectAtIndex:M_index]; }

	id get () const									{ return value(); }
	operator id () const								{ return value(); }
	operator objc_ptr<id> () const				{ return value(); }
	self& operator= (id object)					{ [mutableArray() replaceObjectAtIndex:M_index withObject:object]; return *this; }
	self& operator= (self const& rhs)			{ [mutableArray() replaceObjectAtIndex:M_index withObject:rhs]; return *this; }

	bool operator== (id rhs) const				{ return [value() isEqual:rhs] == YES; }
	bool operator!= (id rhs) const				{ return !(*this == rhs); }
	bool operator< (id rhs) const					{ return [(NSObject*)value() compare:rhs] == NSOrderedAscending; }
	bool operator> (id rhs) const					{ return [(NSObject*)value() compare:rhs] == NSOrderedDescending; }
	bool operator<= (id rhs) const				{ return *this == rhs || *this < rhs; }
	bool operator>= (id rhs) const				{ return *this == rhs || *this > rhs; }
};

template <typename T>
objc_ptr<T>::objc_ptr (OakArrayIteratorRef const& ref) : M_object(nil) { reset(ref); }

struct OakArrayIterator : public std::iterator<std::random_access_iterator_tag, objc_ptr<id>, ptrdiff_t, objc_ptr<id>*, OakArrayIteratorRef>
{
	typedef OakArrayIterator self;
	OakArrayIterator (NSArray* array, NSUInteger index) : M_array(array), M_index(index)		{ }

	self& operator++ ()								{ M_index++; return *this; }
	self& operator-- ()								{ M_index--; return *this; }
	self operator++ (int)							{ self tmp(*this); M_index++; return tmp; }
	self operator-- (int)							{ self tmp(*this); M_index--; return tmp; }
	self operator+ (int i) const					{ return self(M_array, M_index + i); }
	self operator- (int i) const					{ return self(M_array, M_index - i); }
	self& operator+= (int i)						{ M_index += i; return *this; }
	self& operator-= (int i)						{ M_index -= i; return *this; }
	int operator- (self const& rhs) const		{ return M_index - rhs.M_index; }

	bool operator== (self const& rhs) const	{ return M_index == rhs.M_index && M_array.get() == rhs.M_array.get(); }
	bool operator!= (self const& rhs) const	{ return !(*this == rhs); }
	bool operator< (self const& rhs) const		{ return M_index < rhs.M_index; }

	OakArrayIteratorRef operator* ()				{ return OakArrayIteratorRef(M_array, M_index); }
	OakArrayIteratorRef operator[] (int i)		{ return OakArrayIteratorRef(M_array, M_index + i); }

	id operator* () const							{ return [M_array.get() objectAtIndex:M_index]; }
	id operator[] (int i) const					{ return [M_array.get() objectAtIndex:M_index + i]; }

	NSUInteger index () const						{ return M_index; }

private:
	objc_ptr<NSArray*> M_array;
	NSUInteger M_index;
};

/* ========================= */
/* = NSDictionary iterator = */
/* ========================= */

struct OakDictionaryIterator : public std::iterator<std::random_access_iterator_tag, std::pair<NSObject*, NSObject*> const>
{
	typedef OakDictionaryIterator self;
	typedef std::pair<id, id> value_type;

	objc_ptr<NSDictionary*> M_dictionary;
	size_t M_index, M_count;
	mutable value_type M_value;
	mutable OakSharedBuffer<NSObject*> M_buffer;

	OakDictionaryIterator (NSDictionary* dictionary, size_t index) : M_dictionary(dictionary), M_index(index), M_count([dictionary count]) { }
	OakDictionaryIterator (NSDictionary* dictionary, size_t index, OakSharedBuffer<NSObject*> const& buffer) : M_dictionary(dictionary), M_index(index), M_count([dictionary count]), M_buffer(buffer) { }

	self& operator++ ()								{ ++M_index; return *this; }
	self& operator-- ()								{ --M_index; return *this; }
	self operator++ (int)							{ self tmp(*this); ++M_index; return tmp; }
	self operator-- (int)							{ self tmp(*this); --M_index; return tmp; }
	self operator+ (int i) const					{ return self(M_dictionary, M_index + i, M_buffer); }
	self operator- (int i) const					{ return self(M_dictionary, M_index - i, M_buffer); }
	self& operator+= (int i)						{ M_index += i; return *this; }
	self& operator-= (int i)						{ M_index -= i; return *this; }
	int operator- (self const& rhs) const		{ return M_index - rhs.M_index; }

	bool operator== (self const& rhs) const	{ return M_index == rhs.M_index; }
	bool operator!= (self const& rhs) const	{ return !(*this == rhs); }
	bool operator< (self const& rhs) const		{ return M_index < rhs.M_index; }

	void acquire () const
	{
		if(M_buffer.get() == NULL)
		{
			id* mem = new id [2*M_count];
			CFDictionaryGetKeysAndValues((CFDictionaryRef)M_dictionary.get(), (void const**)mem, (void const**)(mem + M_count));
			M_buffer.set(mem);
		}
		M_value = value_type(M_buffer.get()[M_index], M_buffer.get()[M_index + M_count]);
	}

	value_type const* operator-> () const		{ acquire(); return &M_value; }
	value_type const& operator* () const		{ acquire(); return M_value;; }
};

/* ================== */
/* = NSSet iterator = */
/* ================== */

struct OakSetIterator : public std::iterator<std::forward_iterator_tag, id>
{
	typedef OakSetIterator self;

	objc_ptr<NSSet*> M_set;
	objc_ptr<NSEnumerator*> M_enumerator;
	objc_ptr<id> M_value;

	OakSetIterator (NSSet* set, NSEnumerator* enumerator) : M_set(set), M_enumerator(enumerator)	{ advance(); }

	void advance ()									{ M_value = [M_enumerator.get() nextObject]; }
	self& operator++ ()								{ advance(); return *this; }
	self operator++ (int)							{ self tmp(*this); advance(); return tmp; }

	bool operator== (self const& rhs) const	{ return (M_value.get() == nil && rhs.M_value.get() == nil) || (M_set.get() == rhs.M_set.get() && M_value.get() == rhs.M_value.get() && M_enumerator.get() == rhs.M_enumerator.get()); }
	bool operator!= (self const& rhs) const	{ return !(*this == rhs); }

	id operator* () const							{ return M_value; }
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

inline unichar* OakStringIteratorInit (NSString* string)
{
	unichar* res = new unichar[[string length]];
	[string getCharacters:res];
	return res;
}

typedef OakBufferIterator<NSIndexSet*, NSUInteger const, OakIndexSetIteratorInit> OakIndexSetIterator;
typedef OakBufferIterator<NSString*, unichar, OakStringIteratorInit> OakStringIterator;

/* =============================================== */
/* = beginof/endof functions to create iterators = */
/* =============================================== */

inline OakArrayIterator beginof (NSArray* anArray)							{ return OakArrayIterator(anArray, 0); }
inline OakArrayIterator endof (NSArray* anArray)							{ return OakArrayIterator(anArray, [anArray count]); }
inline OakDictionaryIterator beginof (NSDictionary* aDictionary)		{ return OakDictionaryIterator(aDictionary, 0); }
inline OakDictionaryIterator endof (NSDictionary* aDictionary)			{ return OakDictionaryIterator(aDictionary, [aDictionary count]); }
inline OakSetIterator beginof (NSSet* aSet)									{ return OakSetIterator(aSet, [aSet objectEnumerator]); }
inline OakSetIterator endof (NSSet* aSet)										{ return OakSetIterator(aSet, nil); }
inline OakIndexSetIterator beginof (NSIndexSet* anIndexSet)				{ return OakIndexSetIterator(anIndexSet, 0); }
inline OakIndexSetIterator endof (NSIndexSet* anIndexSet)				{ return OakIndexSetIterator(anIndexSet, [anIndexSet count]); }
inline OakStringIterator beginof (NSString* aString)						{ return OakStringIterator(aString, 0); }
inline OakStringIterator endof (NSString* aString)							{ return OakStringIterator(aString, [aString length]); }
template <typename T> T* beginof (NSData* data)								{ return (T*)[data bytes]; }
template <typename T> T* endof (NSData* data)								{ return (T*)((char*)[data bytes] + [data length]); }
//template <typename T> T* beginof (NSMutableData* data)					{ return (T*)[data mutableBytes]; }
//template <typename T> T* endof (NSMutableData* data)					{ return (T*)((char*)[data mutableBytes] + [data length]); }
inline unsigned char const* beginof (NSData* data)							{ return beginof<unsigned char const>(data); }
inline unsigned char const* endof (NSData* data)							{ return endof<unsigned char const>(data); }
//inline unsigned char* beginof (NSMutableData* data)						{ return beginof<unsigned char>(data); }
//inline unsigned char* endof (NSMutableData* data)						{ return endof<unsigned char>(data); }

/* ================================================= */
/* = rbeginof/rendof functions to create iterators = */
/* = wrapped in a std::reverse_iterator            = */
/* ================================================= */

inline std::reverse_iterator<OakArrayIterator> rbeginof (NSArray* anArray)							{ return std::reverse_iterator<OakArrayIterator>(endof(anArray)); }
inline std::reverse_iterator<OakArrayIterator> rendof (NSArray* anArray)							{ return std::reverse_iterator<OakArrayIterator>(beginof(anArray)); }
inline std::reverse_iterator<OakIndexSetIterator> rbeginof (NSIndexSet* anIndexSet)				{ return std::reverse_iterator<OakIndexSetIterator>(endof(anIndexSet)); }
inline std::reverse_iterator<OakIndexSetIterator> rendof (NSIndexSet* anIndexSet)				{ return std::reverse_iterator<OakIndexSetIterator>(beginof(anIndexSet)); }
inline std::reverse_iterator<OakStringIterator> rbeginof (NSString* aString)						{ return std::reverse_iterator<OakStringIterator>(endof(aString)); }
inline std::reverse_iterator<OakStringIterator> rendof (NSString* aString)							{ return std::reverse_iterator<OakStringIterator>(beginof(aString)); }
template <typename T> std::reverse_iterator<T*> rbeginof (NSData* data)								{ return std::reverse_iterator<T*>(endof<T>(data)); }
template <typename T> std::reverse_iterator<T*> rendof (NSData* data)								{ return std::reverse_iterator<T*>(beginof<T>(data)); }
//template <typename T> std::reverse_iterator<T*> rbeginof (NSMutableData* data)					{ return std::reverse_iterator<T*>(endof<T>(data)); }
//template <typename T> std::reverse_iterator<T*> rendof (NSMutableData* data)					{ return std::reverse_iterator<T*>(beginof<T>(data)); }
inline std::reverse_iterator<unsigned char const*> rbeginof (NSData* data)							{ return std::reverse_iterator<unsigned char const*>(endof(data)); }
inline std::reverse_iterator<unsigned char const*> rendof (NSData* data)							{ return std::reverse_iterator<unsigned char const*>(beginof(data)); }
//inline std::reverse_iterator<unsigned char*> rbeginof (NSMutableData* data)						{ return std::reverse_iterator<unsigned char*>(endof(data)); }
//inline std::reverse_iterator<unsigned char*> rendof (NSMutableData* data)						{ return std::reverse_iterator<unsigned char*>(beginof(data)); }

/* ======================================= */
/* = Some functions to create a Cocoa    = */
/* = container from an iterator sequence = */
/* ======================================= */

template <typename _InputIter, typename _OutputIter>
_OutputIter decode_utf8 (_InputIter first, _InputIter const& last, _OutputIter res)
{
	for(; first != last; ++first)
	{
		typename std::iterator_traits<_InputIter>::value_type ch = *first;
		if(ch < 0x80)
		{
			*res++ = ch;
		}
		else
		{
			size_t numBytes = 1;
			while((ch & (1 << (7-numBytes))) && numBytes < 6)
				++numBytes;

			uint32_t decoded = numBytes == 1 ? 0xFFFD : ch & ((1 << (7-numBytes))-1);
			while(--numBytes && ++first != last)
			{
				if((*first & 0xC0) != 0x80)
				{
					++numBytes;
					--first;
					break;
				}
				decoded = (decoded << 6) | (*first & 0x3F);
			}

			if(numBytes)
				*res++ = 0xFFFD;
			else if(decoded < 0x10000)
				*res++ = decoded;
			else
			{
				*res++ = 0xD800 - (0x10000 >> 10) + (decoded >> 10);
				*res++ = 0xDC00 + (decoded & 0x3FF);
			}

			if(first == last)
				break;
		}
	}
	return res;
}

template <typename _InputIter>
NSString* NSStringFromUTF8Sequence (_InputIter const& first, _InputIter const& last)
{
	if(first == last)
		return @"";

	std::vector<UniChar> v;
	decode_utf8(first, last, back_inserter(v));
	return [(NSString*)CFStringCreateWithCharacters(kCFAllocatorDefault, &v[0], v.size()) autorelease];
}

template <typename _InputIter>
NSString* NSStringFromUTF16Sequence (_InputIter first, _InputIter last)
{
	CFIndex numChars = std::distance(first, last);
	UniChar* chars = (UniChar*)CFAllocatorAllocate(kCFAllocatorDefault, sizeof(UniChar) * numChars, 0);
	std::copy(first, last, chars);
	return [(NSString*)CFStringCreateWithCharactersNoCopy(kCFAllocatorDefault, chars, numChars, kCFAllocatorDefault) autorelease];
}

template <typename T, typename _InputIter>
NSData* NSDataFromSequence (_InputIter first, _InputIter last)
{
	CFIndex length = std::distance(first, last);
	T* bytes = (T*)CFAllocatorAllocate(kCFAllocatorDefault, sizeof(T) * length, 0);
	std::copy(first, last, bytes);
	return [(NSData*)CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, (UInt8*)bytes, sizeof(T) * length, kCFAllocatorDefault) autorelease];
}

template <typename _InputIter>
NSArray* NSArrayFromSequence (_InputIter first, _InputIter last)
{
	CFIndex length = std::distance(first, last);
	NSMutableArray* res = [NSMutableArray arrayWithCapacity:length];
	std::copy(first, last, back_inserter(res));
	return res;
}

/* ================================== */
/* = Specially for Cocoa containers = */
/* ================================== */

#define enumerate(container,var) for(NSEnumerator* _enumerator = [container objectEnumerator]; var = [_enumerator nextObject]; )

/* ==================================== */
/* = Adaptable Function Objects which = */
/* = wrap for an Objective-C selector = */
/* ==================================== */

template <typename R = id>
struct method : std::unary_function<id, R>
{
	SEL M_selector;
	method (char const* str) : M_selector(sel_getUid(str)) { }
	method (SEL selector) : M_selector(selector) { }
	R operator() (id obj) const { return ((R(*)(id, SEL, ...))objc_msgSend)(obj, M_selector); }
};

template <typename A = id, typename R = id>
struct unary_method : std::binary_function<id, A, R>
{
	SEL M_selector;
	unary_method (char const* str) : M_selector(sel_getUid(str)) { }
	unary_method (SEL selector) : M_selector(selector) { }
	R operator() (id obj, A arg) const { return ((R(*)(id, SEL, ...))objc_msgSend)(obj, M_selector, arg); }
	std::binder1st< unary_method<A, R> > operator() (id obj) const { return std::binder1st< unary_method<A, R> >(*this, obj); }
};

#endif /* end of include guard: OAK_COCOASTL_H_MGEOT7R7 */
