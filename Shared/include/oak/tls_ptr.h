#ifndef TLS_PTR_H_186F0BAB
#define TLS_PTR_H_186F0BAB

namespace oak
{
	template<typename T>
	struct tls_ptr_t
	{
		tls_ptr_t () { pthread_key_create(&_key, destructor); }
		~tls_ptr_t () { }

		tls_ptr_t (tls_ptr_t const& rhs) = delete;
		tls_ptr_t& operator= (tls_ptr_t const& rhs) = delete;

		T& operator*  () const { return *get(); }
		T* operator-> () const { return  get(); }

	private:
		T* get () const
		{
			T* valuePtr = static_cast<T*>(pthread_getspecific(_key));
			if(!valuePtr)
			{
				pthread_setspecific(_key, new T);
				valuePtr = static_cast<T*>(pthread_getspecific(_key));
			}
			return valuePtr;
		}

		static void destructor (void* valuePtr) { delete static_cast<T*>(valuePtr); }

		pthread_key_t _key;
	};
} /* oak */

#endif /* end of include guard: TLS_PTR_H_186F0BAB */
