#ifndef CONNECTION_H_B2QX4NXP
#define CONNECTION_H_B2QX4NXP

#include <io/src/socket.h>
#include <oak/oak.h>

template <typename T> struct container_traits { typedef typename T::value_type insert_type; };
template <template <typename, typename, typename, typename> class T, typename A, typename B, typename C, typename D> struct container_traits < T<A, B, C, D> > { typedef typename std::pair<A, B> insert_type; };

struct connection_t
{
	connection_t (int socket = -1);
	connection_t (std::string const& socketPath);

	explicit operator bool () const { return _socket; }

	connection_t& operator<< (bool value);
	connection_t& operator<< (int value);
	connection_t& operator<< (size_t value);
	connection_t& operator<< (char const* str);
	connection_t& operator<< (std::string const& value);

	template <typename A, typename B>
	connection_t& operator<< (std::pair<A, B> const& value)
	{
		return *this << value.first << value.second;
	}

	template <typename T>
	connection_t& operator<< (T const& container)
	{
		*this << container.size();
		for(auto const& value : container)
			*this << value;
		return *this;
	}

	connection_t& operator>> (bool& res);
	connection_t& operator>> (int& res);
	connection_t& operator>> (size_t& res);
	connection_t& operator>> (std::string& res);

	template <typename A, typename B>
	connection_t& operator>> (std::pair<A, B>& value)
	{
		return *this >> value.first >> value.second;
	}

	template <typename T>
	connection_t& operator>> (T& container)
	{
		int count = 0;
		for(*this >> count; count; --count)
		{
			typename container_traits<T>::insert_type value;
			container.insert(container.end(), (*this >> value, value));
		}
		return *this;
	}

private:
	socket_t _socket;
};

#endif /* end of include guard: CONNECTION_H_B2QX4NXP */
