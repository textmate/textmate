#include "connection.h"
#include <oak/debug.h>

namespace
{
	template <int T> class width_tag_t { };

	template <typename T> T to_network (T value, width_tag_t<2>)   { return CFSwapInt16HostToBig(value); }
	template <typename T> T to_network (T value, width_tag_t<4>)   { return CFSwapInt32HostToBig(value); }
	template <typename T> T to_network (T value, width_tag_t<8>)   { return CFSwapInt64HostToBig(value); }
	template <typename T> T to_network (T value)                   { return to_network(value, width_tag_t<sizeof(T)>()); }

	template <typename T> T from_network (T value, width_tag_t<2>) { return CFSwapInt16BigToHost(value); }
	template <typename T> T from_network (T value, width_tag_t<4>) { return CFSwapInt32BigToHost(value); }
	template <typename T> T from_network (T value, width_tag_t<8>) { return CFSwapInt64BigToHost(value); }
	template <typename T> T from_network (T value)                 { return from_network(value, width_tag_t<sizeof(T)>()); }
}

connection_t::connection_t (int socket) : _socket(socket)
{
}

connection_t::connection_t (std::string const& socketPath)
{
	_socket = socket(AF_UNIX, SOCK_STREAM, 0);
	struct sockaddr_un addr = { 0, AF_UNIX };
	ASSERT_LT(socketPath.size(), sizeof(addr.sun_path));
	strcpy(addr.sun_path, socketPath.c_str());
	addr.sun_len = SUN_LEN(&addr);

	connect(_socket, (sockaddr*)&addr, sizeof(addr));
}

connection_t& connection_t::operator<< (bool value)
{
	write(_socket, &value, sizeof(value));
	return *this;
}

connection_t& connection_t::operator<< (int value)
{
	value = to_network(value);
	write(_socket, &value, sizeof(value));
	return *this;
}

connection_t& connection_t::operator<< (size_t value)
{
	value = to_network(value);
	write(_socket, &value, sizeof(value));
	return *this;
}

connection_t& connection_t::operator<< (char const* str)
{
	return *this << std::string(str);
}

connection_t& connection_t::operator<< (std::string const& value)
{
	*this << value.size();
	write(_socket, value.data(), value.size());
	return *this;
}

connection_t& connection_t::operator>> (bool& res)
{
	read(_socket, &res, sizeof(res));
	return *this;
}

connection_t& connection_t::operator>> (int& res)
{
	read(_socket, &res, sizeof(res));
	res = from_network(res);
	return *this;
}

connection_t& connection_t::operator>> (size_t& res)
{
	read(_socket, &res, sizeof(res));
	res = from_network(res);
	return *this;
}

connection_t& connection_t::operator>> (std::string& res)
{
	std::string::size_type size = 0;
	*this >> size;
	res.resize(size);
	for(size_t i = 0; i < size; )
	{
		ssize_t len = read(_socket, &res[0] + i, size - i);
		if(len == -1)
		{
			perror("connection_t: read");
			abort();
		}
		i += len;
	}
	return *this;
}
