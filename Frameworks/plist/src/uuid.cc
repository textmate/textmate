#include "uuid.h"

namespace oak
{
	uuid_t::uuid_t ()                         { uuid_clear(data); }
	uuid_t::uuid_t (uuid_t const& rhs)        { uuid_copy(data, rhs.data); }
	uuid_t::uuid_t (char const* str)          { reset(str); }
	uuid_t::uuid_t (std::string const& str)   { reset(str); }
	uuid_t::uuid_t (CFUUIDBytes const& bytes) { uuid_copy(data, &bytes.byte0); }
	uuid_t::uuid_t (::uuid_t const& uuid)     { uuid_copy(data, uuid); }

	uuid_t& uuid_t::generate ()
	{
		uuid_generate(data);
		return *this;
	}

	bool uuid_t::is_valid (std::string const& str)
	{
		static std::string const sep = "------------------------------------";
		::uuid_t data;
		return sep == str || uuid_parse(str.c_str(), data) == 0;
	}

	void uuid_t::reset (std::string const& str)
	{
		static std::string const sep = "------------------------------------";
		if(sep == str)
		{
			uuid_clear(data);
			return;
		}

		if(uuid_parse(str.c_str(), data) != 0)
		{
			fprintf(stderr, "uuid_t: error parsing ‘%s’\n", str.c_str());
			uuid_clear(data);
		}
	}

	bool uuid_t::operator< (uuid_t const& rhs) const
	{
		return uuid_compare(data, rhs.data) < 0;
	}

	bool uuid_t::operator== (uuid_t const& rhs) const
	{
		return uuid_compare(data, rhs.data) == 0;
	}

	bool uuid_t::operator!= (uuid_t const& rhs) const
	{
		return uuid_compare(data, rhs.data) != 0;
	}

	uuid_t::operator bool () const
	{
		return *this != uuid_t();
	}

	uuid_t::operator std::string () const
	{
		std::string str(40, ' ');
		uuid_unparse_upper(data, &str[0]);
		str.resize(36);
		return str;
	}

	std::string to_s (oak::uuid_t const& uuid)
	{
		std::string str(40, ' ');
		uuid_unparse_upper(uuid.data, &str[0]);
		str.resize(36);
		return str;
	}

} /* oak */
