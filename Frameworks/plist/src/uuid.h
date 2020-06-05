#ifndef UUID_H_7R1D1FHT
#define UUID_H_7R1D1FHT

namespace oak
{
	struct uuid_t
	{
		uuid_t ();
		uuid_t (char const* str);
		uuid_t (std::string const& str);
		uuid_t (CFUUIDBytes const& bytes);
		uuid_t (::uuid_t const& uuid);
		uuid_t (uuid_t const& rhs);

		uuid_t& generate ();

		bool operator< (uuid_t const& rhs) const;
		bool operator== (uuid_t const& rhs) const;
		bool operator!= (uuid_t const& rhs) const;

		explicit operator bool () const;
		operator std::string () const;

		static bool is_valid (std::string const& str);

		::uuid_t data;
	private:
		void reset (std::string const& str);
	};

	std::string to_s (oak::uuid_t const& uuid);

} /* oak */

#endif /* end of include guard: UUID_H_7R1D1FHT */
