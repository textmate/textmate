#ifndef KEY_CHAIN_H_VALL5FR2
#define KEY_CHAIN_H_VALL5FR2

#include <oak/misc.h>
#include <oak/debug.h>

struct PUBLIC key_chain_t
{
	WATCH_LEAKS(key_chain_t);

	struct PUBLIC key_t
	{
		WATCH_LEAKS(key_chain_t::key_t);

		key_t (std::string const& identity, std::string const& name, std::string const& key_data);
		key_t (key_t const& rhs);
		key_t& operator= (key_t const& rhs) = delete;
		~key_t ();

		std::string const& identity () const { return _identity; }
		std::string const& name () const     { return _name; }

		operator EVP_PKEY* () const { setup(); return _ssl_key; }

	private:
		friend struct key_chain_t;
		std::string _identity;
		std::string _name;
		std::string _key_data;

		mutable EVP_PKEY* _ssl_key;
		mutable BIO* _ssl_bio;
		mutable DSA* _ssl_data;

		void init () const;
		bool setup () const;
		void cleanup () const;
	};

	typedef std::tr1::shared_ptr<key_t> key_ptr;

	void load (std::string const& path);
	void save (std::string const& path) const;

	void add (key_t const& key);
	key_ptr find (std::string const& identity) const;

private:
	std::vector<key_ptr> keys;
};

#endif /* end of include guard: KEY_CHAIN_H_VALL5FR2 */
