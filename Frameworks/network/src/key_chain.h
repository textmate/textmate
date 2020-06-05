#ifndef KEY_CHAIN_H_VALL5FR2
#define KEY_CHAIN_H_VALL5FR2

#include <oak/debug.h>

struct key_chain_t
{
	struct key_t
	{
		key_t (std::string const& identity, std::string const& key_data);
		key_t (key_t const& rhs);
		key_t& operator= (key_t const& rhs) = delete;
		~key_t ();

		std::string const& identity () const { return _identity; }

		operator SecKeyRef () const { setup(); return _sec_key; }

	private:
		friend struct key_chain_t;
		std::string _identity;
		std::string _key_data;

		mutable SecKeyRef _sec_key;

		void init () const;
		bool setup () const;
		void cleanup () const;
	};

	typedef std::shared_ptr<key_t> key_ptr;

	void add (key_t const& key);
	key_ptr find (std::string const& identity) const;

private:
	std::vector<key_ptr> keys;
};

#endif /* end of include guard: KEY_CHAIN_H_VALL5FR2 */
