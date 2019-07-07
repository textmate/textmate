#ifndef INTERMEDIATE_H_LWU9YRUW
#define INTERMEDIATE_H_LWU9YRUW

#include <oak/misc.h>

namespace path
{
	enum struct atomic_t {
		always = 0, external_volumes, remote_volumes, never, legacy
	};

	struct PUBLIC intermediate_t
	{
		struct strategy_t
		{
			virtual ~strategy_t () { }
			virtual char const* path () const = 0;
			virtual bool commit (std::string* errorMsg) const = 0;
		};

		intermediate_t (std::string const& dest, atomic_t atomicSave = atomic_t::always);
		operator char const* () const                       { return _strategy->path(); }
		bool commit (std::string* errorMsg = nullptr) const { return _strategy->commit(errorMsg); }

	private:
		std::unique_ptr<strategy_t> _strategy;
	};

} /* path */

#endif /* end of include guard: INTERMEDIATE_H_LWU9YRUW */
