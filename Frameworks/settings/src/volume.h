#ifndef VOLUME_H_3HE6P24J
#define VOLUME_H_3HE6P24J

#include <oak/misc.h>

namespace volume
{
	struct settings_t
	{
		bool extended_attributes () const { return _extended_attributes; }

	private:
		static std::map<std::string, settings_t> create ();
		friend volume::settings_t const& settings (std::string const& path);

		bool _extended_attributes = true;
		bool _scm_badges          = true;
		bool _display_names       = true;
	};

	PUBLIC volume::settings_t const& settings (std::string const& path);

} /* volume */

#endif /* end of include guard: VOLUME_H_3HE6P24J */
