#ifndef SCM_NG_H_FXJGXN9B
#define SCM_NG_H_FXJGXN9B

#include "status.h"

namespace scm { namespace ng
{
	struct info_t;
	typedef std::shared_ptr<info_t> info_ptr;

	struct shared_info_t;
	typedef std::shared_ptr<shared_info_t> shared_info_ptr;
	typedef std::weak_ptr<shared_info_t> shared_info_weak_ptr;

	struct PUBLIC info_t : std::enable_shared_from_this<info_t>
	{
		info_t (std::string const& path);
		~info_t ();

		bool dry () const;

		std::string root_path () const;
		std::map<std::string, std::string> variables () const;
		std::map<std::string, scm::status::type> const& status () const;
		scm::status::type status (std::string const& path) const;

		bool tracks_directories () const;

		void add_callback (void (^block)(info_t const&));

		void set_shared_info (shared_info_ptr info);
		void did_update_shared_info ();

	private:
		std::vector<void(^)(info_t const&)> _callbacks;
		shared_info_ptr _shared_info;
	};

	PUBLIC std::string root_for_path (std::string const& path);
	PUBLIC info_ptr info (std::string path);

} /* ng */ } /* scm */

#endif /* end of include guard: SCM_NG_H_FXJGXN9B */
