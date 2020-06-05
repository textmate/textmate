#ifndef SCM_NG_H_FXJGXN9B
#define SCM_NG_H_FXJGXN9B

#include "status.h"

namespace scm
{
	struct info_t;
	typedef std::shared_ptr<info_t> info_ptr;
	typedef std::weak_ptr<info_t> weak_info_ptr;

	struct shared_info_t;
	typedef std::shared_ptr<shared_info_t> shared_info_ptr;
	typedef std::weak_ptr<shared_info_t> shared_info_weak_ptr;

	struct info_t
	{
		info_t (std::string const& path);
		~info_t ();

		info_t (info_t const& rhs) = delete;
		info_t& operator= (info_t const& rhs) = delete;

		bool dry () const;

		std::string root_path () const;
		std::map<std::string, std::string> scm_variables () const;
		std::map<std::string, scm::status::type> const& status () const;
		scm::status::type status (std::string const& path) const;

		bool tracks_directories () const;
		void push_callback (void (^block)(info_t const&));
		void pop_callback ();

	private:
		friend info_ptr info (std::string path);
		void set_shared_info (shared_info_ptr info);

		friend shared_info_t;
		void did_update_shared_info ();

		std::vector<void(^)(info_t const&)> _callbacks;
		shared_info_ptr _shared_info;
	};

	void disable ();
	void enable ();
	std::string root_for_path (std::string const& path);
	bool scm_enabled_for_path (std::string const& path);
	info_ptr info (std::string path);
	void wait_for_status (info_ptr info);

} /* scm */

#endif /* end of include guard: SCM_NG_H_FXJGXN9B */
