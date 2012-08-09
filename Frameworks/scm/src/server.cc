#include "drivers/api.h"
#include "server.h"
#include <io/path.h>
#include <oak/server.h>

namespace
{
	struct payload_t
	{
		typedef payload_t* request_t;

		payload_t (std::string const& path, scm::driver_t const* driver, oak::date_t const& updated, fs::snapshot_t const& snapshot, void(*callback)(bool, std::string const&, fs::snapshot_t const&, scm::status_map_t const&));
		~payload_t ();

		static bool handle_request (payload_t* request)
		{
			double elapsed = oak::date_t::now() - request->_updated;
			if(elapsed < 3)
				usleep((3 - elapsed) * 1000000);

			if(request->_snapshot == fs::snapshot_t(request->_path))
				return false;

			request->_status     = request->_driver->status(request->_path);
			request->_snapshot   = fs::snapshot_t(request->_path);
			request->_updated    = oak::date_t::now();
			return true;
		}

		void handle_reply (bool didUpdate)
		{
			_callback(didUpdate, _path, _snapshot, _status);
			delete this;
		}

		std::string const _path;
		scm::driver_t const* _driver;
		oak::date_t _updated;
		fs::snapshot_t _snapshot;
		scm::status_map_t _status;

		void(*_callback)(bool, std::string const&, fs::snapshot_t const&, scm::status_map_t const&);
		size_t _client_key;
	};

	static oak::server_t<payload_t>& server ()
	{
		static oak::server_t<payload_t> server;
		return server;
	}

	payload_t::payload_t (std::string const& path, scm::driver_t const* driver, oak::date_t const& updated, fs::snapshot_t const& snapshot, void(*callback)(bool, std::string const&, fs::snapshot_t const&, scm::status_map_t const&)) : _path(path), _driver(driver), _updated(updated), _snapshot(snapshot), _callback(callback)
	{
		_client_key = server().register_client(this);
		server().send_request(_client_key, this);
	}

	payload_t::~payload_t ()
	{
		server().unregister_client(_client_key);
	}
}

namespace scm
{
	void background_status (std::string const& path, driver_t const* driver, oak::date_t const& updated, fs::snapshot_t const& snapshot, void(*callback)(bool, std::string const&, fs::snapshot_t const&, scm::status_map_t const&))
	{
		new payload_t(path, driver, updated, snapshot, callback);
	}

} /* scm */
