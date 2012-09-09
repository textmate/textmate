#include "session.h"
#include "collection.h"
#include <cf/timer.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Session);

namespace document
{
	static void perform_session_backup (void* dummy)
	{
		D(DBF_Session, bug("\n"););
		save_session(true);
	}

	void schedule_session_backup ()
	{
		static cf::timer_ptr SessionBackupTimer;

		D(DBF_Session, bug("\n"););
		SessionBackupTimer = cf::setup_timer(0.5, std::bind(&perform_session_backup, nullptr));
	}

} /* document */