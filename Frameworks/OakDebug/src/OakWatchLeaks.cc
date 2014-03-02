#include "OakDebug.h"
#include <oak/oak.h>

namespace
{
	static OSSpinLock spinlock = 0;

	struct counts_t
	{
		counts_t (std::map<std::string, ssize_t>& counts) : counts(counts) { }
		~counts_t () { OSSpinLockUnlock(&spinlock); }

		std::map<std::string, ssize_t>& counts;
	};

	static counts_t get_counts ()
	{
		OSSpinLockLock(&spinlock);
		static std::map<std::string, ssize_t>* counts = NULL;
		if(counts == NULL)
			counts = new std::map<std::string, ssize_t>;
		return counts_t(*counts);
	}

	static struct helper_t
	{
		~helper_t ()
		{
			for(auto const& it : get_counts().counts)
			{
				if(it.second != 0)
					bug("%4zd: %s\n", it.second, it.first.c_str());
			}
		}

	} helper;
}

namespace oak_debug
{
	void increase (char const* name)
	{
		counts_t c = get_counts();
		++c.counts[name];
	}

	void decrease (char const* name)
	{
		counts_t c = get_counts();
		ASSERTF(c.counts[name] > 0, "name = %s, count = %zd\n", name, c.counts[name]);
		--c.counts[name];
	}

} /* oak_debug */
