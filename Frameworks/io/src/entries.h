#ifndef PATH_ENTRIES_H_NFV0WV76
#define PATH_ENTRIES_H_NFV0WV76

#include <oak/misc.h>

namespace path
{
	struct PUBLIC entries
	{
		typedef dirent const* const* const_iterator;

		entries (std::string const& path, std::string const& globString = NULL_STR);
		const_iterator begin () const { return _helper ? _helper->begin() : NULL; }
		const_iterator end () const   { return _helper ? _helper->end()   : NULL; }

	private:
		entries ();

		struct helper_t
		{
			helper_t (struct dirent** entries, int size, int actual) : _entries(entries), _size(size), _actual(actual) { }
			~helper_t ()
			{
				for(int i = 0; i < _actual; ++i)
					free(_entries[i]);
				free(_entries);
			}
			dirent** begin () const { return _entries; }
			dirent** end () const   { return _entries + _size; }
		private:
			struct dirent** _entries;
			int _size;
			int _actual;
		};

		std::shared_ptr<helper_t> _helper;
	};

} /* path */

#endif /* end of include guard: PATH_ENTRIES_H_NFV0WV76 */
