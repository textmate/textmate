namespace path
{
	namespace flag
	{
		extern uint32_t
			meta,             /* for “.” and “..” entries */
			file,
			directory,
			symlink,
			dotfile,
			hidden,           /* file is hidden, does not include ‘dotfile’ */
			alias,
			package,
			application,
			stationery_pad,
			hidden_volume,
			hidden_extension;
	}

	uint32_t info (std::string const& path, uint32_t mask = 0xFFFFFF); // the type of the file, mask given to limit work done, see possible values below

} /* path */
