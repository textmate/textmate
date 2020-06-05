#ifndef INTERMEDIATE_H_LWU9YRUW
#define INTERMEDIATE_H_LWU9YRUW

namespace path
{
	enum struct atomic_t {
		always = 0, external_volumes, remote_volumes, never, legacy
	};

	struct intermediate_t
	{
		struct strategy_t
		{
			virtual ~strategy_t () { }
			virtual char const* setup (std::string* errorMsg) = 0;
			virtual bool commit (std::string* errorMsg) = 0;
		};

		intermediate_t (std::string const& dest, atomic_t atomicSave = atomic_t::always, mode_t mode = S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
		~intermediate_t ();
		int open (std::string* errorMsg = nullptr, int oflag = O_CREAT|O_TRUNC|O_WRONLY|O_CLOEXEC);
		bool close (std::string* errorMsg = nullptr);

	private:
		std::unique_ptr<strategy_t> _strategy;
		int _fileDescriptor = -1;
		mode_t _mode;
	};

} /* path */

#endif /* end of include guard: INTERMEDIATE_H_LWU9YRUW */
