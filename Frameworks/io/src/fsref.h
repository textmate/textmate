#ifndef FSREF_H_LMA1F8SI
#define FSREF_H_LMA1F8SI

inline std::string to_s (FSRef const& ref)
{
	UInt8 buf[PATH_MAX];
	if(noErr == FSRefMakePath(&ref, buf, sizeof(buf)))
		return std::string(buf, std::find(buf, buf + sizeof(buf), '\0'));
	fprintf(stderr, "unable to get path from FSRef\n");
	return NULL_STR;
}

struct fsref_t
{
	fsref_t () {}
	fsref_t (std::string const& path) { FSPathMakeRefWithOptions((UInt8 const*)path.c_str(), kFSPathMakeRefDoNotFollowLeafSymlink, &ref, NULL); }

	operator FSRef const* () const    { return &ref; }
	operator FSRef* ()                { return &ref; }
	std::string path () const         { return to_s(ref); }
private:
	FSRef ref;
};

#endif /* end of include guard: FSREF_H_LMA1F8SI */
