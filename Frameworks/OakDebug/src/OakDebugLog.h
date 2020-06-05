#ifndef OAK_DEBUGLOG_H_G4IOWMJN
#define OAK_DEBUGLOG_H_G4IOWMJN

#ifndef NDEBUG

void OakPrintF (char const* file, char const* function, int line, char const* format, ...) __attribute__ ((format (printf, 4, 5)));

struct OakDebugBaseClass
{
	static bool checkForDebugVar (char const* name);
	static std::map<std::string, bool>& registry ();
	static std::vector<std::string> sectionNames ();
	static std::string sectionName (std::string const& title);
};

#define OAK_DEBUG(expr) expr

#define OAK_DEBUG_VAR(name) \
	static struct OakDebug ## name : public OakDebugBaseClass \
	{\
		OakDebug ## name () { }\
\
		static bool isEnabled ()\
		{\
			bool res = checkForDebugVar(#name);\
			return res;\
		}\
\
	} const DBF_ ## name

#define bug(format, args...) OakPrintF(__FILE__, __FUNCTION__, __LINE__, format , ## args)

#define D(name, code)   \
	if(name.isEnabled()) \
	{                    \
		code              \
	}

#else

#define OAK_DEBUG(expr)
#define OAK_DEBUG_VAR(name) ;
#define bug(format, args...)
#define D(flag, code) ;

#endif

#endif /* end of include guard: OAK_DEBUGLOG_H_G4IOWMJN */
