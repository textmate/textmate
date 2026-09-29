#ifndef OSX_AUTHORIZATION_H_ST1ZIKX9
#define OSX_AUTHORIZATION_H_ST1ZIKX9

#include <oak/oak.h>
#include <text/src/format.h>

namespace osx
{
	struct authorization_t
	{
		authorization_t () : helper(std::make_shared<helper_t>()) { }
		authorization_t (std::string const& hex) : helper(std::make_shared<helper_t>(hex)) { }

		bool check_right (std::string const& right) const  { return helper->copy_right(right, kAuthorizationFlagDefaults); }
		bool obtain_right (std::string const& right) const { return helper->copy_right(right, kAuthorizationFlagInteractionAllowed|kAuthorizationFlagExtendRights); }

		operator AuthorizationRef () const { return helper->reference(); }
		operator std::string () const      { return helper->as_string(); }

	private:
		struct helper_t
		{
			helper_t (std::string const& hex) : _valid(false)
			{
				std::vector<char> v;
				for(size_t i = 0; i+1 < hex.size() && isxdigit(hex[i]) && isxdigit(hex[i+1]); i += 2)
					v.push_back(digittoint(hex[i]) << 4 | digittoint(hex[i+1]));

				if(v.size() == sizeof(AuthorizationExternalForm))
				{
					AuthorizationExternalForm const* extAuth = (AuthorizationExternalForm const*)&v[0];
					if(errAuthorizationSuccess == AuthorizationCreateFromExternalForm(extAuth, &_authorization))
						_valid = true;
				}
			}

			helper_t () : _valid(false) { }
			~helper_t ()                { if(_valid) AuthorizationFree(_authorization, kAuthorizationFlagDefaults); }

			bool copy_right (std::string const& right, AuthorizationFlags flags)
			{
				setup();

				if(!_valid)
					return false;

				AuthorizationItem rightsItems[]     = { { right.c_str(), 0, NULL, 0 }, };
				AuthorizationRights const allRights = { sizeofA(rightsItems), rightsItems };

				bool res = false;
				AuthorizationRights* myAuthorizedRights = NULL;
				int myStatus = AuthorizationCopyRights(_authorization, &allRights, kAuthorizationEmptyEnvironment, flags, &myAuthorizedRights);
				if(myStatus == errAuthorizationSuccess)
				{
					res = true;
					AuthorizationFreeItemSet(myAuthorizedRights);
				}
				else if(myStatus == errAuthorizationCanceled)
				{
					os_log_info(OS_LOG_DEFAULT, "authorization (pid %d): user canceled", getpid());
				}
				else if(myStatus == errAuthorizationDenied)
				{
					os_log_info(OS_LOG_DEFAULT, "authorization (pid %d): rights denied", getpid());
				}
				else if(myStatus == errAuthorizationInteractionNotAllowed)
				{
					os_log_info(OS_LOG_DEFAULT, "authorization (pid %d): interaction not allowed", getpid());
				}
				else
				{
					os_log_info(OS_LOG_DEFAULT, "authorization (pid %d): error %d", getpid(), myStatus);
				}
				return res;
			}

			AuthorizationRef reference () const { return _authorization; }

			std::string as_string () const
			{
				std::string res;
				AuthorizationExternalForm extAuth;
				if(AuthorizationMakeExternalForm(_authorization, &extAuth) == errAuthorizationSuccess)
				{
					for(char* it = (char*)&extAuth; it != (char*)(&extAuth + 1); ++it)
						res += text::format("%02X", *it);
				}
				return res;
			}

		private:
			void setup () { _valid = _valid || AuthorizationCreate(NULL, kAuthorizationEmptyEnvironment, kAuthorizationFlagDefaults, &_authorization) == errAuthorizationSuccess; }

			AuthorizationRef _authorization;
			bool _valid;
		};

		std::shared_ptr<helper_t> helper;
	};

} /* osx */

#endif /* end of include guard: OSX_AUTHORIZATION_H_ST1ZIKX9 */
