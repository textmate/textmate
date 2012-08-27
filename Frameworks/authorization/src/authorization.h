#ifndef OSX_AUTHORIZATION_H_ST1ZIKX9
#define OSX_AUTHORIZATION_H_ST1ZIKX9

#include <oak/oak.h>
#include <text/format.h>

namespace osx
{
	struct authorization_t
	{
		authorization_t () : helper(new helper_t) { }
		authorization_t (std::string const& hex) : helper(new helper_t(hex)) { }

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
				for(size_t i = 0; i+1 < hex.size(); i += 2)
					v.push_back(strtol(hex.substr(i, 2).c_str(), NULL, 16));

				if(v.size() == sizeof(AuthorizationExternalForm))
				{
					AuthorizationExternalForm const* extAuth = (AuthorizationExternalForm const*)&v[0];
					if(errAuthorizationSuccess == AuthorizationCreateFromExternalForm(extAuth, &_authorization))
						_valid = true;
				}
			}

			helper_t () : _valid(false) { }
			~helper_t ()                { if(_valid) AuthorizationFree(_authorization, kAuthorizationFlagDestroyRights); }

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
					for(size_t i = 0; i < myAuthorizedRights->count; ++i)
						fprintf(stderr, "authorization (pid %d): got ‘%s’\n", getpid(), myAuthorizedRights->items[i].name);
					AuthorizationFreeItemSet(myAuthorizedRights);
				}
				else if(myStatus == errAuthorizationCanceled)
				{
					fprintf(stderr, "authorization (pid %d): user canceled\n", getpid());
				}
				else if(myStatus == errAuthorizationDenied)
				{
					fprintf(stderr, "authorization (pid %d): rights denied\n", getpid());
				}
				else if(myStatus == errAuthorizationInteractionNotAllowed)
				{
					fprintf(stderr, "authorization (pid %d): interaction not allowed\n", getpid());
				}
				else
				{
					fprintf(stderr, "authorization (pid %d): error %d\n", getpid(), myStatus);
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
					foreach(ch, (char*)&extAuth, (char*)(&extAuth + 1))
						res += text::format("%02X", *ch);
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
