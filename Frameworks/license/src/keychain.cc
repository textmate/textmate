#include "keychain.h"
#include <text/src/my_ctype.h>
#include <text/src/format.h>
#include <cf/src/cf.h>
#include <oak/debug.h>

static std::string error_keychain (char const* prefix, OSStatus status)
{
	CFStringRef message = SecCopyErrorMessageString(status, nullptr);
	std::string const res = text::format("%s: %s", prefix, cf::to_s(message).c_str());
	CFRelease(message);
	return res;
}

static void perror_keychain (char const* prefix, OSStatus status)
{
	os_log_error(OS_LOG_DEFAULT, "%{public}s", error_keychain(prefix, status).c_str());
}

namespace license
{
	std::string find (std::string const& key)
	{
		std::string res = NULL_STR;

		auto const cfAccount = cf::wrap(key);

		CFTypeRef keys[] = { kSecReturnData, kSecClass,                kSecAttrService,   kSecAttrDescription,  kSecAttrAccount };
		CFTypeRef vals[] = { kCFBooleanTrue, kSecClassGenericPassword, CFSTR("TextMate"), CFSTR("license key"), cfAccount       };

		if(CFDictionaryRef query = CFDictionaryCreate(kCFAllocatorDefault, keys, vals, sizeofA(keys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
		{
			CFDataRef data = nullptr;
			OSStatus err = SecItemCopyMatching(query, (CFTypeRef*)&data);
			if(err == errSecSuccess)
			{
				UInt8 const* bytes = CFDataGetBytePtr(data);
				res = std::string(bytes, bytes + CFDataGetLength(data));

				CFRelease(data);
			}
			else
			{
				perror_keychain("find: SecItemCopyMatching", err);
			}
			CFRelease(query);
		}
		return res;
	}

	std::vector<std::string> find_all ()
	{
		std::vector<std::string> res;

		CFTypeRef keys[] = { kSecMatchLimit,    kSecReturnRef,  kSecClass,                kSecAttrService,   kSecAttrDescription  };
		CFTypeRef vals[] = { kSecMatchLimitAll, kCFBooleanTrue, kSecClassGenericPassword, CFSTR("TextMate"), CFSTR("license key") };

		if(CFDictionaryRef query = CFDictionaryCreate(kCFAllocatorDefault, keys, vals, sizeofA(keys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
		{
			CFArrayRef results = nullptr;

			OSStatus err = SecItemCopyMatching(query, (CFTypeRef*)&results);
			if(err == errSecSuccess)
			{
				for(CFIndex i = 0; i < CFArrayGetCount(results); ++i)
				{
					if(SecKeychainItemRef item = (SecKeychainItemRef)CFArrayGetValueAtIndex(results, i))
					{
						UInt32 tag    = kSecAccountItemAttr;
						UInt32 format = CSSM_DB_ATTRIBUTE_FORMAT_STRING;
						SecKeychainAttributeInfo info = { 1, &tag, &format };

						SecKeychainAttributeList* authAttrList = nullptr;
						err = SecKeychainItemCopyAttributesAndData(item, &info, nullptr, &authAttrList, nullptr, nullptr);
						if(err == noErr)
						{
							res.emplace_back((char const*)authAttrList->attr->data, ((char const*)authAttrList->attr->data) + authAttrList->attr->length);
							SecKeychainItemFreeAttributesAndData(authAttrList, nullptr);
						}
						else
						{
							perror_keychain("SecKeychainItemCopyAttributesAndData", err);
						}
					}
				}
				CFRelease(results);
			}
			else
			{
				perror_keychain("find_all: SecItemCopyMatching", err);
			}
			CFRelease(query);
		}

		std::sort(res.begin(), res.end(), text::less_t());
		return res;
	}

	bool add (std::string const& key, std::string const& data, std::string* error)
	{
		std::string dummy;
		std::string& err = error ? *error : dummy;

		bool res = false;
		if(CFDataRef cfData = CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, (const UInt8*)data.data(), data.size(), kCFAllocatorNull))
		{
			auto const cfKey = cf::wrap(key);
			CFTypeRef keys[] = { kSecClass,                kSecAttrService,   kSecAttrDescription,  kSecAttrLabel,             kSecAttrComment,                kSecAttrAccount, kSecValueData };
			CFTypeRef vals[] = { kSecClassGenericPassword, CFSTR("TextMate"), CFSTR("license key"), CFSTR("TextMate license"), CFSTR("Keep prying eyes away"), cfKey,           cfData        };

			if(CFDictionaryRef createDict = CFDictionaryCreate(kCFAllocatorDefault, keys, vals, sizeofA(keys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
			{
				OSStatus status = SecItemAdd(createDict, nullptr);
				if(status == noErr)
				{
					res = true;
				}
				else if(status == errKCDuplicateItem)
				{
					CFTypeRef queryKeys[] = { kSecClass,                kSecAttrService,   kSecAttrDescription,  kSecAttrAccount };
					CFTypeRef queryVals[] = { kSecClassGenericPassword, CFSTR("TextMate"), CFSTR("license key"), cfKey           };
					if(CFDictionaryRef queryDict = CFDictionaryCreate(kCFAllocatorDefault, queryKeys, queryVals, sizeofA(queryKeys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
					{
						CFTypeRef updateKeys[] = { kSecValueData };
						CFTypeRef updateVals[] = { cfData        };
						if(CFDictionaryRef updateDict = CFDictionaryCreate(kCFAllocatorDefault, updateKeys, updateVals, sizeofA(updateKeys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
						{
							status = SecItemUpdate(queryDict, updateDict);
							if(status == noErr)
									res = true;
							else	err = error_keychain("SecItemUpdate", status);
							CFRelease(updateDict);
						}
						CFRelease(queryDict);
					}
				}
				else
				{
					err = error_keychain("SecItemAdd", status);
				}

				CFRelease(createDict);
			}
			CFRelease(cfData);
		}
		return res;
	}

} /* license */
