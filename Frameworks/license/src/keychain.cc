#include "keychain.h"
#include <text/ctype.h>
#include <cf/cf.h>
#include <oak/debug.h>

static void perror_keychain (char const* prefix, OSStatus status)
{
	CFStringRef message = SecCopyErrorMessageString(status, NULL);
	fprintf(stderr, "%s: %s\n", prefix, cf::to_s(message).c_str());
	CFRelease(message);
}

namespace license
{
	std::string find (std::string const& key)
	{
		std::string res = NULL_STR;

		auto const cfCreator = cf::wrap('TxMt');
		auto const cfAccount = cf::wrap(key);

		CFTypeRef keys[] = { kSecReturnData, kSecClass,                kSecAttrCreator, kSecAttrService,   kSecAttrDescription,  kSecAttrAccount };
		CFTypeRef vals[] = { kCFBooleanTrue, kSecClassGenericPassword, cfCreator,       CFSTR("TextMate"), CFSTR("license key"), cfAccount       };

		if(CFDictionaryRef query = CFDictionaryCreate(kCFAllocatorDefault, keys, vals, sizeofA(keys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
		{
			CFDataRef data = NULL;
			OSStatus err = SecItemCopyMatching(query, (CFTypeRef*)&data);
			if(err == errSecSuccess)
			{
				UInt8 const* bytes = CFDataGetBytePtr(data);
				res = std::string(bytes, bytes + CFDataGetLength(data));

				CFRelease(data);
			}
			else
			{
				perror_keychain("SecItemCopyMatching", err);
			}
			CFRelease(query);
		}
		return res;
	}

	std::vector<std::string> find_all ()
	{
		std::vector<std::string> res;

		auto const cfCreator = cf::wrap('TxMt');

		CFTypeRef keys[] = { kSecMatchLimit,    kSecReturnRef,  kSecClass,                kSecAttrCreator, kSecAttrService,   kSecAttrDescription  };
		CFTypeRef vals[] = { kSecMatchLimitAll, kCFBooleanTrue, kSecClassGenericPassword, cfCreator,       CFSTR("TextMate"), CFSTR("license key") };

		if(CFDictionaryRef query = CFDictionaryCreate(kCFAllocatorDefault, keys, vals, sizeofA(keys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
		{
			OSStatus err;

			CFArrayRef results = NULL;
			if(err = SecItemCopyMatching(query, (CFTypeRef*)&results) == errSecSuccess)
			{
				for(CFIndex i = 0; i < CFArrayGetCount(results); ++i)
				{
					if(SecKeychainItemRef item = (SecKeychainItemRef)CFArrayGetValueAtIndex(results, i))
					{
						UInt32 tag    = kSecAccountItemAttr;
						UInt32 format = CSSM_DB_ATTRIBUTE_FORMAT_STRING;
						SecKeychainAttributeInfo info = { 1, &tag, &format };

						SecKeychainAttributeList* authAttrList = NULL;
						err = SecKeychainItemCopyAttributesAndData(item, &info, NULL, &authAttrList, NULL, NULL);
						if(err == noErr)
						{
							res.emplace_back((char const*)authAttrList->attr->data, ((char const*)authAttrList->attr->data) + authAttrList->attr->length);
							SecKeychainItemFreeContent(authAttrList, NULL);
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
				perror_keychain("SecItemCopyMatching", err);
			}
			CFRelease(query);
		}

		std::sort(res.begin(), res.end(), text::less_t());
		return res;
	}

	bool add (std::string const& key, std::string const& data)
	{
		bool res = false;
		if(CFDataRef cfData = CFDataCreateWithBytesNoCopy(NULL, (const UInt8*)data.data(), data.size(), kCFAllocatorNull))
		{
			auto const cfKey     = cf::wrap(key);
			auto const cfCreator = cf::wrap('TxMt');

			CFTypeRef keys[] = { kSecClass,                kSecAttrCreator, kSecAttrService,   kSecAttrDescription,  kSecAttrLabel,             kSecAttrComment,                kSecAttrAccount, kSecValueData };
			CFTypeRef vals[] = { kSecClassGenericPassword, cfCreator,       CFSTR("TextMate"), CFSTR("license key"), CFSTR("TextMate license"), CFSTR("Keep prying eyes away"), cfKey,           cfData        };

			if(CFDictionaryRef createDict = CFDictionaryCreate(kCFAllocatorDefault, keys, vals, sizeofA(keys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
			{
				OSStatus status = SecItemAdd(createDict, NULL);
				if(status == errKCDuplicateItem)
				{
					CFTypeRef queryKeys[] = { kSecClass,                kSecAttrCreator, kSecAttrService,   kSecAttrDescription,  kSecAttrLabel,             kSecAttrComment,                kSecAttrAccount };
					CFTypeRef queryVals[] = { kSecClassGenericPassword, cfCreator,       CFSTR("TextMate"), CFSTR("license key"), CFSTR("TextMate license"), CFSTR("Keep prying eyes away"), cfKey           };
					if(CFDictionaryRef queryDict = CFDictionaryCreate(kCFAllocatorDefault, queryKeys, queryVals, sizeofA(queryKeys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
					{
						CFTypeRef updateKeys[] = { kSecValueData };
						CFTypeRef updateVals[] = { cfData        };
						if(CFDictionaryRef updateDict = CFDictionaryCreate(kCFAllocatorDefault, updateKeys, updateVals, sizeofA(updateKeys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks))
						{
							status = SecItemUpdate(queryDict, updateDict);
							CFRelease(updateDict);
						}
						CFRelease(queryDict);
					}
				}

				if(status == noErr)
				{
					res = true;
				}
				else
				{
					perror_keychain("SecItemAdd", status);
				}

				CFRelease(createDict);
			}
			CFRelease(cfData);
		}
		return res;
	}

} /* license */