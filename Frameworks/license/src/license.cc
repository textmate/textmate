#include "license.h"
#include <cf/src/cf.h>
#include <text/src/decode.h>
#include <text/src/hexdump.h>
#include <text/src/tokenize.h>
#include <text/src/case.h>
#include <text/src/format.h>
#include <crash/src/info.h>

// extern std::vector<size_t> const& revoked_serials () WEAK_IMPORT_ATTRIBUTE;

namespace
{
	static std::string sha_digest (std::string const& str)
	{
		char md[CC_SHA1_DIGEST_LENGTH];
		CC_SHA1(str.data(), str.size(), (unsigned char*)md);

		std::string res = "";
		for(char ch : md)
			text::int_to_hex(ch, back_inserter(res), 2);
		return res;
	}

	static std::map<std::string, std::string> parse (std::string const& src)
	{
		std::map<std::string, std::string> res;
		for(auto line : text::tokenize(src.begin(), src.end(), '\n'))
		{
			std::string::size_type split = line.find('\t');
			if(split != std::string::npos)
				res[line.substr(0, split)] = line.substr(split+1);
		}
		return res;
	}

	static SecKeyRef parse_public_key (std::string const& publicKey)
	{
		SecKeyRef res = nullptr;
		if(CFDataRef keyData = CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, (const UInt8*)publicKey.data(), publicKey.size(), kCFAllocatorNull))
		{
			SecExternalFormat format = kSecFormatOpenSSL;
			SecExternalItemType type = kSecItemTypePublicKey;

			CFArrayRef cfItems = nullptr;
			OSStatus err = SecItemImport(keyData, CFSTR(".pem"), &format, &type, 0, nullptr, nullptr, &cfItems);
			if(err == noErr && cfItems)
			{
				if(CFArrayGetCount(cfItems) == 1)
				{
					if(res = (SecKeyRef)CFArrayGetValueAtIndex(cfItems, 0))
						CFRetain(res);
				}
				else
				{
					os_log_error(OS_LOG_DEFAULT, "SecItemImport() returned %ld items, expected 1", CFArrayGetCount(cfItems));
				}
				CFRelease(cfItems);
			}
			else
			{
				if(CFStringRef message = SecCopyErrorMessageString(err, NULL))
				{
					os_log_error(OS_LOG_DEFAULT, "SecItemImport: %{public}@", message);
					CFRelease(message);
				}
			}
			CFRelease(keyData);
		}
		else
		{
			os_log_error(OS_LOG_DEFAULT, "Failed to create CFDataRef for public key");
		}
		return res;
	}

	static std::string ssl_decode (std::string const& src, std::string const& publicKey)
	{
		crash_reporter_info_t info("key size %zu, cipher size %zu", publicKey.size(), src.size());

		std::string res = NULL_STR;
		if(SecKeyRef key = parse_public_key(publicKey))
		{
			if(CFDataRef data = CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, (const UInt8*)src.data(), src.size(), kCFAllocatorNull))
			{
				CFErrorRef err;
				if(CFDataRef tmp = SecKeyCreateDecryptedData(key, kSecKeyAlgorithmRSAEncryptionRaw, (CFDataRef)data, &err))
				{
					char const* first = (char const*)CFDataGetBytePtr(tmp);
					char const* last  = first + CFDataGetLength(tmp);
					if(last - first > 3 && *first++ == '\0' && *first++ == '\1')
					{
						while(first != last && *first++ != '\0')
							;
						res = std::string(first, last);
					}
					else
					{
						os_log_error(OS_LOG_DEFAULT, "Failed to decode license: Unexpected padding format");
					}
					CFRelease(tmp);
				}
				else
				{
					os_log_error(OS_LOG_DEFAULT, "Failed to decrypt license: %{public}@", err);
				}
				CFRelease(data);
			}
			else
			{
				os_log_error(OS_LOG_DEFAULT, "Failed to create CFDataRef for license key");
			}
			CFRelease(key);
		}
		else
		{
			os_log_error(OS_LOG_DEFAULT, "Failed to parse public key: %{public}s", publicKey.c_str());
		}
		return res;
	}

	static std::string const& public_key ()
	{
		static std::string const key = "-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDLFL3xBKeG19N7/6uCnpPLESJ+\n2VDL3LWH0KxIRse/sdkbWw1bYNPMeXtdVNfZRiF03umdxaPgzpNCN/JY7P91m8Lv\ndizRDO8I411Wcf+G7W5CA3GuJbQfNvBBOn+3KwHrG4v+RR+XtdEbw5uPHyIvYWOI\nmwvjE6oSRSBkWMIATQIDAQAB\n-----END PUBLIC KEY-----\n";
		return key;
	}
#if 0
	static bool is_serial_revoked (size_t serial)
	{
		if(revoked_serials)
		{
			auto v = revoked_serials();
			if(std::binary_search(v.begin(), v.end(), serial))
				return true;
		}
		return false;
	}
#endif
}

namespace license
{
	std::map<std::string, std::string> decode (std::string const& license, std::string const& publicKey)
	{
		return parse(ssl_decode(decode::base32(license), publicKey != NULL_STR ? publicKey : public_key()));
	}

	bool is_valid (std::map<std::string, std::string> const& license, std::string const& owner)
	{
		auto digest = license.find("owner");
		auto serial = license.find("serial");
		auto type   = license.find("type");
		if(digest != license.end() && serial != license.end() && type != license.end())
		{
			if(sha_digest(owner) == text::uppercase(digest->second))
			{
				static std::string const legacyTypes[] = { "heist", "eval" };
				if(!oak::contains(std::begin(legacyTypes), std::end(legacyTypes), type->second))
				{
					if(!is_serial_revoked(strtol(serial->second.c_str(), nullptr, 10)))
						return true;
				}
			}
		}
		return false;
	}

	bool is_revoked (std::map<std::string, std::string> const& license)
	{
		bool res = false;

		auto serial = license.find("serial");
		if(serial == license.end())
			return res;

		if(hostent* bl = gethostbyname(text::format("%zu.bl.textmate.org", strtol(serial->second.c_str(), nullptr, 10)).c_str()))
		{
			for(size_t i = 0; bl->h_addr_list[i]; ++i)
			{
				uint32_t response = ntohl(*(uint32_t*)bl->h_addr_list[i]);
				if(response == 0xDEADBEEF)
					res = true;
			}
		}
		return res;
	}

	std::string error_description (std::string const& license, std::string const& owner, std::string const& publicKey)
	{
		auto map = parse(ssl_decode(decode::base32(license), publicKey != NULL_STR ? publicKey : public_key()));
		if(is_valid(map, owner))
			return NULL_STR;

		if(map.size() == 0)
			return "Malformed license key. Try pasting it again.";
		else if(sha_digest(owner) != text::uppercase(map["owner"]))
			return "This license is tied to another owner name. Check registration email.";
		else if(map["type"] == "heist")
			return "This MacHeist license is for TextMate 1.x.";
		else if(map["type"] == "eval")
			return "This evaluation license is for TextMate 1.x.";
		else if(is_serial_revoked(strtol(map["serial"].c_str(), nullptr, 10)))
			return "This license has been revoked.";
		return "Unknown error.";
	}

} /* license */
