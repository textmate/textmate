#include "license.h"
#include <cf/cf.h>
#include <text/decode.h>
#include <text/hexdump.h>
#include <text/tokenize.h>
#include <text/case.h>
#include <text/format.h>

#ifdef DEPRECATED_IN_MAC_OS_X_VERSION_10_7_AND_LATER
#undef DEPRECATED_IN_MAC_OS_X_VERSION_10_7_AND_LATER
#define DEPRECATED_IN_MAC_OS_X_VERSION_10_7_AND_LATER
#endif

#include <openssl/bio.h>
#include <openssl/pem.h>
#include <openssl/rsa.h>

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

	static std::string ssl_decode (std::string const& src, std::string const& publicKey)
	{
		std::string res = NULL_STR;
		if(BIO* bio = BIO_new_mem_buf((char*)publicKey.data(), publicKey.size()))
		{
			RSA* rsa_key = 0;
			if(PEM_read_bio_RSA_PUBKEY(bio, &rsa_key, NULL, NULL))
			{
				std::string dst(src.size(), ' ');
				if(RSA_size(rsa_key) == (int)src.size())
				{
					int len = RSA_public_decrypt(src.size(), (unsigned char*)src.data(), (unsigned char*)&dst[0], rsa_key, RSA_PKCS1_PADDING);
					if(len > 0)
							res = dst.substr(0, len);
					else	fprintf(stderr, "RSA_public_decrypt failed\n");
				}
				else
				{
					fprintf(stderr, "Wrong RSA size\n");
				}
				RSA_free(rsa_key);
			}
			else
			{
				fprintf(stderr, "PEM_read_bio_RSA_PUBKEY failed\n");
			}
			BIO_free(bio);
		}
		else
		{
			fprintf(stderr, "BIO_new_mem_buf failed\n");
		}
		return res;
	}

	static std::pair<std::string, std::string> search_keychain ()
	{
		std::pair<std::string, std::string> res(NULL_STR, NULL_STR);
		CFTypeRef keys[] = { kSecMatchLimit,    kSecReturnRef,  kSecClass,                kSecAttrCreator,  kSecAttrService,   kSecAttrDescription  };
		CFTypeRef vals[] = { kSecMatchLimitAll, kCFBooleanTrue, kSecClassGenericPassword, cf::wrap('TxMt'), CFSTR("TextMate"), CFSTR("license key") };

		if(CFDictionaryRef query = CFDictionaryCreate(NULL, keys, vals, sizeofA(keys), NULL, NULL))
		{
			OSStatus err;

			CFArrayRef results = NULL;
			if(err = SecItemCopyMatching(query, (CFTypeRef*)&results) == errSecSuccess)
			{
				if(CFArrayGetCount(results) == 1)
				{
					void* data = NULL;
					UInt32 dataLen = 0;

					UInt32 tag    = kSecAccountItemAttr;
					UInt32 format = CSSM_DB_ATTRIBUTE_FORMAT_STRING;
					SecKeychainAttributeInfo info = { 1, &tag, &format };

					SecKeychainAttributeList* authAttrList = NULL;
					if(SecKeychainItemCopyAttributesAndData((SecKeychainItemRef)CFArrayGetValueAtIndex(results, 0), &info, NULL, &authAttrList, &dataLen, &data) == noErr)
					{
						res.first  = std::string((char const*)authAttrList->attr->data, ((char const*)authAttrList->attr->data) + authAttrList->attr->length);
						res.second = std::string((char const*)data, ((char const*)data) + dataLen);

						SecKeychainItemFreeContent(authAttrList, data);
					}
				}
				CFRelease(results);
			}
			else
			{
				CFStringRef message = SecCopyErrorMessageString(err, NULL);
				fprintf(stderr, "failed to copy matching items from keychain: ‘%s’\n", cf::to_s(message).c_str());
				CFRelease(message);
			}
			CFRelease(query);
		}
		return res;
	}

	static std::string const& public_key ()
	{
		static std::string const key = "-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDLFL3xBKeG19N7/6uCnpPLESJ+\n2VDL3LWH0KxIRse/sdkbWw1bYNPMeXtdVNfZRiF03umdxaPgzpNCN/JY7P91m8Lv\ndizRDO8I411Wcf+G7W5CA3GuJbQfNvBBOn+3KwHrG4v+RR+XtdEbw5uPHyIvYWOI\nmwvjE6oSRSBkWMIATQIDAQAB\n-----END PUBLIC KEY-----\n";
		return key;
	}
}

namespace license
{
	std::map<std::string, std::string> current (std::string const& publicKey)
	{
		static std::map<std::string, std::string> const err;

		auto pair = search_keychain();
		if(pair.second == NULL_STR)
			return err;

		auto map = parse(ssl_decode(decode::base32(pair.second), publicKey != NULL_STR ? publicKey : public_key()));
		auto it = map.find("owner");
		if(it == map.end())
			return err;

		if(sha_digest(pair.first) != text::uppercase(it->second))
			return err;

		it->second = pair.first;
		return map;
	}

} /* license */
