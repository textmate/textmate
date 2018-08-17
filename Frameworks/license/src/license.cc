#include "license.h"
#include <cf/cf.h>
#include <text/decode.h>
#include <text/hexdump.h>
#include <text/tokenize.h>
#include <text/case.h>
#include <text/format.h>
#include <openssl/bio.h>
#include <openssl/pem.h>
#include <openssl/rsa.h>
#include <crash/info.h>

extern std::vector<size_t> const& revoked_serials () WEAK_IMPORT_ATTRIBUTE;

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
		crash_reporter_info_t info("key size %zu, cipher size %zu", publicKey.size(), src.size());

		std::string res = NULL_STR;
		if(BIO* bio = BIO_new_mem_buf((void*)publicKey.data(), publicKey.size()))
		{
			RSA* rsa_key = nullptr;
			if(PEM_read_bio_RSA_PUBKEY(bio, &rsa_key, nullptr, nullptr))
			{
				std::string dst(src.size(), '\0');
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

	static std::string const& public_key ()
	{
		static std::string const key = "-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDLFL3xBKeG19N7/6uCnpPLESJ+\n2VDL3LWH0KxIRse/sdkbWw1bYNPMeXtdVNfZRiF03umdxaPgzpNCN/JY7P91m8Lv\ndizRDO8I411Wcf+G7W5CA3GuJbQfNvBBOn+3KwHrG4v+RR+XtdEbw5uPHyIvYWOI\nmwvjE6oSRSBkWMIATQIDAQAB\n-----END PUBLIC KEY-----\n";
		return key;
	}

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
