#include "key_chain.h"
#include <cf/src/cf.h>
#include <oak/oak.h>
#include <plist/src/plist.h>

key_chain_t::key_t::key_t (std::string const& identity, std::string const& key_data) : _identity(identity), _key_data(key_data)
{
	init();
}

key_chain_t::key_t::key_t (key_t const& rhs) : _identity(rhs._identity), _key_data(rhs._key_data)
{
	init();
}

key_chain_t::key_t::~key_t ()
{
	cleanup();
}

void key_chain_t::key_t::init () const
{
	_sec_key = nullptr;
}

bool key_chain_t::key_t::setup () const
{
	if(_sec_key)
		return true;

	bool res = false;

	SecItemImportExportKeyParameters params = { .keyUsage = nullptr, .keyAttributes = nullptr };
	SecExternalItemType type = kSecItemTypePublicKey;
	SecExternalFormat format = kSecFormatPEMSequence;

	CFDataRef data = CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, (const UInt8*)_key_data.data(), _key_data.size(), kCFAllocatorNull);
	CFArrayRef items = nullptr;
	OSStatus err = SecItemImport(data, nullptr, &format, &type, 0, &params, nullptr, &items);
	if(err == errSecSuccess)
	{
		_sec_key = (SecKeyRef)CFArrayGetValueAtIndex(items, 0);
		if(_sec_key != nullptr)
		{
			CFRetain(_sec_key);
			res = true;
		}
		CFRelease(items);
	}
	else
	{
		CFStringRef message = SecCopyErrorMessageString(err, nullptr);
		os_log_error(OS_LOG_DEFAULT, "Error importing key: %{public}@", message);
		CFRelease(message);
	}

	CFRelease(data);

	return res;
}

void key_chain_t::key_t::cleanup () const
{
	if(_sec_key)
		CFRelease(_sec_key);

	init();
}

// =============
// = Key Chain =
// =============

void key_chain_t::add (key_t const& key)
{
	keys.push_back(std::make_shared<key_t>(key.identity(), key._key_data));
}

key_chain_t::key_ptr key_chain_t::find (std::string const& identity) const
{
	for(auto const& key : keys)
	{
		if(key->identity() == identity && key->setup())
			return key;
	}
	return key_ptr();
}
