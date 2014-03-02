#include "key_chain.h"
#include <cf/cf.h>
#include <oak/oak.h>
#include <plist/plist.h>

key_chain_t::key_t::key_t (std::string const& identity, std::string const& name, std::string const& key_data) : _identity(identity), _name(name), _key_data(key_data)
{
	init();
}

key_chain_t::key_t::key_t (key_t const& rhs) : _identity(rhs._identity), _name(rhs._name), _key_data(rhs._key_data)
{
	init();
}

key_chain_t::key_t::~key_t ()
{
	cleanup();
}

void key_chain_t::key_t::init () const
{
	_sec_key = NULL;
}

bool key_chain_t::key_t::setup () const
{
	if(_sec_key)
		return true;
	
	bool res = false;
	
	SecItemImportExportKeyParameters params = { .keyUsage = NULL, .keyAttributes = NULL };
	SecExternalItemType type = kSecItemTypePublicKey;
	SecExternalFormat format = kSecFormatPEMSequence;

	CFDataRef data = CFDataCreateWithBytesNoCopy(NULL, (const UInt8*)_key_data.data(), _key_data.size(), kCFAllocatorNull);
	CFArrayRef items = NULL;
	OSStatus err;
	if(err = SecItemImport(data, NULL, &type, &format, 0, &params, NULL, &items) == errSecSuccess)
	{
		_sec_key = (SecKeyRef)CFArrayGetValueAtIndex(items, 0);
		if(_sec_key != NULL)
		{
			CFRetain(_sec_key);
			res = true;
		}
		CFRelease(items);
	}
	else
	{
		CFStringRef message = SecCopyErrorMessageString(err, NULL);
		fprintf(stderr, "*** error importing key: %s\n", cf::to_s(message).c_str());
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

void key_chain_t::load (std::string const& path)
{
	keys.clear();

	plist::dictionary_t identities;
	if(plist::get_key_path(plist::load(path), "identities", identities))
	{
		for(auto const& identity : identities)
		{
			std::string name, keyData;
			if(plist::get_key_path(identity.second, "name", name) && plist::get_key_path(identity.second, "key", keyData))
			{
				keys.push_back(std::make_shared<key_t>(identity.first, name, keyData));
			}
			else
			{
				fprintf(stderr, "no name/key in entry:\n%s", to_s(identity.second).c_str());
			}
		}
	}
	else
	{
		fprintf(stderr, "no identities in ‘%s’\n", path.c_str());
	}
}

void key_chain_t::save (std::string const& path) const
{
	plist::dictionary_t identities;
	for(auto const& key : keys)
	{
		plist::dictionary_t entry;
		entry.emplace("name", key->name());
		entry.emplace("key",  key->_key_data);
		identities.emplace(key->identity(), entry);
	}

	plist::dictionary_t plist;
	plist.emplace("identities", identities);

	plist::save(path, plist);
}

void key_chain_t::add (key_t const& key)
{
	keys.push_back(std::make_shared<key_t>(key.identity(), key.name(), key._key_data));
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
