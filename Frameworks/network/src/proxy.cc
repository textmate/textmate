#include "proxy.h"
#include <plist/plist.h>
#include <oak/debug.h>
#include <cf/cf.h>

OAK_DEBUG_VAR(Proxy);

static proxy_settings_t user_pw_settings (std::string const& server, UInt32 port)
{
	D(DBF_Proxy, bug("%s:%zu\n", server.c_str(), (size_t)port););
	std::string user = NULL_STR, pw = NULL_STR;

	CFTypeRef keys[] = {
		kSecMatchLimit, kSecReturnRef,
		kSecClass,
		kSecAttrProtocol,
		kSecAttrPort,
		kSecAttrServer
	};
	CFTypeRef vals[] = {
		kSecMatchLimitAll, kCFBooleanTrue,
		kSecClassInternetPassword,
		kSecAttrProtocolHTTPProxy,
		cf::wrap(port),
		cf::wrap(server)
	};
	CFDictionaryRef query = CFDictionaryCreate(NULL, keys, vals, sizeofA(keys), NULL, NULL);
	
	CFArrayRef results = NULL;
	OSStatus err;
	if(err = SecItemCopyMatching(query, (CFTypeRef*)&results) == errSecSuccess)
	{
		CFIndex numResults = CFArrayGetCount(results);
		for(CFIndex i = 0; user == NULL_STR && i < numResults; ++i)
		{
			SecKeychainItemRef item = (SecKeychainItemRef)CFArrayGetValueAtIndex(results, i);
			
			UInt32 tag    = kSecAccountItemAttr;
			UInt32 format = CSSM_DB_ATTRIBUTE_FORMAT_STRING;
			SecKeychainAttributeInfo info = { 1, &tag, &format };

			void* data = NULL;
			UInt32 dataLen = 0;

			SecKeychainAttributeList* authAttrList = NULL;
			if(SecKeychainItemCopyAttributesAndData(item, &info, NULL, &authAttrList, &dataLen, &data) == noErr)
			{
				ASSERT(authAttrList->count == 1 && authAttrList->attr->tag == kSecAccountItemAttr);
				user = std::string((char const*)authAttrList->attr->data, ((char const*)authAttrList->attr->data) + authAttrList->attr->length);
				pw   = std::string((char const*)data, ((char const*)data) + dataLen);
				SecKeychainItemFreeContent(authAttrList, data);
				D(DBF_Proxy, bug("found user ‘%s’\n", user.c_str()););
			}
			else
				D(DBF_Proxy, bug("unable to obtain attributes from key chain entry\n"););
		}
		
		CFRelease(results);
	}
	else
	{
		CFStringRef message = SecCopyErrorMessageString(err, NULL);
		D(DBF_Proxy, bug("failed to copy matching items from keychain: ‘%s’\n", cf::to_s(message).c_str()););
		CFRelease(message);
	}

	if(query)
		CFRelease(query);

	return proxy_settings_t(true, server, port, user, pw);
}

#if MAC_OS_X_VERSION_MAX_ALLOWED > MAC_OS_X_VERSION_10_4
static void pac_proxy_callback (void* client, CFArrayRef proxyList, CFErrorRef error)
{
	proxy_settings_t& settings = *(proxy_settings_t*)client;
	settings.enabled = true;

	if(error)
	{
		CFStringRef str = CFErrorCopyDescription(error);
		fprintf(stderr, "proxy error: %s\n", cf::to_s(str).c_str());
		CFRelease(str);
		return;
	}

	if(!proxyList)
		return;

	for(CFIndex i = 0; i < CFArrayGetCount(proxyList); ++i)
	{
		plist::dictionary_t dict = plist::convert((CFDictionaryRef)CFArrayGetValueAtIndex(proxyList, i));
		D(DBF_Proxy, bug("%s\n", to_s(dict).c_str()););

		int32_t port;
		std::string type;
		if(plist::get_key_path(dict, cf::to_s(kCFProxyTypeKey), type) && type == cf::to_s(kCFProxyTypeHTTP) && plist::get_key_path(dict, cf::to_s(kCFProxyHostNameKey), settings.server) && plist::get_key_path(dict, cf::to_s(kCFProxyPortNumberKey), port))
		{
			settings.port = port;
			break;
		}
	}
}
#endif

proxy_settings_t get_proxy_settings ()
{
	proxy_settings_t res(false);

	CFDictionaryRef tmp = SCDynamicStoreCopyProxies(NULL);
	plist::dictionary_t const& plist = plist::convert(tmp);
	D(DBF_Proxy, bug("%s\n", to_s(plist).c_str()););

	bool enabled = false;
	if(plist::get_key_path(plist, cf::to_s(kSCPropNetProxiesHTTPEnable), enabled) && enabled)
	{
		D(DBF_Proxy, bug("proxy enabled: %s\n", BSTR(enabled)););
		std::string host; int32_t port;
		if(plist::get_key_path(plist, cf::to_s(kSCPropNetProxiesHTTPProxy), host) && plist::get_key_path(plist, cf::to_s(kSCPropNetProxiesHTTPPort), port))
			res = user_pw_settings(host, port);
	}
	else if(plist::get_key_path(plist, cf::to_s(kSCPropNetProxiesProxyAutoConfigEnable), enabled) && enabled)
	{
		D(DBF_Proxy, bug("pac enabled: %s\n", BSTR(enabled)););
#if MAC_OS_X_VERSION_MAX_ALLOWED > MAC_OS_X_VERSION_10_4
		std::string pacString;
		if(plist::get_key_path(plist, cf::to_s(kSCPropNetProxiesProxyAutoConfigURLString), pacString))
		{
			D(DBF_Proxy, bug("pac script: %s\n", pacString.c_str()););
			CFStreamClientContext context    = { 0, &res, NULL, NULL, NULL };

			CFURLRef pacURL                  = CFURLCreateWithString(kCFAllocatorDefault, cf::wrap(pacString), NULL);
			CFURLRef targetURL               = CFURLCreateWithString(kCFAllocatorDefault, CFSTR("http://macromates.com/"), NULL);
			CFRunLoopSourceRef runLoopSource = CFNetworkExecuteProxyAutoConfigurationURL(pacURL, targetURL, &pac_proxy_callback, &context);
			CFRelease(targetURL);
			CFRelease(pacURL);

			CFStringRef runLoopMode = CFSTR("OakRunPACScriptRunLoopMode");
			CFRunLoopAddSource(CFRunLoopGetCurrent(), runLoopSource, runLoopMode);
			while(!res.enabled)
				CFRunLoopRunInMode(runLoopMode, 0.1, TRUE);

			if(CFRunLoopSourceIsValid(runLoopSource))
				CFRunLoopSourceInvalidate(runLoopSource);
			CFRelease(runLoopSource);

			if(res.server == NULL_STR)
				res.enabled = false;
		}
#endif
	}
	else if(plist::get_key_path(plist, cf::to_s(kSCPropNetProxiesProxyAutoDiscoveryEnable), enabled) && enabled)
	{
		D(DBF_Proxy, bug("auto discovery enabled: %s\n", BSTR(enabled)););
	}
	CFRelease(tmp);

	return res;
}
