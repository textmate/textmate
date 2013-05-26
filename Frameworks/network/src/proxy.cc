#include "proxy.h"
#include <plist/plist.h>
#include <regexp/regexp.h>
#include <text/encode.h>
#include <oak/debug.h>
#include <cf/cf.h>

OAK_DEBUG_VAR(Proxy);

static proxy_settings_t user_pw_settings (CFStringRef server, CFNumberRef portNumber)
{
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
		portNumber,
		server
	};
	CFDictionaryRef query = CFDictionaryCreate(NULL, keys, vals, sizeofA(keys), NULL, NULL);
	
	CFArrayRef results = NULL;
	OSStatus err = SecItemCopyMatching(query, (CFTypeRef*)&results);
	if(err == errSecSuccess)
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
	else if(err != errSecItemNotFound)
	{
		CFStringRef message = SecCopyErrorMessageString(err, NULL);
		fprintf(stderr, "TextMate/proxy: SecItemCopyMatching() failed with error ‘%s’\n", cf::to_s(message).c_str());
		CFRelease(message);
	}

	if(query)
		CFRelease(query);

	int32_t port = 0;
	CFNumberGetValue(portNumber, kCFNumberSInt32Type, &port);

	return proxy_settings_t(true, cf::to_s(server), port, user, pw);
}

struct pac_proxy_callback_result_t
{
	bool has_result    = false;
	CFArrayRef proxies = NULL;
	CFErrorRef error   = NULL;
};

static void pac_proxy_callback (void* client, CFArrayRef proxies, CFErrorRef error)
{
	pac_proxy_callback_result_t* res = (pac_proxy_callback_result_t*)client;
	res->proxies    = proxies ? (CFArrayRef)CFRetain(proxies) : NULL;
	res->error      = error   ? (CFErrorRef)CFRetain(error)   : NULL;
	res->has_result = true;
}

proxy_settings_t first_proxy_from_array (CFArrayRef proxies, CFURLRef targetURL)
{
	for(CFIndex i = 0; i < CFArrayGetCount(proxies); ++i)
	{
		CFDictionaryRef proxy = (CFDictionaryRef)CFArrayGetValueAtIndex(proxies, i);
		if(!proxy || CFGetTypeID(proxy) != CFDictionaryGetTypeID())
		{
			fprintf(stderr, "TextMate/proxy: Expected proxy info to be a CFDictionaryRef\n");
			continue;
		}

		CFStringRef type = (CFStringRef)CFDictionaryGetValue(proxy, kCFProxyTypeKey);
		if(!type || CFGetTypeID(type) != CFStringGetTypeID())
		{
			fprintf(stderr, "TextMate/proxy: Expected kCFProxyTypeKey to be a CFStringRef\n");
			continue;
		}

		if(CFEqual(type, kCFProxyTypeNone))
		{
			break;
		}
		else if(CFEqual(type, kCFProxyTypeAutoConfigurationURL))
		{
			CFURLRef pacURL = (CFURLRef)CFDictionaryGetValue(proxy, kCFProxyAutoConfigurationURLKey);
			if(pacURL && CFGetTypeID(pacURL) == CFURLGetTypeID())
			{
				fprintf(stderr, "TextMate DEBUG: Resolving PAC URL ‘%s’\n", cf::to_s(CFURLGetString(pacURL)).c_str());

				pac_proxy_callback_result_t result;

				CFStreamClientContext context = { 0, &result, NULL, NULL, NULL };
				CFRunLoopSourceRef runLoopSource = CFNetworkExecuteProxyAutoConfigurationURL(pacURL, targetURL, &pac_proxy_callback, &context);

				CFStringRef runLoopMode = CFSTR("OakRunPACScriptRunLoopMode");
				CFRunLoopAddSource(CFRunLoopGetCurrent(), runLoopSource, runLoopMode);
				while(!result.has_result)
					CFRunLoopRunInMode(runLoopMode, 0.1, TRUE);

				if(CFRunLoopSourceIsValid(runLoopSource))
					CFRunLoopSourceInvalidate(runLoopSource);
				CFRelease(runLoopSource);

				proxy_settings_t res;

				if(result.error)
				{
					CFStringRef str = CFErrorCopyDescription(result.error);
					fprintf(stderr, "TextMate/proxy: PAC error ‘%s’ for ‘%s’\n", cf::to_s(str).c_str(), cf::to_s(CFURLGetString(pacURL)).c_str());
					CFRelease(str);
				}
				else if(result.proxies)
				{
					res = first_proxy_from_array(result.proxies, targetURL);
				}
				else
				{
					fprintf(stderr, "TextMate/proxy: No proxies returned from ‘%s’\n", cf::to_s(CFURLGetString(pacURL)).c_str());
				}

				if(result.proxies)
					CFRelease(result.proxies);
				if(result.error)
					CFRelease(result.error);

				if(res)
				{
					fprintf(stderr, "return PAC proxy\n");
					return res;
				}
			}
			else
			{
				fprintf(stderr, "TextMate/proxy: Expected kCFProxyAutoConfigurationURLKey to be a CFURLRef\n");
			}
		}
		else if(CFEqual(type, kCFProxyTypeHTTP) || CFEqual(type, kCFProxyTypeHTTPS) || CFEqual(type, kCFProxyTypeSOCKS))
		{
			CFStringRef hostName   = (CFStringRef)CFDictionaryGetValue(proxy, kCFProxyHostNameKey);
			CFNumberRef portNumber = (CFNumberRef)CFDictionaryGetValue(proxy, kCFProxyPortNumberKey);

			if(CFGetTypeID(hostName) == CFStringGetTypeID() && CFGetTypeID(portNumber) == CFNumberGetTypeID())
			{
				auto res = user_pw_settings(hostName, portNumber);
				res.socks = CFEqual(type, kCFProxyTypeSOCKS);
				return res;
			}

			fprintf(stderr, "TextMate/proxy: Expected kCFProxyHostNameKey/kCFProxyPortNumberKey to be CFStringRef/CFNumberRef\n");
		}
		else
		{
			fprintf(stderr, "TextMate/proxy: Unknown proxy type: %s\n", cf::to_s(type).c_str());
		}
	}
	return proxy_settings_t();
}

proxy_settings_t get_proxy_settings (std::string const& url)
{
	proxy_settings_t res(false);
	if(regexp::search("^https?:/{2}localhost[:/]", url))
		return res;

	if(CFURLRef targetURL = CFURLCreateWithString(kCFAllocatorDefault, cf::wrap(url), NULL))
	{
		if(CFDictionaryRef proxySettings = SCDynamicStoreCopyProxies(NULL))
		{
			if(CFArrayRef proxies = CFNetworkCopyProxiesForURL(targetURL, proxySettings))
			{
				res = first_proxy_from_array(proxies, targetURL);
				CFRelease(proxies);
			}
			else
			{
				fprintf(stderr, "TextMate/proxy: NULL returned from CFNetworkCopyProxiesForURL(‘%s’)\n", url.c_str());
			}
			CFRelease(proxySettings);
		}
		else
		{
			fprintf(stderr, "TextMate/proxy: NULL returned from SCDynamicStoreCopyProxies()\n");
		}
		CFRelease(targetURL);
	}
	else
	{
		fprintf(stderr, "TextMate/proxy: Unable to create CFURLRef from ‘%s’\n", url.c_str());
	}

	return res;
}
