#include "proxy.h"
#include <regexp/src/regexp.h>
#include <text/src/encode.h>
#include <oak/debug.h>
#include <cf/src/cf.h>

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
	CFDictionaryRef query = CFDictionaryCreate(kCFAllocatorDefault, keys, vals, sizeofA(keys), nullptr, nullptr);

	CFArrayRef results = nullptr;
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

			void* data = nullptr;
			UInt32 dataLen = 0;

			SecKeychainAttributeList* authAttrList = nullptr;
			if(SecKeychainItemCopyAttributesAndData(item, &info, nullptr, &authAttrList, &dataLen, &data) == noErr)
			{
				ASSERT(authAttrList->count == 1 && authAttrList->attr->tag == kSecAccountItemAttr);
				user = std::string((char const*)authAttrList->attr->data, ((char const*)authAttrList->attr->data) + authAttrList->attr->length);
				pw   = std::string((char const*)data, ((char const*)data) + dataLen);
				SecKeychainItemFreeAttributesAndData(authAttrList, data);
			}
		}

		CFRelease(results);
	}
	else if(err != errSecItemNotFound)
	{
		CFStringRef message = SecCopyErrorMessageString(err, nullptr);
		os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: SecItemCopyMatching() failed with error ‘%{public}@’", message);
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
	CFArrayRef proxies = nullptr;
	CFErrorRef error   = nullptr;
};

static void pac_proxy_callback (void* client, CFArrayRef proxies, CFErrorRef error)
{
	auto res = static_cast<pac_proxy_callback_result_t*>(client);
	res->proxies    = proxies ? (CFArrayRef)CFRetain(proxies) : nullptr;
	res->error      = error   ? (CFErrorRef)CFRetain(error)   : nullptr;
	res->has_result = true;
}

proxy_settings_t first_proxy_from_array (CFArrayRef proxies, CFURLRef targetURL)
{
	for(CFIndex i = 0; i < CFArrayGetCount(proxies); ++i)
	{
		CFDictionaryRef proxy = (CFDictionaryRef)CFArrayGetValueAtIndex(proxies, i);
		if(!proxy || CFGetTypeID(proxy) != CFDictionaryGetTypeID())
		{
			os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: Expected proxy info to be a CFDictionaryRef");
			continue;
		}

		CFStringRef type = (CFStringRef)CFDictionaryGetValue(proxy, kCFProxyTypeKey);
		if(!type || CFGetTypeID(type) != CFStringGetTypeID())
		{
			os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: Expected kCFProxyTypeKey to be a CFStringRef");
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
				os_log_info(OS_LOG_DEFAULT, "Resolving PAC URL ‘%{public}@’", CFURLGetString(pacURL));

				pac_proxy_callback_result_t result;

				CFStreamClientContext context = { 0, &result, nullptr, nullptr, nullptr };
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
					os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: PAC error ‘%{public}@’ for ‘%{public}@’", str, CFURLGetString(pacURL));
					CFRelease(str);
				}
				else if(result.proxies)
				{
					res = first_proxy_from_array(result.proxies, targetURL);
				}
				else
				{
					os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: No proxies returned from ‘%{public}@’", CFURLGetString(pacURL));
				}

				if(result.proxies)
					CFRelease(result.proxies);
				if(result.error)
					CFRelease(result.error);

				if(res)
				{
					os_log(OS_LOG_DEFAULT, "Return PAC proxy");
					return res;
				}
			}
			else
			{
				os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: Expected kCFProxyAutoConfigurationURLKey to be a CFURLRef");
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

			os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: Expected kCFProxyHostNameKey/kCFProxyPortNumberKey to be CFStringRef/CFNumberRef");
		}
		else
		{
			os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: Unknown proxy type: %{public}@", type);
		}
	}
	return proxy_settings_t();
}

proxy_settings_t get_proxy_settings (std::string const& url)
{
	proxy_settings_t res(false);
	if(regexp::search("^https?:/{2}localhost[:/]", url))
		return res;

	if(CFURLRef targetURL = CFURLCreateWithString(kCFAllocatorDefault, cf::wrap(url), nullptr))
	{
		if(CFDictionaryRef proxySettings = SCDynamicStoreCopyProxies(nullptr))
		{
			if(CFArrayRef proxies = CFNetworkCopyProxiesForURL(targetURL, proxySettings))
			{
				res = first_proxy_from_array(proxies, targetURL);
				CFRelease(proxies);
			}
			else
			{
				os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: NULL returned from CFNetworkCopyProxiesForURL(‘%{public}s’)", url.c_str());
			}
			CFRelease(proxySettings);
		}
		else
		{
			os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: NULL returned from SCDynamicStoreCopyProxies()");
		}
		CFRelease(targetURL);
	}
	else
	{
		os_log_error(OS_LOG_DEFAULT, "TextMate/proxy: Unable to create CFURLRef from ‘%{public}s’", url.c_str());
	}

	return res;
}
