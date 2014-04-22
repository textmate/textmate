#import "plugin.h"
#import <buffer/buffer.h>
#import <bundles/bundles.h>
#import <file/bytes.h>
#import <file/type.h>
#import <file/reader.h>
#import <io/path.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import <oak/misc.h>
#import <plist/fs_cache.h>
#import <scope/scope.h>
#import <settings/settings.h>
#import <theme/theme.h>
#import <OakAppKit/NSColor Additions.h>
#import <OakFoundation/NSString Additions.h>

OAK_EXTERN_C_BEGIN

static void initialize (CFBundleRef generatorBundle)
{
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		// Load settings
		NSURL* bundleURL = (__bridge_transfer NSURL*)CFBundleCopyBundleURL(generatorBundle);
		NSString* parentBundlePath = [[[[[[bundleURL filePathURL] path] stringByDeletingLastPathComponent] stringByDeletingLastPathComponent] stringByDeletingLastPathComponent] stringByDeletingLastPathComponent];
		NSBundle* parentBundle = [NSBundle bundleWithPath:parentBundlePath];

		settings_t::set_default_settings_path([[parentBundle pathForResource:@"Default" ofType:@"tmProperties"] fileSystemRepresentation]);
		settings_t::set_global_settings_path(path::join(path::home(), "Library/Application Support/TextMate/Global.tmProperties"));

		// Load bundle index
		std::vector<std::string> paths;
		for(auto path : bundles::locations())
			paths.push_back(path::join(path, "Bundles"));

		plist::cache_t cache;
		cache.load(path::join(path::home(), "Library/Caches/com.macromates.TextMate/BundlesIndex.plist"));

		auto index = create_bundle_index(paths, cache);
		bundles::set_index(index.first, index.second);
	});
}

// =========================
// = QLGenerator interface =
// =========================

// I see no attempt here at memory management, is it ok for QL generators to leak?


OSStatus TextMateQuickLookPlugIn_GenerateThumbnailForURL (void* instance, QLThumbnailRequestRef request, CFURLRef url, CFStringRef contentTypeUTI, CFDictionaryRef options, CGSize maxSize)
{
	initialize(QLPreviewRequestGetGeneratorBundle(request));
	
	
	NSAttributedString* output = TextMateQuickLookPlugIn_CreateAttributedString(url);
	
	
    CGContextRef cgContext = QLPreviewRequestCreateContext(preview, &maxSize, false, NULL);
    if(cgContext) {
        NSGraphicsContext* context = [NSGraphicsContext graphicsContextWithGraphicsPort:(void *)cgContext flipped:YES];
        if(context) {
			NSRect destRect = NSMakeRect(0,0,maxSize.width, maxSize.height);
            [output drawInRect:destRect];
        }
        QLPreviewRequestFlushContext(preview, cgContext);
        CFRelease(cgContext);
    }

	return noErr;
}

// As written here this has the HUGE bug of reading in the entire file. We only need 40 lines for a thumbnail, and say 500 for a preview...
// a 2GB file will trash quite badly it looks like.
NSAttributedString* TextMateQuickLookPlugIn_CreateAttributedString(CFURLRef url)
{
	// Load file
	ng::buffer_t buffer;

	std::string filePath = NULL_STR;
	if(CFStringRef path = CFURLCopyFileSystemPath(url, kCFURLPOSIXPathStyle))
	{
		filePath = cf::to_s(path);
		CFRelease(path);
	}

	std::string fileContents = file::read_utf8(filePath);
	buffer.insert(0, fileContents);

	// Apply appropriate grammar
	std::string fileType = file::type(filePath, std::make_shared<io::bytes_t>(fileContents.data(), fileContents.size(), false));
	if(fileType != NULL_STR)
	{
		for(auto item : bundles::query(bundles::kFieldGrammarScope, fileType, scope::wildcard, bundles::kItemTypeGrammar))
		{
			buffer.set_grammar(item);
			break;
		}
	}

	// Apply appropriate theme
	settings_t const settings = settings_for_path(filePath, fileType);
	std::string themeUUID = settings.get(kSettingsThemeKey, NULL_STR);
	bundles::item_ptr themeItem = themeUUID != NULL_STR ? bundles::lookup(themeUUID) : bundles::item_ptr();
	theme_ptr theme = themeItem ? parse_theme(themeItem) : theme_ptr();
	if(theme) theme = theme->copy_with_font_name_and_size(settings.get(kSettingsFontNameKey, NULL_STR), settings.get(kSettingsFontSizeKey, 11));

	if(!theme)
	{
		NSData* data = [NSData dataWithContentsOfURL:(__bridge NSURL*)url];
		QLPreviewRequestSetDataRepresentation(request, (__bridge CFDataRef)data, kUTTypePlainText, nil);
		return noErr;
	}

	// Perform syntax highlighting
	buffer.wait_for_repair();
	std::map<size_t, scope::scope_t> scopes = buffer.scopes(0, buffer.size());

	// Construct RTF output
	NSMutableAttributedString* output = (__bridge_transfer NSMutableAttributedString*)CFAttributedStringCreateMutable(kCFAllocatorDefault, buffer.size());
	size_t from = 0;
	for(auto pair = scopes.begin(); pair != scopes.end(); )
	{
		styles_t styles = theme->styles_for_scope(pair->second);

		size_t to = ++pair != scopes.end() ? pair->first : buffer.size();

		[output appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:buffer.substr(from, to)] attributes:@{
			NSForegroundColorAttributeName : [NSColor tmColorWithCGColor:styles.foreground()],
			NSBackgroundColorAttributeName : [NSColor tmColorWithCGColor:styles.background()],
			NSFontAttributeName            : (__bridge NSFont*)styles.font(),
			NSUnderlineStyleAttributeName  : @(styles.underlined() ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
		}]];

		from = to;
	}
	return output;
}

void TextMateQuickLookPlugIn_CancelThumbnailGeneration (void* instance, QLThumbnailRequestRef request)
{
}

OSStatus TextMateQuickLookPlugIn_GeneratePreviewForURL (void* instance, QLPreviewRequestRef request, CFURLRef url, CFStringRef contentTypeUTI, CFDictionaryRef options)
{
	initialize(QLPreviewRequestGetGeneratorBundle(request));
	
	NSAttributedString* output = TextMateQuickLookPlugIn_CreateAttributedString(url);

	NSData* outputData = [output RTFFromRange:NSMakeRange(0, [output length]) documentAttributes:@{
		NSDocumentTypeDocumentAttribute : [NSString stringWithCxxString:fileType],
		NSBackgroundColorDocumentAttribute : theme ? [NSColor tmColorWithCGColor:theme->background(fileType)] : [NSColor whiteColor],
	}];

	QLPreviewRequestSetDataRepresentation(request, (__bridge CFDataRef)outputData, kUTTypeRTF, nil);
	return noErr;
}

void TextMateQuickLookPlugIn_CancelPreviewGeneration (void* instance, QLPreviewRequestRef request)
{
}

OAK_EXTERN_C_END
