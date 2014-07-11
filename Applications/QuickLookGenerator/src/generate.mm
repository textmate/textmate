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

static std::string const kMacClassicThemeUUID = "71D40D9D-AE48-11D9-920A-000D93589AF6";

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
		cache.load_capnp(path::join(path::home(), "Library/Caches/com.macromates.TextMate/BundlesIndex.binary"));

		auto index = create_bundle_index(paths, cache);
		bundles::set_index(index.first, index.second);
	});
}

static std::string URLtoString (CFURLRef url)
{
	std::string filePath = NULL_STR;
	if(CFStringRef path = CFURLCopyFileSystemPath(url, kCFURLPOSIXPathStyle))
	{
		filePath = cf::to_s(path);
		CFRelease(path);
	}
	return filePath;
}

// =========================
// = QLGenerator interface =
// =========================

OSStatus TextMateQuickLookPlugIn_GenerateThumbnailForURL (void* instance, QLThumbnailRequestRef request, CFURLRef url, CFStringRef contentTypeUTI, CFDictionaryRef options, CGSize maxSize)
{
	initialize(QLThumbnailRequestGetGeneratorBundle(request));

	// Load file
	ng::buffer_t buffer;
	std::string filePath = URLtoString(url);
	std::string fileContents = file::read_utf8(filePath, nullptr, 1024); // 1Kb should be more than enough

	// Trim to 50 lines, since we don't need more
	size_t maxLines = 50;
	fileContents.erase(std::find_if(fileContents.begin(), fileContents.end(), [&maxLines](char ch) { return ch == '\n' && --maxLines == 0; }), fileContents.end());
	buffer.insert(0, fileContents);

	// Check if cancelled
	if(QLThumbnailRequestIsCancelled(request))
		return noErr;

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

	// Check if cancelled
	if(QLThumbnailRequestIsCancelled(request))
		return noErr;

	// Apply appropriate theme
	theme_ptr theme = parse_theme(bundles::lookup(kMacClassicThemeUUID));
	if(!theme)
		return noErr;

	NSFont* font = [NSFont userFixedPitchFontOfSize:4];
	theme = theme->copy_with_font_name_and_size(to_s([font fontName]), [font pointSize]);

	// Check if cancelled
	if(QLThumbnailRequestIsCancelled(request))
		return noErr;

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
			NSBackgroundColorAttributeName : [NSColor whiteColor],
			NSFontAttributeName            : font,
			NSUnderlineStyleAttributeName  : @(styles.underlined() ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
		}]];

		from = to;
	}

	// Check if cancelled
	if(QLThumbnailRequestIsCancelled(request))
		return noErr;

	// w/e the 3rd parameter, the context will always be a bitmap context
	CGContextRef bitmapContext = QLThumbnailRequestCreateContext(request, maxSize, true, NULL);
	if(bitmapContext)
	{
		NSGraphicsContext* context = [NSGraphicsContext graphicsContextWithGraphicsPort:bitmapContext flipped:YES];
		if(context)
		{
			[NSGraphicsContext saveGraphicsState];
			[NSGraphicsContext setCurrentContext:context];
			CGContextSaveGState(bitmapContext);
			CGContextTranslateCTM(bitmapContext, 0.0, maxSize.height);
			CGContextScaleCTM(bitmapContext, 1.0, -1.0);
			[output drawAtPoint:(CGPoint){0.0, 0.0}];
			CGContextRestoreGState(bitmapContext);
			[NSGraphicsContext restoreGraphicsState];
		}
		QLThumbnailRequestFlushContext(request, bitmapContext);
		CGContextRelease(bitmapContext);
	}

	return noErr;
}

void TextMateQuickLookPlugIn_CancelThumbnailGeneration (void* instance, QLThumbnailRequestRef request)
{
}

OSStatus TextMateQuickLookPlugIn_GeneratePreviewForURL (void* instance, QLPreviewRequestRef request, CFURLRef url, CFStringRef contentTypeUTI, CFDictionaryRef options)
{
	initialize(QLPreviewRequestGetGeneratorBundle(request));

	// Load file
	ng::buffer_t buffer;
	std::string filePath = URLtoString(url);
	std::string fileContents = file::read_utf8(filePath, nullptr, 20480);
	buffer.insert(0, fileContents);

	// Check if cancelled
	if(QLPreviewRequestIsCancelled(request))
		return noErr;

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

	// Check if cancelled
	if(QLPreviewRequestIsCancelled(request))
		return noErr;

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

	// Check if cancelled
	if(QLPreviewRequestIsCancelled(request))
		return noErr;

	// Perform syntax highlighting
	buffer.wait_for_repair();
	std::map<size_t, scope::scope_t> scopes = buffer.scopes(0, buffer.size());

	// Check if cancelled
	if(QLPreviewRequestIsCancelled(request))
		return noErr;

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

	// Check if cancelled
	if(QLPreviewRequestIsCancelled(request))
		return noErr;

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
