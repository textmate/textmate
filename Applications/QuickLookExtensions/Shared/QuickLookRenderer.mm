#import "QuickLookRenderer.h"

#import <OSAKit/OSAKit.h>
#import <buffer/src/buffer.h>
#import <bundles/src/bundles.h>
#import <cf/src/cf.h>
#import <file/src/bytes.h>
#import <file/src/reader.h>
#import <file/src/type.h>
#import <io/src/path.h>
#import <ns/src/ns.h>
#import <plist/src/fs_cache.h>
#import <scope/src/scope.h>
#import <settings/src/settings.h>
#import <theme/src/theme.h>
#import <OakFoundation/src/NSString Additions.h>

static NSString* const TMQuickLookErrorDomain = @"com.macromates.TextMate.QuickLook";

@interface TMQuickLookRenderedContent ()

- (instancetype)initWithAttributedString:(NSAttributedString*)attributedString backgroundColor:(NSColor*)backgroundColor fileType:(NSString*)fileType;

@end


@implementation TMQuickLookRenderedContent

- (instancetype)initWithAttributedString:(NSAttributedString*)attributedString backgroundColor:(NSColor*)backgroundColor fileType:(NSString*)fileType
{
	if((self = [super init]))
	{
		_attributedString = attributedString;
		_backgroundColor = backgroundColor;
		_fileType = fileType;
	}
	return self;
}

@end


static NSError* TMQuickLookError(NSInteger code, NSString* description)
{
	return [NSError errorWithDomain:TMQuickLookErrorDomain code:code userInfo:@{ NSLocalizedDescriptionKey: description }];
}

static NSBundle* TMHostApplicationBundle(void)
{
	NSURL* url = NSBundle.mainBundle.bundleURL;
	while(url && ![url.pathExtension.lowercaseString isEqualToString:@"app"])
	{
		NSURL* parent = url.URLByDeletingLastPathComponent;
		if([parent isEqual:url])
			return nil;
		url = parent;
	}
	return url ? [NSBundle bundleWithURL:url] : nil;
}

static void TMInitializeTextMateEngine(void)
{
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		NSBundle* hostBundle = TMHostApplicationBundle();
		if(!hostBundle)
			return;

		NSString* defaultSettings = [hostBundle pathForResource:@"Default" ofType:@"tmProperties"];
		if(defaultSettings)
			settings_t::set_default_settings_path(defaultSettings.fileSystemRepresentation);

		NSString* applicationSupport = [NSHomeDirectory() stringByAppendingPathComponent:@"Library/Application Support/TextMate"];
		settings_t::set_global_settings_path([[applicationSupport stringByAppendingPathComponent:@"Global.tmProperties"] fileSystemRepresentation]);

		NSMutableArray<NSString*>* candidates = [NSMutableArray arrayWithArray:@[
			[applicationSupport stringByAppendingPathComponent:@"Bundles"],
			[applicationSupport stringByAppendingPathComponent:@"Pristine Copy/Bundles"],
			[applicationSupport stringByAppendingPathComponent:@"Managed/Bundles"],
			@"/Library/Application Support/TextMate/Bundles",
			@"/Library/Application Support/TextMate/Pristine Copy/Bundles",
		]];
		[candidates addObject:[hostBundle.bundlePath stringByAppendingPathComponent:@"Contents/SharedSupport/Bundles"]];

		std::vector<std::string> paths;
		for(NSString* candidate in candidates)
		{
			if([NSFileManager.defaultManager isReadableFileAtPath:candidate])
				paths.emplace_back(candidate.fileSystemRepresentation);
		}

		plist::cache_t cache;
		auto index = create_bundle_index(paths, cache);
		bundles::set_index(index.first, index.second);
	});
}

static std::string TMReadSource(NSURL* url, size_t maxSize)
{
	if([url.pathExtension.lowercaseString isEqualToString:@"scpt"])
	{
		NSDictionary* details = nil;
		OSAScript* script = [[OSAScript alloc] initWithContentsOfURL:url error:&details];
		if(script.source)
		{
			std::string source = to_s(script.source);
			std::replace(source.begin(), source.end(), '\r', '\n');
			if(source.size() > maxSize)
				source.resize(maxSize);
			return source;
		}
	}
	return file::read_utf8(url.path.fileSystemRepresentation, nullptr, maxSize);
}

static std::string TMSetupBuffer(NSURL* url, ng::buffer_t& buffer, size_t maxSize, size_t maxLines)
{
	std::string contents = TMReadSource(url, maxSize);
	if(maxLines != SIZE_T_MAX)
	{
		size_t linesLeft = maxLines;
		auto end = std::find_if(contents.begin(), contents.end(), [&linesLeft](char ch) {
			return ch == '\n' && --linesLeft == 0;
		});
		contents.erase(end, contents.end());
	}

	buffer.insert(0, contents);
	std::string path = url.path.fileSystemRepresentation;
	std::string fileType = file::type(path, std::make_shared<io::bytes_t>(contents.data(), contents.size(), false));
	if(fileType != NULL_STR)
	{
		auto grammars = bundles::query(bundles::kFieldGrammarScope, fileType, scope::wildcard, bundles::kItemTypeGrammar);
		if(!grammars.empty())
			buffer.set_grammar(grammars.front());
	}
	return fileType;
}

static BOOL TMUseDarkTheme(void)
{
	NSUserDefaults* defaults = [[NSUserDefaults alloc] initWithSuiteName:@"com.macromates.TextMate"];
	NSString* appearance = [defaults stringForKey:@"themeAppearance"];
	if([appearance isEqualToString:@"dark"])
		return YES;
	if([appearance isEqualToString:@"light"])
		return NO;
	return [[NSAppearance.currentDrawingAppearance bestMatchFromAppearancesWithNames:@[ NSAppearanceNameAqua, NSAppearanceNameDarkAqua ]] isEqualToString:NSAppearanceNameDarkAqua];
}

static theme_ptr TMTheme(BOOL dark, NSString** themeUUID)
{
	NSUserDefaults* defaults = [[NSUserDefaults alloc] initWithSuiteName:@"com.macromates.TextMate"];
	NSString* uuid = [defaults stringForKey:dark ? @"darkModeThemeUUID" : @"universalThemeUUID"];
	if(!uuid)
		uuid = @(dark ? kTwilightThemeUUID : kMacClassicThemeUUID);

	theme_ptr theme = parse_theme(bundles::lookup(to_s(uuid)));
	if(!theme)
	{
		uuid = @(dark ? kTwilightThemeUUID : kMacClassicThemeUUID);
		theme = parse_theme(bundles::lookup(to_s(uuid)));
	}
	if(themeUUID)
		*themeUUID = uuid;
	return theme;
}

static NSAttributedString* TMCreateAttributedString(ng::buffer_t& buffer, theme_ptr theme, NSString* fontName, CGFloat fontSize)
{
	if(!theme || !fontName || fontSize <= 0)
		return nil;

	theme = theme->copy_with_font_name_and_size(to_s(fontName), fontSize);
	buffer.wait_for_repair();
	std::map<size_t, scope::scope_t> scopes = buffer.scopes(0, buffer.size());
	NSMutableAttributedString* output = [[NSMutableAttributedString alloc] init];
	size_t from = 0;
	for(auto pair = scopes.begin(); pair != scopes.end(); )
	{
		styles_t styles = theme->styles_for_scope(pair->second);
		size_t to = ++pair != scopes.end() ? pair->first : buffer.size();
		NSString* string = [NSString stringWithCxxString:buffer.substr(from, to)];
		[output appendAttributedString:[[NSAttributedString alloc] initWithString:string attributes:@{
			NSForegroundColorAttributeName: [NSColor colorWithCGColor:styles.foreground()],
			NSBackgroundColorAttributeName: [NSColor colorWithCGColor:styles.background()],
			NSFontAttributeName: (__bridge NSFont*)styles.font(),
			NSUnderlineStyleAttributeName: @(styles.underlined() ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
			NSStrikethroughStyleAttributeName: @(styles.strikethrough() ? NSUnderlineStyleSingle : NSUnderlineStyleNone),
		}]];
		from = to;
	}
	return output;
}

TMQuickLookRenderedContent* TMQuickLookRenderURL(NSURL* url, BOOL thumbnail, NSError** error)
{
	TMInitializeTextMateEngine();
	if(!url.isFileURL || ![NSFileManager.defaultManager fileExistsAtPath:url.path])
	{
		if(error)
			*error = TMQuickLookError(1, @"The previewed file is not available.");
		return nil;
	}

	ng::buffer_t buffer;
	std::string fileType = TMSetupBuffer(url, buffer, thumbnail ? 1024 : 20480, thumbnail ? 50 : SIZE_T_MAX);
	BOOL dark = TMUseDarkTheme();
	theme_ptr theme = TMTheme(dark, nullptr);
	NSFont* fallbackFont = [NSFont userFixedPitchFontOfSize:thumbnail ? 4 : 0];
	NSString* fontName = fallbackFont.fontName;
	CGFloat fontSize = fallbackFont.pointSize;
	if(!thumbnail && fileType != NULL_STR)
	{
		settings_t settings = settings_for_path(url.path.fileSystemRepresentation, fileType);
		fontName = to_ns(settings.get(kSettingsFontNameKey, to_s(fontName)));
		fontSize = settings.get(kSettingsFontSizeKey, fontSize);
	}

	NSAttributedString* attributedString = TMCreateAttributedString(buffer, theme, fontName, fontSize);
	if(!attributedString)
	{
		NSString* plain = [NSString stringWithCxxString:buffer.substr(0, buffer.size())];
		attributedString = [[NSAttributedString alloc] initWithString:plain attributes:@{
			NSForegroundColorAttributeName: dark ? NSColor.whiteColor : NSColor.textColor,
			NSFontAttributeName: fallbackFont,
		}];
	}

	NSString* type = fileType == NULL_STR ? @"text.plain" : to_ns(fileType);
	NSColor* background = theme ? [NSColor colorWithCGColor:theme->background(to_s(type))] : (dark ? NSColor.blackColor : NSColor.textBackgroundColor);
	return [[TMQuickLookRenderedContent alloc] initWithAttributedString:attributedString backgroundColor:background fileType:type];
}

NSData* TMQuickLookRTFData(TMQuickLookRenderedContent* content, NSError** error)
{
	NSDictionary* attributes = @{
		NSDocumentTypeDocumentAttribute: NSRTFTextDocumentType,
		NSBackgroundColorDocumentAttribute: content.backgroundColor,
	};
	return [content.attributedString dataFromRange:NSMakeRange(0, content.attributedString.length) documentAttributes:attributes error:error];
}
