#import "FileItem.h"
#import "FileItemImage.h"
#import "SCMManager.h"
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakFinderTag.h>
#import <OakFoundation/OakFoundation.h>
#import <Preferences/Keys.h>
#import <ns/ns.h>

NSURL* const kURLLocationComputer  = [[NSURL alloc] initWithString:@"computer:///"];
NSURL* const kURLLocationFavorites = [[NSURL alloc] initFileURLWithPath:[NSHomeDirectory() stringByAppendingPathComponent:@"Library/Application Support/TextMate/Favorites"] isDirectory:YES];

@implementation NSURL (CompatibilityWrapper)
- (BOOL)tmHasDirectoryPath
{
	if([self respondsToSelector:@selector(hasDirectoryPath)])
		return self.hasDirectoryPath; // MAC_OS_X_VERSION_10_11

	NSString* urlString = self.absoluteString;
	NSRange range = [urlString rangeOfCharacterFromSet:[NSCharacterSet characterSetWithCharactersInString:@"?;"]];
	if(range.location != NSNotFound)
		urlString = [urlString substringToIndex:range.location];
	return [urlString hasSuffix:@"/"];
}
@end

@interface FileItem ()
@property (nonatomic, readonly) BOOL alwaysShowFileExtension;
@property (nonatomic, readwrite) NSImage* image;
@property (nonatomic, readwrite) scm::status::type SCMStatus;
@property (nonatomic) id SCMStatusObserver;
@end

static NSMutableDictionary* SchemeToClass;

@implementation FileItem
+ (void)load
{
	[self registerClass:[FileItem class] forURLScheme:@"file"];
}

+ (void)registerClass:(Class)klass forURLScheme:(NSString*)urlScheme
{
	if(!SchemeToClass)
		SchemeToClass = [NSMutableDictionary dictionary];
	SchemeToClass[urlScheme] = klass;
}

+ (Class)classForURL:(NSURL*)url
{
	return SchemeToClass[url.scheme];
}

+ (instancetype)fileItemWithURL:(NSURL*)url
{
	return [[[self classForURL:url] alloc] initWithURL:url];
}

- (instancetype)initWithURL:(NSURL*)url
{
	if(self = [super init])
	{
		self.URL = url;

		NSNumber* flag;
		BOOL disableSCMStatus = [url getResourceValue:&flag forKey:@"org.textmate.disable-scm-status" error:nil] && [flag boolValue];

		[self updateFileProperties];
		[self addSCMStatusObserver:url.isFileURL && !disableSCMStatus];
	}
	return self;
}

- (void)dealloc
{
	[self addSCMStatusObserver:NO];
}

- (void)addSCMStatusObserver:(BOOL)flag
{
	if(_SCMStatusObserver)
	{
		[SCMManager.sharedInstance removeObserver:_SCMStatusObserver];
		_SCMStatusObserver = nil;
	}

	if(flag)
	{
		__weak FileItem* weakSelf = self;
		_SCMStatusObserver = [SCMManager.sharedInstance addObserverToFileAtURL:_URL usingBlock:^(scm::status::type newStatus){
			weakSelf.SCMStatus = newStatus;
			weakSelf.missing   = newStatus == scm::status::deleted;
		}];
	}
}

- (BOOL)canRename
{
	NSNumber* flag;
	return _URL.isFileURL && !self.isMissing && (![_URL getResourceValue:&flag forKey:NSURLIsVolumeKey error:nil] || ![flag boolValue]);
}

- (void)updateFileProperties
{
	if(!_URL.isFileURL)
		return;

	NSNumber* flag;

	self.hidden          = [_URL getResourceValue:&flag forKey:NSURLIsHiddenKey error:nil] && [flag boolValue];
	self.hiddenExtension = [_URL getResourceValue:&flag forKey:NSURLHasHiddenExtensionKey error:nil] && [flag boolValue];
	self.symbolicLink    = [_URL getResourceValue:&flag forKey:NSURLIsSymbolicLinkKey error:nil] && [flag boolValue];
	self.package         = !_symbolicLink && [_URL getResourceValue:&flag forKey:NSURLIsPackageKey error:nil] && [flag boolValue];
	self.linkToDirectory = _symbolicLink && [self.resolvedURL getResourceValue:&flag forKey:NSURLIsDirectoryKey error:nil] && [flag boolValue];
	self.linkToPackage   = _symbolicLink && [self.resolvedURL getResourceValue:&flag forKey:NSURLIsPackageKey error:nil] && [flag boolValue];;
	self.finderTags      = [OakFinderTagManager finderTagsForURL:self.URL];
	self.missing         = _missing && ![NSFileManager.defaultManager fileExistsAtPath:_URL.path];
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"<%@: %@>", self.class, _URL];
}

- (NSURL*)previewItemURL
{
	return self.isMissing ? nil : self.resolvedURL.filePathURL;
}

- (NSString*)previewItemTitle
{
	return self.localizedName;
}

// =======================
// = Computed Properties =
// =======================

- (BOOL)alwaysShowFileExtension
{
	return [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsShowFileExtensionsKey];
}

- (BOOL)isDirectory
{
	return _URL.tmHasDirectoryPath;
}

- (NSString*)displayName
{
	return [self.localizedName stringByAppendingString:_disambiguationSuffix ?: @""];
}

- (NSString*)localizedName
{
	if(!_localizedName && _URL.isFileURL)
	{
		NSString* name;
		if([_URL getResourceValue:&name forKey:NSURLLocalizedNameKey error:nil])
		{
			_localizedName = name;
			if(_hiddenExtension && self.alwaysShowFileExtension && OakNotEmptyString(_URL.pathExtension))
				_localizedName = [_localizedName stringByAppendingPathExtension:_URL.pathExtension];
		}
	}
	return _localizedName ?: _URL.lastPathComponent;
}

- (NSImage*)image
{
	if(!_image)
		_image = CreateIconImageForURL(_URL, _modified, _missing, self.isDirectory || _linkToDirectory, _symbolicLink, _SCMStatus);
	return _image;
}

- (NSURL*)resolvedURL
{
	NSURL* url = _URL;
	if(_symbolicLink)
	{
		// For /{etc,tmp,var} we get /{etc,tmp,var}/ insteawd of /private/{etc,tmp,var}/
		url = [url URLByResolvingSymlinksInPath];

		NSNumber* flag;
		if([url getResourceValue:&flag forKey:NSURLIsSymbolicLinkKey error:nil] && [flag boolValue])
		{
			NSError* error;
			if(NSString* path = [NSFileManager.defaultManager destinationOfSymbolicLinkAtPath:_URL.path error:&error])
				url = [[_URL URLByDeletingLastPathComponent] URLByAppendingPathComponent:path];
		}
	}
	return url;
}

- (NSURL*)parentURL
{
	if(_URL.isFileURL)
	{
		NSNumber* flag;
		NSURL* parentURL;

		if([_URL getResourceValue:&flag forKey:NSURLIsVolumeKey error:nil] && [flag boolValue])
			return kURLLocationComputer;
		else if([_URL getResourceValue:&parentURL forKey:NSURLParentDirectoryURLKey error:nil] && parentURL)
			return parentURL;
	}
	return nil;
}

- (BOOL)isApplication
{
	// NSURLIsApplicationKey requires MAC_OS_X_VERSION_10_11

	LSItemInfoRecord itemInfo;
	if(_URL.isFileURL && LSCopyItemInfoForURL((__bridge CFURLRef)_URL, kLSRequestBasicFlagsOnly, &itemInfo) == noErr)
	{
		OptionBits flags = itemInfo.flags;
		if(flags & kLSItemInfoIsApplication)
			return YES;
	}
	return NO;
}

// ===========================================
// = Setters that invalidate other propertes =
// ===========================================

+ (NSSet*)keyPathsForValuesAffectingImage
{
	return [NSSet setWithObjects:@"URL", @"linkToDirectory", @"missing", @"modified", @"SCMStatus", @"symbolicLink", nil];
}

+ (NSSet*)keyPathsForValuesAffectingDisplayName
{
	return [NSSet setWithObjects:@"localizedName", @"disambiguationSuffix", nil];
}

+ (NSSet*)keyPathsForValuesAffectingLocalizedName
{
	return [NSSet setWithObjects:@"URL", @"hiddenExtension", nil];
}

- (void)setURL:(NSURL*)newURL
{
	if(_URL != newURL && ![_URL isEqual:newURL])
	{
		_URL           = newURL;
		_localizedName = nil;
		_image         = nil;

		[self addSCMStatusObserver:_SCMStatusObserver ? YES : NO];

		for(FileItem* child in _children)
		{
			if(child.URL.isFileURL && _URL.isFileURL)
				child.URL = [_URL URLByAppendingPathComponent:child.URL.lastPathComponent isDirectory:child.URL.tmHasDirectoryPath];
		}
	}

	_fileReferenceURL = _URL.fileReferenceURL;
}

- (void)setHiddenExtension:(BOOL)flag
{
	if(_hiddenExtension != flag)
	{
		_hiddenExtension = flag;
		_localizedName   = nil;
	}
}

- (void)setLinkToDirectory:(BOOL)flag
{
	if(_linkToDirectory != flag)
	{
		_linkToDirectory = flag;
		_image           = nil;
	}
}

- (void)setMissing:(BOOL)flag
{
	if(_missing != flag)
	{
		_missing = flag;
		_image   = nil;
	}
}

- (void)setModified:(BOOL)flag
{
	if(_modified != flag)
	{
		_modified = flag;
		_image    = nil;
	}
}

- (void)setSCMStatus:(scm::status::type)newStatus
{
	if(_SCMStatus != newStatus)
	{
		_SCMStatus = newStatus;
		_image     = nil;
	}
}

- (void)setSymbolicLink:(BOOL)flag
{
	if(_symbolicLink != flag)
	{
		_symbolicLink = flag;
		_image        = nil;
	}
}
@end
