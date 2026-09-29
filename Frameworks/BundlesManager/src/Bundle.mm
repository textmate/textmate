#import "Bundle.h"
#import "BundlesManager.h"
#import <SoftwareUpdate/src/SoftwareUpdate.h> // OakCompareVersionStrings()
#import <ns/src/ns.h>
#import <text/src/decode.h>
#import <regexp/src/format_string.h>

@implementation Bundle
- (BOOL)isEqual:(id)other    { return [other isKindOfClass:[self class]] && [self.identifier isEqual:[other identifier]]; }
- (NSUInteger)hash           { return [self.identifier hash]; }
- (NSString*)description     { return [NSString stringWithFormat:@"<%@: %@ by %@%@>", [self class], _name, _contactName, _installed && _path ? [@", " stringByAppendingString:_path] : @""]; }

+ (NSSet*)keyPathsForValuesAffectingHasUpdate
{
	return [NSSet setWithObjects:@"downloadLastUpdated", @"lastUpdated", nil];
}

+ (NSSet*)keyPathsForValuesAffectingCompatible
{
	return [NSSet setWithObjects:@"minimumAppVersion", nil];
}

+ (NSSet*)keyPathsForValuesAffectingTextSummary
{
	return [NSSet setWithObjects:@"summary", nil];
}

- (instancetype)initWithIdentifier:(NSUUID*)anIdentifier
{
	if(self = [self init])
	{
		_identifier = anIdentifier;
	}
	return self;
}

- (NSString*)textSummary
{
	std::string str = to_s(self.summary);
	str = format_string::replace(str, "\\A\\s+|<[^>]*>|\\s+\\z", "");
	str = format_string::replace(str, "\\s+", " ");
	str = decode::entities(str);
	return to_ns(str);
}

- (BOOL)hasUpdate
{
	return _downloadLastUpdated && _lastUpdated && [_downloadLastUpdated laterDate:_lastUpdated] != _lastUpdated;
}

- (BOOL)isCompatible
{
	NSString* appVersion = [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleShortVersionString"];
	return OakCompareVersionStrings(appVersion, _minimumAppVersion) != NSOrderedAscending;
}
@end

@implementation BundleGrammar
- (NSString*)description
{
	return [NSString stringWithFormat:@"<%@: %@ (%@)>", [self class], self.name, self.fileType];
}
@end
