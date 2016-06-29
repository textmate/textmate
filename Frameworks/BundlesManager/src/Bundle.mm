#import "Bundle.h"
#import "BundlesManager.h"
#import <ns/ns.h>
#import <text/decode.h>
#import <regexp/format_string.h>
#import <version/version.h>

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
	std::string appVersion = to_s([[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleShortVersionString"]);
	return !version::less(appVersion, to_s(_minimumAppVersion));
}
@end

@implementation BundleGrammar
- (NSString*)description
{
	return [NSString stringWithFormat:@"<%@: %@ (%@)>", [self class], self.name, self.fileType];
}
@end
