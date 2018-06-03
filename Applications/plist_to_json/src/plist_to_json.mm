@interface FileHandleOutputStream : NSOutputStream
{
	NSFileHandle* _fileHandle;
}
+ (FileHandleOutputStream*)outputStreamWithFileHandle:(NSFileHandle*)fileHandle;
- (id)initWithFileHandle:(NSFileHandle*)fileHandle;
@end

@implementation FileHandleOutputStream
+ (FileHandleOutputStream*)outputStreamWithFileHandle:(NSFileHandle*)fileHandle
{
	return [[self alloc] initWithFileHandle:fileHandle];
}

- (id)initWithFileHandle:(NSFileHandle*)fileHandle
{
	if(self = [super init])
		_fileHandle = fileHandle;
	return self;
}

- (BOOL)hasSpaceAvailable
{
	return YES;
}

- (NSStreamStatus)streamStatus
{
	return NSStreamStatusOpen;
}

- (NSInteger)write:(uint8_t const*)buffer maxLength:(NSUInteger)length
{
	[_fileHandle writeData:[NSData dataWithBytesNoCopy:(void*)buffer length:length freeWhenDone:NO]];
	return length;
}
@end

static id DeleteNonJSONObjects (id container)
{
	if([container isKindOfClass:[NSDate class]])
	{
		time_t time = [container timeIntervalSince1970];
		struct tm bsdDate;
		gmtime_r(&time, &bsdDate);
		char buf[64];
		strftime(buf, sizeof(buf), "%F %T +0000", &bsdDate);
		container = [NSString stringWithUTF8String:buf];
	}
	else if([container isKindOfClass:[NSData class]])
	{
		container = [[NSString alloc] initWithData:[container base64EncodedDataWithOptions:0] encoding:NSUTF8StringEncoding];
	}
	else if([container isKindOfClass:[NSArray class]])
	{
		NSMutableArray* array = [NSMutableArray array];
		for(id value in container)
		{
			if(id newValue = DeleteNonJSONObjects(value))
				[array addObject:newValue];
		}
		container = array;
	}
	else if([container isKindOfClass:[NSDictionary class]])
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionary];
		for(id key in [container allKeys])
		{
			if(id newValue = DeleteNonJSONObjects(container[key]))
				dict[key] = newValue;
		}
		container = dict;
	}
	return container;
}

int main (int argc, char const* argv[])
{
	NSString* path = argc < 2 ? @"/Users/duff/Music/iTunes/iTunes Music Library.xml" : [NSString stringWithUTF8String:argv[1]];
	if(NSDictionary* dict = [NSDictionary dictionaryWithContentsOfFile:path])
	{
		dict = DeleteNonJSONObjects(dict);
		[NSJSONSerialization writeJSONObject:dict toStream:[FileHandleOutputStream outputStreamWithFileHandle:[NSFileHandle fileHandleWithStandardOutput]] options:0 error:nullptr];
	}
	return 0;
}
