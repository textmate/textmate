static NSArray<NSDictionary*>* BrowsableVolumes ()
{
	NSMutableArray* array = [NSMutableArray array];

	struct statfs* mnts;
	int mnt_count = getmntinfo(&mnts, MNT_WAIT); // getfsstat
	for(int i = 0; i < mnt_count; ++i)
	{
		if(mnts[i].f_flags & MNT_DONTBROWSE)
			continue;

		NSString* path = [NSString stringWithUTF8String:mnts[i].f_mntonname];
		[array addObject:@{
			NSMetadataItemPathKey:        path,
			NSMetadataItemDisplayNameKey: [[NSFileManager defaultManager] displayNameAtPath:path],
			@"rootVolume":                @([path isEqualToString:@"/"]),
		}];
	}

	return array;
}

int main (int argc, char const* argv[])
{
	NSData* jsonData = [NSJSONSerialization dataWithJSONObject:@{ @"items": BrowsableVolumes() } options:0 error:nullptr];
	[[NSFileHandle fileHandleWithStandardOutput] writeData:jsonData];
	return 0;
}
