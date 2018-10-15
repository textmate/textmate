#import "FileItemImage.h"
#import <OakAppKit/NSImage Additions.h>

static NSImage* CustomIconForPath (NSString* path)
{
	static NSMutableDictionary* bindings = [NSMutableDictionary dictionary];

	static dispatch_once_t onceToken = 0;
	dispatch_once(&onceToken, ^{
		NSDictionary* map = [NSDictionary dictionaryWithContentsOfFile:[[NSBundle bundleForClass:NSClassFromString(@"OakFileIconImage")] pathForResource:@"bindings" ofType:@"plist"]];
		for(NSString* key in map)
		{
			for(NSString* ext in map[key])
				bindings[ext.lowercaseString] = key;
		}
	});

	NSString* pathName  = path.lastPathComponent.lowercaseString;
	NSString* imageName = bindings[pathName];

	NSRange range = [pathName rangeOfString:@"."];
	if(range.location != NSNotFound)
	{
		imageName = bindings[[pathName substringFromIndex:NSMaxRange(range)]];
		imageName = imageName ?: bindings[pathName.pathExtension];
	}

	NSImage* res = nil;
	if(imageName)
	{
		static NSMutableDictionary* images = [NSMutableDictionary dictionary];
		@synchronized(images) {
			if(!(res = images[imageName]))
			{
				if(res = [NSImage imageNamed:imageName inSameBundleAsClass:NSClassFromString(@"OakFileIconImage")])
					images[imageName] = res;
			}
		}
	}
	return res;
}

static NSImage* IconBadgeForAlias ()
{
	IconRef iconRef;
	if(GetIconRef(kOnSystemDisk, kSystemIconsCreator, kAliasBadgeIcon, &iconRef) == noErr)
	{
		NSImage* badge = [[NSImage alloc] initWithIconRef:iconRef];
		ReleaseIconRef(iconRef);
		return badge;
	}
	return nil;
}

static NSImage* SystemIconForHFSType (OSType hfsFileTypeCode)
{
	return [NSWorkspace.sharedWorkspace iconForFileType:NSFileTypeForHFSTypeCode(hfsFileTypeCode)];
}

static NSImage* BadgeForSCMStatus (scm::status::type scmStatus)
{
	switch(scmStatus)
	{
		case scm::status::conflicted:   return [NSImage imageNamed:@"Conflicted"  inSameBundleAsClass:NSClassFromString(@"OakFileIconImage")];
		case scm::status::modified:     return [NSImage imageNamed:@"Modified"    inSameBundleAsClass:NSClassFromString(@"OakFileIconImage")];
		case scm::status::added:        return [NSImage imageNamed:@"Added"       inSameBundleAsClass:NSClassFromString(@"OakFileIconImage")];
		case scm::status::deleted:      return [NSImage imageNamed:@"Deleted"     inSameBundleAsClass:NSClassFromString(@"OakFileIconImage")];
		case scm::status::unversioned:  return [NSImage imageNamed:@"Unversioned" inSameBundleAsClass:NSClassFromString(@"OakFileIconImage")];
		case scm::status::mixed:        return [NSImage imageNamed:@"Mixed"       inSameBundleAsClass:NSClassFromString(@"OakFileIconImage")];
	}
	return nil;
}

NSImage* CreateIconImageForURL (NSURL* url, BOOL isModified, BOOL isMissing, BOOL isDirectory, BOOL isSymbolicLink, scm::status::type scmStatus)
{
	return [NSImage imageWithSize:NSMakeSize(16, 16) flipped:NO drawingHandler:^BOOL(NSRect dstRect){
		NSImage* image;
		BOOL drawLinkBadge = isSymbolicLink;
		if(isMissing)
		{
			image = SystemIconForHFSType(kUnknownFSObjectIcon);
		}
		else if(url.isFileURL)
		{
			if(!isDirectory)
				image = CustomIconForPath(url.path);

			if(!image)
			{
				[url getResourceValue:&image forKey:NSURLEffectiveIconKey error:nil];
				drawLinkBadge = NO; // Suppress custom link badge
			}
		}
		else
		{
			image = [NSWorkspace.sharedWorkspace iconForFileType:NSFileTypeForHFSTypeCode(isDirectory ? kGenericFolderIcon : kGenericDocumentIcon)];
		}

		if(!image)
			return NO;

		[image drawInRect:dstRect fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:isModified ? 0.4 : 1];

		if(NSImage* badge = BadgeForSCMStatus(scmStatus))
			[badge drawInRect:dstRect fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:isModified ? 0.4 : 1];

		if(drawLinkBadge)
			[IconBadgeForAlias() drawInRect:dstRect fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:isModified ? 0.4 : 1];

		return YES;
	}];
}
