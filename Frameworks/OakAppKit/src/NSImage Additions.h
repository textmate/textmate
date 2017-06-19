@interface NSImage (ImageFromBundle)

+ (NSImage *)imageNamed:(NSString *)aName isTemplate:(BOOL)templ;
+ (NSImage *)imageNamed:(NSString *)aName inSameBundleAsClass:(id)anObject;

@end
