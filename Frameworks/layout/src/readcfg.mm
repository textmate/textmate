#import <Cocoa/Cocoa.h>

extern bool        g_need_kern_noascii;

void read_userdefaults()
{
	g_need_kern_noascii = [[NSUserDefaults standardUserDefaults] boolForKey:@"g_need_kern_noascii"];
}
