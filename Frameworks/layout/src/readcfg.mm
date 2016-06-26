#import <Cocoa/Cocoa.h>

extern bool        g_need_kern_noascii;
extern unsigned    g_double_width_unicode;

void read_userdefaults()
{
	g_need_kern_noascii = [[NSUserDefaults standardUserDefaults] boolForKey:@"g_need_kern_noascii"];
	g_double_width_unicode = [[NSUserDefaults standardUserDefaults] integerForKey:@"g_double_width_unicode"];
	if(g_double_width_unicode < 256)
	{
		g_double_width_unicode = 256;
	}
}
