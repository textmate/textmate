#import <Foundation/Foundation.h>
#import <ns/ns.h>
#import <test/jail.h>
#import "../src/TerminalEnvironment.h"

void test_terminal_environment_reads_project_tm_properties ()
{
	test::jail_t jail;
	// Lowercase keys are settings, not environment variables, and are dropped.
	jail.set_content("project/.tm_properties", "TM_TERMINAL_TEST_VARIABLE = from-project\nterminalTestSetting = hidden\n");
	jail.mkdir("project/src");

	for(NSString* directory in @[ to_ns(jail.path("project")), to_ns(jail.path("project/src")) ])
	{
		NSDictionary<NSString*, NSString*>* environment = TerminalEnvironmentForDirectory(directory);
		OAK_ASSERT_EQ(to_s(environment[@"TM_TERMINAL_TEST_VARIABLE"]), "from-project");
		OAK_ASSERT(environment[@"terminalTestSetting"] == nil);
	}
}
