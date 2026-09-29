#include <authorization/src/connection.h>

#define SOCKET_PATH "/tmp/auth_server.sock"

void test_connection ()
{
	int sockets[2];
	socketpair(AF_UNIX, SOCK_STREAM, 0, &sockets[0]);

	pid_t pid = fork();
	if(pid == 0) // child
	{
		connection_t conn(sockets[0]);

		std::string greeting;
		int version;
		conn >> greeting >> version;
		OAK_ASSERT_EQ(greeting, "welcome!");
		OAK_ASSERT_EQ(version, 42);
		conn << "I am bored" << "quit";
		_exit(0);
	}
	else if(pid != -1) // parent
	{
		connection_t conn(sockets[1]);
		conn << "welcome!" << 42;

		std::string msg, cmd;
		conn >> msg >> cmd;

		OAK_ASSERT_EQ(msg, "I am bored");
		OAK_ASSERT_EQ(cmd, "quit");

		int status;
		waitpid(pid, &status, 0);
	}
}
