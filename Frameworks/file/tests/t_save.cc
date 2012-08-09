#include <file/save.h>
#include <io/path.h>
#include <cf/cf.h>
#include <cf/run_loop.h>
#include <OakSystem/application.h>
#include <text/hexdump.h>
#include <test/jail.h>

class SaveTests : public CxxTest::TestSuite
{
	struct stall_t : file::save_callback_t
	{
		stall_t (bool* success = NULL, std::string const& path = NULL_STR, std::string const& encoding = NULL_STR, bool bom = false) : _success(success), _path(path), _encoding(encoding), _bom(bom), _run_loop(CFSTR("OakThreadSignalsRunLoopMode")) { }

		void select_path (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)
		{
			if(_path != NULL_STR)
				context->set_path(_path);
		}

		void select_encoding (std::string const& path, io::bytes_ptr content, std::string const& encoding, file::save_context_ptr context)
		{
			if(_encoding != NULL_STR)
				context->set_encoding(_encoding, _bom);
		}

		void did_save (std::string const& path, io::bytes_ptr content, std::string const& pathAttributes, std::string const& encoding, bool bom, std::string const& lineFeeds, bool success, std::string const& message, oak::uuid_t const& filter)
		{
			if(_success)
				*_success = success;
			_run_loop.stop();
		}

		void wait () { _run_loop.start(); }

	private:
		bool* _success;
		std::string _path;
		std::string _encoding;
		bool _bom;
		cf::run_loop_t _run_loop;
	};

	static std::string sha1 (std::string const& src)
	{
		char md[SHA_DIGEST_LENGTH];
		CC_SHA1((unsigned char*)src.data(), src.size(), (unsigned char*)md);
		return std::string(md, md + sizeof(md));
	}
public:
	SaveTests ()
	{
		signal(SIGPIPE, SIG_IGN);
	}

	void test_save ()
	{
		test::jail_t jail;

		bool success        = false;
		std::string path    = jail.path("test.cc");
		std::string content = "// a comment\n";

		stall_t* cb = new stall_t(&success);
		file::save_callback_ptr sharedPtr((file::save_callback_t*)cb);
		file::save(path, sharedPtr, osx::authorization_t(), io::bytes_ptr(new io::bytes_t(content)), std::map<std::string, std::string>(), NULL_STR /* file type */, "UTF-8", false /* byte order mark */, "\n", std::vector<oak::uuid_t>() /* binary import filters */, std::vector<oak::uuid_t>() /* text import filters */);
		cb->wait();

		TS_ASSERT_EQUALS(success, true);
		TS_ASSERT_EQUALS(path::content(path), content);
	}

	void test_save_untitled ()
	{
		test::jail_t jail;

		bool success        = false;
		std::string path    = jail.path("test.cc");
		std::string content = "// a comment\n";

		stall_t* cb = new stall_t(&success, path);
		file::save_callback_ptr sharedPtr((file::save_callback_t*)cb);
		file::save(NULL_STR, sharedPtr, osx::authorization_t(), io::bytes_ptr(new io::bytes_t(content)), std::map<std::string, std::string>(), NULL_STR /* file type */, "UTF-8", false /* byte order mark */, "\n", std::vector<oak::uuid_t>() /* binary import filters */, std::vector<oak::uuid_t>() /* text import filters */);
		cb->wait();

		TS_ASSERT_EQUALS(success, true);
		TS_ASSERT_EQUALS(path::content(path), content);
	}

	void test_save_untitled_failure ()
	{
		test::jail_t jail;

		bool success        = false;
		std::string content = "// a comment\n";

		stall_t* cb = new stall_t(&success);
		file::save_callback_ptr sharedPtr((file::save_callback_t*)cb);
		file::save(NULL_STR, sharedPtr, osx::authorization_t(), io::bytes_ptr(new io::bytes_t(content)), std::map<std::string, std::string>(), NULL_STR /* file type */, "UTF-8", false /* byte order mark */, "\n", std::vector<oak::uuid_t>() /* binary import filters */, std::vector<oak::uuid_t>() /* text import filters */);
		cb->wait();

		TS_ASSERT_EQUALS(success, false);
	}

	void test_save_translit ()
	{
		test::jail_t jail;

		bool success        = false;
		std::string path    = jail.path("test.txt");
		std::string content = "Æblegrød…\n";

		stall_t* cb = new stall_t(&success, path, "ASCII//TRANSLIT");
		file::save_callback_ptr sharedPtr((file::save_callback_t*)cb);
		file::save(path, sharedPtr, osx::authorization_t(), io::bytes_ptr(new io::bytes_t(content)), std::map<std::string, std::string>(), NULL_STR /* file type */, "ASCII", false /* byte order mark */, "\n", std::vector<oak::uuid_t>() /* binary import filters */, std::vector<oak::uuid_t>() /* text import filters */);
		cb->wait();

		TS_ASSERT_EQUALS(success, true);
		TS_ASSERT_EQUALS(path::content(path), "AEblegrod...\n");
	}

	void test_save_encoding_failure ()
	{
		test::jail_t jail;

		bool success        = false;
		std::string path    = jail.path("test.txt");
		std::string content = "Æblegrød…\n";

		stall_t* cb = new stall_t(&success, path);
		file::save_callback_ptr sharedPtr((file::save_callback_t*)cb);
		file::save(path, sharedPtr, osx::authorization_t(), io::bytes_ptr(new io::bytes_t(content)), std::map<std::string, std::string>(), NULL_STR /* file type */, "ASCII", false /* byte order mark */, "\n", std::vector<oak::uuid_t>() /* binary import filters */, std::vector<oak::uuid_t>() /* text import filters */);
		cb->wait();

		TS_ASSERT_EQUALS(success, false);
	}

	void test_export_filter ()
	{
		test::jail_t jail;
		std::string content = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";

		bool didSave = false;
		stall_t* cb = new stall_t(&didSave);
		file::save_callback_ptr sharedPtr((file::save_callback_t*)cb);
		file::save(jail.path("file.sha1"), sharedPtr, osx::authorization_t(), io::bytes_ptr(new io::bytes_t(content)), std::map<std::string, std::string>(), NULL_STR /* file type */, "UTF-8", false /* byte order mark */, "\n", std::vector<oak::uuid_t>() /* binary import filters */, std::vector<oak::uuid_t>() /* text import filters */);
		cb->wait();

		TS_ASSERT(didSave);
		TS_ASSERT_EQUALS(path::content(jail.path("file.sha1")), sha1(content));
	}
};
