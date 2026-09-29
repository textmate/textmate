#include <file/src/open.h>
#include <io/src/path.h>
#include <text/src/hexdump.h>
#include <test/jail.h>
#include <cf/src/run_loop.h>

struct stall_t : file::open_callback_t
{
	stall_t (std::string const& encoding = NULL_STR) : _encoding(encoding)
	{
	}

	void select_charset (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)
	{
		std::string encoding = _encoding;
		_encoding = NULL_STR;
		if(encoding != NULL_STR)
			context->set_charset(encoding);
	}

	void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter)
	{
		_error = true;

		_run_loop.stop();
	}

	void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters)
	{
		_encoding   = encoding.charset();
		_line_feeds = encoding.newlines();
		_content    = content;

		_run_loop.stop();
	}

	void wait ()
	{
		_run_loop.start();
	}

	bool _error = false;
	cf::run_loop_t _run_loop;
	std::string _encoding;
	std::string _line_feeds = NULL_STR;
	io::bytes_ptr _content;
};

void test_encoding ()
{
	test::jail_t jail;
	path::set_content(jail.path("test.txt"), std::string("\xAE\x62\x6C\x65\x67\x72\xBF\x64", 8));

	auto cb = std::make_shared<stall_t>("MACINTOSH");
	file::open(jail.path("test.txt"), osx::authorization_t(), cb);
	cb->wait();

	OAK_ASSERT_EQ(cb->_error, false);
	OAK_ASSERT_EQ(std::string(cb->_content->begin(), cb->_content->end()), "Æblegrød");
}

void test_encoding_failure ()
{
	test::jail_t jail;
	path::set_content(jail.path("test.txt"), std::string("\xAE\x62\x6C\x65\x67\x72\xBF\x64", 8));

	auto cb = std::make_shared<stall_t>();
	file::open(jail.path("test.txt"), osx::authorization_t(), cb);
	cb->wait();

	OAK_ASSERT_EQ(cb->_error, true);
}
