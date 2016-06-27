#include <file/open.h>
#include <io/path.h>
#include <text/hexdump.h>
#include <test/jail.h>

struct stall_t : file::open_callback_t
{
	stall_t (std::string const& encoding = NULL_STR) : _encoding(encoding)
	{
	   _semaphore = dispatch_semaphore_create(0);
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

	   dispatch_semaphore_signal(_semaphore);
	}

	void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters)
	{
		_encoding   = encoding.charset();
		_line_feeds = encoding.newlines();
		_content    = content;

	   dispatch_semaphore_signal(_semaphore);
	}

	void wait ()
	{
		OAK_ASSERT(dispatch_get_current_queue() != dispatch_get_main_queue());
		dispatch_semaphore_wait(_semaphore, DISPATCH_TIME_FOREVER);
	}

	bool _error = false;
	dispatch_semaphore_t _semaphore;
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
