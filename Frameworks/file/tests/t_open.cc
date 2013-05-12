#include <file/open.h>
#include <io/path.h>
#include <cf/cf.h>
#include <cf/run_loop.h>
#include <OakSystem/application.h>
#include <text/hexdump.h>
#include <test/jail.h>

class OpenTests : public CxxTest::TestSuite
{
	struct stall_t : file::open_callback_t
	{
		stall_t (std::string const& encoding = NULL_STR, std::string const& fileType = NULL_STR) : _error(false), _run_loop(CFSTR("OakThreadSignalsRunLoopMode")), _bom(false), _encoding(encoding), _file_type(fileType), _line_feeds(NULL_STR) { }

		void select_charset (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)
		{
			std::string encoding = _encoding;
			_encoding = NULL_STR;
			if(encoding != NULL_STR)
				context->set_charset(encoding);
		}

		void select_file_type (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)
		{
			if(_file_type != NULL_STR)
				context->set_file_type(_file_type);
		}

		void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter)
		{
			_error = true;

			_run_loop.stop();
		}

		void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters)
		{
			_bom        = encoding.byte_order_mark();
			_encoding   = encoding.charset();
			_file_type  = fileType;
			_line_feeds = encoding.newlines();
			_content    = content;

			_run_loop.stop();
		}

		void wait () { _run_loop.start(); }

		bool _error;
		cf::run_loop_t _run_loop;
		bool _bom;
		std::string _encoding;
		std::string _file_type;
		std::string _line_feeds;
		io::bytes_ptr _content;
	};

public:
	void test_file_type_from_path ()
	{
		test::jail_t jail;

		stall_t* cb = new stall_t;
		file::open_callback_ptr sharedPtr((file::open_callback_t*)cb);
		file::open(jail.path("test.c"), osx::authorization_t(), sharedPtr);
		cb->wait();

		TS_ASSERT_EQUALS(cb->_error, false);
		TS_ASSERT_EQUALS(cb->_file_type, "source.c");
	}

	void test_file_type_from_disk_content ()
	{
		test::jail_t jail;
		path::set_content(jail.path("test"), "/* -*- C -*- */");

		stall_t* cb = new stall_t;
		file::open_callback_ptr sharedPtr((file::open_callback_t*)cb);
		file::open(jail.path("test"), osx::authorization_t(), sharedPtr);
		cb->wait();

		TS_ASSERT_EQUALS(cb->_error, false);
		TS_ASSERT_EQUALS(cb->_file_type, "source.c");
	}

	void test_file_type_from_memory_content ()
	{
		test::jail_t jail;

		stall_t* cb = new stall_t;
		file::open_callback_ptr sharedPtr((file::open_callback_t*)cb);
		file::open(jail.path("test"), osx::authorization_t(), sharedPtr, io::bytes_ptr(new io::bytes_t("/* -*- C -*- */")));
		cb->wait();

		TS_ASSERT_EQUALS(cb->_error, false);
		TS_ASSERT_EQUALS(cb->_file_type, "source.c");
	}

	void test_encoding ()
	{
		test::jail_t jail;
		path::set_content(jail.path("test.txt"), std::string("\xAE\x62\x6C\x65\x67\x72\xBF\x64", 8));

		stall_t* cb = new stall_t("MACINTOSH");
		file::open_callback_ptr sharedPtr((file::open_callback_t*)cb);
		file::open(jail.path("test.txt"), osx::authorization_t(), sharedPtr);
		cb->wait();

		TS_ASSERT_EQUALS(cb->_error, false);
		TS_ASSERT_EQUALS(cb->_file_type, "text.plain");
		TS_ASSERT_EQUALS(std::string(cb->_content->begin(), cb->_content->end()), "Æblegrød");
	}

	void test_encoding_failure ()
	{
		test::jail_t jail;
		path::set_content(jail.path("test.txt"), std::string("\xAE\x62\x6C\x65\x67\x72\xBF\x64", 8));

		stall_t* cb = new stall_t;
		file::open_callback_ptr sharedPtr((file::open_callback_t*)cb);
		file::open(jail.path("test.txt"), osx::authorization_t(), sharedPtr);
		cb->wait();

		TS_ASSERT_EQUALS(cb->_error, true);
	}

	void test_file_type ()
	{
		test::jail_t jail;
		path::set_content(jail.path("test.x-unknown"), "dummy");

		stall_t* cb = new stall_t(NULL_STR, "x.test");
		file::open_callback_ptr sharedPtr((file::open_callback_t*)cb);
		file::open(jail.path("test.x-unknown"), osx::authorization_t(), sharedPtr);
		cb->wait();

		TS_ASSERT_EQUALS(cb->_error, false);
		TS_ASSERT_EQUALS(cb->_file_type, "x.test");
		TS_ASSERT_EQUALS(std::string(cb->_content->begin(), cb->_content->end()), "dummy");
	}

	void test_file_type_no_ext ()
	{
		test::jail_t jail;
		path::set_content(jail.path("test"), "dummy");

		stall_t* cb = new stall_t(NULL_STR, "x.test");
		file::open_callback_ptr sharedPtr((file::open_callback_t*)cb);
		file::open(jail.path("test"), osx::authorization_t(), sharedPtr);
		cb->wait();

		TS_ASSERT_EQUALS(cb->_error, false);
		TS_ASSERT_EQUALS(cb->_file_type, "text.plain");
		TS_ASSERT_EQUALS(std::string(cb->_content->begin(), cb->_content->end()), "dummy");
	}

	void test_file_type_failure ()
	{
		test::jail_t jail;
		path::set_content(jail.path("test.x-unknown"), "dummy");

		stall_t* cb = new stall_t;
		file::open_callback_ptr sharedPtr((file::open_callback_t*)cb);
		file::open(jail.path("test.x-unknown"), osx::authorization_t(), sharedPtr);
		cb->wait();

		TS_ASSERT_EQUALS(cb->_error, true);
	}
};
