#include <document/document.h>
#include <cf/run_loop.h>
#include <test/jail.h>

class document_tests : public CxxTest::TestSuite
{
	struct callback_t : document::open_callback_t
	{
		callback_t (std::string const& encoding) : _run_loop(CFSTR("OakThreadSignalsRunLoopMode")), _success(false), _encoding(encoding) { }

		void select_encoding (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)                             { std::string encoding = _encoding; _encoding = NULL_STR; if(encoding != NULL_STR) context->set_encoding(encoding); }
		void show_document (std::string const& path, document::document_ptr document)                                                     { _success = true; _run_loop.stop(); }
		void show_error (std::string const& path, document::document_ptr document, std::string const& message, oak::uuid_t const& filter) { _run_loop.stop(); }
		void wait ()                                                                                                                      { _run_loop.start(); }

		cf::run_loop_t _run_loop;
		bool _success;
		std::string _encoding;
	};

	static bool open (document::document_ptr doc, std::string const& enc)
	{
		document::open_callback_ptr cb(new callback_t(enc));
		doc->try_open(cb);
		std::tr1::static_pointer_cast<callback_t>(cb)->wait();
		return !std::tr1::static_pointer_cast<callback_t>(cb)->_success;
	}

	void compare_content (std::string const& path, std::string const& content) const
	{
		document::document_ptr doc = document::create(path);
		doc->open();
		TS_ASSERT_EQUALS(doc->is_open(), true);
		TS_ASSERT_EQUALS(doc->buffer().substr(0, doc->buffer().size()), content);
		doc->buffer().replace(0, content.size(), "world");
		doc->save();
		doc->close();
	}

public:
	void test_encoding ()
	{
		test::jail_t jail;

		jail.set_content("utf32-be.txt",  std::string("\x00\x00\xFE\xFF\x00\x00\x00\x48\x00\x00\x00\x65\x00\x00\x00\x6C\x00\x00\x00\x6C\x00\x00\x00\x6F", 24));
		jail.set_content("utf32-le.txt",  std::string("\xFF\xFE\x00\x00\x48\x00\x00\x00\x65\x00\x00\x00\x6C\x00\x00\x00\x6C\x00\x00\x00\x6F\x00\x00\x00", 24));
		jail.set_content("utf16-be.txt",  std::string("\xFE\xFF\x00\x48\x00\x65\x00\x6C\x00\x6C\x00\x6F", 12));
		jail.set_content("utf16-le.txt",  std::string("\xFF\xFE\x48\x00\x65\x00\x6C\x00\x6C\x00\x6F\x00", 12));
		jail.set_content("utf8-bom.txt",  std::string("\xEF\xBB\xBFHello", 8));
		jail.set_content("utf8.txt",      std::string("Æblegrød"));
		jail.set_content("mac-roman.txt", std::string("\xAE\x62\x6C\x65\x67\x72\xBF\x64", 8));
		jail.set_content("cp-1252.txt",   std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));

		setxattr(jail.path("mac-roman.txt").c_str(), "com.apple.TextEncoding", "MACINTOSH;0",       11, 0, 0);
		setxattr(jail.path("cp-1252.txt").c_str(),   "com.apple.TextEncoding", "WINDOWS-1252;1280", 17, 0, 0);

		compare_content(jail.path("utf32-be.txt"),  "Hello");
		compare_content(jail.path("utf32-le.txt"),  "Hello");
		compare_content(jail.path("utf16-be.txt"),  "Hello");
		compare_content(jail.path("utf16-le.txt"),  "Hello");
		compare_content(jail.path("utf8-bom.txt"),  "Hello");
		compare_content(jail.path("utf8.txt"),      "Æblegrød");
		compare_content(jail.path("mac-roman.txt"), "Æblegrød");
		compare_content(jail.path("cp-1252.txt"),   "Æblegrød");

		TS_ASSERT_EQUALS(path::content(jail.path("utf32-be.txt")), std::string("\x00\x00\xFE\xFF\x00\x00\x00\x77\x00\x00\x00\x6F\x00\x00\x00\x72\x00\x00\x00\x6C\x00\x00\x00\x64", 24));
		TS_ASSERT_EQUALS(path::content(jail.path("utf32-le.txt")), std::string("\xFF\xFE\x00\x00\x77\x00\x00\x00\x6F\x00\x00\x00\x72\x00\x00\x00\x6C\x00\x00\x00\x64\x00\x00\x00", 24));
		TS_ASSERT_EQUALS(path::content(jail.path("utf16-be.txt")), std::string("\xFE\xFF\x00\x77\x00\x6F\x00\x72\x00\x6C\x00\x64", 12));
		TS_ASSERT_EQUALS(path::content(jail.path("utf16-le.txt")), std::string("\xFF\xFE\x77\x00\x6F\x00\x72\x00\x6C\x00\x64\x00", 12));
		TS_ASSERT_EQUALS(path::content(jail.path("utf8-bom.txt")), std::string("\xEF\xBB\xBF\x77\x6F\x72\x6C\x64", 8));
		TS_ASSERT_EQUALS(path::content(jail.path("utf8.txt")),     std::string("\x77\x6F\x72\x6C\x64", 5));
	}

	void test_callback_mac_roman ()
	{
		test::jail_t jail;

		document::document_ptr doc = document::create(jail.path("mac-roman.txt"));
		jail.set_content("mac-roman.txt", std::string("\xAE\x62\x6C\x65\x67\x72\xBF\x64", 8));
		open(doc, "MacRoman");
		TS_ASSERT_EQUALS(doc->buffer().substr(0, doc->buffer().size()), "Æblegrød");
		doc->close();
	}

	void test_callback_mac_roman_from_buffer ()
	{
		test::jail_t jail;

		document::document_ptr doc = document::from_content(std::string("\xAE\x62\x6C\x65\x67\x72\xBF\x64", 8));
		open(doc, "MacRoman");
		TS_ASSERT_EQUALS(doc->buffer().substr(0, doc->buffer().size()), "Æblegrød");
		doc->close();
	}

	void test_callback_cp_1252 ()
	{
		test::jail_t jail;

		document::document_ptr doc = document::create(jail.path("cp-1252.txt"));
		jail.set_content("cp-1252.txt", std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));
		open(doc, "CP1252");
		TS_ASSERT_EQUALS(doc->buffer().substr(0, doc->buffer().size()), "Æblegrød");
		doc->close();
	}

	void test_callback_unknown ()
	{
		test::jail_t jail;

		document::document_ptr doc = document::create(jail.path("cp-1252.txt"));
		jail.set_content("cp-1252.txt", std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));
		bool err = open(doc, NULL_STR);
		TS_ASSERT_EQUALS(err, true);
	}

	void test_callback_wrong ()
	{
		test::jail_t jail;

		document::document_ptr doc = document::create(jail.path("cp-1252.txt"));
		jail.set_content("cp-1252.txt", std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));
		bool err = open(doc, "UTF-8");
		TS_ASSERT_EQUALS(err, true);
	}

	void test_callback_wrong_wrong ()
	{
		test::jail_t jail;

		document::document_ptr doc = document::create(jail.path("cp-1252.txt"));
		jail.set_content("cp-1252.txt", std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));
		setxattr(jail.path("cp-1252.txt").c_str(), "com.apple.TextEncoding", "UTF-8", 5, 0, 0);
		bool err = open(doc, "UTF-8");
		TS_ASSERT_EQUALS(err, true);
	}

	void test_callback_wrong_right ()
	{
		test::jail_t jail;

		document::document_ptr doc = document::create(jail.path("cp-1252.txt"));
		jail.set_content("cp-1252.txt", std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));
		setxattr(jail.path("cp-1252.txt").c_str(), "com.apple.TextEncoding", "UTF-8", 5, 0, 0);
		bool err = open(doc, "CP1252");
		TS_ASSERT_EQUALS(err, false);
		TS_ASSERT_EQUALS(doc->buffer().substr(0, doc->buffer().size()), "Æblegrød");
		doc->close();
	}
};
