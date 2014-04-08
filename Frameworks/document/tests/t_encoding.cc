#include <document/document.h>
#include <test/jail.h>

struct callback_t : document::open_callback_t
{
	callback_t (std::string const& encoding) : _encoding(encoding)                                                                    { _semaphore = dispatch_semaphore_create(0); }
	void select_charset (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)                              { std::string encoding = _encoding; _encoding = NULL_STR; if(encoding != NULL_STR) context->set_charset(encoding); }
	void show_document (std::string const& path, document::document_ptr document)                                                     { _success = true; stop(); }
	void show_error (std::string const& path, document::document_ptr document, std::string const& message, oak::uuid_t const& filter) { stop(); }
	void wait ()                                                                                                                      { dispatch_semaphore_wait(_semaphore, DISPATCH_TIME_FOREVER); }
	void stop ()                                                                                                                      { dispatch_semaphore_signal(_semaphore); }

	dispatch_semaphore_t _semaphore;
	bool _success = false;
	std::string _encoding;
};

static bool open (document::document_ptr doc, std::string const& enc)
{
	auto cb = std::make_shared<callback_t>(enc);
	doc->try_open(cb);
	cb->wait();
	return !cb->_success;
}

static void compare_content (std::string const& path, std::string const& content)
{
	document::document_ptr doc = document::create(path);
	doc->sync_open();
	OAK_ASSERT_EQ(doc->is_open(), true);
	OAK_ASSERT_EQ(doc->content(), content);
	doc->set_content("world");
	doc->sync_save();
	doc->close();
}

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

	OAK_ASSERT_EQ(path::content(jail.path("utf32-be.txt")), std::string("\x00\x00\xFE\xFF\x00\x00\x00\x77\x00\x00\x00\x6F\x00\x00\x00\x72\x00\x00\x00\x6C\x00\x00\x00\x64", 24));
	OAK_ASSERT_EQ(path::content(jail.path("utf32-le.txt")), std::string("\xFF\xFE\x00\x00\x77\x00\x00\x00\x6F\x00\x00\x00\x72\x00\x00\x00\x6C\x00\x00\x00\x64\x00\x00\x00", 24));
	OAK_ASSERT_EQ(path::content(jail.path("utf16-be.txt")), std::string("\xFE\xFF\x00\x77\x00\x6F\x00\x72\x00\x6C\x00\x64", 12));
	OAK_ASSERT_EQ(path::content(jail.path("utf16-le.txt")), std::string("\xFF\xFE\x77\x00\x6F\x00\x72\x00\x6C\x00\x64\x00", 12));
	OAK_ASSERT_EQ(path::content(jail.path("utf8-bom.txt")), std::string("\xEF\xBB\xBF\x77\x6F\x72\x6C\x64", 8));
	OAK_ASSERT_EQ(path::content(jail.path("utf8.txt")),     std::string("\x77\x6F\x72\x6C\x64", 5));
}

void test_callback_mac_roman ()
{
	test::jail_t jail;

	document::document_ptr doc = document::create(jail.path("mac-roman.txt"));
	jail.set_content("mac-roman.txt", std::string("\xAE\x62\x6C\x65\x67\x72\xBF\x64", 8));
	open(doc, "MacRoman");
	OAK_ASSERT_EQ(doc->content(), "Æblegrød");
	doc->close();
}

void test_callback_mac_roman_from_buffer ()
{
	test::jail_t jail;

	document::document_ptr doc = document::from_content(std::string("\xAE\x62\x6C\x65\x67\x72\xBF\x64", 8));
	open(doc, "MacRoman");
	OAK_ASSERT_EQ(doc->content(), "Æblegrød");
	doc->close();
}

void test_callback_cp_1252 ()
{
	test::jail_t jail;

	document::document_ptr doc = document::create(jail.path("cp-1252.txt"));
	jail.set_content("cp-1252.txt", std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));
	open(doc, "CP1252");
	OAK_ASSERT_EQ(doc->content(), "Æblegrød");
	doc->close();
}

void test_callback_unknown ()
{
	test::jail_t jail;

	document::document_ptr doc = document::create(jail.path("cp-1252.txt"));
	jail.set_content("cp-1252.txt", std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));
	bool err = open(doc, NULL_STR);
	OAK_ASSERT_EQ(err, true);
}

void test_callback_wrong ()
{
	test::jail_t jail;

	document::document_ptr doc = document::create(jail.path("cp-1252.txt"));
	jail.set_content("cp-1252.txt", std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));
	bool err = open(doc, "UTF-8");
	OAK_ASSERT_EQ(err, true);
}

void test_callback_wrong_wrong ()
{
	test::jail_t jail;

	document::document_ptr doc = document::create(jail.path("cp-1252.txt"));
	jail.set_content("cp-1252.txt", std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));
	setxattr(jail.path("cp-1252.txt").c_str(), "com.apple.TextEncoding", "UTF-8", 5, 0, 0);
	bool err = open(doc, "UTF-8");
	OAK_ASSERT_EQ(err, true);
}

void test_callback_wrong_right ()
{
	test::jail_t jail;

	document::document_ptr doc = document::create(jail.path("cp-1252.txt"));
	jail.set_content("cp-1252.txt", std::string("\xC6\x62\x6C\x65\x67\x72\xF8\x64", 8));
	setxattr(jail.path("cp-1252.txt").c_str(), "com.apple.TextEncoding", "UTF-8", 5, 0, 0);
	bool err = open(doc, "CP1252");
	OAK_ASSERT_EQ(err, false);
	OAK_ASSERT_EQ(doc->content(), "Æblegrød");
	doc->close();
}
