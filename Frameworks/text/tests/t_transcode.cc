#include <text/transcode.h>
#include <text/hexdump.h>

static std::string convert_batch (std::string const& fromCharset, std::string const& toCharset, std::string const& src)
{
	if(auto transcode = text::transcode_t(fromCharset, toCharset))
	{
		std::string dst;
		transcode(transcode(src.data(), src.data() + src.size(), back_inserter(dst)));
		return dst;
	}
	return NULL_STR;
}

static std::string convert (std::string const& fromCharset, std::string const& toCharset, std::string const& src)
{
	if(auto transcode = text::transcode_t(fromCharset, toCharset))
	{
		std::string dst;
		for(size_t i = 0; i < src.size(); ++i)
			transcode(src.data() + i, src.data() + i + 1, back_inserter(dst));
		transcode(back_inserter(dst));
		return dst;
	}
	return NULL_STR;
}

void test_transcode ()
{
	std::string macRoman("\xAE""blegr\xBF""d", 8);

	OAK_ASSERT_EQ(convert_batch("MACINTOSH",     "UTF-8",           macRoman),        "Æblegrød"         );
	OAK_ASSERT_EQ(convert_batch("MACINTOSH",     "UTF-8//BOM",      macRoman),        "\uFEFFÆblegrød"   );
	OAK_ASSERT_EQ(convert_batch("MACINTOSH",     "ASCII",           macRoman),        "\\xAEblegr\\xBFd" );
	OAK_ASSERT_EQ(convert_batch("MACINTOSH",     "ASCII//IGNORE",   macRoman),        "blegrd"           );
	OAK_ASSERT_EQ(convert_batch("UTF-8",         "MACINTOSH",      "Æblegrød"),       macRoman           );
	OAK_ASSERT_EQ(convert_batch("UTF-8//BOM",    "MACINTOSH",      "\uFEFFÆblegrød"), macRoman           );
	OAK_ASSERT_EQ(convert_batch("UTF-8",         "MACINTOSH",      "\uFEFFÆblegrød"), "\\xEF\\xBB\\xBF" + macRoman);
	OAK_ASSERT_EQ(convert_batch("UTF-8",         "ISO-8859-1",     "Æble\xA0grød"),   "\xC6""ble\\xA0gr\xF8""d");
	OAK_ASSERT_EQ(convert_batch("UTF-8",         "WINDOWS-1252",   "\xE2\x82\xAC"),   "\x80");
	OAK_ASSERT_EQ(convert_batch("UTF-8",         "WINDOWS-1252",   "\xE2\x82"),       "\\xE2\\x82");
	OAK_ASSERT_EQ(convert_batch("UTF-8",         "WINDOWS-1252",   "\xE2"),           "\\xE2");

	OAK_ASSERT_EQ(convert("MACINTOSH",     "UTF-8",           macRoman),        "Æblegrød"         );
	OAK_ASSERT_EQ(convert("MACINTOSH",     "UTF-8//BOM",      macRoman),        "\uFEFFÆblegrød"   );
	OAK_ASSERT_EQ(convert("MACINTOSH",     "ASCII",           macRoman),        "\\xAEblegr\\xBFd" );
	OAK_ASSERT_EQ(convert("MACINTOSH",     "ASCII//IGNORE",   macRoman),        "blegrd"           );
	OAK_ASSERT_EQ(convert("UTF-8",         "MACINTOSH",      "Æblegrød"),       macRoman           );
	OAK_ASSERT_EQ(convert("UTF-8//BOM",    "MACINTOSH",      "\uFEFFÆblegrød"), macRoman           );
	OAK_ASSERT_EQ(convert("UTF-8",         "MACINTOSH",      "\uFEFFÆblegrød"), "\\xEF\\xBB\\xBF" + macRoman);
	OAK_ASSERT_EQ(convert("UTF-8",         "ISO-8859-1",     "Æble\xA0grød"),   "\xC6""ble\\xA0gr\xF8""d");
	OAK_ASSERT_EQ(convert("UTF-8",         "WINDOWS-1252",   "\xE2\x82\xAC"),   "\x80");
	OAK_ASSERT_EQ(convert("UTF-8",         "WINDOWS-1252",   "\xE2\x82"),       "\\xE2\\x82");
	OAK_ASSERT_EQ(convert("UTF-8",         "WINDOWS-1252",   "\xE2"),           "\\xE2");

	OAK_ASSERT_EQ(convert("UTF-32BE//BOM", "UTF-8", std::string("\x00\x00\xFE\xFF\x00\x00\x00Q", 8)), "Q");
	OAK_ASSERT_EQ(convert("UTF-32LE//BOM", "UTF-8", std::string("\xFF\xFE\x00\x00Q\x00\x00\x00", 8)), "Q");
	OAK_ASSERT_EQ(convert("UTF-16BE//BOM", "UTF-8", std::string("\xFE\xFF\x00Q", 4)), "Q");
	OAK_ASSERT_EQ(convert("UTF-16LE//BOM", "UTF-8", std::string("\xFF\xFEQ\x00", 4)), "Q");

	OAK_ASSERT_EQ(convert_batch("UTF-32BE//BOM", "UTF-8", std::string("\x00\x00\xFE\xFF\x00\x00\x00Q", 8)), "Q");
	OAK_ASSERT_EQ(convert_batch("UTF-32LE//BOM", "UTF-8", std::string("\xFF\xFE\x00\x00Q\x00\x00\x00", 8)), "Q");
	OAK_ASSERT_EQ(convert_batch("UTF-16BE//BOM", "UTF-8", std::string("\xFE\xFF\x00Q", 4)), "Q");
	OAK_ASSERT_EQ(convert_batch("UTF-16LE//BOM", "UTF-8", std::string("\xFF\xFEQ\x00", 4)), "Q");

	OAK_ASSERT_EQ(convert_batch("UTF-8", "UTF-32BE//BOM", "Q"), std::string("\x00\x00\xFE\xFF\x00\x00\x00Q", 8));
	OAK_ASSERT_EQ(convert_batch("UTF-8", "UTF-32LE//BOM", "Q"), std::string("\xFF\xFE\x00\x00Q\x00\x00\x00", 8));
	OAK_ASSERT_EQ(convert_batch("UTF-8", "UTF-16BE//BOM", "Q"), std::string("\xFE\xFF\x00Q", 4));
	OAK_ASSERT_EQ(convert_batch("UTF-8", "UTF-16LE//BOM", "Q"), std::string("\xFF\xFEQ\x00", 4));

	OAK_ASSERT_EQ(convert("UTF-8", "UTF-32BE//BOM", "Q"), std::string("\x00\x00\xFE\xFF\x00\x00\x00Q", 8));
	OAK_ASSERT_EQ(convert("UTF-8", "UTF-32LE//BOM", "Q"), std::string("\xFF\xFE\x00\x00Q\x00\x00\x00", 8));
	OAK_ASSERT_EQ(convert("UTF-8", "UTF-16BE//BOM", "Q"), std::string("\xFE\xFF\x00Q", 4));
	OAK_ASSERT_EQ(convert("UTF-8", "UTF-16LE//BOM", "Q"), std::string("\xFF\xFEQ\x00", 4));

	OAK_ASSERT_EQ(convert("UTF-8", "UTF-32BE", "Q"), std::string("\x00\x00\x00Q", 4));
	OAK_ASSERT_EQ(convert("UTF-8", "UTF-32LE", "Q"), std::string("Q\x00\x00\x00", 4));
	OAK_ASSERT_EQ(convert("UTF-8", "UTF-16BE", "Q"), std::string("\x00Q", 2));
	OAK_ASSERT_EQ(convert("UTF-8", "UTF-16LE", "Q"), std::string("Q\x00", 2));

	if(auto transcode = text::transcode_t("MACINTOSH", "ASCII"))
	{
		std::string dst;
		transcode(transcode(macRoman.data(), macRoman.data() + macRoman.size(), back_inserter(dst)));
		OAK_ASSERT_EQ(dst, "\\xAEblegr\\xBFd");
		OAK_ASSERT_EQ(transcode.invalid_count(), 2);
	}

	if(auto transcode = text::transcode_t("MACINTOSH", "ASCII//IGNORE"))
	{
		std::string dst;
		transcode(transcode(macRoman.data(), macRoman.data() + macRoman.size(), back_inserter(dst)));
		OAK_ASSERT_EQ(dst, "blegrd");
		OAK_ASSERT_EQ(transcode.invalid_count(), 0);
	}

	if(auto transcode = text::transcode_t("UTF-8", "MACINTOSH"))
	{
		std::string src = "\uFEFFÆblegrød";

		std::string dst;
		transcode(transcode(src.data(), src.data() + src.size(), back_inserter(dst)));
		OAK_ASSERT_EQ(dst, "\\xEF\\xBB\\xBF" + macRoman);
		OAK_ASSERT_EQ(transcode.invalid_count(), 3);
	}
}
