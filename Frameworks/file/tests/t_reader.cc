#include <file/src/reader.h>

static void read (std::string const& path)
{
	std::string charset;
	std::string const str = file::read_utf8(path, &charset);
	fprintf(stderr, "%s (%s):\n%s\n", path.c_str(), charset.c_str(), str.c_str());
}

void x_test_reader ()
{
	read("/usr/include/sys/sbuf.h");
	read("/Users/duff/Downloads/GF Havneholmen.csv");
	read("/Users/duff/Downloads/TextEdit/English.lproj/InfoPlist.strings");
	read("/Users/duff/Desktop/Test-Mac.txt");
	read("/Users/duff/Desktop/Test-Latin.txt");
}
