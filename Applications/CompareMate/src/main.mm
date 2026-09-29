#import <OakSystem/src/application.h>
#import <io/src/path.h>

int main (int argc, char const* argv[])
{
	oak::application_t::set_support(path::join(path::home(), "Library/Application Support/CompareMate"));
	oak::application_t app(argc, argv);
	return NSApplicationMain(argc, argv);
}
