#include <SoftwareUpdate/sw_update.h>
#include <OakSystem/application.h>
#include <io/path.h>
#include <test/web_server.h>
#include <crt_externs.h>

#define WEB_SERVER_PORT 64762

static std::string fixtures_path ()
{
	int* argc    = _NSGetArgc();
	char*** argv = _NSGetArgv();
	std::string path = argc && argv ? path::join((*argv)[0], "../fixtures") : NULL_STR;
	if(!path::is_directory(path))
	{
		fprintf(stderr, "*** unable to locate fixtures\nRe-run ./configure to setup the project.\n");
		abort();
	}
	return path;
}

static class WebServerFixture : public CxxTest::GlobalFixture
{
public:
	bool setUpWorld()
	{
		if(web::setup_server(WEB_SERVER_PORT))
		{
			std::thread([]{ web::run_server(fixtures_path()); }).detach();
			return true;
		}
		return false;
	}

} fixture;

using namespace sw_update;

class sw_update_tests : public CxxTest::TestSuite
{
	static std::string const& pubkey ()
	{
		static std::string const pubkey = path::content(path::join(fixtures_path(), "public.pem"));
		return pubkey;
	}

public:
	sw_update_tests ()
	{
		oak::application_t::set_name("TextMate");
	}

	key_chain_t key_chain ()
	{
		key_chain_t keyChain;
		keyChain.add(key_chain_t::key_t("org.textmate.duff", "Allan Odgaard", pubkey()));
		return keyChain;
	}

	void test_wrong_server ()
	{
		std::string url("http://non-existing.local./");
		std::string err = NULL_STR;
		std::string archive = sw_update::download_update(url, key_chain(), &err);
		TS_ASSERT_DIFFERS(err, NULL_STR);
		TS_ASSERT_EQUALS(archive, NULL_STR);
	}

	// void test_wrong_url ()
	// {
	// 	std::string url("http://localhost:" STRINGIFY(WEB_SERVER_PORT) "/forbidden.tbz");
	// 	error_t err = controller_t(url).wait_for_download();
	// 	TS_ASSERT_EQUALS(err, "error_t::server_response");
	// 	TS_ASSERT_EQUALS(archive, NULL_STR);
	// }

	void test_bad_bzip ()
	{
		std::string url("http://localhost:" STRINGIFY(WEB_SERVER_PORT) "/bad_bzip.tbz");
		std::string err = NULL_STR;
		std::string archive = sw_update::download_update(url, key_chain(), &err);
		TS_ASSERT_EQUALS(err, "Extracting archive.");
		TS_ASSERT_EQUALS(archive, NULL_STR);
	}

	void test_bad_tar ()
	{
		std::string url("http://localhost:" STRINGIFY(WEB_SERVER_PORT) "/bad_tar.tbz");
		std::string err = NULL_STR;
		std::string archive = sw_update::download_update(url, key_chain(), &err);
		TS_ASSERT_EQUALS(err, "Extracting archive.");
		TS_ASSERT_EQUALS(archive, NULL_STR);
	}

	void test_bad_signature ()
	{
		std::string url("http://localhost:" STRINGIFY(WEB_SERVER_PORT) "/bad_signature.tbz");
		std::string err = NULL_STR;
		std::string archive = sw_update::download_update(url, key_chain(), &err);
		TS_ASSERT_EQUALS(err, "Bad signature.");
		TS_ASSERT_EQUALS(archive, NULL_STR);
	}

	void test_wrong_signature ()
	{
		std::string url("http://localhost:" STRINGIFY(WEB_SERVER_PORT) "/wrong_signature.tbz");
		std::string err = NULL_STR;
		std::string archive = sw_update::download_update(url, key_chain(), &err);
		TS_ASSERT_EQUALS(err, "Bad signature.");
		TS_ASSERT_EQUALS(archive, NULL_STR);
	}

	// void test_bad_content ()
	// {
	// 	std::string url("http://localhost:" STRINGIFY(WEB_SERVER_PORT) "/bad_content.tbz");
	// 	std::string err = NULL_STR;
	// 	std::string archive = sw_update::download_update(url, key_chain(), &err);
	// 	TS_ASSERT_EQUALS(err, "error_t::wrong_archive");
	// 	TS_ASSERT_EQUALS(archive, NULL_STR);
	// }

	void test_good ()
	{
		std::string url("http://localhost:" STRINGIFY(WEB_SERVER_PORT) "/good.tbz");
		std::string err = NULL_STR;
		std::string archive = sw_update::download_update(url, key_chain(), &err);
		TS_ASSERT_EQUALS(err, NULL_STR);
		TS_ASSERT_DIFFERS(archive, NULL_STR);
		path::remove(archive);
	}

	// void test_successful_download ()
	// {
	// 	key_chain_t keyChain;
	// 	keyChain.add(key_chain_t::key_t("org.textmate.duff", "Allan Odgaard", "-----BEGIN PUBLIC KEY-----\nMIIBtjCCASsGByqGSM44BAEwggEeAoGBAPIE9PpXPK3y2eBDJ0dnR/D8xR1TiT9m\n8DnPXYqkxwlqmjSShmJEmxYycnbliv2JpojYF4ikBUPJPuerlZfOvUBC99ERAgz7\nN1HYHfzFIxVo1oTKWurFJ1OOOsfg8AQDBDHnKpS1VnwVoDuvO05gK8jjQs9E5LcH\ne/opThzSrI7/AhUAy02E9H7EOwRyRNLofdtPxpa10o0CgYBKDfcBscidAoH4pkHR\nIOEGTCYl3G2Pd1yrblCp0nCCUEBCnvmrWVSXUTVa2/AyOZUTN9uZSC/Kq9XYgqwj\nhgzqa8h/a8yD+ao4q8WovwGeb6Iso3WlPl8waz6EAPR/nlUTnJ4jzr9t6iSH9owS\nvAmWrgeboia0CI2AH++liCDvigOBhAACgYAFWO66xFvmF2tVIB+4E7CwhrSi2uIk\ndeBrpmNcZZ+AVFy1RXJelNe/cZ1aXBYskn/57xigklpkfHR6DGqpEbm6KC/47Jfy\ny5GEx+F/eBWEePi90XnLinytjmXRmS2FNqX6D15XNG1xJfjociA8bzC7s4gfeTUd\nlpQkBq2z71yitA==\n-----END PUBLIC KEY-----\n"));
	// 	std::string url("http://s3.textmate.org/Avian_r6978.tbz");
	// 	std::string err = NULL_STR;
	// 	std::string archive = sw_update::download_update(url, keyChain, &err);
	// 	TS_ASSERT_EQUALS(err, NULL_STR);
	// 	TS_ASSERT_DIFFERS(archive, NULL_STR);
	// 	path::remove(archive);
	// }
};
