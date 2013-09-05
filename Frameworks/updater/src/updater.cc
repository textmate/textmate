#include "updater.h"
#include "download.h"
#include <io/path.h>
#include <io/move_path.h>
#include <io/swap_file_data.h>
#include <io/entries.h>
#include <network/download_tbz.h>
#include <plist/ascii.h>
#include <text/format.h>
#include <text/ctype.h>
#include <text/decode.h>

static char const* kBundleAttributeOrigin  = "org.textmate.bundle.origin";
static char const* kBundleAttributeUpdated = "org.textmate.bundle.updated";

namespace plist
{
	static plist::array_t const& as_array (plist::any_t const& any)
	{
		if(plist::array_t const* array = boost::get<plist::array_t>(&any))
			return *array;
		static plist::array_t const* dummy = new plist::array_t;
		return *dummy;
	}

} /* plist */

namespace bundles_db
{
	// ===========
	// = Sources =
	// ===========

	static std::string sources_base_path (std::string const& installDir)            { return installDir == NULL_STR ? path::join(path::home(), "Library/Application Support/TextMate/Managed") : installDir; }
	static std::string sources_index_path (std::string const& installDir)           { return path::join(sources_base_path(installDir), "Sources.plist"); }
	static std::string sources_path (std::string const& installDir)                 { return path::join(sources_base_path(installDir), "Cache"); }

	static std::string local_bundle_path (std::string const& installDir = NULL_STR) { return installDir == NULL_STR ? path::join(path::home(), "Library/Application Support/TextMate/Managed") : installDir; }
	static std::string local_index_path (std::string const& installDir = NULL_STR)  { return path::join(local_bundle_path(installDir), "LocalIndex.plist"); }

	std::string source_t::path () const                                             { return _path; }
	bool source_t::needs_update (double pollInterval) const                         { return oak::date_t::now() - last_check() > pollInterval; }
	oak::date_t source_t::last_check () const                                       { return path::get_attr(path(), "last-check"); }

	key_chain_t source_t::key_chain () const
	{
		key_chain_t res;

		plist::array_t keys;
		if(plist::get_key_path(plist::load(_path), "keys", keys))
		{
			iterate(key, keys)
			{
				std::string identity, name, publicKey;
				if(plist::get_key_path(*key, "identity", identity) && plist::get_key_path(*key, "name", name) && plist::get_key_path(*key, "publicKey", publicKey))
						res.add(key_chain_t::key_t(identity, name, publicKey));
				else	fprintf(stderr, "bad public key entry:\n%s\n", to_s(*key).c_str());
			}
		}
		else
		{
			std::string const key_chain_path = path::join(path::home(), "Library/Application Support/TextMate/Managed/KeyChain.plist");
			if(path::exists(key_chain_path))
			{
				res.load(key_chain_path);
			}
			else
			{
				res.add(key_chain_t::key_t("org.textmate.duff",    "Allan Odgaard",  "-----BEGIN PUBLIC KEY-----\nMIIBtjCCASsGByqGSM44BAEwggEeAoGBAPIE9PpXPK3y2eBDJ0dnR/D8xR1TiT9m\n8DnPXYqkxwlqmjSShmJEmxYycnbliv2JpojYF4ikBUPJPuerlZfOvUBC99ERAgz7\nN1HYHfzFIxVo1oTKWurFJ1OOOsfg8AQDBDHnKpS1VnwVoDuvO05gK8jjQs9E5LcH\ne/opThzSrI7/AhUAy02E9H7EOwRyRNLofdtPxpa10o0CgYBKDfcBscidAoH4pkHR\nIOEGTCYl3G2Pd1yrblCp0nCCUEBCnvmrWVSXUTVa2/AyOZUTN9uZSC/Kq9XYgqwj\nhgzqa8h/a8yD+ao4q8WovwGeb6Iso3WlPl8waz6EAPR/nlUTnJ4jzr9t6iSH9owS\nvAmWrgeboia0CI2AH++liCDvigOBhAACgYAFWO66xFvmF2tVIB+4E7CwhrSi2uIk\ndeBrpmNcZZ+AVFy1RXJelNe/cZ1aXBYskn/57xigklpkfHR6DGqpEbm6KC/47Jfy\ny5GEx+F/eBWEePi90XnLinytjmXRmS2FNqX6D15XNG1xJfjociA8bzC7s4gfeTUd\nlpQkBq2z71yitA==\n-----END PUBLIC KEY-----\n"));
				res.add(key_chain_t::key_t("org.textmate.msheets", "Michael Sheets", "-----BEGIN PUBLIC KEY-----\nMIIDOzCCAi4GByqGSM44BAEwggIhAoIBAQDfYsqBc18uL7yYb/bDrrEtVTBG8tML\nmMtNFyU8XhlVKWdQJwBGG/fV2Wjc0hVYSeTWv3VueITZbuuVZEePXlem6Dki1DEL\nsMNeDvE/l0MKHXi1+sr1cht7QvuTi/c1UK4I6QNWDJWi7KmqJg3quLCwJfMef1x5\n/qgLUln5cU6+pAj43Vp62bzHJBjAnrC432yD7F4Mxu4oV/PEm5QC6pU7RcvUwAox\np7m7c8+CxX7Aq4dH6Jd8Jt6XuYIktlfcFivvvF60CvxhABDBdGMra4roO0wlJmID\n91oQ3PLxFBsDmbluPJlkmTp4YetsF8/Zd9P3WwBQUArtNdiqKZIQ4uHXAhUAvNZ5\ntZkzuUiblIxZKmOCBN/JeMsCggEBAK9jUiC98+hwY5XcDQjDSLPE4uvv+dHZ29Bx\n8KevX+qzd6shIhp6urvyBXrM+h8l7iB6Jh4Wm3WhqKMBjquRqyGogQDGxJr7QBVk\nQSOiyaKDT4Ue/Nhg1MFsrt3PtS1/nscZ6GGWswrCfQ1t4m/wXDasUSfz2smae+Jd\nZ6UGBzWQMRawyU/O/LX0PlJkBOMHopecAUcxHc2G02P2QwAMKPavwksQ4tWCJvIr\n7ZELfCcVQtG2UnpTRWqLZQaVwSYMHoNK9/reu099sdv9CQ+trH2Q5LlBXJmHloFK\nafiuQPjTmaJVf/piiQ79xJB6VmwoEpOJJG4NYNt7f+I7YCk07xwDggEFAAKCAQA5\nSBwWJouMKUI6Hi0EZ4/Yh98qQmItx4uWTYFdjcUVVYCKK7GIuXu67rfkbCJUrvT9\nID1vw2eyTmbuW2TPuRDsxUcB7WRyyLekl67vpUgMgLBLgYMXQf6RF4HM2tW7UWg7\noNQHkZKWbhDgXdumKzKf/qZPB/LT2Yndv/zqkQ+YXIu08j0RGkxJaAjB7nEv1XGq\nL2VJf8aEi+MnihAtMPCHcW34qswqO1kOCbOWNShlfWHGjKlfdsPYv87RcalHNqps\nk1r60kyEkeZvKGM+FDT80N7cafX286v8n9L4IvvnLr/FDOH4XXzEjXB9Vr5Ffvj1\ndxNPRmDZOo6JNKA8Uvki\n-----END PUBLIC KEY-----\n"));
				res.save(key_chain_path);
			}
		}
		return res;
	}

	std::vector<source_ptr> sources (std::string const& installDir)
	{
		plist::any_t default_index = plist::parse_ascii(
			"{	version = 4;"
			"	sources = {"
			"		org.textmate.updates.default  = { rank =  0; name = 'TextMate Bundles'; url = '" REST_API "/bundles/default'; };"
			"	};"
			"}"
			"");

		std::string const path = sources_index_path(installDir);
		if(!path::exists(path))
			plist::save(path, default_index);

		plist::any_t plist = plist::load(path);

		int32_t version = 0, minVersion = 0;
		plist::get_key_path(default_index, "version", minVersion);
		if(!plist::get_key_path(plist, "version", version) || version < minVersion)
		{
			plist = default_index;
			plist::save(path, plist);
		}

		std::vector<source_ptr> res;

		plist::dictionary_t sources;
		if(plist::get_key_path(plist, "sources", sources))
		{
			iterate(source, sources)
			{
				std::string name = NULL_STR, url = NULL_STR;
				int32_t rank = 0; bool disabled = false;

				plist::get_key_path(source->second, "name",     name);
				plist::get_key_path(source->second, "url",      url);
				plist::get_key_path(source->second, "rank",     rank);
				plist::get_key_path(source->second, "disabled", disabled);

				res.push_back(std::make_shared<source_t>(name, source->first, url, path::join(sources_path(installDir), source->first), rank, disabled));
			}
		}

		std::sort(res.begin(), res.end(), [](source_ptr const& lhs, source_ptr const& rhs){ return lhs->rank() > rhs->rank(); });
		return res;
	}

	bool save_sources (std::vector<source_ptr> const& sources, std::string const& installDir)
	{
		plist::dictionary_t dict;
		iterate(source, sources)
		{
			plist::dictionary_t item;
			item["name"]     = (*source)->name();
			item["url"]      = (*source)->url();
			item["rank"]     = (*source)->rank();
			item["disabled"] = (*source)->disabled();
			dict[(*source)->identifier()] = item;
		}

		plist::dictionary_t plist;
		plist["version"] = 1;
		plist["sources"] = dict;
		return plist::save(sources_index_path(installDir), plist);
	}

	bool update (source_ptr source, double* progress, double min, double max)
	{
		std::string etag = path::get_attr(source->path(), "org.w3.http.etag");
		std::string path = download_etag(source->url(), source->key_chain(), &etag, progress, min, max);
		if(path != NULL_STR)
		{
			if(path::swap_and_unlink(path, source->path()))
					path::set_attr(source->path(), "org.w3.http.etag", etag);
			else	fprintf(stderr, "*** swap_and_unlink(‘%s’ → ‘%s’): %s\n", path.c_str(), source->path().c_str(), strerror(errno));
		}
		else if(etag == NULL_STR)
		{
			fprintf(stderr, "*** error retrieving ‘%s’ (no etag given)\n", source->url().c_str());
		}

		path::set_attr(source->path(), "last-check", to_s(oak::date_t::now()));
		return path != NULL_STR;
	}

	// ===========

	static bool bundle_name_less_ptr (bundle_t const* lhs, bundle_t const* rhs)
	{
		if(lhs->uuid() == rhs->uuid())
			return lhs->installed() == rhs->installed() ? lhs->rank() > rhs->rank() : lhs->installed();
		return text::less_t()(lhs->name(), rhs->name());
	}

	static bool bundle_name_less (bundles_db::bundle_ptr const& lhs, bundles_db::bundle_ptr const& rhs)
	{
		return bundle_name_less_ptr(lhs.get(), rhs.get());
	}

	// =======================
	// = Bundle Dependencies =
	// =======================

	struct dependency_info_t
	{
		dependency_info_t () : _name(NULL_STR), _grammar(NULL_STR) { }

		oak::uuid_t _uuid;
		std::string _name;
		std::string _grammar;
	};

	static bundle_ptr find_bundle (std::string const& scope, std::vector<bundle_ptr> const& bundles)
	{
		citerate(bundle, bundles)
		{
			citerate(grammarInfo, (*bundle)->grammars())
			{
				if((*grammarInfo)->scope() == scope)
					return *bundle;
			}
		}
		return bundle_ptr();
	}

	static bundle_ptr find_bundle (oak::uuid_t const& uuid, std::vector<bundle_ptr> const& bundles)
	{
		citerate(bundle, bundles)
		{
			if((*bundle)->uuid() == uuid)
				return *bundle;
		}
		return bundle_ptr();
	}

	std::vector<bundle_t const*> bundle_t::dependencies (std::vector<bundle_ptr> const& bundles, bool includeImplicitDependencies) const
	{
		std::vector<bundle_t const*> res;
		if(includeImplicitDependencies)
		{
			std::set<oak::uuid_t> seen;
			std::vector<bundle_t const*> pending(1, this);
			while(!pending.empty())
			{
				bundle_t const* current = pending.back();
				pending.pop_back();
				if(seen.find(current->uuid()) != seen.end())
					continue;
				seen.insert(current->uuid());
				if(current != this)
					res.push_back(current);

				citerate(dependency, current->_dependencies)
				{
					if(bundle_ptr bundle = (*dependency)->_uuid ? find_bundle((*dependency)->_uuid, bundles) : find_bundle((*dependency)->_grammar, bundles))
						pending.push_back(bundle.get());
				}
			}
		}
		else
		{
			citerate(dependency, _dependencies)
			{
				if(bundle_ptr bundle = (*dependency)->_uuid ? find_bundle((*dependency)->_uuid, bundles) : find_bundle((*dependency)->_grammar, bundles))
					res.push_back(bundle.get());
			}
		}

		std::sort(res.begin(), res.end(), &bundle_name_less_ptr);
		return res;
	}

	// =======================

	template <typename _OutputIter>
	_OutputIter parse_grammars_array (plist::array_t const& grammars, _OutputIter out)
	{
		iterate(grammar, grammars)
		{
			auto info = std::make_shared<grammar_info_t>();

			plist::get_key_path(*grammar, "name", info->_name);
			plist::get_key_path(*grammar, "scope", info->_scope);
			plist::get_key_path(*grammar, "uuid", info->_uuid);
			plist::get_key_path(*grammar, "firstLineMatch", info->_mode_line);

			plist::array_t fileTypes;
			if(plist::get_key_path(*grammar, "fileTypes", fileTypes))
			{
				iterate(type, fileTypes)
				{
					if(std::string const* ext = boost::get<std::string>(&*type))
						info->_file_types.push_back(*ext);
				}
			}

			*out++ = info;
		}

		return out;
	}

	template <typename _OutputIter>
	_OutputIter parse_dependencies_array (plist::array_t const& dependencies, _OutputIter out)
	{
		iterate(dependency, dependencies)
		{
			auto info = std::make_shared<dependency_info_t>();
			plist::get_key_path(*dependency, "uuid", info->_uuid);
			plist::get_key_path(*dependency, "name", info->_name);
			plist::get_key_path(*dependency, "grammar", info->_grammar);
			*out++ = info;
		}

		return out;
	}

	template <typename _OutputIter>
	_OutputIter parse_remote_bundle_index (source_ptr src, _OutputIter out)
	{
		citerate(pair, plist::load(src->path()))
		{
			if(pair->first != "bundles")
				continue;

			citerate(item, plist::as_array(pair->second))
			{
				auto bundle = std::make_shared<bundle_t>();
				bundle->_source = src;

				if(!plist::get_key_path(*item, "uuid", bundle->_uuid))
					continue;

				plist::array_t versions;
				if(!plist::get_key_path(*item, "versions", versions))
					continue;

				iterate(version, versions)
				{
					plist::get_key_path(*version, "url",       bundle->_url);
					plist::get_key_path(*version, "updated",   bundle->_url_updated);
					plist::get_key_path(*version, "size",      bundle->_size);
				}

				plist::get_key_path(*item, "name",              bundle->_name);
				plist::get_key_path(*item, "category",          bundle->_category);
				plist::get_key_path(*item, "html_url",          bundle->_html_url);
				plist::get_key_path(*item, "contactName",       bundle->_contact_name);
				plist::get_key_path(*item, "contactEmailRot13", bundle->_contact_email);
				plist::get_key_path(*item, "description",       bundle->_description);

				if(bundle->_contact_email != NULL_STR)
					bundle->_contact_email = decode::rot13(bundle->_contact_email);

				plist::array_t grammars;
				if(plist::get_key_path(*item, "grammars", grammars))
				{
					parse_grammars_array(grammars, back_inserter(bundle->_grammars));
					std::sort(bundle->_grammars.begin(), bundle->_grammars.end(), [](grammar_info_ptr lhs, grammar_info_ptr const& rhs){ return text::less_t()(lhs->name(), rhs->name()); });
				}

				plist::array_t dependencies;
				if(plist::get_key_path(*item, "dependencies", dependencies))
					parse_dependencies_array(dependencies, back_inserter(bundle->_dependencies));

				*out++ = bundle;
			}
		}
		return out;
	}

	static std::vector<bundle_ptr> remote_bundles (std::string const& installDir)
	{
		std::vector<bundle_ptr> res;
		citerate(src, bundles_db::sources(installDir))
			parse_remote_bundle_index(*src, back_inserter(res));
		return res;
	}

	static std::string expand_path (std::string const& path, std::string const& installDir)
	{
		return path.find("~/") == 0 ? path::normalize(path::home() + path.substr(1)) : path::join(local_bundle_path(installDir), path);
	}

	std::vector<bundle_ptr> bundle_t::local_bundles (std::string const& installDir)
	{
		std::string const base = path::join(local_bundle_path(installDir), "Bundles");
		plist::dictionary_t const localIndexPlist = plist::load(local_index_path(installDir));

		std::set<std::string> actualPaths, indexedPaths;
		citerate(entry, path::entries(base, "*.tm[Bb]undle"))
			actualPaths.insert(path::join(base, (*entry)->d_name));

		iterate(pair, localIndexPlist)
		{
			citerate(item, plist::as_array(pair->second))
			{
				std::string path;
				if(plist::get_key_path(*item, "path", path))
					indexedPaths.insert(expand_path(path, installDir));
			}
		}

		std::set<std::string> inIndexButNotDisk;
		std::set<std::string> onDiskButNotIndex;
		std::set_difference(actualPaths.begin(), actualPaths.end(), indexedPaths.begin(), indexedPaths.end(), inserter(onDiskButNotIndex, onDiskButNotIndex.begin()));
		std::set_difference(indexedPaths.begin(), indexedPaths.end(), actualPaths.begin(), actualPaths.end(), inserter(inIndexButNotDisk, inIndexButNotDisk.begin()));

		std::vector<bundle_ptr> res;
		iterate(pair, localIndexPlist)
		{
			if(pair->first != "bundles")
				continue;

			citerate(item, plist::as_array(pair->second))
			{
				auto bundle = std::make_shared<bundle_t>();
				if(!plist::get_key_path(*item, "category", bundle->_category))
					bundle->_category = "Discontinued";
				if(plist::get_key_path(*item, "source", bundle->_origin) && plist::get_key_path(*item, "name", bundle->_name) && plist::get_key_path(*item, "uuid", bundle->_uuid) && plist::get_key_path(*item, "updated", bundle->_path_updated) && plist::get_key_path(*item, "path", bundle->_path))
				{
					bundle->_path = expand_path(bundle->_path, installDir);
					if(inIndexButNotDisk.find(bundle->_path) == inIndexButNotDisk.end())
							res.push_back(bundle);
					else	fprintf(stderr, "Bundle missing on disk: ‘%s’ (source ‘%s’)\n", path::with_tilde(bundle->_path).c_str(), bundle->_origin.c_str());
				}
			}
		}

		iterate(path, onDiskButNotIndex)
		{
			auto bundle = std::make_shared<bundle_t>();
			bundle->_category     = "Orphaned";
			bundle->_path         = *path;
			bundle->_path_updated = path::get_attr(*path, kBundleAttributeUpdated);
			bundle->_origin       = path::get_attr(*path, kBundleAttributeOrigin);

			if(bundle->_origin == NULL_STR)
				bundle->_origin = "x.unknown.origin";

			plist::dictionary_t infoPlist = plist::load(path::join(*path, "info.plist"));
			if(plist::get_key_path(infoPlist, "uuid", bundle->_uuid) && plist::get_key_path(infoPlist, "name", bundle->_name))
			{
				plist::get_key_path(infoPlist, "description", bundle->_description);
				plist::get_key_path(infoPlist, "contactName", bundle->_contact_name);
				if(plist::get_key_path(infoPlist, "contactEmailRot13", bundle->_contact_email))
					bundle->_contact_email = decode::rot13(bundle->_contact_email);
				res.push_back(bundle);
			}

			fprintf(stderr, "Bundle missing in local index: ‘%s’ (source ‘%s’)\n", bundle->name().c_str(), bundle->origin().c_str());
		}

		return res;
	}

	std::vector<bundle_ptr> index (std::string const& installDir)
	{
		std::vector<bundle_ptr> bundlesByRank = remote_bundles(installDir);
		std::sort(bundlesByRank.begin(), bundlesByRank.end(), [](bundle_ptr const& lhs, bundle_ptr const& rhs){ return lhs->source()->rank() > rhs->source()->rank(); });

		std::map<oak::uuid_t, bundle_ptr> bundles;
		iterate(bundle, bundlesByRank)
			bundles.emplace((*bundle)->uuid(), *bundle);

		citerate(bundle, bundle_t::local_bundles(installDir))
		{
			std::map<oak::uuid_t, bundle_ptr>::iterator remote = bundles.find((*bundle)->uuid());
			if(remote != bundles.end())
			{
				remote->second->_name         = (*bundle)->_name;
				remote->second->_path         = (*bundle)->_path;
				remote->second->_path_updated = (*bundle)->_path_updated;
				remote->second->_origin       = (*bundle)->_origin;
			}
			else
			{
				bundles.emplace((*bundle)->uuid(), (*bundle));
				fprintf(stderr, "Bundle missing in remote index: ‘%s’ (source ‘%s’)\n", (*bundle)->name().c_str(), (*bundle)->origin().c_str());
			}
		}

		std::vector<bundle_ptr> res;
		std::transform(bundles.begin(), bundles.end(), back_inserter(res), [](std::pair<oak::uuid_t, bundle_ptr> const& p){ return p.second; });
		res.erase(std::remove_if(res.begin(), res.end(), [](bundles_db::bundle_ptr const& bundle){ return !bundle->installed() && bundle->source() && bundle->source()->disabled(); }), res.end());
		std::sort(res.begin(), res.end(), &bundle_name_less);
		return res;
	}

	// ==============
	// = Save Index =
	// ==============

	bool save_index (std::vector<bundle_ptr> const& bundles, std::string const& installDir)
	{
		plist::dictionary_t plist;
		iterate(bundle, bundles)
		{
			if(!(*bundle)->installed())
				continue;

			plist::dictionary_t dict;
			dict["name"]     = (*bundle)->name();
			dict["uuid"]     = to_s((*bundle)->uuid());
			dict["category"] = (*bundle)->category();
			dict["path"]     = path::relative_to((*bundle)->path(), local_bundle_path(installDir));
			dict["updated"]  = (*bundle)->path_updated();
			dict["source"]   = (*bundle)->origin();

			plist::dictionary_t::iterator array = plist.find("bundles");
			if(array == plist.end())
				array = plist.emplace("bundles", plist::array_t()).first;
			boost::get<plist::array_t>(array->second).push_back(dict);
		}
		return plist::save(local_index_path(installDir), plist);
	}

	// ==================
	// = Install Bundle =
	// ==================

	static std::string safe_basename (std::string base)
	{
		std::replace(base.begin(), base.end(), '/', ':');
		std::replace(base.begin(), base.end(), '.', '_');
		return base;
	}

	bool update (bundle_ptr bundle, std::string const& installDir, double* progress, double min, double max)
	{
		std::string const folder = path::join(path::home(), "Library/Caches/com.macromates.TextMate/Bundles");
		std::string const name   = decode::url_part(path::name(bundle->url()));
		std::string const suffix = to_s(bundle->url_updated(), " (%F)");
		std::string const path   = path::join(folder, safe_basename(path::strip_extension(name)) + suffix + path::extension(name));

		std::string error = NULL_STR;
		std::string const src = network::download_tbz(bundle->url(), bundle->key_chain(), path, error, progress, min, max);
		if(src != NULL_STR)
		{
			std::string const dst = bundle->_path == NULL_STR ? path::join(local_bundle_path(installDir), text::format("Bundles/%s.tmbundle", safe_basename(bundle->name()).c_str())) : bundle->_path;
			if(path::exists(dst) && !path::remove(dst))
			{
				fprintf(stderr, "destination already exists ‘%s’\n", dst.c_str());
			}
			else if(!path::make_dir(path::parent(dst)))
			{
				fprintf(stderr, "destination directoy doesn’t exist ‘%s’\n", path::parent(dst).c_str());
			}
			else if(path::move(src, dst))
			{
				bundle->_path         = dst;
				bundle->_path_updated = bundle->_url_updated;
				bundle->_origin       = bundle->source()->identifier();
				path::set_attr(dst, kBundleAttributeUpdated, to_s(bundle->_url_updated));
				path::set_attr(dst, kBundleAttributeOrigin, bundle->origin());
				return true;
			}
		}
		else
		{
			fprintf(stderr, "*** error downloading ‘%s’: %s\n", bundle->url().c_str(), error.c_str());
		}
		return false;
	}

	bool install (bundle_ptr bundle, std::string const& installDir, double* progress, double min, double max)
	{
		return bundle->installed() ? false : update(bundle, installDir, progress, min, max);
	}

	bool uninstall (bundle_ptr bundle, std::string const& installDir)
	{
		if(!bundle->installed())
			return false;

		std::string const path = bundle->path();
		if(!path::exists(path) || path::remove(path))
		{
			bundle->_path         = NULL_STR;
			bundle->_path_updated = oak::date_t();
			return true;
		}
		return false;
	}

	std::vector<std::string> release_notes (std::string const& installDir)
	{
		std::vector<std::string> res;
		for(auto bundle : bundle_t::local_bundles(installDir))
		{
			std::string changes = path::join(bundle->path(), "Changes.json");
			if(path::exists(changes))
				res.push_back(changes);
		}
		return res;
	}

	// ===========================
	// = Bundle Dependency Logic =
	// ===========================

	std::vector<bundle_ptr> dependencies (std::vector<bundle_ptr> const& index, std::vector<bundle_ptr> const& startBundles, bool excludeInstalledBundles, bool excludeStartBundles)
	{
		std::map<oak::uuid_t, bundle_ptr> bundles;
		iterate(bundle, index)
			bundles.emplace((*bundle)->uuid(), *bundle);

		std::set<oak::uuid_t> dependencies, queue;
		iterate(bundle, startBundles)
			queue.insert((*bundle)->uuid());

		while(!queue.empty())
		{
			dependencies.insert(queue.begin(), queue.end());
			std::set<oak::uuid_t> tmp;
			tmp.swap(queue);
			iterate(uuid, tmp)
			{
				citerate(dependency, bundles[*uuid]->dependencies(index))
				{
					if(dependencies.find((*dependency)->uuid()) == dependencies.end())
					{
						dependencies.insert((*dependency)->uuid());
						queue.insert((*dependency)->uuid());
					}
				}
			}
		}

		std::set<oak::uuid_t> exclude;
		if(excludeStartBundles)
		{
			iterate(bundle, startBundles)
				exclude.insert((*bundle)->uuid());
		}

		if(excludeInstalledBundles)
		{
			iterate(bundle, index)
			{
				if((*bundle)->installed())
					exclude.insert((*bundle)->uuid());
			}
		}

		std::vector<oak::uuid_t> missing;
		std::set_difference(dependencies.begin(), dependencies.end(), exclude.begin(), exclude.end(), back_inserter(missing));

		std::vector<bundle_ptr> res;
		iterate(uuid, missing)
			res.push_back(bundles[*uuid]);
		std::sort(res.begin(), res.end(), &bundle_name_less);
		return res;
	}

	std::vector<bundle_ptr> dependents (std::vector<bundle_ptr> const& index, std::vector<bundle_ptr> const& bundles, bool onlyInstalledBundles)
	{
		std::vector<bundle_ptr> res;
		std::set<oak::uuid_t> seen;
		iterate(bundle, bundles)
		{
			iterate(candidate, index)
			{
				if(onlyInstalledBundles && !(*candidate)->installed())
					continue;

				if(seen.find((*candidate)->uuid()) != seen.end())
					continue;

				citerate(dependency, (*candidate)->dependencies(index, false))
				{
					if((*bundle)->uuid() == (*dependency)->uuid())
					{
						res.push_back(*candidate);
						seen.insert((*candidate)->uuid());
						break;
					}
				}
			}
		}
		std::sort(res.begin(), res.end(), &bundle_name_less);
		return res;
	}

} /* bundles_db */
