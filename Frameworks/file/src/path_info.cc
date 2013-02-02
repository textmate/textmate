#include "path_info.h"
#include <scm/scm.h>
#include <settings/settings.h>
#include <io/entries.h>
#include <regexp/glob.h>
#include <plist/ascii.h>
#include <text/tokenize.h>
#include <oak/oak.h>

namespace
{
	struct attribute_rule_t
	{
		attribute_rule_t (std::string const& attribute, path::glob_t const& glob, std::string const& group) : attribute(attribute), glob(glob), group(group) { }

		std::string attribute;
		path::glob_t glob;
		std::string group;
	};

	template <typename _OutputIter>
	_OutputIter parse_rules (plist::any_t const& plist, _OutputIter out)
	{
		plist::array_t rules;
		if(plist::get_key_path(plist, "rules", rules))
		{
			iterate(dict, rules)
			{
				std::string attr, glob, group = NULL_STR;
				if(plist::get_key_path(*dict, "attribute", attr) && plist::get_key_path(*dict, "glob", glob))
				{
					plist::get_key_path(*dict, "group", group);
					*out++ = attribute_rule_t(attr, glob, group);
				}
			}
		}
		return out;
	}

	static std::vector<attribute_rule_t> attribute_specifiers ()
	{
		static std::string const DefaultScopeAttributes =
			"{ rules = ("
			"	{ attribute = 'attr.scm.svn';       glob = '.svn';           group = 'scm';   },"
			"	{ attribute = 'attr.scm.hg';        glob = '.hg';            group = 'scm';   },"
			"	{ attribute = 'attr.scm.git';       glob = '.git';           group = 'scm';   },"
			"	{ attribute = 'attr.scm.p4';        glob = '.p4config';      group = 'scm';   },"
			"	{ attribute = 'attr.project.ninja'; glob = 'build.ninja';    group = 'build'; },"
			"	{ attribute = 'attr.project.make';  glob = 'Makefile';       group = 'build'; },"
			"	{ attribute = 'attr.project.xcode'; glob = '*.xcodeproj';    group = 'build'; },"
			"	{ attribute = 'attr.project.rake';  glob = 'Rakefile';       group = 'build'; },"
			"	{ attribute = 'attr.project.ant';   glob = 'build.xml';      group = 'build'; },"
			"	{ attribute = 'attr.project.cmake'; glob = 'CMakeLists.txt'; group = 'build'; },"
			"	{ attribute = 'attr.project.maven'; glob = 'pom.xml';        group = 'build'; },"
			"	{ attribute = 'attr.project.scons'; glob = 'SConstruct';     group = 'build'; },"
			"); }";

		std::vector<attribute_rule_t> res;
		parse_rules(plist::load(path::join(path::home(), "Library/Application Support/TextMate/ScopeAttributes.plist")), back_inserter(res));
		parse_rules(plist::parse_ascii(DefaultScopeAttributes), back_inserter(res));
		return res;
	}

	static void directory_attributes (std::string const& dir, std::vector<std::string>& res)
	{
		if(dir == NULL_STR || dir == "" || dir[0] != '/')
			return;

		std::set<std::string> groups;
		for(std::string cwd = dir; cwd != "/"; cwd = path::parent(cwd))
		{
			auto entries = path::entries(cwd);

			static std::vector<attribute_rule_t> const specifiers = attribute_specifiers();
			iterate(specifier, specifiers)
			{
				if(groups.find(specifier->group) != groups.end())
					continue;

				iterate(entry, entries)
				{
					if(specifier->glob.does_match((*entry)->d_name))
					{
						res.push_back(specifier->attribute);
						if(specifier->group != NULL_STR)
						{
							groups.insert(specifier->group);
							break;
						}
					}
				}
			}
		}
	}
}

namespace file
{
	std::string path_attributes (std::string const& path, std::string const& dir)
	{
		std::vector<std::string> res;
		if(path != NULL_STR)
		{
			std::vector<std::string> revPath;
			citerate(token, text::tokenize(path.begin(), path.end(), '/'))
			{
				std::string tmp = *token;
				citerate(subtoken, text::tokenize(tmp.begin(), tmp.end(), '.'))
				{
					if((*subtoken).empty())
						continue;
					revPath.push_back(*subtoken);
					std::replace(revPath.back().begin(), revPath.back().end(), ' ', '_');
				}
			}
			revPath.push_back("rev-path");
			revPath.push_back("attr");
			std::reverse(revPath.begin(), revPath.end());
			res.push_back(text::join(revPath, "."));
		}
		else
		{
			res.push_back("attr.untitled");
		}

		SInt32 major, minor, bugFix;
		Gestalt(gestaltSystemVersionMajor,  &major);
		Gestalt(gestaltSystemVersionMinor,  &minor);
		Gestalt(gestaltSystemVersionBugFix, &bugFix);
		res.push_back(text::format("attr.os-version.%zd.%zd.%zd", (ssize_t)major, (ssize_t)minor, (ssize_t)bugFix));

		std::string const parentDir = dir == NULL_STR ? path::parent(path) : dir;
		directory_attributes(parentDir, res);

		res.push_back(settings_for_path(path, text::join(res, " "), parentDir).get(kSettingsScopeAttributesKey, ""));
		res.erase(std::remove(res.begin(), res.end(), ""), res.end());
		return text::join(res, " ");
	}

	std::map<std::string, std::string> variables (std::string const& path)
	{
		std::map<std::string, std::string> map;
		if(path != NULL_STR)
		{
			map["TM_DISPLAYNAME"] = path::display_name(path);
			map["TM_FILEPATH"]    = path;
			map["TM_FILENAME"]    = path::name(path);
			map["TM_DIRECTORY"]   = path::parent(path);
			map["PWD"]            = path::parent(path);
		}
		else
		{
			map["TM_DISPLAYNAME"] = "untitled";
		}
		return variables_for_path(path, "", map);
	}
	
} /* file */