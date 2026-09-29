#ifndef UPDATER_H_D2910JR4
#define UPDATER_H_D2910JR4

#include <plist/src/plist.h>
#include <network/src/key_chain.h>

namespace bundles_db
{
	struct source_t;
	struct bundle_t;
	struct grammar_info_t;
	typedef std::shared_ptr<source_t> source_ptr;
	typedef std::shared_ptr<bundle_t> bundle_ptr;
	typedef std::shared_ptr<grammar_info_t> grammar_info_ptr;

	// internal (private) type
	struct dependency_info_t;
	typedef std::shared_ptr<dependency_info_t> dependency_info_ptr;

	struct source_t
	{
		source_t (std::string const& name, std::string const& identifier, std::string const& url, std::string const& path, int32_t rank = 0, bool disabled = false) : _name(name), _identifier(identifier), _url(url), _path(path), _rank(rank), _disabled(disabled) { }

		std::string name () const       { return _name; }
		std::string identifier () const { return _identifier; }
		std::string url () const        { return _url; }
		int32_t rank () const           { return _rank; }
		bool disabled () const          { return _disabled; }
		void set_disabled (bool flag)   { _disabled = flag; }

		std::string path () const;
		key_chain_t key_chain () const;
		bool needs_update (double pollInterval = 60*60) const;
		oak::date_t last_check () const;

	private:
		std::string _name;
		std::string _identifier;
		std::string _url;
		std::string _path;
		int32_t _rank;
		bool _disabled;
	};

	struct bundle_t
	{
		oak::uuid_t uuid () const              { return _uuid; }
		std::string origin () const            { return _origin; }
		std::string name () const              { return _name; }
		std::string category () const          { return _category; }
		std::string html_url () const          { return _html_url; }
		std::string description () const       { return _description; }
		std::string contact_name () const      { return _contact_name; }
		std::string contact_email () const     { return _contact_email; }
		std::string url () const               { return _url; }
		oak::date_t url_updated () const       { return _url_updated; }
		std::string path () const              { return _path; }
		oak::date_t path_updated () const      { return _path_updated; }
		int32_t rank () const                  { return _source ? _source->rank() : 0; }
		int32_t size () const                  { return _size; }
		bool is_mandatory () const             { return _is_mandatory; }
		bool is_default () const               { return _is_default; }
		bool is_dependency () const            { return _is_dependency; }
		void set_dependency (bool flag)        { _is_dependency = flag; }

		bool installed () const                { return _path != NULL_STR; }
		bool has_update () const               { return installed() && _path_updated < _url_updated; }
		key_chain_t key_chain () const         { return _source ? _source->key_chain() : key_chain_t(); }

		std::vector<grammar_info_ptr> const& grammars () const { return _grammars; }
		std::vector<bundle_t const*> dependencies (std::vector<bundle_ptr> const& bundles, bool includeImplicitDependencies = true) const;

		source_ptr source ()                   { return _source; }

	private:
		static std::vector<bundle_ptr> local_bundles (std::string const& installDir);
		template <typename _OutputIter> friend _OutputIter parse_remote_bundle_index (source_ptr src, _OutputIter out);
		friend std::vector<bundle_ptr> index (std::string const& installDir);
		friend bool update (bundle_ptr bundle, std::string const& installDir, double* progress, double min, double max);
		friend bool uninstall (bundle_ptr bundle, std::string const& installDir);
		friend std::vector<std::string> release_notes (std::string const& installDir);

		oak::uuid_t _uuid;

		std::string _name          = NULL_STR;
		std::string _requires      = NULL_STR;
		std::string _category      = NULL_STR;
		std::string _html_url      = NULL_STR;
		std::string _origin        = NULL_STR;
		std::string _description   = NULL_STR;
		std::string _contact_name  = NULL_STR;
		std::string _contact_email = NULL_STR;

		std::vector<grammar_info_ptr> _grammars;
		std::vector<dependency_info_ptr> _dependencies;

		source_ptr _source;
		std::string _url = NULL_STR;
		oak::date_t _url_updated;
		int32_t _size = 0;
		bool _is_default = false;
		bool _is_mandatory = false;
		bool _is_dependency = false; // if installed because another bundle depends on it

		std::string _path = NULL_STR;
		oak::date_t _path_updated;
	};

	struct grammar_info_t
	{
		grammar_info_t () : _name(NULL_STR), _scope(NULL_STR), _mode_line(NULL_STR) { }

		oak::uuid_t const& uuid ()                    { return _uuid; }
		std::string const& name ()                    { return _name; }
		std::string const& scope ()                   { return _scope; }
		std::vector<std::string> const& file_types () { return _file_types; }
		std::string const& mode_line ()               { return _mode_line; }

	private:
		template <typename _OutputIter> friend _OutputIter parse_grammars_array (plist::array_t const& grammars, _OutputIter out);

		oak::uuid_t _uuid;
		std::string _name;
		std::string _scope;
		std::vector<std::string> _file_types;
		std::string _mode_line;
	};

	bool update (source_ptr source, double* progress = NULL, double min = 0, double max = 1);
	std::vector<source_ptr> sources (std::string const& installDir = NULL_STR);

	std::vector<bundle_ptr> index (std::string const& installDir = NULL_STR);
	bool save_index (std::vector<bundle_ptr> const& bundles, std::string const& installDir = NULL_STR);

	std::vector<bundle_ptr> dependencies (std::vector<bundle_ptr> const& index, std::vector<bundle_ptr> const& startBundles, bool excludeInstalledBundles = true, bool excludeStartBundles = true);
	std::vector<bundle_ptr> dependents (std::vector<bundle_ptr> const& index, std::vector<bundle_ptr> const& bundles, bool onlyInstalledBundles = true);

	inline std::vector<bundle_ptr> dependencies (std::vector<bundle_ptr> const& index, bundle_ptr bundle, bool excludeInstalledBundles = true, bool excludeStartBundles = true) { return dependencies(index, std::vector<bundle_ptr>(1, bundle), excludeInstalledBundles, excludeStartBundles); }
	inline std::vector<bundle_ptr> dependents (std::vector<bundle_ptr> const& index, bundle_ptr bundle, bool onlyInstalledBundles = true)                                       { return dependents(index, std::vector<bundle_ptr>(1, bundle), onlyInstalledBundles); }

	bool update (bundle_ptr bundle, std::string const& installDir = NULL_STR, double* progress = NULL, double min = 0, double max = 1);
	bool install (bundle_ptr bundle, std::string const& installDir = NULL_STR, double* progress = NULL, double min = 0, double max = 1);
	bool uninstall (bundle_ptr bundle, std::string const& installDir = NULL_STR);

	std::vector<std::string> release_notes (std::string const& installDir = NULL_STR);

} /* bundles_db */

#endif /* end of include guard: UPDATER_H_D2910JR4 */
