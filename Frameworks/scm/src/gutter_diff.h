#ifndef SCM_GUTTER_DIFF_H_HOH88JIM
#define SCM_GUTTER_DIFF_H_HOH88JIM

#include <cstdint>
#include <map>
#include <string>

namespace scm { namespace gutter_diff {

	enum class change : uint8_t { added, modified, deleted };

	// ADL hook for the OAK_ASSERT_EQ stringifier in bin/gen_test;
	// also handy for trace logs.
	std::string to_s (change c);

	// Marks for the gutter, keyed by 1-indexed line number in the
	// new-side text (matches `mate --line=N` and the existing
	// SCM Diff Gutter Ruby bundle's contract).
	using result_t = std::map<size_t, change>;

	// Pure: produce gutter marks from two byte buffers. Uses xdiff's
	// histogram algorithm, the same one git's own `diff` defaults to.
	// No I/O. Used by both the async path below and by unit tests.
	//
	// The walk over xdiff's line-level output mirrors the existing
	// Ruby bundle's parser (Update Gutter on Save.tmCommand:39-66):
	//   ' ' resets a deletion counter
	//   '-' increments it
	//   '+' becomes `modified` if deleted > 0 (consumes one), else `added`
	// At hunk end, any unpaired `-` lines emit one `deleted` mark on
	// the line preceding the deletion site.
	result_t diff_bytes (std::string const& head_blob,
	                     std::string const& current_text);

	// Async: look up (or fetch + cache) the HEAD blob for repo_root /
	// rel_path via `git show HEAD:<rel_path>`, diff against
	// current_text, and deliver the result on the main queue.
	// Untracked files synthesise an empty HEAD blob so additions show
	// as additions.
	void compute (std::string repo_root,
	              std::string rel_path,
	              std::string current_text,
	              void (^completion)(result_t));

	// Drop a single (repo_root, rel_path) cache entry. Caller invokes
	// this on rename / save-as.
	void invalidate_blob (std::string const& repo_root,
	                      std::string const& rel_path);

	// Drop every cached blob under repo_root. Workstream E3 hooks this
	// from shared_info_t::fs_did_change when .git/HEAD, .git/index, or
	// .git/refs/** changes — the events that move HEAD or rewrite the
	// snapshot the cache is built against.
	void invalidate_repo (std::string const& repo_root);

} /* gutter_diff */ } /* scm */

#endif /* end of include guard: SCM_GUTTER_DIFF_H_HOH88JIM */
