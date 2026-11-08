#include "gutter_diff.h"

#include <regex.h>
#include <sys/types.h>
#include <xdiff.h>

#include <dispatch/dispatch.h>

#include <mutex>
#include <unordered_map>

#include <io/io.h>
#include <oak/oak.h>
#include "drivers/api.h"

namespace scm { namespace gutter_diff {

	std::string to_s (change c)
	{
		switch(c)
		{
			case change::added:    return "added";
			case change::modified: return "modified";
			case change::deleted:  return "deleted";
		}
		return "unknown";
	}

	// =========================
	// = Pure xdiff line walker =
	// =========================

	namespace
	{
		struct walker_t
		{
			result_t out;
			size_t lineno = 1;     // 1-indexed cursor in the new-side text
			size_t deleted = 0;    // unpaired `-` lines so far in this hunk
			bool   in_hunk = false;
		};

		// Flush any unpaired `-` lines as a single `deleted` mark on
		// the line that follows the deletion in the new-side frame.
		// If the deletion lives past the end of the new file, fall
		// back to the last line we know about.
		void flush_unpaired_deletions (walker_t& w)
		{
			if(w.deleted == 0)
				return;

			size_t mark = w.lineno;
			if(mark < 1)
				mark = 1;

			// Don't overwrite a `modified` already placed at this
			// position by a paired +/- on the same hunk; the
			// surviving line is more useful as `modified` than
			// `deleted`. Drop the mark one line further if we'd
			// collide.
			auto it = w.out.find(mark);
			if(it != w.out.end() && it->second == change::modified)
				++mark;

			w.out[mark] = change::deleted;
			w.deleted = 0;
		}

		// Parse the new-side starting line number from a `@@ -A,B +C,D @@`
		// header. xdiff guarantees the format; we don't need a full
		// parser, just enough to extract C.
		size_t new_begin_from_hunk_header (char const* ptr, size_t size)
		{
			char const* end = ptr + size;
			char const* p = ptr;

			while(p < end && *p != '+')
				++p;
			if(p == end)
				return 1;
			++p;

			size_t value = 0;
			while(p < end && *p >= '0' && *p <= '9')
			{
				value = value * 10 + (size_t)(*p - '0');
				++p;
			}
			return value == 0 ? 1 : value;
		}

		int out_line_cb (void* priv, mmbuffer_t* mb, int nbuf)
		{
			walker_t& w = *(walker_t*)priv;
			if(nbuf == 0 || mb[0].size == 0)
				return 0;

			char const c = mb[0].ptr[0];

			// xdiff emits a diff record as prefix + source text chunks in
			// one callback; only the prefix/header chunk is diff syntax.
			if(mb[0].size >= 2 && mb[0].ptr[0] == '@' && mb[0].ptr[1] == '@')
			{
				flush_unpaired_deletions(w);
				w.lineno = new_begin_from_hunk_header(mb[0].ptr, (size_t)mb[0].size);
				w.deleted = 0;
				w.in_hunk = true;
				return 0;
			}

			if(!w.in_hunk)
				return 0;

			switch(c)
			{
				case ' ':
					flush_unpaired_deletions(w);
					++w.lineno;
					break;

				case '-':
					++w.deleted;
					break;

				case '+':
					if(w.deleted > 0)
					{
						w.out[w.lineno] = change::modified;
						--w.deleted;
					}
					else
					{
						w.out[w.lineno] = change::added;
					}
					++w.lineno;
					break;

				case '\\':
					// "\ No newline at end of file" — pre-EOF
					// normalisation in diff_bytes already removes
					// the cases where this would change marks.
					break;

				default:
					break;
			}
			return 0;
		}

		// Both inputs need a trailing newline for xdiff to report a
		// change-of-trailing-newline-only as "no change". Mutating a
		// local copy is cheaper than handling \ No newline at end of
		// file markers in the parser.
		void normalise_trailing_newline (std::string& s)
		{
			if(!s.empty() && s.back() != '\n')
				s.push_back('\n');
		}
	}

	result_t diff_bytes (std::string const& head_blob, std::string const& current_text)
	{
		std::string a = head_blob;
		std::string b = current_text;
		normalise_trailing_newline(a);
		normalise_trailing_newline(b);

		mmfile_t mf_a, mf_b;
		mf_a.ptr  = a.empty() ? nullptr : &a[0];
		mf_a.size = (long)a.size();
		mf_b.ptr  = b.empty() ? nullptr : &b[0];
		mf_b.size = (long)b.size();

		xpparam_t xpp;
		memset(&xpp, 0, sizeof(xpp));
		xpp.flags = XDF_HISTOGRAM_DIFF;

		xdemitconf_t xec;
		memset(&xec, 0, sizeof(xec));
		xec.ctxlen = 0;

		walker_t w;
		xdemitcb_t ecb;
		memset(&ecb, 0, sizeof(ecb));
		ecb.priv     = &w;
		ecb.out_line = out_line_cb;

		xdl_diff(&mf_a, &mf_b, &xpp, &xec, &ecb);

		flush_unpaired_deletions(w);
		return w.out;
	}

	// =========
	// = Cache =
	// =========

	namespace
	{
		struct blob_entry_t
		{
			std::string bytes;
			bool tracked = false;
		};

		std::mutex&                                                          cache_mutex ()
		{
			static std::mutex m;
			return m;
		}

		std::unordered_map<std::string, blob_entry_t>& cache ()
		{
			static std::unordered_map<std::string, blob_entry_t> c;
			return c;
		}

		std::string cache_key (std::string const& root, std::string const& rel)
		{
			std::string k = root;
			k += '\0';
			k += rel;
			return k;
		}

		// Synchronously fetch HEAD:<rel> from the repo at root via
		// `git show`. Returns (bytes, tracked). For untracked / unknown
		// paths returns ("", false) so the diff against current_text
		// shows everything as additions.
		blob_entry_t fetch_head_blob (std::string const& root, std::string const& rel)
		{
			static std::string const git = scm::find_executable("git", "TM_GIT");
			blob_entry_t e;
			if(git == NULL_STR)
				return e;

			std::map<std::string, std::string> env = oak::basic_environment();
			env["GIT_WORK_TREE"] = root;
			env["GIT_DIR"]       = path::join(root, ".git");

			std::string spec = "HEAD:" + rel;
			std::string out  = io::exec(env, git, "show", spec.c_str(), nullptr);
			if(out != NULL_STR)
			{
				e.bytes   = out;
				e.tracked = true;
			}
			return e;
		}
	}

	void invalidate_blob (std::string const& repo_root, std::string const& rel_path)
	{
		std::lock_guard<std::mutex> g(cache_mutex());
		cache().erase(cache_key(repo_root, rel_path));
	}

	void invalidate_repo (std::string const& repo_root)
	{
		std::lock_guard<std::mutex> g(cache_mutex());
		auto& c = cache();
		std::string const prefix = repo_root + '\0';
		for(auto it = c.begin(); it != c.end(); )
		{
			if(it->first.compare(0, prefix.size(), prefix) == 0)
				it = c.erase(it);
			else
				++it;
		}
	}

	void compute (std::string repo_root,
	              std::string rel_path,
	              std::string current_text,
	              void (^completion)(result_t))
	{
		auto block_completion = Block_copy(completion);

		dispatch_async(dispatch_get_global_queue(QOS_CLASS_UTILITY, 0), ^{
			std::string const key = cache_key(repo_root, rel_path);

			blob_entry_t entry;
			bool cached = false;
			{
				std::lock_guard<std::mutex> g(cache_mutex());
				auto it = cache().find(key);
				if(it != cache().end())
				{
					entry  = it->second;
					cached = true;
				}
			}

			if(!cached)
			{
				entry = fetch_head_blob(repo_root, rel_path);
				std::lock_guard<std::mutex> g(cache_mutex());
				cache()[key] = entry;
			}

			result_t result = diff_bytes(entry.bytes, current_text);

			dispatch_async(dispatch_get_main_queue(), ^{
				block_completion(result);
				Block_release(block_completion);
			});
		});
	}

} /* gutter_diff */ } /* scm */
