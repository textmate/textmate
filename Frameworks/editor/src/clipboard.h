#ifndef CLIPBOARD_H_QNI7BPU5
#define CLIPBOARD_H_QNI7BPU5

extern std::string const kClipboardOptionIndent;
extern std::string const kClipboardOptionComplete;
extern std::string const kClipboardOptionColumnar;

struct clipboard_t
{
	struct entry_t
	{
		entry_t (std::vector<std::string> const& contents, std::map<std::string, std::string> const& options = { });
		entry_t (std::vector<std::string> const& contents, std::string const& indent, bool complete, bool columnar);
		entry_t (std::string const& content, std::map<std::string, std::string> const& options = { });
		~entry_t ()                                                { }
		std::string content () const;
		std::vector<std::string> const& contents () const          { return _contents; }
		std::map<std::string, std::string> const& options () const { return _options; }
	private:
		std::vector<std::string> _contents;
		std::map<std::string, std::string> _options;
	};

	typedef std::shared_ptr<entry_t> entry_ptr;

	clipboard_t ()          { }
	virtual ~clipboard_t () { }

	virtual bool empty () const = 0;
	virtual entry_ptr previous () = 0;
	virtual entry_ptr current () const = 0;
	virtual entry_ptr next () = 0;
	virtual void push_back (entry_ptr entry) = 0;

	void push_back (std::string const& content, std::map<std::string, std::string> const& options = { }) { push_back(std::make_shared<entry_t>(content, options)); }
};

typedef std::shared_ptr<clipboard_t> clipboard_ptr;

clipboard_ptr create_simple_clipboard ();

#endif /* end of include guard: CLIPBOARD_H_QNI7BPU5 */
