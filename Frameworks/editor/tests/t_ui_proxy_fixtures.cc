#include <document/collection.h>
#include <bundles/bundles.h>
#include <settings/settings.h>
#include <file/path_info.h>
#include <command/runner.h>

void setup_fixtures ()
{
	static struct proxy_t : document::ui_proxy_t
	{
		void show_browser (std::string const& path) const { }
		void show_documents (std::vector<document::document_ptr> const& documents) const { }
		void show_document (oak::uuid_t const& collection, document::document_ptr document, text::range_t const& range, bool bringToFront) const { }

		void run (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, document::document_ptr document, std::map<std::string, std::string> const& baseEnv, std::string const& pwd)
		{
			struct delegate_t : command::delegate_t
			{
				delegate_t (document::document_ptr document) : _document(document)
				{
				}

				bool accept_html_data (command::runner_ptr runner, char const* data, size_t len) { return fprintf(stderr, "html: %.*s", (int)len, data), false; }

				void show_document (std::string const& str) { fprintf(stderr, "document: %s\n", str.c_str()); }
				void show_tool_tip (std::string const& str) { fprintf(stderr, "tool tip: %s\n", str.c_str()); }
				void show_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err) { fprintf(stderr, "error: %s%s\n", out.c_str(), err.c_str()); }

				text::range_t write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection)
				{
					close(fd);
					return text::range_t::undefined;
				}

				bool accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, text::range_t inputRange, std::map<std::string, std::string> const& environment)
				{
					if(_document && _document->is_open())
						return ng::editor_for_document(_document)->handle_result(out, placement, format, outputCaret, inputRange, environment);
					return false;
				}

			private:
				document::document_ptr _document;
			};

			// The following is only required because runner_t::wait currently uses a CFRunLoop
			dispatch_sync(dispatch_get_main_queue(), ^{
				command::runner_ptr runner = command::runner(command, buffer, selection, baseEnv, command::delegate_ptr((command::delegate_t*)new delegate_t(document)));
				runner->launch();
				runner->wait();
			});
		}

	} proxy;

	document::set_ui_proxy(&proxy);
}
