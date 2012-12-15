#import "DocumentCommand.h"
#import "DocumentController.h"
#import "DocumentTabs.h"
#import "DocumentSaveHelper.h"
#import <OakAppKit/OakToolTip.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <BundleEditor/BundleEditor.h>
#import <HTMLOutputWindow/HTMLOutputWindow.h>
#import <OakTextView/OakDocumentView.h>
#import <OakSystem/application.h>
#import <OakSystem/process.h>
#import <command/runner.h>
#import <ns/ns.h>
#import <oak/oak.h>
#import <bundles/bundles.h>
#import <document/collection.h>
#import <editor/editor.h>
#import <editor/write.h>
#import <io/path.h>
#import <text/trim.h>
#import <text/tokenize.h>

@interface OakAlertBlockWrapper : NSObject
@property (nonatomic, retain) NSAlert* alert;
@property (nonatomic, retain) NSString* info;
@property (nonatomic, retain) void(^completionHandler)(OakAlertBlockWrapper*, NSInteger);
@end

@implementation OakAlertBlockWrapper
- (void)showAlert:(NSAlert*)anAlert forWindow:(NSWindow*)aWindow completionHandler:(void(^)(OakAlertBlockWrapper*, NSInteger))aCallback
{
	self.completionHandler = aCallback;
	self.alert = anAlert;
	[self retain];

	if(aWindow)
			[anAlert beginSheetModalForWindow:aWindow modalDelegate:self didEndSelector:@selector(alertSheetDidEnd:returnCode:contextInfo:) contextInfo:NULL];
	else	[self alertSheetDidEnd:anAlert returnCode:[anAlert runModal] contextInfo:NULL];
}

- (void)alertSheetDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(void*)info
{
	self.completionHandler(self, returnCode);
	self.completionHandler = nil;
	self.alert = nil;
	self.info = nil;
	[self release];
}
@end

@interface DocumentController (Variables)
- (void)updateVariables:(std::map<std::string, std::string>&)env;
@end

@implementation DocumentController (Variables)
- (void)updateVariables:(std::map<std::string, std::string>&)env
{
	[fileBrowser updateVariables:env];

	if(NSString* projectDir = self.projectPath)
	{
		env["TM_PROJECT_DIRECTORY"] = [projectDir fileSystemRepresentation];
		env["TM_PROJECT_UUID"]      = to_s(identifier);
	}

	if(auto theme = documentView.textView.theme)
	{
		if(auto themeItem = bundles::lookup(theme->uuid()))
		{
			if(!themeItem->paths().empty())
				env["TM_CURRENT_THEME_PATH"] = themeItem->paths().back();
		}
	}
}
@end

namespace
{
	struct delegate_t : command::delegate_t
	{
		delegate_t (DocumentController* controller, document::document_ptr document) : _controller(controller), _document(document), _did_open_html_window(false)
		{
			if(_controller)
				_collection = to_s(_controller.identifier);
		}

		text::range_t write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection);

		bool accept_html_data (command::runner_ptr runner, char const* data, size_t len);
		bool accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, text::range_t inputRange, std::map<std::string, std::string> const& environment);

		void show_tool_tip (std::string const& str);
		void show_document (std::string const& str);
		void show_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err);

	private:
		DocumentController* _controller;
		document::document_ptr _document;
		oak::uuid_t _collection;
		bool _did_open_html_window;
	};
}

// =========================
// = Checking requirements =
// =========================

static std::string find_first_executable (std::vector<std::string> const& locations, std::map<std::string, std::string> const& environment)
{
	iterate(path, locations)
	{
		std::string const exe = format_string::expand(*path, environment);
		if(path::is_executable(exe))
			return exe;
	}
	return NULL_STR;
}

static std::vector<std::string> search_paths (std::map<std::string, std::string> const& environment)
{
	std::vector<std::string> res;
	auto searchPath = environment.find("PATH");
	if(searchPath != environment.end())
	{
		citerate(it, text::tokenize(searchPath->second.begin(), searchPath->second.end(), ':'))
		{
			if(*it != "")
				res.push_back(*it);
		}
	}
	else
	{
		fprintf(stderr, "no PATH!!!\n");
		iterate(pair, environment)
			fprintf(stderr, "%s = %s\n", pair->first.c_str(), pair->second.c_str());
	}
	return res;
}

static bool find_executable (std::string const& command, std::string const& variable, std::map<std::string, std::string> const& environment)
{
	auto var = environment.find(variable);
	if(var != environment.end())
		return path::is_executable(var->second);

	citerate(it, search_paths(environment))
	{
		if(path::is_executable(path::join(*it, command)))
			return true;
	}
	return false;
}

// =======================
// = Init, Saving, Input =
// =======================

text::range_t delegate_t::write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection)
{
	if(!_document)
		return text::range_t::undefined;

	bool isOpen = _document->is_open();
	if(!isOpen)
		_document->open();
	text::range_t res = ng::write_unit_to_fd(_document->buffer(), ng::editor_for_document(_document)->ranges().last(), _document->buffer().indent().tab_size(), fd, unit, fallbackUnit, format, scopeSelector, variables, inputWasSelection);
	if(!isOpen)
		_document->close();
	return res;
}

// ====================
// = Accepting Output =
// ====================

bool delegate_t::accept_html_data (command::runner_ptr runner, char const* data, size_t len)
{
	if(!_did_open_html_window)
	{
		_did_open_html_window = true;
		if(_controller)
		{
			if(![_controller setCommandRunner:runner])
				oak::kill_process_group_in_background(runner->process_id());
		}
		else
		{
			[HTMLOutputWindowController HTMLOutputWindowWithRunner:runner];
		}
	}
	return true;
}

bool delegate_t::accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, text::range_t inputRange, std::map<std::string, std::string> const& environment)
{
	bool res;
	if(_document && _document->is_open())
	{
		res = ng::editor_for_document(_document)->handle_result(out, placement, format, outputCaret, inputRange, environment);
	}
	else
	{
		document::document_ptr doc = document::create();
		doc->open();
		res = ng::editor_for_document(doc)->handle_result(out, placement, format, outputCaret, text::pos_t(0, 0) /* inputRange */, environment);
		document::show(doc);
		doc->close();
	}
	return res;
}

// ========================================
// = Showing tool tip, document, or error =
// ========================================

void delegate_t::show_tool_tip (std::string const& str)
{
	NSPoint location = _controller ? [_controller positionForWindowUnderCaret] : [NSEvent mouseLocation];
	OakShowToolTip([NSString stringWithCxxString:str], location);
}

void delegate_t::show_document (std::string const& str)
{
	document::show(document::from_content(str), _collection);
}

void delegate_t::show_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err)
{
	show_command_error(text::trim(err + out).empty() ? text::format("Command returned status code %d.", rc) : err + out, command.uuid, _controller.window);
}

// ==============
// = Public API =
// ==============

void run (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, document::document_ptr document, std::map<std::string, std::string> baseEnv, document::run_callback_ptr callback)
{
	DocumentController* controller = [DocumentController controllerForDocument:document];

	std::vector<document::document_ptr> documentsToSave;
	switch(command.pre_exec)
	{
		case pre_exec::save_document:
		{
			if(document && (document->is_modified() || !document->is_on_disk()))
				documentsToSave.push_back(document);
		}
		break;

		case pre_exec::save_project:
		{
			if(controller)
			{
				iterate(tab, controller->documentTabs)
				{
					document::document_ptr doc = **tab;
					if(doc->is_modified() && doc->path() != NULL_STR)
						documentsToSave.push_back(doc);
				}
			}
		}
		break;
	}

	if(!documentsToSave.empty())
	{
		struct callback_t : document_save_callback_t
		{
			callback_t (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, document::document_ptr document, std::map<std::string, std::string> const& environment, document::run_callback_ptr callback, size_t count) : _command(command), _buffer(buffer), _selection(selection), _document(document), _environment(environment), _callback(callback), _count(count) {  }

			void did_save_document (document::document_ptr document, bool flag, std::string const& message, oak::uuid_t const& filter)
			{
				if(--_count == 0 && flag)
					::run(_command, _buffer, _selection, _document, _environment, _callback);

				if(_count == 0 || !flag)
					delete this;
			}

		private:
			bundle_command_t                   _command;
			ng::buffer_t const&                _buffer;
			ng::ranges_t const&                _selection;
			document::document_ptr             _document;
			std::map<std::string, std::string> _environment;
			document::run_callback_ptr         _callback;
			size_t                             _count;
		};

		[DocumentSaveHelper trySaveDocuments:documentsToSave forWindow:controller.window defaultDirectory:controller.untitledSavePath andCallback:new callback_t(command, buffer, selection, document, baseEnv, callback, documentsToSave.size())];
	}
	else
	{
		if(document && document->is_open())
			baseEnv = ng::editor_for_document(document)->variables(baseEnv, to_s([controller scopeAttributes]));
		else if(document)
			baseEnv = document->variables(baseEnv);
		else
			baseEnv = variables_for_path(NULL_STR, "", baseEnv);

		if(controller)
			[controller updateVariables:baseEnv];

		if(callback)
			callback->update_environment(baseEnv);

		bundles::item_ptr item = bundles::lookup(command.uuid);
		if(item)
			baseEnv = item->environment(baseEnv);

		iterate(it, command.requirements)
		{
			if(find_executable(it->command, it->variable, baseEnv))
				continue;

			std::string exe = find_first_executable(it->locations, baseEnv);
			if(exe == NULL_STR)
			{
				std::vector<std::string> paths;
				for(auto path : search_paths(baseEnv))
					paths.push_back(path::with_tilde(path));
				return show_command_error(text::format("This command requires ‘%1$s’ which wasn’t found on your system.\n\nThe following locations were searched:%2$s\n\nIf ‘%1$s’ is installed elsewhere then you need to set %3$s in Preferences → Variables to the full path of where you installed it.", it->command.c_str(), ("\n\u2003• " + text::join(paths, "\n\u2003• ")).c_str(), it->variable.c_str()), command.uuid, [controller window]);
			}

			if(it->variable != NULL_STR)
					baseEnv[it->variable] = exe;
			else	baseEnv["PATH"] += ":" + path::parent(exe);
		}

		command::runner_ptr runner = command::runner(command, buffer, selection, baseEnv, command::delegate_ptr((command::delegate_t*)new delegate_t(controller, document)));
		runner->launch();
		runner->wait();
	}
}

void show_command_error (std::string const& message, oak::uuid_t const& uuid, NSWindow* window)
{
	std::string commandName = "(unknown)";
	if(bundles::item_ptr item = bundles::lookup(uuid))
		commandName = item->name();

	NSAlert* alert = [[NSAlert alloc] init]; // released in didEndSelector
	[alert setAlertStyle:NSCriticalAlertStyle];
	[alert setMessageText:[NSString stringWithCxxString:text::format("Failure running “%.*s”.", (int)commandName.size(), commandName.data())]];
	[alert setInformativeText:[NSString stringWithCxxString:message] ?: @"No output"];
	[alert addButtons:@"OK", @"Edit Command", nil];

	OakAlertBlockWrapper* wrapper = [[OakAlertBlockWrapper new] autorelease];
	wrapper.info = [NSString stringWithCxxString:uuid];
	[wrapper showAlert:alert forWindow:window completionHandler:^(OakAlertBlockWrapper* aWrapper, NSInteger button){
		if(button == NSAlertSecondButtonReturn)
			[[BundleEditor sharedInstance] revealBundleItem:bundles::lookup(to_s(aWrapper.info))];
	}];
}
