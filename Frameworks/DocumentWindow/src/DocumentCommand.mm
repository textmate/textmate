#import "DocumentCommand.h"
#import "DocumentController.h"
#import "DocumentSaveHelper.h"
#import <OakAppKit/OakToolTip.h>
#import <OakAppKit/OakAppKit.h>
#import <OakFoundation/NSString Additions.h>
#import <BundleEditor/BundleEditor.h>
#import <HTMLOutputWindow/HTMLOutputWindow.h>
#import <OakTextView/OakDocumentView.h>
#import <OakFileBrowser/OakFileBrowser.h>
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
		void discard_html ();

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

void delegate_t::discard_html ()
{
	if(_did_open_html_window && _controller.htmlOutputVisible)
		_controller.htmlOutputVisible = NO;
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
				citerate(document, controller.documents)
				{
					if((*document)->is_modified() && (*document)->path() != NULL_STR)
						documentsToSave.push_back((*document));
				}
			}
		}
		break;
	}

	if(!documentsToSave.empty())
	{
		bundle_command_t commandNonRef = command;
		[DocumentSaveHelper trySaveDocuments:documentsToSave forWindow:controller.window defaultDirectory:controller.untitledSavePath completionHandler:^(BOOL success){
			if(success)
				::run(commandNonRef, buffer, selection, document, baseEnv, callback);
		}];
	}
	else
	{
		if(document && document->is_open())
			baseEnv = ng::editor_for_document(document)->variables(baseEnv, to_s([controller scopeAttributes]));
		else if(document)
			baseEnv = bundles::environment(document->file_type(), document->variables(baseEnv));
		else
			baseEnv = bundles::environment(scope::scope_t(), variables_for_path(NULL_STR, "", baseEnv));

		if(controller)
			[controller updateVariables:baseEnv];

		if(callback)
			callback->update_environment(baseEnv);

		bundles::item_ptr item = bundles::lookup(command.uuid);
		if(item)
			baseEnv = item->environment(baseEnv);

		for(auto requirement : command.requirements)
		{
			std::vector<std::string> candidates;

			if(baseEnv.find(requirement.variable) != baseEnv.end())
				candidates.push_back(baseEnv[requirement.variable]);

			for(auto path : search_paths(baseEnv))
				candidates.push_back(path::join(path, requirement.command));

			for(auto path : requirement.locations)
				candidates.push_back(format_string::expand(path, baseEnv));

			auto exe = std::find_if(candidates.begin(), candidates.end(), [](std::string const& path){ return path::is_executable(path); });
			if(exe != candidates.end())
			{
				if(requirement.variable != NULL_STR)
						baseEnv[requirement.variable] = *exe;
				else	baseEnv["PATH"] += ":" + path::parent(*exe);
			}
			else
			{
				std::vector<std::string> paths;
				for(auto path : search_paths(baseEnv))
					paths.push_back(path::with_tilde(path));

				std::string const title = text::format("Unable to run “%.*s”.", (int)command.name.size(), command.name.data());
				std::string const message = text::format("This command requires ‘%1$s’ which wasn’t found on your system.\n\nThe following locations were searched:%2$s\n\nIf ‘%1$s’ is installed elsewhere then you need to set %3$s in Preferences → Variables to the full path of where you installed it.", requirement.command.c_str(), ("\n\u2003• " + text::join(paths, "\n\u2003• ")).c_str(), requirement.variable.c_str());

				NSAlert* alert = [[NSAlert alloc] init];
				[alert setAlertStyle:NSCriticalAlertStyle];
				[alert setMessageText:[NSString stringWithCxxString:title]];
				[alert setInformativeText:[NSString stringWithCxxString:message]];
				[alert addButtonWithTitle:@"OK"];
				if(requirement.more_info_url != NULL_STR)
					[alert addButtonWithTitle:@"More Info…"];

				NSString* moreInfo = [NSString stringWithCxxString:requirement.more_info_url];
				OakShowAlertForWindow(alert, [controller window], ^(NSInteger button){
					if(button == NSAlertSecondButtonReturn)
						[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:moreInfo]];
				});

				return;
			}
		}

		command::runner_ptr runner = command::runner(command, buffer, selection, baseEnv, command::delegate_ptr((command::delegate_t*)new delegate_t(controller, document)));
		runner->launch();
		runner->wait();
	}
}

void show_command_error (std::string const& message, oak::uuid_t const& uuid, NSWindow* window)
{
	bundles::item_ptr bundleItem = bundles::lookup(uuid);
	std::string commandName = bundleItem ? bundleItem->name() : "(unknown)";

	NSAlert* alert = [[NSAlert alloc] init];
	[alert setAlertStyle:NSCriticalAlertStyle];
	[alert setMessageText:[NSString stringWithCxxString:text::format("Failure running “%.*s”.", (int)commandName.size(), commandName.data())]];
	[alert setInformativeText:[NSString stringWithCxxString:message] ?: @"No output"];
	[alert addButtonWithTitle:@"OK"];
	if(bundleItem)
		[alert addButtonWithTitle:@"Edit Command"];

	OakShowAlertForWindow(alert, window, ^(NSInteger button){
		if(button == NSAlertSecondButtonReturn)
			[[BundleEditor sharedInstance] revealBundleItem:bundleItem];
	});
}
