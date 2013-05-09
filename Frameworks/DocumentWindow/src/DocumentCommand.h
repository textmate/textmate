#ifndef DOCUMENT_COMMAND_H_KLPQDYHU
#define DOCUMENT_COMMAND_H_KLPQDYHU

#import <document/document.h>
#import <document/collection.h>
#import <command/parser.h>

void run_impl (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, document::document_ptr document, std::map<std::string, std::string> env, std::string const& pwd, document::run_callback_ptr callback);
void show_command_error (std::string const& message, oak::uuid_t const& uuid, NSWindow* window = nil);

#endif /* end of include guard: DOCUMENT_COMMAND_H_KLPQDYHU */
