#ifndef DOCUMENT_COMMAND_H_KLPQDYHU
#define DOCUMENT_COMMAND_H_KLPQDYHU

#import <oak/oak.h>
#import <plist/uuid.h>

void show_command_error (std::string const& message, oak::uuid_t const& uuid, NSWindow* window = nil, std::string commandName = NULL_STR);

#endif /* end of include guard: DOCUMENT_COMMAND_H_KLPQDYHU */
