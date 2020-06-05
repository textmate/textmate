#ifndef AUTH_SERVER_H_G3FZ07F2
#define AUTH_SERVER_H_G3FZ07F2

#include "authorization.h"
#include "connection.h"
connection_t connect_to_auth_server (osx::authorization_t const& auth, bool retry = false);

#endif /* end of include guard: AUTH_SERVER_H_G3FZ07F2 */
