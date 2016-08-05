#ifndef OAK_SERVER_H_Q8M693GB
#define OAK_SERVER_H_Q8M693GB

#include <oak/oak.h>

namespace oak
{
	template <typename T, typename ARG = typename T::request_t, typename RESULT = decltype(T::handle_request(ARG()))>
	struct server_t
	{
		size_t register_client (T* callback);
		void unregister_client (size_t clientKey);

		void send_request (size_t clientKey, ARG const request);

	private:
		size_t next_client_key = 0;
		std::map<size_t, T*> client_to_callback;
	};

	// ============
	// = server_t =
	// ============

	template <typename T, typename ARG, typename RESULT>
	size_t server_t<T, ARG, RESULT>::register_client (T* callback)
	{
		client_to_callback.emplace(next_client_key, callback);
		return next_client_key++;
	}

	template <typename T, typename ARG, typename RESULT>
	void server_t<T, ARG, RESULT>::unregister_client (size_t clientKey)
	{
		client_to_callback.erase(client_to_callback.find(clientKey));
	}

	template <typename T, typename ARG, typename RESULT>
	void server_t<T, ARG, RESULT>::send_request (size_t clientKey, ARG const request)
	{
		CFRunLoopRef runLoop = CFRunLoopGetCurrent();
		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			RESULT const result = T::handle_request(request);
			CFRunLoopPerformBlock(runLoop, kCFRunLoopCommonModes, ^{
				typename std::map<size_t, T*>::iterator client = client_to_callback.find(clientKey);
				if(client != client_to_callback.end())
				{
					T* obj = client->second;
					obj->handle_reply(result);
				}
			});
			CFRunLoopWakeUp(runLoop);
		});
	}

} /* oak */

#endif /* end of include guard: OAK_SERVER_H_Q8M693GB */
