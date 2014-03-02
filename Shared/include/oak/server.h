#ifndef OAK_SERVER_H_Q8M693GB
#define OAK_SERVER_H_Q8M693GB

#include "duration.h"
#include "compat.h"
#include "oak.h"
#include <cf/callback.h>

namespace oak
{
	template <typename T, typename ARG = typename T::request_t, typename RESULT = decltype(T::handle_request(ARG()))>
	struct server_t
	{
		server_t (size_t threadStackSize = 0);
		~server_t ();

		size_t register_client (T* callback);
		void unregister_client (size_t clientKey);

		void send_request (size_t clientKey, ARG const& request);
		void remove_requests (size_t clientKey);
		void send_reply (size_t clientKey, RESULT const& result);

	private:
	 	struct equal_to_1st_helper_t : std::binary_function<std::pair<size_t, ARG>, size_t, bool>
		{
			bool operator() (std::pair<size_t, ARG> lhs, size_t rhs) const { return lhs.first == rhs; }
		};

		size_t next_client_key;
		std::map<size_t, T*> client_to_callback;

		void master_run ();
		void server_run ();

		pthread_t server_thread;

		cf::callback_ptr run_loop_source;
		std::vector< std::pair<size_t, ARG> > requests;
		std::vector< std::pair<size_t, RESULT>  > results;
		std::mutex client_lock;
		pthread_mutex_t requests_mutex;
		pthread_mutex_t results_mutex;
		pthread_cond_t cond;
		volatile bool should_terminate;
	};

	// ============
	// = server_t =
	// ============

	template <typename T, typename ARG, typename RESULT>
	server_t<T, ARG, RESULT>::server_t (size_t threadStackSize) : next_client_key(1)
	{
		struct runner_t {
			static void* server (void* arg) { ((server_t*)arg)->server_run(); return NULL; }
		};

		should_terminate = false;
		run_loop_source = cf::create_callback(std::bind(&server_t::master_run, this));

		pthread_mutex_init(&requests_mutex, NULL);
		pthread_mutex_init(&results_mutex, NULL);
		pthread_cond_init(&cond, NULL);

		pthread_attr_t stackSizeAttribute;
		if(pthread_attr_init(&stackSizeAttribute) == 0)
		{
			size_t stackSize = 0;
			if(pthread_attr_getstacksize(&stackSizeAttribute, &stackSize) == 0)
			{
				if(threadStackSize != 0)
					pthread_attr_setstacksize(&stackSizeAttribute, std::max<size_t>(threadStackSize, stackSize));
				pthread_create(&server_thread, &stackSizeAttribute, &runner_t::server, this);
			}
		}
	}

	template <typename T, typename ARG, typename RESULT>
	server_t<T, ARG, RESULT>::~server_t ()
	{
		should_terminate = true;
		pthread_cond_signal(&cond);

		pthread_join(server_thread, NULL);
		pthread_cond_destroy(&cond);
		pthread_mutex_destroy(&requests_mutex);
		pthread_mutex_destroy(&results_mutex);
	}

	template <typename T, typename ARG, typename RESULT>
	size_t server_t<T, ARG, RESULT>::register_client (T* callback)
	{
		std::lock_guard<std::mutex> lock(client_lock);
		client_to_callback.emplace(next_client_key, callback);
		return next_client_key++;
	}

	template <typename T, typename ARG, typename RESULT>
	void server_t<T, ARG, RESULT>::unregister_client (size_t clientKey)
	{
		std::lock_guard<std::mutex> lock(client_lock);
		client_to_callback.erase(client_to_callback.find(clientKey));
		remove_requests(clientKey);
	}

	template <typename T, typename ARG, typename RESULT>
	void server_t<T, ARG, RESULT>::send_request (size_t clientKey, ARG const& request)
	{
		pthread_mutex_lock(&requests_mutex);
		requests.push_back(std::make_pair(clientKey, request));
		pthread_cond_signal(&cond);
		pthread_mutex_unlock(&requests_mutex);
	}

	template <typename T, typename ARG, typename RESULT>
	void server_t<T, ARG, RESULT>::remove_requests (size_t clientKey)
	{
		pthread_mutex_lock(&requests_mutex);
		requests.erase(std::remove_if(requests.begin(), requests.end(), std::bind2nd(equal_to_1st_helper_t(), clientKey)), requests.end());
		pthread_mutex_unlock(&requests_mutex);
	}

	template <typename T, typename ARG, typename RESULT>
	void server_t<T, ARG, RESULT>::server_run ()
	{
		char buf[64];
		snprintf(buf, sizeof(buf), "server (%s)", typeid(T).name());
		oak::set_thread_name(buf);

		pthread_mutex_lock(&requests_mutex);
		while(!should_terminate)
		{
			if(requests.empty())
				pthread_cond_wait(&cond, &requests_mutex);

			if(requests.empty())
				continue;

			std::pair<size_t, ARG> request = requests.front();
			requests.erase(requests.begin());

			pthread_mutex_unlock(&requests_mutex);
			RESULT const& result = T::handle_request(request.second);
			pthread_mutex_lock(&results_mutex);
			results.push_back(std::make_pair(request.first, result));
			pthread_mutex_unlock(&results_mutex);
			run_loop_source->signal();
			pthread_mutex_lock(&requests_mutex);
		}
		pthread_mutex_unlock(&requests_mutex);
	}

	template <typename T, typename ARG, typename RESULT>
	void server_t<T, ARG, RESULT>::master_run ()
	{
		pthread_mutex_lock(&results_mutex);
		std::vector< std::pair<size_t, RESULT>  > offload;
		results.swap(offload);
		pthread_mutex_unlock(&results_mutex);

		std::lock_guard<std::mutex> lock(client_lock);
		for(auto const& it : offload)
		{
			typename std::map<size_t, T*>::iterator client = client_to_callback.find(it.first);
			if(client != client_to_callback.end())
			{
				T* obj = client->second;
				client_lock.unlock();
				obj->handle_reply(it.second);
				client_lock.lock();
			}
		}
	}

} /* oak */ 

#endif /* end of include guard: OAK_SERVER_H_Q8M693GB */
