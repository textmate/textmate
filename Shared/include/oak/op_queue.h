#ifndef OAK_OP_QUEUE_H_TS01SAAQ
#define OAK_OP_QUEUE_H_TS01SAAQ

#include "oak.h"

namespace op_queue
{
	struct task_t
	{
		WATCH_LEAKS(op_queue::task_t);

		task_t () : state(waiting)      { }
		virtual ~task_t ()              { }
		virtual void was_unscheduled () { }
		virtual void run () = 0;

		enum state_t { waiting, running, failed, succeeded };

	protected:
		size_t task_id;
		state_t state;
		void set_state (state_t state) const;
	private:
		friend struct manager_t;

		std::set<size_t> dependencies;

		bool depends_on (std::set<size_t> const& other) const
		{
			std::vector<size_t> tmp;
			std::set_intersection(dependencies.begin(), dependencies.end(), other.begin(), other.end(), back_inserter(tmp));
			return !tmp.empty();
		}
	};

	typedef std::tr1::shared_ptr<task_t> task_ptr;

	struct manager_t
	{
		manager_t () : next_task_id(1) { }

		size_t add_task (task_ptr newTask, std::set<size_t> const& dependencies)
		{
			std::set<size_t> pendingTasks;
			iterate(task, queue)
				pendingTasks.insert((*task)->task_id);

			std::vector<size_t> newDependencies;
			std::set_intersection(pendingTasks.begin(), pendingTasks.end(), dependencies.begin(), dependencies.end(), back_inserter(newDependencies));

			newTask->task_id      = next_task_id++;
			newTask->dependencies = std::set<size_t>(newDependencies.begin(), newDependencies.end());
			queue.push_back(newTask);

			run();

			return newTask->task_id;
		}

		bool has_completed (size_t taskId) const
		{
			iterate(task, queue)
			{
				if((*task)->task_id == taskId)
					return (*task)->state != task_t::waiting && (*task)->state != task_t::running;
			}
			return true;
		}

		void run ()
		{
			iterate(task, queue)
			{
				if((*task)->dependencies.empty() && (*task)->state == task_t::waiting)
				{
					(*task)->state = task_t::running;
					return (*task)->run();
				}
			}
		}

		void set_state (size_t taskId, task_t::state_t state)
		{
			iterate(task, queue)
			{
				if((*task)->task_id == taskId)
					(*task)->state = state;
			}
			prune();
			run();
		}

	private:
		void prune ()
		{
			std::set<size_t> failed, succeeded;
			iterate(task, queue)
			{
				if((*task)->state == task_t::succeeded)
					succeeded.insert((*task)->task_id);
				else if((*task)->state == task_t::failed)
					failed.insert((*task)->task_id);
			}

			size_t oldFailedSize;
			do {

				oldFailedSize = failed.size();
				iterate(task, queue)
				{
					if((*task)->state != task_t::failed && (*task)->depends_on(failed))
					{
						(*task)->was_unscheduled();
						(*task)->state = task_t::failed;
						failed.insert((*task)->task_id);
					}
				}

			} while(oldFailedSize != failed.size());

			succeeded.insert(failed.begin(), failed.end());
			for(size_t i = queue.size(); i > 0; --i)
			{
				if(succeeded.find(queue[i-1]->task_id) != succeeded.end())
					queue.erase(queue.begin() + i-1, queue.begin() + i);
			}

			iterate(task, queue)
			{
				std::vector<size_t> tmp;
				std::set_difference((*task)->dependencies.begin(), (*task)->dependencies.end(), succeeded.begin(), succeeded.end(), back_inserter(tmp));
				(*task)->dependencies = std::set<size_t>(tmp.begin(), tmp.end());
			}
		}

		std::vector<task_ptr> queue;
		size_t next_task_id;
	};

	extern manager_t manager;

	template <typename T> size_t add_task (T const& task, size_t dependency)                                         { return manager.add_task(task_ptr(new T(task)), std::set<size_t>(&dependency, &dependency + 1)); }
	template <typename T> size_t add_task (T const& task, std::set<size_t> const& dependencies = std::set<size_t>()) { return manager.add_task(task_ptr(new T(task)), dependencies); }
	inline bool has_completed (size_t taskId)                                                                        { return manager.has_completed(taskId); }

	inline void task_t::set_state (state_t state) const { manager.set_state(task_id, state); }

} /* op_queue */

#endif /* end of include guard: OAK_OP_QUEUE_H_TS01SAAQ */
