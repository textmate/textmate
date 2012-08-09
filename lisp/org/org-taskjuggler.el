;;; org-taskjuggler.el --- TaskJuggler exporter for org-mode
;;
;; Copyright (C) 2007-2012 Free Software Foundation, Inc.
;;
;; Emacs Lisp Archive Entry
;; Filename: org-taskjuggler.el
;; Author: Christian Egli
;; Maintainer: Christian Egli
;; Keywords: org, taskjuggler, project planning
;; Description: Converts an org-mode buffer into a taskjuggler project plan
;; URL:

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; This library implements a TaskJuggler exporter for org-mode.
;; TaskJuggler uses a text format to define projects, tasks and
;; resources, so it is a natural fit for org-mode. It can produce all
;; sorts of reports for tasks or resources in either HTML, CSV or PDF.
;; The current version of TaskJuggler requires KDE but the next
;; version is implemented in Ruby and should therefore run on any
;; platform.
;;
;; The exporter is a bit different from other exporters, such as the
;; HTML and LaTeX exporters for example, in that it does not export
;; all the nodes of a document or strictly follow the order of the
;; nodes in the document.
;;
;; Instead the TaskJuggler exporter looks for a tree that defines the
;; tasks and a optionally tree that defines the resources for this
;; project. It then creates a TaskJuggler file based on these trees
;; and the attributes defined in all the nodes.
;;
;; * Installation
;;
;; Put this file into your load-path and the following line into your
;; ~/.emacs:
;;
;;   (require 'org-taskjuggler)
;;
;; The interactive functions are similar to those of the HTML and LaTeX
;; exporters:
;;
;; M-x `org-export-as-taskjuggler'
;; M-x `org-export-as-taskjuggler-and-open'
;;
;; * Tasks
;;
;; Let's illustrate the usage with a small example. Create your tasks
;; as you usually do with org-mode. Assign efforts to each task using
;; properties (it's easiest to do this in the column view). You should
;; end up with something similar to the example by Peter Jones in
;; http://www.contextualdevelopment.com/static/artifacts/articles/2008/project-planning/project-planning.org.
;; Now mark the top node of your tasks with a tag named
;; "taskjuggler_project" (or whatever you customized
;; `org-export-taskjuggler-project-tag' to). You are now ready to
;; export the project plan with `org-export-as-taskjuggler-and-open'
;; which will export the project plan and open a Gantt chart in
;; TaskJugglerUI.
;;
;; * Resources
;;
;; Next you can define resources and assign those to work on specific
;; tasks. You can group your resources hierarchically. Tag the top
;; node of the resources with "taskjuggler_resource" (or whatever you
;; customized `org-export-taskjuggler-resource-tag' to). You can
;; optionally assign an identifier (named "resource_id") to the
;; resources (using the standard org properties commands) or you can
;; let the exporter generate identifiers automatically (the exporter
;; picks the first word of the headline as the identifier as long as
;; it is unique, see the documentation of
;; `org-taskjuggler-get-unique-id'). Using that identifier you can
;; then allocate resources to tasks. This is again done with the
;; "allocate" property on the tasks. Do this in column view or when on
;; the task type
;;
;;  C-c C-x p allocate RET <resource_id> RET
;;
;; Once the allocations are done you can again export to TaskJuggler
;; and check in the Resource Allocation Graph which person is working
;; on what task at what time.
;;
;; * Export of properties
;;
;; The exporter also takes TODO state information into consideration,
;; i.e. if a task is marked as done it will have the corresponding
;; attribute in TaskJuggler ("complete 100"). Also it will export any
;; property on a task resource or resource node which is known to
;; TaskJuggler, such as limits, vacation, shift, booking, efficiency,
;; journalentry, rate for resources or account, start, note, duration,
;; end, journalentry, milestone, reference, responsible, scheduling,
;; etc for tasks.
;;
;; * Dependencies
;;
;; The exporter will handle dependencies that are defined in the tasks
;; either with the ORDERED attribute (see TODO dependencies in the Org
;; mode manual) or with the BLOCKER attribute (see org-depend.el) or
;; alternatively with a depends attribute. Both the BLOCKER and the
;; depends attribute can be either "previous-sibling" or a reference
;; to an identifier (named "task_id") which is defined for another
;; task in the project. BLOCKER and the depends attribute can define
;; multiple dependencies separated by either space or comma. You can
;; also specify optional attributes on the dependency by simply
;; appending it. The following examples should illustrate this:
;;
;; * Training material
;;   :PROPERTIES:
;;   :task_id:  training_material
;;   :ORDERED:  t
;;   :END:
;; ** Markup Guidelines
;;    :PROPERTIES:
;;    :Effort:   2d
;;    :END:
;; ** Workflow Guidelines
;;    :PROPERTIES:
;;    :Effort:   2d
;;    :END:
;; * Presentation
;;   :PROPERTIES:
;;   :Effort:   2d
;;   :BLOCKER:  training_material { gapduration 1d } some_other_task
;;   :END:
;;
;;;; * TODO
;;   - Use SCHEDULED and DEADLINE information (not just start and end
;;     properties).
;;   - Look at org-file-properties, org-global-properties and
;;     org-global-properties-fixed
;;   - What about property inheritance and org-property-inherit-p?
;;   - Use TYPE_TODO as an way to assign resources
;;   - Make sure multiple dependency definitions (i.e. BLOCKER on
;;     previous-sibling and on a specific task_id) in multiple
;;     attributes are properly exported.
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'org)
(require 'org-exp)

;;; User variables:

(defgroup org-export-taskjuggler nil
  "Options for exporting Org-mode files to TaskJuggler."
  :tag "Org Export TaskJuggler"
  :group 'org-export)

(defcustom org-export-taskjuggler-extension ".tjp"
  "Extension of TaskJuggler files."
  :group 'org-export-taskjuggler
  :version "24.1"
  :type 'string)

(defcustom org-export-taskjuggler-project-tag "taskjuggler_project"
  "Tag, property or todo used to find the tree containing all
the tasks for the project."
  :group 'org-export-taskjuggler
  :version "24.1"
  :type 'string)

(defcustom org-export-taskjuggler-resource-tag "taskjuggler_resource"
  "Tag, property or todo used to find the tree containing all the
resources for the project."
  :group 'org-export-taskjuggler
  :version "24.1"
  :type 'string)

(defcustom org-export-taskjuggler-target-version 2.4
  "Which version of TaskJuggler the exporter is targeting."
  :group 'org-export-taskjuggler
  :version "24.1"
  :type 'number)

(defcustom org-export-taskjuggler-default-project-version "1.0"
  "Default version string for the project."
  :group 'org-export-taskjuggler
  :version "24.1"
  :type 'string)

(defcustom org-export-taskjuggler-default-project-duration 280
  "Default project duration if no start and end date have been defined
in the root node of the task tree, i.e. the tree that has been marked
with `org-export-taskjuggler-project-tag'"
  :group 'org-export-taskjuggler
  :version "24.1"
  :type 'integer)

(defcustom org-export-taskjuggler-default-reports
  '("taskreport \"Gantt Chart\" {
  headline \"Project Gantt Chart\"
  columns hierarchindex, name, start, end, effort, duration, completed, chart
  timeformat \"%Y-%m-%d\"
  hideresource 1
  loadunit shortauto
}"
"resourcereport \"Resource Graph\" {
  headline \"Resource Allocation Graph\"
  columns no, name, utilization, freeload, chart
  loadunit shortauto
  sorttasks startup
  hidetask ~isleaf()
}")
  "Default reports for the project."
  :group 'org-export-taskjuggler
  :version "24.1"
  :type '(repeat (string :tag "Report")))

(defcustom org-export-taskjuggler-default-global-properties
  "shift s40 \"Part time shift\" {
  workinghours wed, thu, fri off
}
"
  "Default global properties for the project. Here you typically
define global properties such as shifts, accounts, rates,
vacation, macros and flags. Any property that is allowed within
the TaskJuggler file can be inserted. You could for example
include another TaskJuggler file.

The global properties are inserted after the project declaration
but before any resource and task declarations."
  :group 'org-export-taskjuggler
  :version "24.1"
  :type '(string :tag "Preamble"))

;;; Hooks

(defvar org-export-taskjuggler-final-hook nil
  "Hook run at the end of TaskJuggler export, in the new buffer.")

;;; Autoload functions:

;; avoid compiler warning about free variable
(defvar org-export-taskjuggler-old-level)

;;;###autoload
(defun org-export-as-taskjuggler ()
  "Export parts of the current buffer as a TaskJuggler file.
The exporter looks for a tree with tag, property or todo that
matches `org-export-taskjuggler-project-tag' and takes this as
the tasks for this project. The first node of this tree defines
the project properties such as project name and project period.
If there is a tree with tag, property or todo that matches
`org-export-taskjuggler-resource-tag' this three is taken as
resources for the project. If no resources are specified, a
default resource is created and allocated to the project. Also
the taskjuggler project will be created with default reports as
defined in `org-export-taskjuggler-default-reports'."
  (interactive)

  (message "Exporting...")
  (setq-default org-done-keywords org-done-keywords)
  (let* ((tasks
	  (org-taskjuggler-resolve-dependencies
	   (org-taskjuggler-assign-task-ids
	    (org-taskjuggler-compute-task-leafiness
	     (org-map-entries
	      'org-taskjuggler-components
	      org-export-taskjuggler-project-tag nil 'archive 'comment)))))
	 (resources
	  (org-taskjuggler-assign-resource-ids
	   (org-map-entries
	    'org-taskjuggler-components
	    org-export-taskjuggler-resource-tag nil 'archive 'comment)))
	 (filename (expand-file-name
		    (concat
		     (file-name-sans-extension
		      (file-name-nondirectory buffer-file-name))
		     org-export-taskjuggler-extension)))
	 (buffer (find-file-noselect filename))
	 (old-buffer (current-buffer))
	 (org-export-taskjuggler-old-level 0)
	 task resource)
    (unless tasks
      (error "No tasks specified"))
    ;; add a default resource
    (unless resources
      (setq resources
	    `((("resource_id" . ,(user-login-name))
	       ("headline" . ,user-full-name)
	       ("level" . 1)))))
    ;; add a default allocation to the first task if none was given
    (unless (assoc "allocate" (car tasks))
      (let ((task (car tasks))
	    (resource-id (cdr (assoc "resource_id" (car resources)))))
	(setcar tasks (push (cons "allocate" resource-id) task))))
    ;; add a default start date to the first task if none was given
    (unless (assoc "start" (car tasks))
      (let ((task (car tasks))
	    (time-string (format-time-string "%Y-%m-%d")))
	(setcar tasks (push (cons "start" time-string) task))))
    ;; add a default version if none was given
    (unless (assoc "version" (car tasks))
      (let ((task (car tasks))
	    (version org-export-taskjuggler-default-project-version))
	(setcar tasks (push (cons "version" version) task))))
    (with-current-buffer buffer
      (erase-buffer)
      (org-clone-local-variables old-buffer "^org-")
      (org-taskjuggler-open-project (car tasks))
      (insert org-export-taskjuggler-default-global-properties)
      (insert "\n")
      (dolist (resource resources)
	(let ((level (cdr (assoc "level" resource))))
	  (org-taskjuggler-close-maybe level)
	  (org-taskjuggler-open-resource resource)
	  (setq org-export-taskjuggler-old-level level)))
      (org-taskjuggler-close-maybe 1)
      (setq org-export-taskjuggler-old-level 0)
      (dolist (task tasks)
	(let ((level (cdr (assoc "level" task))))
	  (org-taskjuggler-close-maybe level)
	  (org-taskjuggler-open-task task)
	  (setq org-export-taskjuggler-old-level level)))
      (org-taskjuggler-close-maybe 1)
      (org-taskjuggler-insert-reports)
      (save-buffer)
      (or (org-export-push-to-kill-ring "TaskJuggler")
	  (message "Exporting... done"))
      (current-buffer))))

;;;###autoload
(defun org-export-as-taskjuggler-and-open ()
  "Export the current buffer as a TaskJuggler file and open it
with the TaskJuggler GUI."
  (interactive)
  (let* ((file-name (buffer-file-name (org-export-as-taskjuggler)))
	 (process-name "TaskJugglerUI")
	 (command (concat process-name " " file-name)))
    (start-process-shell-command process-name nil command)))

(defun org-taskjuggler-targeting-tj3-p ()
  "Return true if we are targeting TaskJuggler III."
  (>= org-export-taskjuggler-target-version 3.0))

(defun org-taskjuggler-parent-is-ordered-p ()
  "Return true if the parent of the current node has a property
\"ORDERED\". Return nil otherwise."
  (save-excursion
    (and (org-up-heading-safe) (org-entry-get (point) "ORDERED"))))

(defun org-taskjuggler-components ()
  "Return an alist containing all the pertinent information for
the current node such as the headline, the level, todo state
information, all the properties, etc."
  (let* ((props (org-entry-properties))
	 (components (org-heading-components))
	 (level (nth 1 components))
	 (headline
	  (replace-regexp-in-string
	   "\"" "\\\"" (nth 4 components) t t)) ; quote double quotes in headlines
	 (parent-ordered (org-taskjuggler-parent-is-ordered-p)))
    (push (cons "level" level) props)
    (push (cons "headline" headline) props)
    (push (cons "parent-ordered" parent-ordered) props)))

(defun org-taskjuggler-assign-task-ids (tasks)
  "Given a list of tasks return the same list assigning a unique id
and the full path to each task. Taskjuggler takes hierarchical ids.
For that reason we have to make ids locally unique and we have to keep
a path to the current task."
  (let ((previous-level 0)
	unique-ids unique-id
	path
	task resolved-tasks tmp)
    (dolist (task tasks resolved-tasks)
      (let ((level (cdr (assoc "level" task))))
	(cond
	 ((< previous-level level)
	  (setq unique-id (org-taskjuggler-get-unique-id task (car unique-ids)))
	  (dotimes (tmp (- level previous-level))
	    (push (list unique-id) unique-ids)
	    (push unique-id path)))
	 ((= previous-level level)
	  (setq unique-id (org-taskjuggler-get-unique-id task (car unique-ids)))
	  (push unique-id (car unique-ids))
	  (setcar path unique-id))
	 ((> previous-level level)
	  (dotimes (tmp (- previous-level level))
	    (pop unique-ids)
	    (pop path))
	  (setq unique-id (org-taskjuggler-get-unique-id task (car unique-ids)))
	  (push unique-id (car unique-ids))
	  (setcar path unique-id)))
	(push (cons "unique-id" unique-id) task)
	(push (cons "path" (mapconcat 'identity (reverse path) ".")) task)
	(setq previous-level level)
	(setq resolved-tasks (append resolved-tasks (list task)))))))

(defun org-taskjuggler-compute-task-leafiness (tasks)
  "Figure out if each task is a leaf by looking at it's level,
and the level of its successor. If the successor is higher (ie
deeper), then it's not a leaf."
  (let (new-list)
    (while (car tasks)
      (let ((task (car tasks))
	    (successor (car (cdr tasks))))
	(cond
	 ;; if a task has no successors it is a leaf
	 ((null successor)
	  (push (cons (cons "leaf-node" t) task) new-list))
	 ;; if the successor has a lower level than task it is a leaf
	 ((<= (cdr (assoc "level" successor)) (cdr (assoc "level" task)))
	  (push (cons (cons "leaf-node" t) task) new-list))
	 ;; otherwise examine the rest of the tasks
	 (t (push task new-list))))
      (setq tasks (cdr tasks)))
    (nreverse new-list)))

(defun org-taskjuggler-assign-resource-ids (resources)
  "Given a list of resources return the same list, assigning a
unique id to each resource."
  (let (unique-ids new-list)
    (dolist (resource resources new-list)
      (let ((unique-id (org-taskjuggler-get-unique-id resource unique-ids)))
	(push (cons "unique-id" unique-id) resource)
	(push unique-id unique-ids)
	(push resource new-list)))
    (nreverse new-list)))

(defun org-taskjuggler-resolve-dependencies (tasks)
  (let ((previous-level 0)
	siblings
	task resolved-tasks)
    (dolist (task tasks resolved-tasks)
      (let* ((level (cdr (assoc "level" task)))
	     (depends (cdr (assoc "depends" task)))
	     (parent-ordered (cdr (assoc "parent-ordered" task)))
	     (blocker (cdr (assoc "BLOCKER" task)))
	     (blocked-on-previous
	      (and blocker (string-match "previous-sibling" blocker)))
	     (dependencies
	      (org-taskjuggler-resolve-explicit-dependencies
	       (append
		(and depends (org-taskjuggler-tokenize-dependencies depends))
		(and blocker (org-taskjuggler-tokenize-dependencies blocker)))
	       tasks))
	      previous-sibling)
	; update previous sibling info
	(cond
	 ((< previous-level level)
	  (dotimes (tmp (- level previous-level))
	    (push task siblings)))
	 ((= previous-level level)
	  (setq previous-sibling (car siblings))
	  (setcar siblings task))
	 ((> previous-level level)
	  (dotimes (tmp (- previous-level level))
	    (pop siblings))
	  (setq previous-sibling (car siblings))
	  (setcar siblings task)))
	; insert a dependency on previous sibling if the parent is
	; ordered or if the tasks has a BLOCKER attribute with value "previous-sibling"
	(when (or (and previous-sibling parent-ordered) blocked-on-previous)
	  (push (format "!%s" (cdr (assoc "unique-id" previous-sibling))) dependencies))
	; store dependency information
	(when dependencies
	  (push (cons "depends" (mapconcat 'identity dependencies ", ")) task))
	(setq previous-level level)
	(setq resolved-tasks (append resolved-tasks (list task)))))))

(defun org-taskjuggler-tokenize-dependencies (dependencies)
  "Split a dependency property value DEPENDENCIES into the
individual dependencies and return them as a list while keeping
the optional arguments (such as gapduration) for the
dependencies. A dependency will have to match `[-a-zA-Z0-9_]+'."
  (cond
   ((string-match "^ *$" dependencies) nil)
   ((string-match "^[ \t]*\\([-a-zA-Z0-9_]+\\([ \t]*{[^}]+}\\)?\\)[ \t,]*" dependencies)
    (cons
     (substring dependencies (match-beginning 1) (match-end 1))
     (org-taskjuggler-tokenize-dependencies (substring dependencies (match-end 0)))))
   (t (error (format "invalid dependency id %s" dependencies)))))

(defun org-taskjuggler-resolve-explicit-dependencies (dependencies tasks)
  "For each dependency in DEPENDENCIES try to find a
corresponding task with a matching property \"task_id\" in TASKS.
Return a list containing the resolved links for all DEPENDENCIES
where a matching tasks was found. If the dependency is
\"previous-sibling\" it is ignored (as this is dealt with in
`org-taskjuggler-resolve-dependencies'). If there is no matching
task the dependency is ignored and a warning is displayed ."
  (unless (null dependencies)
    (let*
	;; the dependency might have optional attributes such as "{
	;; gapduration 5d }", so only use the first string as id for the
	;; dependency
	((dependency (car dependencies))
	 (id (car (split-string dependency)))
	 (optional-attributes
	  (mapconcat 'identity (cdr (split-string dependency)) " "))
	 (path (org-taskjuggler-find-task-with-id id tasks)))
      (cond
       ;; ignore previous sibling dependencies
       ((equal (car dependencies) "previous-sibling")
	(org-taskjuggler-resolve-explicit-dependencies (cdr dependencies) tasks))
       ;; if the id is found in another task use its path
       ((not (null path))
	(cons (mapconcat 'identity (list path optional-attributes) " ")
	      (org-taskjuggler-resolve-explicit-dependencies
	       (cdr dependencies) tasks)))
       ;; warn about dangling dependency but otherwise ignore it
       (t (display-warning
	   'org-export-taskjuggler
	   (format "No task with matching property \"task_id\" found for id %s" id))
	  (org-taskjuggler-resolve-explicit-dependencies (cdr dependencies) tasks))))))

(defun org-taskjuggler-find-task-with-id (id tasks)
  "Find ID in tasks. If found return the path of task. Otherwise
return nil."
  (let ((task-id (cdr (assoc "task_id" (car tasks))))
	(path (cdr (assoc "path" (car tasks)))))
    (cond
     ((null tasks) nil)
     ((equal task-id id) path)
     (t (org-taskjuggler-find-task-with-id id (cdr tasks))))))

(defun org-taskjuggler-get-unique-id (item unique-ids)
  "Return a unique id for an ITEM which can be a task or a resource.
The id is derived from the headline and made unique against
UNIQUE-IDS. If the (downcased) first token of the headline is not
unique try to add more (downcased) tokens of the headline or
finally add more underscore characters (\"_\")."
  (let* ((headline (cdr (assoc "headline" item)))
	 (parts (split-string headline))
	 (id (org-taskjuggler-clean-id (downcase (pop parts)))))
    ; try to add more parts of the headline to make it unique
    (while (and (member id unique-ids) (car parts))
      (setq id (concat id "_" (org-taskjuggler-clean-id (downcase (pop parts))))))
    ; if its still not unique add "_"
    (while (member id unique-ids)
      (setq id (concat id "_")))
    id))

(defun org-taskjuggler-clean-id (id)
  "Clean and return ID to make it acceptable for taskjuggler."
  (and id
       ;; replace non-ascii by _
       (replace-regexp-in-string
	"[^a-zA-Z0-9_]" "_"
	;; make sure id doesn't start with a number
	(replace-regexp-in-string "^\\([0-9]\\)" "_\\1" id))))

(defun org-taskjuggler-open-project (project)
  "Insert the beginning of a project declaration. All valid
attributes from the PROJECT alist are inserted. If no end date is
specified it is calculated
`org-export-taskjuggler-default-project-duration' days from now."
  (let* ((unique-id (cdr (assoc "unique-id" project)))
	 (headline (cdr (assoc "headline" project)))
	 (version (cdr (assoc "version" project)))
	 (start (cdr (assoc "start" project)))
	 (end (cdr (assoc "end" project))))
    (insert
     (format "project %s \"%s\" \"%s\" %s +%sd {\n }\n"
	     unique-id headline version start
	     org-export-taskjuggler-default-project-duration))))

(defun org-taskjuggler-filter-and-join (items)
  "Filter all nil elements from ITEMS and join the remaining ones
with separator \"\n\"."
  (let ((filtered-items (remq nil items)))
    (and filtered-items (mapconcat 'identity filtered-items "\n"))))

(defun org-taskjuggler-get-attributes (item attributes)
  "Return all attribute as a single formatted string. ITEM is an
alist representing either a resource or a task. ATTRIBUTES is a
list of symbols. Only entries from ITEM are considered that are
listed in ATTRIBUTES."
  (org-taskjuggler-filter-and-join
   (mapcar
    (lambda (attribute)
      (org-taskjuggler-filter-and-join
       (org-taskjuggler-get-attribute item attribute)))
    attributes)))

(defun org-taskjuggler-get-attribute (item attribute)
  "Return a list of strings containing the properly formatted
taskjuggler declaration for a given ATTRIBUTE in ITEM (an alist).
If the ATTRIBUTE is not in ITEM return nil."
  (cond
   ((null item) nil)
   ((equal (symbol-name attribute) (car (car item)))
    (cons (format "%s %s" (symbol-name attribute) (cdr (car item)))
	  (org-taskjuggler-get-attribute (cdr item) attribute)))
   (t (org-taskjuggler-get-attribute (cdr item) attribute))))

(defun org-taskjuggler-open-resource (resource)
  "Insert the beginning of a resource declaration. All valid
attributes from the RESOURCE alist are inserted. If the RESOURCE
defines a property \"resource_id\" it will be used as the id for
this resource. Otherwise it will use the ID property. If neither
is defined it will calculate a unique id for the resource using
`org-taskjuggler-get-unique-id'."
  (let ((id (org-taskjuggler-clean-id
	     (or (cdr (assoc "resource_id" resource))
		 (cdr (assoc "ID" resource))
		 (cdr (assoc "unique-id" resource)))))
	(headline (cdr (assoc "headline" resource)))
	(attributes '(limits vacation shift booking efficiency journalentry rate)))
    (insert
     (concat
      "resource " id " \"" headline "\" {\n "
      (org-taskjuggler-get-attributes resource attributes) "\n"))))

(defun org-taskjuggler-clean-effort (effort)
  "Translate effort strings into a format acceptable to taskjuggler,
i.e. REAL UNIT. A valid effort string can be anything that is
accepted by `org-duration-string-to-minutes´."
  (cond
   ((null effort) effort)
   (t (let* ((minutes (org-duration-string-to-minutes effort))
	     (hours (/ minutes 60.0)))
	(format "%.1fh" hours)))))

(defun org-taskjuggler-get-priority (priority)
  "Return a priority between 1 and 1000 based on PRIORITY, an
org-mode priority string."
  (max 1 (/ (* 1000 (- org-lowest-priority (string-to-char priority)))
	    (- org-lowest-priority org-highest-priority))))

(defun org-taskjuggler-open-task (task)
  (let* ((unique-id (cdr (assoc "unique-id" task)))
	 (headline (cdr (assoc "headline" task)))
	 (effort (org-taskjuggler-clean-effort (cdr (assoc org-effort-property task))))
	 (depends (cdr (assoc "depends" task)))
	 (allocate (cdr (assoc "allocate" task)))
	 (priority-raw (cdr (assoc "PRIORITY" task)))
	 (priority (and priority-raw (org-taskjuggler-get-priority priority-raw)))
	 (state (cdr (assoc "TODO" task)))
	 (complete (or (and (member state org-done-keywords) "100")
		       (cdr (assoc "complete" task))))
	 (parent-ordered (cdr (assoc "parent-ordered" task)))
	 (previous-sibling (cdr (assoc "previous-sibling" task)))
	 (milestone (or (cdr (assoc "milestone" task))
			(and (assoc "leaf-node" task)
			     (not (or effort
				      (cdr (assoc "duration" task))
				      (cdr (assoc "end" task))
				      (cdr (assoc "period" task)))))))
	 (attributes
	  '(account start note duration endbuffer endcredit end
		    flags journalentry length maxend maxstart minend
		    minstart period reference responsible scheduling
		    startbuffer startcredit statusnote)))
    (insert
     (concat
      "task " unique-id " \"" headline "\" {\n"
      (if (and parent-ordered previous-sibling)
	  (format " depends %s\n" previous-sibling)
	(and depends (format " depends %s\n" depends)))
      (and allocate (format " purge %s\n allocate %s\n"
			    (or (and (org-taskjuggler-targeting-tj3-p) "allocate")
				"allocations")
			    allocate))
      (and complete (format " complete %s\n" complete))
      (and effort (format " effort %s\n" effort))
      (and priority (format " priority %s\n" priority))
      (and milestone (format " milestone\n"))

      (org-taskjuggler-get-attributes task attributes)
      "\n"))))

(defun org-taskjuggler-close-maybe (level)
  (while (> org-export-taskjuggler-old-level level)
    (insert "}\n")
    (setq org-export-taskjuggler-old-level (1- org-export-taskjuggler-old-level)))
  (when (= org-export-taskjuggler-old-level level)
    (insert "}\n")))

(defun org-taskjuggler-insert-reports ()
  (let (report)
    (dolist (report org-export-taskjuggler-default-reports)
      (insert report "\n"))))

(provide 'org-taskjuggler)

;;; org-taskjuggler.el ends here
