;;; vc-dir.el --- Directory status display under VC

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author:   Dan Nicolaescu <dann@ics.uci.edu>
;; Keywords: vc tools
;; Package: vc

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

;;; Credits:

;; The original VC directory status implementation was based on dired.
;; This implementation was inspired by PCL-CVS.
;; Many people contributed comments, ideas and code to this
;; implementation.  These include:
;;
;;   Alexandre Julliard  <julliard@winehq.org>
;;   Stefan Monnier  <monnier@iro.umontreal.ca>
;;   Tom Tromey  <tromey@redhat.com>

;;; Commentary:
;;

;;; Todo:  see vc.el.

(require 'vc-hooks)
(require 'vc)
(require 'tool-bar)
(require 'ewoc)

;;; Code:
(eval-when-compile
  (require 'cl))

(defcustom vc-dir-mode-hook nil
  "Normal hook run by `vc-dir-mode'.
See `run-hooks'."
  :type 'hook
  :group 'vc)

;; Used to store information for the files displayed in the directory buffer.
;; Each item displayed corresponds to one of these defstructs.
(defstruct (vc-dir-fileinfo
            (:copier nil)
            (:type list)            ;So we can use `member' on lists of FIs.
            (:constructor
             ;; We could define it as an alias for `list'.
	     vc-dir-create-fileinfo (name state &optional extra marked directory))
            (:conc-name vc-dir-fileinfo->))
  name                                  ;Keep it as first, for `member'.
  state
  ;; For storing backend specific information.
  extra
  marked
  ;; To keep track of not updated files during a global refresh
  needs-update
  ;; To distinguish files and directories.
  directory)

(defvar vc-ewoc nil)

(defvar vc-dir-process-buffer nil
  "The buffer used for the asynchronous call that computes status.")

(defvar vc-dir-backend nil
  "The backend used by the current *vc-dir* buffer.")

(defun vc-dir-move-to-goal-column ()
  ;; Used to keep the cursor on the file name column.
  (beginning-of-line)
  (unless (eolp)
    ;; Must be in sync with vc-default-dir-printer.
    (forward-char 25)))

(defun vc-dir-prepare-status-buffer (bname dir backend &optional create-new)
  "Find a buffer named BNAME showing DIR, or create a new one."
  (setq dir (file-name-as-directory (expand-file-name dir)))
  (let* ;; Look for another buffer name BNAME visiting the same directory.
      ((buf (save-excursion
              (unless create-new
                (dolist (buffer vc-dir-buffers)
                  (when (buffer-live-p buffer)
                    (set-buffer buffer)
                    (when (and (derived-mode-p 'vc-dir-mode)
                               (eq vc-dir-backend backend)
                               (string= default-directory dir))
                      (return buffer))))))))
    (or buf
        ;; Create a new buffer named BNAME.
	;; We pass a filename to create-file-buffer because it is what
	;; the function expects, and also what uniquify needs (if active)
        (with-current-buffer (create-file-buffer (expand-file-name bname dir))
          (setq default-directory dir)
          (vc-setup-buffer (current-buffer))
          ;; Reset the vc-parent-buffer-name so that it does not appear
          ;; in the mode-line.
          (setq vc-parent-buffer-name nil)
          (current-buffer)))))

(defvar vc-dir-menu-map
  (let ((map (make-sparse-keymap "VC-dir")))
    (define-key map [quit]
      '(menu-item "Quit" quit-window
		  :help "Quit"))
    (define-key map [kill]
      '(menu-item "Kill Update Command" vc-dir-kill-dir-status-process
		  :enable (vc-dir-busy)
		  :help "Kill the command that updates the directory buffer"))
    (define-key map [refresh]
      '(menu-item "Refresh" revert-buffer
		  :enable (not (vc-dir-busy))
		  :help "Refresh the contents of the directory buffer"))
    (define-key map [remup]
      '(menu-item "Hide Up-to-date" vc-dir-hide-up-to-date
		  :help "Hide up-to-date items from display"))
    ;; Movement.
    (define-key map [sepmv] '("--"))
    (define-key map [next-line]
      '(menu-item "Next Line" vc-dir-next-line
		  :help "Go to the next line" :keys "n"))
    (define-key map [previous-line]
      '(menu-item "Previous Line" vc-dir-previous-line
		  :help "Go to the previous line"))
    ;; Marking.
    (define-key map [sepmrk] '("--"))
    (define-key map [unmark-all]
      '(menu-item "Unmark All" vc-dir-unmark-all-files
		  :help "Unmark all files that are in the same state as the current file\
\nWith prefix argument unmark all files"))
    (define-key map [unmark-previous]
      '(menu-item "Unmark Previous " vc-dir-unmark-file-up
		  :help "Move to the previous line and unmark the file"))

    (define-key map [mark-all]
      '(menu-item "Mark All" vc-dir-mark-all-files
		  :help "Mark all files that are in the same state as the current file\
\nWith prefix argument mark all files"))
    (define-key map [unmark]
      '(menu-item "Unmark" vc-dir-unmark
		  :help "Unmark the current file or all files in the region"))

    (define-key map [mark]
      '(menu-item "Mark" vc-dir-mark
		  :help "Mark the current file or all files in the region"))

    (define-key map [sepopn] '("--"))
    (define-key map [qr]
      '(menu-item "Query Replace in Files..." vc-dir-query-replace-regexp
		  :help "Replace a string in the marked files"))
    (define-key map [se]
      '(menu-item "Search Files..." vc-dir-search
		  :help "Search a regexp in the marked files"))
    (define-key map [ires]
      '(menu-item "Isearch Regexp Files..." vc-dir-isearch-regexp
		  :help "Incremental search a regexp in the marked files"))
    (define-key map [ise]
      '(menu-item "Isearch Files..." vc-dir-isearch
		  :help "Incremental search a string in the marked files"))
    (define-key map [open-other]
      '(menu-item "Open in Other Window" vc-dir-find-file-other-window
		  :help "Find the file on the current line, in another window"))
    (define-key map [open]
      '(menu-item "Open File" vc-dir-find-file
		  :help "Find the file on the current line"))
    (define-key map [sepvcdet] '("--"))
    ;; FIXME: This needs a key binding.  And maybe a better name
    ;; ("Insert" like PCL-CVS uses does not sound that great either)...
    (define-key map [ins]
      '(menu-item "Show File" vc-dir-show-fileentry
		  :help "Show a file in the VC status listing even though it might be up to date"))
    (define-key map [annotate]
      '(menu-item "Annotate" vc-annotate
		  :help "Display the edit history of the current file using colors"))
    (define-key map [diff]
      '(menu-item "Compare with Base Version" vc-diff
		  :help "Compare file set with the base version"))
    (define-key map [logo]
      '(menu-item "Show Outgoing Log" vc-log-outgoing
		  :help "Show a log of changes that will be sent with a push operation"))
    (define-key map [logi]
      '(menu-item "Show Incoming Log" vc-log-incoming
		  :help "Show a log of changes that will be received with a pull operation"))
    (define-key map [log]
      '(menu-item "Show History" vc-print-log
		  :help "List the change log of the current file set in a window"))
    (define-key map [rlog]
      '(menu-item "Show Top of the Tree History " vc-print-root-log
		  :help "List the change log for the current tree in a window"))
    ;; VC commands.
    (define-key map [sepvccmd] '("--"))
    (define-key map [update]
      '(menu-item "Update to Latest Version" vc-update
		  :help "Update the current fileset's files to their tip revisions"))
    (define-key map [revert]
      '(menu-item "Revert to Base Version" vc-revert
		  :help "Revert working copies of the selected fileset to their repository contents."))
    (define-key map [next-action]
      ;; FIXME: This really really really needs a better name!
      ;; And a key binding too.
      '(menu-item "Check In/Out" vc-next-action
		  :help "Do the next logical version control operation on the current fileset"))
    (define-key map [register]
      '(menu-item "Register" vc-register
		  :help "Register file set into the version control system"))
    map)
  "Menu for VC dir.")

;; VC backends can use this to add mode-specific menu items to
;; vc-dir-menu-map.
(defun vc-dir-menu-map-filter (orig-binding)
  (when (and (symbolp orig-binding) (fboundp orig-binding))
    (setq orig-binding (indirect-function orig-binding)))
  (let ((ext-binding
         (when (derived-mode-p 'vc-dir-mode)
	   (vc-call-backend vc-dir-backend 'extra-status-menu))))
    (if (null ext-binding)
	orig-binding
      (append orig-binding
	      '("----")
	      ext-binding))))

(defvar vc-dir-mode-map
  (let ((map (make-sparse-keymap)))
    ;; VC commands
    (define-key map "v" 'vc-next-action)   ;; C-x v v
    (define-key map "=" 'vc-diff)	   ;; C-x v =
    (define-key map "i" 'vc-register)	   ;; C-x v i
    (define-key map "+" 'vc-update)	   ;; C-x v +
    (define-key map "l" 'vc-print-log)	   ;; C-x v l
    ;; More confusing than helpful, probably
    ;;(define-key map "R" 'vc-revert) ;; u is taken by vc-dir-unmark.
    ;;(define-key map "A" 'vc-annotate) ;; g is taken by revert-buffer
    ;;                                     bound by `special-mode'.
    ;; Marking.
    (define-key map "m" 'vc-dir-mark)
    (define-key map "M" 'vc-dir-mark-all-files)
    (define-key map "u" 'vc-dir-unmark)
    (define-key map "U" 'vc-dir-unmark-all-files)
    (define-key map "\C-?" 'vc-dir-unmark-file-up)
    (define-key map "\M-\C-?" 'vc-dir-unmark-all-files)
    ;; Movement.
    (define-key map "n" 'vc-dir-next-line)
    (define-key map " " 'vc-dir-next-line)
    (define-key map "\t" 'vc-dir-next-directory)
    (define-key map "p" 'vc-dir-previous-line)
    (define-key map [backtab] 'vc-dir-previous-directory)
    ;;; Rebind paragraph-movement commands.
    (define-key map "\M-}" 'vc-dir-next-directory)
    (define-key map "\M-{" 'vc-dir-previous-directory)
    (define-key map [C-down] 'vc-dir-next-directory)
    (define-key map [C-up] 'vc-dir-previous-directory)
    ;; The remainder.
    (define-key map "f" 'vc-dir-find-file)
    (define-key map "e" 'vc-dir-find-file) ; dired-mode compatibility
    (define-key map "\C-m" 'vc-dir-find-file)
    (define-key map "o" 'vc-dir-find-file-other-window)
    (define-key map "\C-c\C-c" 'vc-dir-kill-dir-status-process)
    (define-key map [down-mouse-3] 'vc-dir-menu)
    (define-key map [mouse-2] 'vc-dir-toggle-mark)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "x" 'vc-dir-hide-up-to-date)
    (define-key map [?\C-k] 'vc-dir-kill-line)
    (define-key map "S" 'vc-dir-search) ;; FIXME: Maybe use A like dired?
    (define-key map "Q" 'vc-dir-query-replace-regexp)
    (define-key map (kbd "M-s a C-s")   'vc-dir-isearch)
    (define-key map (kbd "M-s a M-C-s") 'vc-dir-isearch-regexp)

    ;; Hook up the menu.
    (define-key map [menu-bar vc-dir-mode]
      `(menu-item
	;; VC backends can use this to add mode-specific menu items to
	;; vc-dir-menu-map.
	"VC-dir" ,vc-dir-menu-map :filter vc-dir-menu-map-filter))
    map)
  "Keymap for directory buffer.")

(defmacro vc-dir-at-event (event &rest body)
  "Evaluate BODY with point located at event-start of EVENT.
If BODY uses EVENT, it should be a variable,
 otherwise it will be evaluated twice."
  (let ((posn (make-symbol "vc-dir-at-event-posn")))
    `(save-excursion
       (unless (equal ,event '(tool-bar))
         (let ((,posn (event-start ,event)))
           (set-buffer (window-buffer (posn-window ,posn)))
           (goto-char (posn-point ,posn))))
       ,@body)))

(defun vc-dir-menu (e)
  "Popup the VC dir menu."
  (interactive "e")
  (vc-dir-at-event e (popup-menu vc-dir-menu-map e)))

(defvar vc-dir-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item-from-menu 'find-file "new" map nil
				   :label "New File" :vert-only t)
    (tool-bar-local-item-from-menu 'menu-find-file-existing "open" map nil
				   :label "Open" :vert-only t)
    (tool-bar-local-item-from-menu 'dired "diropen" map nil
				   :vert-only t)
    (tool-bar-local-item-from-menu 'quit-window "close" map vc-dir-mode-map
				   :vert-only t)
    (tool-bar-local-item-from-menu 'vc-next-action "saveas" map
				   vc-dir-mode-map :label "Commit")
    (tool-bar-local-item-from-menu 'vc-print-log "info"
    				   map vc-dir-mode-map
				   :label "Log")
    (define-key-after map [separator-1] menu-bar-separator)
    (tool-bar-local-item-from-menu 'vc-dir-kill-dir-status-process "cancel"
				   map vc-dir-mode-map
				   :label "Stop" :vert-only t)
    (tool-bar-local-item-from-menu 'revert-buffer "refresh"
				   map vc-dir-mode-map :vert-only t)
    (define-key-after map [separator-2] menu-bar-separator)
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [cut])
				   "cut" map nil :vert-only t)
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [copy])
				   "copy" map nil :vert-only t)
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [paste])
				   "paste" map nil :vert-only t)
    (define-key-after map [separator-3] menu-bar-separator)
    (tool-bar-local-item-from-menu 'isearch-forward
    				   "search" map nil
				   :label "Search" :vert-only t)
    map))

(defun vc-dir-node-directory (node)
  ;; Compute the directory for NODE.
  ;; If it's a directory node, get it from the node.
  (let ((data (ewoc-data node)))
    (or (vc-dir-fileinfo->directory data)
	;; Otherwise compute it from the file name.
	(file-name-directory
	 (directory-file-name
	  (expand-file-name
	   (vc-dir-fileinfo->name data)))))))

(defun vc-dir-update (entries buffer &optional noinsert)
  "Update BUFFER's ewoc from the list of ENTRIES.
If NOINSERT, ignore elements on ENTRIES which are not in the ewoc."
  ;; Add ENTRIES to the vc-dir buffer BUFFER.
  (with-current-buffer buffer
    ;; Insert the entries sorted by name into the ewoc.
    ;; We assume the ewoc is sorted too, which should be the
    ;; case if we always add entries with vc-dir-update.
    (setq entries
	  ;; Sort: first files and then subdirectories.
	  ;; XXX: this is VERY inefficient, it computes the directory
	  ;; names too many times
	  (sort entries
		(lambda (entry1 entry2)
		  (let ((dir1 (file-name-directory
			        (directory-file-name (expand-file-name (car entry1)))))
			(dir2 (file-name-directory
			       (directory-file-name (expand-file-name (car entry2))))))
		    (cond
		     ((string< dir1 dir2) t)
		     ((not (string= dir1 dir2)) nil)
		     ((string< (car entry1) (car entry2))))))))
    ;; Insert directory entries in the right places.
    (let ((entry (car entries))
	  (node (ewoc-nth vc-ewoc 0))
	  (to-remove nil)
	  (dotname (file-relative-name default-directory)))
      ;; Insert . if it is not present.
      (unless node
	(ewoc-enter-last
	 vc-ewoc (vc-dir-create-fileinfo
		  dotname nil nil nil default-directory))
	(setq node (ewoc-nth vc-ewoc 0)))

      (while (and entry node)
	(let* ((entryfile (car entry))
	       (entrydir (file-name-directory (directory-file-name
					       (expand-file-name entryfile))))
	       (nodedir (vc-dir-node-directory node)))
	  (cond
	   ;; First try to find the directory.
	   ((string-lessp nodedir entrydir)
	    (setq node (ewoc-next vc-ewoc node)))
	   ((string-equal nodedir entrydir)
	    ;; Found the directory, find the place for the file name.
	    (let ((nodefile (vc-dir-fileinfo->name (ewoc-data node))))
	      (cond
	       ((string= nodefile dotname)
		(setq node (ewoc-next vc-ewoc node)))
	       ((string-lessp nodefile entryfile)
		(setq node (ewoc-next vc-ewoc node)))
	       ((string-equal nodefile entryfile)
		(if (nth 1 entry)
		    (progn
		      (setf (vc-dir-fileinfo->state (ewoc-data node)) (nth 1 entry))
		      (setf (vc-dir-fileinfo->extra (ewoc-data node)) (nth 2 entry))
		      (setf (vc-dir-fileinfo->needs-update (ewoc-data node)) nil)
		      (ewoc-invalidate vc-ewoc node))
		  ;; If the state is nil, the file does not exist
		  ;; anymore, so remember the entry so we can remove
		  ;; it after we are done inserting all ENTRIES.
		  (push node to-remove))
		(setq entries (cdr entries))
		(setq entry (car entries))
		(setq node (ewoc-next vc-ewoc node)))
	       (t
		(unless noinsert
		  (ewoc-enter-before vc-ewoc node
				     (apply 'vc-dir-create-fileinfo entry)))
		(setq entries (cdr entries))
		(setq entry (car entries))))))
	   (t
	    (unless noinsert
	      ;; We might need to insert a directory node if the
	      ;; previous node was in a different directory.
	      (let* ((rd (file-relative-name entrydir))
		     (prev-node (ewoc-prev vc-ewoc node))
		     (prev-dir (vc-dir-node-directory prev-node)))
		(unless (string-equal entrydir prev-dir)
		  (ewoc-enter-before
		   vc-ewoc node (vc-dir-create-fileinfo rd nil nil nil entrydir))))
	      ;; Now insert the node itself.
	      (ewoc-enter-before vc-ewoc node
				 (apply 'vc-dir-create-fileinfo entry)))
	    (setq entries (cdr entries) entry (car entries))))))
      ;; We're past the last node, all remaining entries go to the end.
      (unless (or node noinsert)
	(let ((lastdir (vc-dir-node-directory (ewoc-nth vc-ewoc -1))))
	  (dolist (entry entries)
	    (let ((entrydir (file-name-directory
			     (directory-file-name (expand-file-name (car entry))))))
	      ;; Insert a directory node if needed.
	      (unless (string-equal lastdir entrydir)
		(setq lastdir entrydir)
		(let ((rd (file-relative-name entrydir)))
		  (ewoc-enter-last
		   vc-ewoc (vc-dir-create-fileinfo rd nil nil nil entrydir))))
	      ;; Now insert the node itself.
	      (ewoc-enter-last vc-ewoc
			       (apply 'vc-dir-create-fileinfo entry))))))
      (when to-remove
	(let ((inhibit-read-only t))
	  (apply 'ewoc-delete vc-ewoc (nreverse to-remove)))))))

(defun vc-dir-busy ()
  (and (buffer-live-p vc-dir-process-buffer)
       (get-buffer-process vc-dir-process-buffer)))

(defun vc-dir-kill-dir-status-process ()
  "Kill the temporary buffer and associated process."
  (interactive)
  (when (buffer-live-p vc-dir-process-buffer)
    (let ((proc (get-buffer-process vc-dir-process-buffer)))
      (when proc (delete-process proc))
      (setq vc-dir-process-buffer nil)
      (setq mode-line-process nil))))

(defun vc-dir-kill-query ()
  ;; Make sure that when the status buffer is killed the update
  ;; process running in background is also killed.
  (if (vc-dir-busy)
    (when (y-or-n-p "Status update process running, really kill status buffer? ")
      (vc-dir-kill-dir-status-process)
      t)
    t))

(defun vc-dir-next-line (arg)
  "Go to the next line.
If a prefix argument is given, move by that many lines."
  (interactive "p")
  (with-no-warnings
    (ewoc-goto-next vc-ewoc arg)
    (vc-dir-move-to-goal-column)))

(defun vc-dir-previous-line (arg)
  "Go to the previous line.
If a prefix argument is given, move by that many lines."
  (interactive "p")
  (ewoc-goto-prev vc-ewoc arg)
  (vc-dir-move-to-goal-column))

(defun vc-dir-next-directory ()
  "Go to the next directory."
  (interactive)
  (let ((orig (point)))
    (if
	(catch 'foundit
	  (while t
	    (let* ((next (ewoc-next vc-ewoc (ewoc-locate vc-ewoc))))
	      (cond ((not next)
		     (throw 'foundit t))
		    (t
		     (progn
		       (ewoc-goto-node vc-ewoc next)
		       (vc-dir-move-to-goal-column)
		       (if (vc-dir-fileinfo->directory (ewoc-data next))
			   (throw 'foundit nil))))))))
	(goto-char orig))))

(defun vc-dir-previous-directory ()
  "Go to the previous directory."
  (interactive)
  (let ((orig (point)))
    (if
	(catch 'foundit
	  (while t
	    (let* ((prev (ewoc-prev vc-ewoc (ewoc-locate vc-ewoc))))
	      (cond ((not prev)
		     (throw 'foundit t))
		    (t
		     (progn
		       (ewoc-goto-node vc-ewoc prev)
		       (vc-dir-move-to-goal-column)
		       (if (vc-dir-fileinfo->directory (ewoc-data prev))
			   (throw 'foundit nil))))))))
	(goto-char orig))))

(defun vc-dir-mark-unmark (mark-unmark-function)
  (if (use-region-p)
      (let ((firstl (line-number-at-pos (region-beginning)))
	    (lastl (line-number-at-pos (region-end))))
	(save-excursion
	  (goto-char (region-beginning))
	  (while (<= (line-number-at-pos) lastl)
	    (condition-case nil
		(funcall mark-unmark-function)
	      ;; `vc-dir-mark-file' signals an error if we try marking
	      ;; a directory containing marked files in its tree, or a
	      ;; file in a marked directory tree.  Just continue.
	      (error (vc-dir-next-line 1))))))
    (funcall mark-unmark-function)))

(defun vc-dir-parent-marked-p (arg)
  ;; Non-nil iff a parent directory of arg is marked.
  ;; Return value, if non-nil is the `ewoc-data' for the marked parent.
  (let* ((argdir (vc-dir-node-directory arg))
	 (arglen (length argdir))
	 (crt arg)
	 (found nil))
    ;; Go through the predecessors, checking if any directory that is
    ;; a parent is marked.
    (while (and (null found)
		(setq crt (ewoc-prev vc-ewoc crt)))
      (let ((data (ewoc-data crt))
	    (dir (vc-dir-node-directory crt)))
	(and (vc-dir-fileinfo->directory data)
	     (vc-string-prefix-p dir argdir)
	     (vc-dir-fileinfo->marked data)
	     (setq found data))))
    found))

(defun vc-dir-children-marked-p (arg)
  ;; Non-nil iff a child of ARG is marked.
  ;; Return value, if non-nil, is the `ewoc-data' for the marked child.
  (let* ((argdir-re (concat "\\`" (regexp-quote (vc-dir-node-directory arg))))
	 (is-child t)
	 (crt arg)
	 (found nil))
    (while (and is-child
		(null found)
		(setq crt (ewoc-next vc-ewoc crt)))
      (let ((data (ewoc-data crt))
	    (dir (vc-dir-node-directory crt)))
	(if (string-match argdir-re dir)
	    (if (vc-dir-fileinfo->marked data)
		(setq found data))
	  ;; We are done, we got to an entry that is not a child of `arg'.
	  (setq is-child nil))))
    found))

(defun vc-dir-mark-file (&optional arg)
  ;; Mark ARG or the current file and move to the next line.
  (let* ((crt (or arg (ewoc-locate vc-ewoc)))
         (file (ewoc-data crt))
	 (isdir (vc-dir-fileinfo->directory file))
	 ;; Forbid marking a directory containing marked files in its
	 ;; tree, or a file in a marked directory tree.
	 (conflict (if isdir
		       (vc-dir-children-marked-p crt)
		     (vc-dir-parent-marked-p crt))))
    (when conflict
      (error (if isdir
		 "File `%s' in this directory is already marked"
	       "Parent directory `%s' is already marked")
	     (vc-dir-fileinfo->name conflict)))
    (setf (vc-dir-fileinfo->marked file) t)
    (ewoc-invalidate vc-ewoc crt)
    (unless (or arg (mouse-event-p last-command-event))
      (vc-dir-next-line 1))))

(defun vc-dir-mark ()
  "Mark the current file or all files in the region.
If the region is active, mark all the files in the region.
Otherwise mark the file on the current line and move to the next
line."
  (interactive)
  (vc-dir-mark-unmark 'vc-dir-mark-file))

(defun vc-dir-mark-all-files (arg)
  "Mark all files with the same state as the current one.
With a prefix argument mark all files.
If the current entry is a directory, mark all child files.

The commands operate on files that are on the same state.
This command is intended to make it easy to select all files that
share the same state."
  (interactive "P")
  (if arg
      ;; Mark all files.
      (progn
	;; First check that no directory is marked, we can't mark
	;; files in that case.
	(ewoc-map
	 (lambda (filearg)
	   (when (and (vc-dir-fileinfo->directory filearg)
		      (vc-dir-fileinfo->marked filearg))
	     (error "Cannot mark all files, directory `%s' marked"
		    (vc-dir-fileinfo->name filearg))))
	 vc-ewoc)
	(ewoc-map
	 (lambda (filearg)
	   (unless (vc-dir-fileinfo->marked filearg)
	     (setf (vc-dir-fileinfo->marked filearg) t)
	     t))
	 vc-ewoc))
    (let* ((crt  (ewoc-locate vc-ewoc))
	   (data (ewoc-data crt)))
      (if (vc-dir-fileinfo->directory data)
	  ;; It's a directory, mark child files.
	  (let (crt-data)
	    (while (and (setq crt (ewoc-next vc-ewoc crt))
			(setq crt-data (ewoc-data crt))
			(not (vc-dir-fileinfo->directory crt-data)))
	      (setf (vc-dir-fileinfo->marked crt-data) t)
	      (ewoc-invalidate vc-ewoc crt)))
	;; It's a file
	(let ((state (vc-dir-fileinfo->state data)))
	  (setq crt (ewoc-nth vc-ewoc 0))
	  (while crt
	    (let ((crt-data (ewoc-data crt)))
	      (when (and (not (vc-dir-fileinfo->marked crt-data))
			 (eq (vc-dir-fileinfo->state crt-data) state)
			 (not (vc-dir-fileinfo->directory crt-data)))
		(vc-dir-mark-file crt)))
	    (setq crt (ewoc-next vc-ewoc crt))))))))

(defun vc-dir-unmark-file ()
  ;; Unmark the current file and move to the next line.
  (let* ((crt (ewoc-locate vc-ewoc))
         (file (ewoc-data crt)))
    (setf (vc-dir-fileinfo->marked file) nil)
    (ewoc-invalidate vc-ewoc crt)
    (unless (mouse-event-p last-command-event)
      (vc-dir-next-line 1))))

(defun vc-dir-unmark ()
  "Unmark the current file or all files in the region.
If the region is active, unmark all the files in the region.
Otherwise mark the file on the current line and move to the next
line."
  (interactive)
  (vc-dir-mark-unmark 'vc-dir-unmark-file))

(defun vc-dir-unmark-file-up ()
  "Move to the previous line and unmark the file."
  (interactive)
  ;; If we're on the first line, we won't move up, but we will still
  ;; remove the mark.  This seems a bit odd but it is what buffer-menu
  ;; does.
  (let* ((prev (ewoc-goto-prev vc-ewoc 1))
	 (file (ewoc-data prev)))
    (setf (vc-dir-fileinfo->marked file) nil)
    (ewoc-invalidate vc-ewoc prev)
    (vc-dir-move-to-goal-column)))

(defun vc-dir-unmark-all-files (arg)
  "Unmark all files with the same state as the current one.
With a prefix argument unmark all files.
If the current entry is a directory, unmark all the child files.

The commands operate on files that are on the same state.
This command is intended to make it easy to deselect all files
that share the same state."
  (interactive "P")
  (if arg
      (ewoc-map
       (lambda (filearg)
	 (when (vc-dir-fileinfo->marked filearg)
	   (setf (vc-dir-fileinfo->marked filearg) nil)
	   t))
       vc-ewoc)
    (let* ((crt (ewoc-locate vc-ewoc))
	   (data (ewoc-data crt)))
      (if (vc-dir-fileinfo->directory data)
	  ;; It's a directory, unmark child files.
	  (while (setq crt (ewoc-next vc-ewoc crt))
	    (let ((crt-data (ewoc-data crt)))
	      (unless (vc-dir-fileinfo->directory crt-data)
		(setf (vc-dir-fileinfo->marked crt-data) nil)
		(ewoc-invalidate vc-ewoc crt))))
	;; It's a file
	(let ((crt-state (vc-dir-fileinfo->state (ewoc-data crt))))
	  (ewoc-map
	   (lambda (filearg)
	     (when (and (vc-dir-fileinfo->marked filearg)
			(eq (vc-dir-fileinfo->state filearg) crt-state))
	       (setf (vc-dir-fileinfo->marked filearg) nil)
	       t))
	   vc-ewoc))))))

(defun vc-dir-toggle-mark-file ()
  (let* ((crt (ewoc-locate vc-ewoc))
         (file (ewoc-data crt)))
    (if (vc-dir-fileinfo->marked file)
	(vc-dir-unmark-file)
      (vc-dir-mark-file))))

(defun vc-dir-toggle-mark (e)
  (interactive "e")
  (vc-dir-at-event e (vc-dir-mark-unmark 'vc-dir-toggle-mark-file)))

(defun vc-dir-delete-file ()
  "Delete the marked files, or the current file if no marks."
  (interactive)
  (mapc 'vc-delete-file (or (vc-dir-marked-files)
                            (list (vc-dir-current-file)))))

(defun vc-dir-find-file ()
  "Find the file on the current line."
  (interactive)
  (find-file (vc-dir-current-file)))

(defun vc-dir-find-file-other-window (&optional event)
  "Find the file on the current line, in another window."
  (interactive (list last-nonmenu-event))
  (if event (posn-set-point (event-end event)))
  (find-file-other-window (vc-dir-current-file)))

(defun vc-dir-isearch ()
  "Search for a string through all marked buffers using Isearch."
  (interactive)
  (multi-isearch-files
   (mapcar 'car (vc-dir-marked-only-files-and-states))))

(defun vc-dir-isearch-regexp ()
  "Search for a regexp through all marked buffers using Isearch."
  (interactive)
  (multi-isearch-files-regexp
   (mapcar 'car (vc-dir-marked-only-files-and-states))))

(defun vc-dir-search (regexp)
  "Search through all marked files for a match for REGEXP.
For marked directories, use the files displayed from those directories.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue]."
  (interactive "sSearch marked files (regexp): ")
  (tags-search regexp '(mapcar 'car (vc-dir-marked-only-files-and-states))))

(defun vc-dir-query-replace-regexp (from to &optional delimited)
  "Do `query-replace-regexp' of FROM with TO, on all marked files.
If a directory is marked, then use the files displayed for that directory.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue]."
  ;; FIXME: this is almost a copy of `dired-do-query-replace-regexp'.  This
  ;; should probably be made generic and used in both places instead of
  ;; duplicating it here.
  (interactive
   (let ((common
	  (query-replace-read-args
	   "Query replace regexp in marked files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (dolist (file (mapcar 'car (vc-dir-marked-only-files-and-states)))
    (let ((buffer (get-file-buffer file)))
      (if (and buffer (with-current-buffer buffer
			buffer-read-only))
	  (error "File `%s' is visited read-only" file))))
  (tags-query-replace from to delimited
		      '(mapcar 'car (vc-dir-marked-only-files-and-states))))

(defun vc-dir-current-file ()
  (let ((node (ewoc-locate vc-ewoc)))
    (unless node
      (error "No file available"))
    (expand-file-name (vc-dir-fileinfo->name (ewoc-data node)))))

(defun vc-dir-marked-files ()
  "Return the list of marked files."
  (mapcar
   (lambda (elem) (expand-file-name (vc-dir-fileinfo->name elem)))
   (ewoc-collect vc-ewoc 'vc-dir-fileinfo->marked)))

(defun vc-dir-marked-only-files-and-states ()
  "Return the list of conses (FILE . STATE) for the marked files.
For marked directories return the corresponding conses for the
child files."
  (let ((crt (ewoc-nth vc-ewoc 0))
	result)
    (while crt
      (let ((crt-data (ewoc-data crt)))
	(if (vc-dir-fileinfo->marked crt-data)
	    ;; FIXME: use vc-dir-child-files-and-states here instead of duplicating it.
	    (if (vc-dir-fileinfo->directory crt-data)
		(let* ((dir (vc-dir-fileinfo->directory crt-data))
		       (dirlen (length dir))
		       data)
		  (while
		      (and (setq crt (ewoc-next vc-ewoc crt))
			   (vc-string-prefix-p dir
                                               (progn
                                                 (setq data (ewoc-data crt))
                                                 (vc-dir-node-directory crt))))
		    (unless (vc-dir-fileinfo->directory data)
		      (push
		       (cons (expand-file-name (vc-dir-fileinfo->name data))
			     (vc-dir-fileinfo->state data))
		       result))))
	      (push (cons (expand-file-name (vc-dir-fileinfo->name crt-data))
			  (vc-dir-fileinfo->state crt-data))
		    result)
	      (setq crt (ewoc-next vc-ewoc crt)))
	  (setq crt (ewoc-next vc-ewoc crt)))))
    (nreverse result)))

(defun vc-dir-child-files-and-states ()
  "Return the list of conses (FILE . STATE) for child files of the current entry if it's a directory.
If it is a file, return the corresponding cons for the file itself."
  (let* ((crt (ewoc-locate vc-ewoc))
	 (crt-data (ewoc-data crt))
         result)
    (if (vc-dir-fileinfo->directory crt-data)
	(let* ((dir (vc-dir-fileinfo->directory crt-data))
	       (dirlen (length dir))
	       data)
	  (while
	      (and (setq crt (ewoc-next vc-ewoc crt))
                   (vc-string-prefix-p dir (progn
                                             (setq data (ewoc-data crt))
                                             (vc-dir-node-directory crt))))
	    (unless (vc-dir-fileinfo->directory data)
	      (push
	       (cons (expand-file-name (vc-dir-fileinfo->name data))
		     (vc-dir-fileinfo->state data))
	       result))))
      (push
       (cons (expand-file-name (vc-dir-fileinfo->name crt-data))
	     (vc-dir-fileinfo->state crt-data)) result))
    (nreverse result)))

(defun vc-dir-recompute-file-state (fname def-dir)
  (let* ((file-short (file-relative-name fname def-dir))
	 (remove-me-when-CVS-works
	  (when (eq vc-dir-backend 'CVS)
	    ;; FIXME: Warning: UGLY HACK.  The CVS backend caches the state
	    ;; info, this forces the backend to update it.
	    (vc-call-backend vc-dir-backend 'registered fname)))
	 (state (vc-call-backend vc-dir-backend 'state fname))
	 (extra (vc-call-backend vc-dir-backend
				 'status-fileinfo-extra fname)))
    (list file-short state extra)))

(defun vc-dir-find-child-files (dirname)
  ;; Give a DIRNAME string return the list of all child files shown in
  ;; the current *vc-dir* buffer.
  (let ((crt (ewoc-nth vc-ewoc 0))
	children
	dname)
    ;; Find DIR
    (while (and crt (not (vc-string-prefix-p
			  dirname (vc-dir-node-directory crt))))
      (setq crt (ewoc-next vc-ewoc crt)))
    (while (and crt (vc-string-prefix-p
		     dirname
		     (setq dname (vc-dir-node-directory crt))))
      (let ((data (ewoc-data crt)))
	(unless (vc-dir-fileinfo->directory data)
	  (push (expand-file-name (vc-dir-fileinfo->name data)) children)))
      (setq crt (ewoc-next vc-ewoc crt)))
    children))

(defun vc-dir-resync-directory-files (dirname)
  ;; Update the entries for all the child files of DIRNAME shown in
  ;; the current *vc-dir* buffer.
  (let ((files (vc-dir-find-child-files dirname))
	(ddir default-directory)
	fileentries)
    (when files
      (dolist (crt files)
	(push (vc-dir-recompute-file-state crt ddir)
	      fileentries))
      (vc-dir-update fileentries (current-buffer)))))

(defun vc-dir-resynch-file (&optional fname)
  "Update the entries for FNAME in any directory buffers that list it."
  (let ((file (or fname (expand-file-name buffer-file-name)))
        (drop '()))
    (save-current-buffer
      ;; look for a vc-dir buffer that might show this file.
      (dolist (status-buf vc-dir-buffers)
        (if (not (buffer-live-p status-buf))
            (push status-buf drop)
          (set-buffer status-buf)
          (if (not (derived-mode-p 'vc-dir-mode))
              (push status-buf drop)
            (let ((ddir default-directory))
              (when (vc-string-prefix-p ddir file)
                (if (file-directory-p file)
		    (progn
		      (vc-dir-resync-directory-files file)
		      (ewoc-set-hf vc-ewoc
				   (vc-dir-headers vc-dir-backend default-directory) ""))
                  (let* ((complete-state (vc-dir-recompute-file-state file ddir))
			 (state (cadr complete-state)))
                    (vc-dir-update
                     (list complete-state)
                     status-buf (or (not state)
				    (eq state 'up-to-date)))))))))))
    ;; Remove out-of-date entries from vc-dir-buffers.
    (dolist (b drop) (setq vc-dir-buffers (delq b vc-dir-buffers)))))

(defvar use-vc-backend)  ;; dynamically bound

(define-derived-mode vc-dir-mode special-mode "VC dir"
  "Major mode for VC directory buffers.
Marking/Unmarking key bindings and actions:
m - mark a file/directory
  - if the region is active, mark all the files in region.
    Restrictions: - a file cannot be marked if any parent directory is marked
                  - a directory cannot be marked if any child file or
                    directory is marked
u - unmark a file/directory
  - if the region is active, unmark all the files in region.
M - if the cursor is on a file: mark all the files with the same state as
      the current file
  - if the cursor is on a directory: mark all child files
  - with a prefix argument: mark all files
U - if the cursor is on a file: unmark all the files with the same state
      as the current file
  - if the cursor is on a directory: unmark all child files
  - with a prefix argument: unmark all files
mouse-2  - toggles the mark state

VC commands
VC commands in the `C-x v' prefix can be used.
VC commands act on the marked entries.  If nothing is marked, VC
commands act on the current entry.

Search & Replace
S - searches the marked files
Q - does a query replace on the marked files
M-s a C-s - does an isearch on the marked files
M-s a C-M-s - does a regexp isearch on the marked files
If nothing is marked, these commands act on the current entry.
When a directory is current or marked, the Search & Replace
commands act on the child files of that directory that are displayed in
the *vc-dir* buffer.

\\{vc-dir-mode-map}"
  (set (make-local-variable 'vc-dir-backend) use-vc-backend)
  (setq buffer-read-only t)
  (when (boundp 'tool-bar-map)
    (set (make-local-variable 'tool-bar-map) vc-dir-tool-bar-map))
  (let ((buffer-read-only nil))
    (erase-buffer)
    (set (make-local-variable 'vc-dir-process-buffer) nil)
    (set (make-local-variable 'vc-ewoc) (ewoc-create #'vc-dir-printer))
    (set (make-local-variable 'revert-buffer-function)
	 'vc-dir-revert-buffer-function)
    (setq list-buffers-directory (expand-file-name "*vc-dir*" default-directory))
    (add-to-list 'vc-dir-buffers (current-buffer))
    ;; Make sure that if the directory buffer is killed, the update
    ;; process running in the background is also killed.
    (add-hook 'kill-buffer-query-functions 'vc-dir-kill-query nil t)
    (hack-dir-local-variables-non-file-buffer)
    (vc-dir-refresh)))

(defun vc-dir-headers (backend dir)
  "Display the headers in the *VC dir* buffer.
It calls the `dir-extra-headers' backend method to display backend
specific headers."
  (concat
   ;; First layout the common headers.
   (propertize "VC backend : " 'face 'font-lock-type-face)
   (propertize (format "%s\n" backend) 'face 'font-lock-variable-name-face)
   (propertize "Working dir: " 'face 'font-lock-type-face)
   (propertize (format "%s\n" (abbreviate-file-name dir))
               'face 'font-lock-variable-name-face)
   ;; Then the backend specific ones.
   (vc-call-backend backend 'dir-extra-headers dir)
   "\n"))

(defun vc-dir-refresh-files (files default-state)
  "Refresh some files in the *VC-dir* buffer."
  (let ((def-dir default-directory)
	(backend vc-dir-backend))
    (vc-set-mode-line-busy-indicator)
    ;; Call the `dir-status-files' backend function.
    ;; `dir-status-files' is supposed to be asynchronous.
    ;; It should compute the results, and then call the function
    ;; passed as an argument in order to update the vc-dir buffer
    ;; with the results.
    (unless (buffer-live-p vc-dir-process-buffer)
      (setq vc-dir-process-buffer
            (generate-new-buffer (format " *VC-%s* tmp status" backend))))
    (lexical-let ((buffer (current-buffer)))
      (with-current-buffer vc-dir-process-buffer
        (setq default-directory def-dir)
        (erase-buffer)
        (vc-call-backend
         backend 'dir-status-files def-dir files default-state
         (lambda (entries &optional more-to-come)
           ;; ENTRIES is a list of (FILE VC_STATE EXTRA) items.
           ;; If MORE-TO-COME is true, then more updates will come from
           ;; the asynchronous process.
           (with-current-buffer buffer
             (vc-dir-update entries buffer)
             (unless more-to-come
               (setq mode-line-process nil)
               ;; Remove the ones that haven't been updated at all.
               ;; Those not-updated are those whose state is nil because the
               ;; file/dir doesn't exist and isn't versioned.
               (ewoc-filter vc-ewoc
                            (lambda (info)
			      ;; The state for directory entries might
			      ;; have been changed to 'up-to-date,
			      ;; reset it, otherwise it will be removed when doing 'x'
			      ;; next time.
			      ;; FIXME: There should be a more elegant way to do this.
			      (when (and (vc-dir-fileinfo->directory info)
					 (eq (vc-dir-fileinfo->state info)
					     'up-to-date))
				(setf (vc-dir-fileinfo->state info) nil))

                              (not (vc-dir-fileinfo->needs-update info))))))))))))

(defun vc-dir-revert-buffer-function (&optional ignore-auto noconfirm)
  (vc-dir-refresh))

(defun vc-dir-refresh ()
  "Refresh the contents of the *VC-dir* buffer.
Throw an error if another update process is in progress."
  (interactive)
  (if (vc-dir-busy)
      (error "Another update process is in progress, cannot run two at a time")
    (let ((def-dir default-directory)
	  (backend vc-dir-backend))
      (vc-set-mode-line-busy-indicator)
      ;; Call the `dir-status' backend function.
      ;; `dir-status' is supposed to be asynchronous.
      ;; It should compute the results, and then call the function
      ;; passed as an argument in order to update the vc-dir buffer
      ;; with the results.

      ;; Create a buffer that can be used by `dir-status' and call
      ;; `dir-status' with this buffer as the current buffer.  Use
      ;; `vc-dir-process-buffer' to remember this buffer, so that
      ;; it can be used later to kill the update process in case it
      ;; takes too long.
      (unless (buffer-live-p vc-dir-process-buffer)
        (setq vc-dir-process-buffer
              (generate-new-buffer (format " *VC-%s* tmp status" backend))))
      ;; set the needs-update flag on all non-directory entries
      (ewoc-map (lambda (info)
		  (unless (vc-dir-fileinfo->directory info)
		    (setf (vc-dir-fileinfo->needs-update info) t) nil))
                vc-ewoc)
      ;; Bzr has serious locking problems, so setup the headers first (this is
      ;; synchronous) rather than doing it while dir-status is running.
      (ewoc-set-hf vc-ewoc (vc-dir-headers backend def-dir) "")
      (lexical-let ((buffer (current-buffer)))
        (with-current-buffer vc-dir-process-buffer
          (setq default-directory def-dir)
          (erase-buffer)
          (vc-call-backend
           backend 'dir-status def-dir
           (lambda (entries &optional more-to-come)
             ;; ENTRIES is a list of (FILE VC_STATE EXTRA) items.
             ;; If MORE-TO-COME is true, then more updates will come from
             ;; the asynchronous process.
             (with-current-buffer buffer
               (vc-dir-update entries buffer)
               (unless more-to-come
                 (let ((remaining
                        (ewoc-collect
                         vc-ewoc 'vc-dir-fileinfo->needs-update)))
                   (if remaining
                       (vc-dir-refresh-files
                        (mapcar 'vc-dir-fileinfo->name remaining)
                        'up-to-date)
                     (setq mode-line-process nil))))))))))))

(defun vc-dir-show-fileentry (file)
  "Insert an entry for a specific file into the current *VC-dir* listing.
This is typically used if the file is up-to-date (or has been added
outside of VC) and one wants to do some operation on it."
  (interactive "fShow file: ")
  (vc-dir-update (list (list (file-relative-name file) (vc-state file))) (current-buffer)))

(defun vc-dir-hide-up-to-date ()
  "Hide up-to-date items from display."
  (interactive)
  (let ((crt (ewoc-nth vc-ewoc -1))
	(first (ewoc-nth vc-ewoc 0)))
    ;; Go over from the last item to the first and remove the
    ;; up-to-date files and directories with no child files.
    (while (not (eq crt first))
      (let* ((data (ewoc-data crt))
	     (dir (vc-dir-fileinfo->directory data))
	     (next (ewoc-next vc-ewoc crt))
	     (prev (ewoc-prev vc-ewoc crt))
	     ;; ewoc-delete does not work without this...
	     (inhibit-read-only t))
	  (when (or
		 ;; Remove directories with no child files.
		 (and dir
		      (or
		       ;; Nothing follows this directory.
		       (not next)
		       ;; Next item is a directory.
		       (vc-dir-fileinfo->directory (ewoc-data next))))
		 ;; Remove files in the up-to-date state.
		 (eq (vc-dir-fileinfo->state data) 'up-to-date))
	    (ewoc-delete vc-ewoc crt))
	  (setq crt prev)))))

(defun vc-dir-kill-line ()
  "Remove the current line from display."
  (interactive)
  (let ((crt (ewoc-locate vc-ewoc))
        (inhibit-read-only t))
    (ewoc-delete vc-ewoc crt)))

(defun vc-dir-printer (fileentry)
  (vc-call-backend vc-dir-backend 'dir-printer fileentry))

(defun vc-dir-deduce-fileset (&optional state-model-only-files)
  (let ((marked (vc-dir-marked-files))
	files
	only-files-list
	state
	model)
    (if marked
	(progn
	  (setq files marked)
	  (when state-model-only-files
	    (setq only-files-list (vc-dir-marked-only-files-and-states))))
      (let ((crt (vc-dir-current-file)))
	(setq files (list crt))
	(when state-model-only-files
	  (setq only-files-list (vc-dir-child-files-and-states)))))

    (when state-model-only-files
      (setq state (cdar only-files-list))
      ;; Check that all files are in a consistent state, since we use that
      ;; state to decide which operation to perform.
      (dolist (crt (cdr only-files-list))
	(unless (vc-compatible-state (cdr crt) state)
	  (error "When applying VC operations to multiple files, the files are required\nto  be in similar VC states.\n%s in state %s clashes with %s in state %s"
		 (car crt) (cdr crt) (caar only-files-list) state)))
      (setq only-files-list (mapcar 'car only-files-list))
      (when (and state (not (eq state 'unregistered)))
	(setq model (vc-checkout-model vc-dir-backend only-files-list))))
    (list vc-dir-backend files only-files-list state model)))

;;;###autoload
(defun vc-dir (dir &optional backend)
  "Show the VC status for \"interesting\" files in and below DIR.
This allows you to mark files and perform VC operations on them.
The list omits files which are up to date, with no changes in your copy
or the repository, if there is nothing in particular to say about them.

Preparing the list of file status takes time; when the buffer
first appears, it has only the first few lines of summary information.
The file lines appear later.

Optional second argument BACKEND specifies the VC backend to use.
Interactively, a prefix argument means to ask for the backend.

These are the commands available for use in the file status buffer:

\\{vc-dir-mode-map}"

  (interactive
   (list
    ;; When you hit C-x v d in a visited VC file,
    ;; the *vc-dir* buffer visits the directory under its truename;
    ;; therefore it makes sense to always do that.
    ;; Otherwise if you do C-x v d -> C-x C-f -> C-c v d
    ;; you may get a new *vc-dir* buffer, different from the original
    (file-truename (read-directory-name "VC status for directory: "
					default-directory default-directory t
					nil))
    (if current-prefix-arg
	(intern
	 (completing-read
	  "Use VC backend: "
	  (mapcar (lambda (b) (list (symbol-name b)))
		  vc-handled-backends)
	  nil t nil nil)))))
  (unless backend
    (setq backend (vc-responsible-backend dir)))
  (let (pop-up-windows)		      ; based on cvs-examine; bug#6204
    (pop-to-buffer (vc-dir-prepare-status-buffer "*vc-dir*" dir backend)))
  (if (derived-mode-p 'vc-dir-mode)
      (vc-dir-refresh)
    ;; FIXME: find a better way to pass the backend to `vc-dir-mode'.
    (let ((use-vc-backend backend))
      (vc-dir-mode))))

(defun vc-default-dir-extra-headers (backend dir)
  ;; Be loud by default to remind people to add code to display
  ;; backend specific headers.
  ;; XXX: change this to return nil before the release.
  (concat
   (propertize "Extra      : " 'face 'font-lock-type-face)
   (propertize "Please add backend specific headers here.  It's easy!"
	       'face 'font-lock-warning-face)))

(defvar vc-dir-filename-mouse-map
   (let ((map (make-sparse-keymap)))
     (define-key map [mouse-2] 'vc-dir-find-file-other-window)
    map)
  "Local keymap for visiting a file.")

(defun vc-default-dir-printer (backend fileentry)
  "Pretty print FILEENTRY."
  ;; If you change the layout here, change vc-dir-move-to-goal-column.
  ;; VC backends can implement backend specific versions of this
  ;; function.  Changes here might need to be reflected in the
  ;; vc-BACKEND-dir-printer functions.
  (let* ((isdir (vc-dir-fileinfo->directory fileentry))
	(state (if isdir "" (vc-dir-fileinfo->state fileentry)))
	(filename (vc-dir-fileinfo->name fileentry)))
    (insert
     (propertize
      (format "%c" (if (vc-dir-fileinfo->marked fileentry) ?* ? ))
      'face 'font-lock-type-face)
     "   "
     (propertize
      (format "%-20s" state)
      'face (cond ((eq state 'up-to-date) 'font-lock-builtin-face)
		  ((memq state '(missing conflict)) 'font-lock-warning-face)
		  ((eq state 'edited) 'font-lock-constant-face)
		  (t 'font-lock-variable-name-face))
      'mouse-face 'highlight)
     " "
     (propertize
      (format "%s" filename)
      'face
      (if isdir 'font-lock-comment-delimiter-face 'font-lock-function-name-face)
      'help-echo
      (if isdir
	  "Directory\nVC operations can be applied to it\nmouse-3: Pop-up menu"
	"File\nmouse-3: Pop-up menu")
      'mouse-face 'highlight
      'keymap vc-dir-filename-mouse-map))))

(defun vc-default-extra-status-menu (backend)
  nil)

(defun vc-default-status-fileinfo-extra (backend file)
  "Default absence of extra information returned for a file."
  nil)

(provide 'vc-dir)

;;; vc-dir.el ends here
