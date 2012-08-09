;;; semantic/db-mode.el --- Semanticdb Minor Mode

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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

;;; Commentary:
;;
;; Major mode for managing Semantic Databases automatically.

;;; Code:

(require 'semantic/db)

(declare-function semantic-lex-spp-set-dynamic-table "semantic/lex-spp")

;;; Start/Stop database use
;;
(defvar semanticdb-hooks
  '((semanticdb-semantic-init-hook-fcn semantic-init-db-hook)
    (semanticdb-synchronize-table semantic-after-toplevel-cache-change-hook)
    (semanticdb-partial-synchronize-table semantic-after-partial-cache-change-hook)
    (semanticdb-revert-hook before-revert-hook)
    (semanticdb-kill-hook kill-buffer-hook)
    (semanticdb-kill-hook change-major-mode-hook) ;; Not really a kill, but we need the same effect.
    (semanticdb-kill-emacs-hook kill-emacs-hook)
    (semanticdb-save-all-db-idle auto-save-hook)
    )
  "List of hooks and values to add/remove when configuring semanticdb.")

;;; SEMANTICDB-MODE
;;
;;;###autoload
(defun semanticdb-minor-mode-p ()
  "Return non-nil if `semanticdb-minor-mode' is active."
  (member (car (car semanticdb-hooks))
	  (symbol-value (car (cdr (car semanticdb-hooks))))))

;;;###autoload
(define-minor-mode global-semanticdb-minor-mode
  "Toggle Semantic DB mode.
With ARG, turn Semantic DB mode on if ARG is positive, off otherwise.

In Semantic DB mode, Semantic parsers store results in a
database, which can be saved for future Emacs sessions."
  :global t
  :group 'semantic
  (if global-semanticdb-minor-mode
      ;; Enable
      (dolist (elt semanticdb-hooks)
	(add-hook (cadr elt) (car elt)))
    ;; Disable
    (dolist (elt semanticdb-hooks)
      (add-hook (cadr elt) (car elt)))))

(defvaralias 'semanticdb-mode-hook 'global-semanticdb-minor-mode-hook)
(defvaralias 'semanticdb-global-mode 'global-semanticdb-minor-mode)
(semantic-varalias-obsolete 'semanticdb-mode-hooks
			    'global-semanticdb-minor-mode-hook "23.2")


(defun semanticdb-toggle-global-mode ()
  "Toggle use of the Semantic Database feature.
Update the environment of Semantic enabled buffers accordingly."
  (interactive)
  (if (semanticdb-minor-mode-p)
      ;; Save databases before disabling semanticdb.
      (semanticdb-save-all-db))
  ;; Toggle semanticdb minor mode.
  (global-semanticdb-minor-mode))

;;; Hook Functions:
;;
;; Functions used in hooks to keep SemanticDB operating.
;;
(defun semanticdb-semantic-init-hook-fcn ()
  "Function saved in `semantic-init-db-hook'.
Sets up the semanticdb environment."
  ;; Only initialize semanticdb if we have a file name.
  ;; There is no reason to cache a tag table if there is no
  ;; way to load it back in later.
  (when (buffer-file-name)
    (let* ((ans (semanticdb-create-table-for-file (buffer-file-name)))
	   (cdb (car ans))
	   (ctbl (cdr ans))
	   )
      ;; Get the current DB for this directory
      (setq semanticdb-current-database cdb)
      ;; We set the major mode because we know what it is.
      (oset ctbl major-mode major-mode)
      ;; Local state
      (setq semanticdb-current-table ctbl)
      ;; Try to swap in saved tags
      (if (or (not (slot-boundp ctbl 'tags)) (not (oref ctbl tags))
	      (/= (or (oref ctbl pointmax) 0) (point-max))
	      )
	  (semantic-clear-toplevel-cache)
	;; Unmatched syntax
	(condition-case nil
	    (semantic-set-unmatched-syntax-cache
	     (oref ctbl unmatched-syntax))
	  (unbound-slot
	   ;; Old version of the semanticdb table can miss the unmatched
	   ;; syntax slot.  If so, just clear the unmatched syntax cache.
	   (semantic-clear-unmatched-syntax-cache)
	   ;; Make sure it has a value.
	   (oset ctbl unmatched-syntax nil)
	   ))
	;; Keep lexical tables up to date.  Don't load
	;; semantic-spp if it isn't needed.
	(let ((lt (oref ctbl lexical-table)))
	  (when lt
	    (require 'semantic/lex-spp)
	    (semantic-lex-spp-set-dynamic-table lt)))
	;; Set the main tag cache.
	;; This must happen after setting up buffer local variables
	;; since this will turn around and re-save those variables.
	(semantic--set-buffer-cache (oref ctbl tags))
	;; Don't need it to be dirty.  Set dirty due to hooks from above.
	(oset ctbl dirty nil) ;; Special case here.
	(oset ctbl buffer (current-buffer))
	;; Bind into the buffer.
	(semantic--tag-link-cache-to-buffer)
	)
      )))

(defun semanticdb-revert-hook ()
  "Hook run before a revert buffer.
We can't track incremental changes due to a revert, so just clear the cache.
This will prevent the next batch of hooks from wasting time parsing things
that don't need to be parsed."
  (if (and (semantic-active-p)
	   semantic--buffer-cache
	   semanticdb-current-table)
      (semantic-clear-toplevel-cache)))

(defun semanticdb-kill-hook ()
  "Function run when a buffer is killed.
If there is a semantic cache, slurp out the overlays, and store
it in our database.  If that buffer has no cache, ignore it, we'll
handle it later if need be."
  (when (and (semantic-active-p)
	     semantic--buffer-cache
	     semanticdb-current-table)

    ;; Try to get a fast update.
    (semantic-fetch-tags-fast)

    ;; If the buffer is in a bad state, don't save anything...
    (if (semantic-parse-tree-needs-rebuild-p)
	;; If this is the case, don't save anything.
	(progn
	  (semantic-clear-toplevel-cache)
	  (oset semanticdb-current-table pointmax 0)
	  (oset semanticdb-current-table fsize 0)
	  (oset semanticdb-current-table lastmodtime nil)
	  )
      ;; We have a clean buffer, save it off.
      (condition-case nil
	  (progn
	    (semantic--tag-unlink-cache-from-buffer)
	    ;; Set pointmax only if we had some success in the unlink.
	    (oset semanticdb-current-table pointmax (point-max))
	    (let ((fattr (file-attributes
			  (semanticdb-full-filename
			   semanticdb-current-table))))
	      (oset semanticdb-current-table fsize (nth 7 fattr))
	      (oset semanticdb-current-table lastmodtime (nth 5 fattr))
	      (oset semanticdb-current-table buffer nil)
	      ))
	;; If this messes up, just clear the system
	(error
	 (semantic-clear-toplevel-cache)
	 (message "semanticdb: Failed to deoverlay tag cache.")))
      )
    ))

(defun semanticdb-kill-emacs-hook ()
  "Function called when Emacs is killed.
Save all the databases."
  (semanticdb-save-all-db))

;;; SYNCHRONIZATION HOOKS
;;
(defun semanticdb-synchronize-table (new-table)
  "Function run after parsing.
Argument NEW-TABLE is the new table of tags."
  (when semanticdb-current-table
    (semanticdb-synchronize semanticdb-current-table new-table)))

(defun semanticdb-partial-synchronize-table (new-table)
  "Function run after parsing.
Argument NEW-TABLE is the new table of tags."
  (when semanticdb-current-table
    (semanticdb-partial-synchronize semanticdb-current-table new-table)))


(provide 'semantic/db-mode)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/db-mode"
;; End:

;;; semantic/db-mode.el ends here
