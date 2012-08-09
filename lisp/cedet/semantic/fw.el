;;; semantic/fw.el --- Framework for Semantic

;;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; Semantic has several core features shared across it's lex/parse/util
;; stages.  This used to clutter semantic.el some.  These routines are all
;; simple things that are not parser specific, but aid in making
;; semantic flexible and compatible amongst different Emacs platforms.

;;; Code:
;;
(require 'mode-local)
(require 'eieio)
(load "semantic/loaddefs" nil 'nomessage)

;;; Compatibility

(defalias 'semantic-buffer-local-value      'buffer-local-value)
(defalias 'semantic-overlay-live-p          'overlay-buffer)
(defalias 'semantic-make-overlay            'make-overlay)
(defalias 'semantic-overlay-put             'overlay-put)
(defalias 'semantic-overlay-get             'overlay-get)
(defalias 'semantic-overlay-properties      'overlay-properties)
(defalias 'semantic-overlay-move            'move-overlay)
(defalias 'semantic-overlay-delete          'delete-overlay)
(defalias 'semantic-overlays-at             'overlays-at)
(defalias 'semantic-overlays-in             'overlays-in)
(defalias 'semantic-overlay-buffer          'overlay-buffer)
(defalias 'semantic-overlay-start           'overlay-start)
(defalias 'semantic-overlay-end             'overlay-end)
(defalias 'semantic-overlay-size            'overlay-size)
(defalias 'semantic-overlay-next-change     'next-overlay-change)
(defalias 'semantic-overlay-previous-change 'previous-overlay-change)
(defalias 'semantic-overlay-lists           'overlay-lists)
(defalias 'semantic-overlay-p               'overlayp)
(defalias 'semantic-read-event              'read-event)
(defalias 'semantic-popup-menu              'popup-menu)
(defalias 'semantic-make-local-hook         'identity)
(defalias 'semantic-mode-line-update        'force-mode-line-update)
(defalias 'semantic-run-mode-hooks          'run-mode-hooks)
(defalias 'semantic-compile-warn            'byte-compile-warn)
(defalias 'semantic-menu-item               'identity)

(defun semantic-event-window (event)
  "Extract the window from EVENT."
  (car (car (cdr event))))

(defun semantic-delete-overlay-maybe (overlay)
  "Delete OVERLAY if it is a semantic token overlay."
  (if (semantic-overlay-get overlay 'semantic)
      (semantic-overlay-delete overlay)))

;;; Positional Data Cache
;;
(defvar semantic-cache-data-overlays nil
  "List of all overlays waiting to be flushed.")

(defun semantic-cache-data-to-buffer (buffer start end value name &optional lifespan)
  "In BUFFER over the region START END, remember VALUE.
NAME specifies a special name that can be searched for later to
recover the cached data with `semantic-get-cache-data'.
LIFESPAN indicates how long the data cache will be remembered.
The default LIFESPAN is 'end-of-command.
Possible Lifespans are:
  'end-of-command - Remove the cache at the end of the currently
                    executing command.
  'exit-cache-zone - Remove when point leaves the overlay at the
                    end of the currently executing command."
  ;; Check if LIFESPAN is valid before to create any overlay
  (or lifespan (setq lifespan 'end-of-command))
  (or (memq lifespan '(end-of-command exit-cache-zone))
      (error "semantic-cache-data-to-buffer: Unknown LIFESPAN: %s"
             lifespan))
  (let ((o (semantic-make-overlay start end buffer)))
    (semantic-overlay-put o 'cache-name   name)
    (semantic-overlay-put o 'cached-value value)
    (semantic-overlay-put o 'lifespan     lifespan)
    (setq semantic-cache-data-overlays
          (cons o semantic-cache-data-overlays))
    ;;(message "Adding to cache: %s" o)
    (add-hook 'post-command-hook 'semantic-cache-data-post-command-hook)
    ))

(defun semantic-cache-data-post-command-hook ()
  "Flush `semantic-cache-data-overlays' based 'lifespan property.
Remove self from `post-command-hook' if it is empty."
  (let ((newcache nil)
        (oldcache semantic-cache-data-overlays))
    (while oldcache
      (let* ((o    (car oldcache))
             (life (semantic-overlay-get o 'lifespan))
             )
        (if (or (eq life 'end-of-command)
                (and (eq life 'exit-cache-zone)
                     (not (member o (semantic-overlays-at (point))))))
            (progn
              ;;(message "Removing from cache: %s" o)
              (semantic-overlay-delete o)
              )
          (setq newcache (cons o newcache))))
      (setq oldcache (cdr oldcache)))
    (setq semantic-cache-data-overlays (nreverse newcache)))

  ;; Remove ourselves if we have removed all overlays.
  (unless semantic-cache-data-overlays
    (remove-hook 'post-command-hook
                 'semantic-cache-data-post-command-hook)))

(defun semantic-get-cache-data (name &optional point)
  "Get cached data with NAME from optional POINT."
  (save-excursion
    (if point (goto-char point))
    (let ((o (semantic-overlays-at (point)))
          (ans nil))
      (while (and (not ans) o)
        (if (equal (semantic-overlay-get (car o) 'cache-name) name)
            (setq ans (car o))
          (setq o (cdr o))))
      (when ans
        (semantic-overlay-get ans 'cached-value)))))

;;; Obsoleting various functions & variables
;;
(defun semantic-overload-symbol-from-function (name)
  "Return the symbol for overload used by NAME, the defined symbol."
  (let ((sym-name (symbol-name name)))
    (if (string-match "^semantic-" sym-name)
	(intern (substring sym-name (match-end 0)))
      name)))

(defun semantic-alias-obsolete (oldfnalias newfn when)
  "Make OLDFNALIAS an alias for NEWFN.
Mark OLDFNALIAS as obsolete, such that the byte compiler
will throw a warning when it encounters this symbol."
  (defalias oldfnalias newfn)
  (make-obsolete oldfnalias newfn when)
  (when (and (function-overload-p newfn)
             (not (overload-obsoleted-by newfn))
             ;; Only throw this warning when byte compiling things.
             (boundp 'byte-compile-current-file)
             byte-compile-current-file
	     (not (string-match "cedet" byte-compile-current-file))
	     )
    (make-obsolete-overload oldfnalias newfn when)
    (semantic-compile-warn
     "%s: `%s' obsoletes overload `%s'"
     byte-compile-current-file
     newfn
     (semantic-overload-symbol-from-function oldfnalias))
    ))

(defun semantic-varalias-obsolete (oldvaralias newvar when)
  "Make OLDVARALIAS an alias for variable NEWVAR.
Mark OLDVARALIAS as obsolete, such that the byte compiler
will throw a warning when it encounters this symbol."
  (make-obsolete-variable oldvaralias newvar when)
  (condition-case nil
      (defvaralias oldvaralias newvar)
    (error
     ;; Only throw this warning when byte compiling things.
     (when (and (boundp 'byte-compile-current-file)
                byte-compile-current-file)
       (semantic-compile-warn
        "variable `%s' obsoletes, but isn't alias of `%s'"
        newvar oldvaralias)
     ))))

;;; Help debugging
;;
(defmacro semantic-safe (format &rest body)
  "Turn into a FORMAT message any error caught during eval of BODY.
Return the value of last BODY form or nil if an error occurred.
FORMAT can have a %s escape which will be replaced with the actual
error message.
If `debug-on-error' is set, errors are not caught, so that you can
debug them.
Avoid using a large BODY since it is duplicated."
  ;;(declare (debug t) (indent 1))
  `(if debug-on-error
       ;;(let ((inhibit-quit nil)) ,@body)
       ;; Note to self: Doing the above screws up the wisent parser.
       (progn ,@body)
     (condition-case err
	 (progn ,@body)
       (error
        (message ,format (format "%S - %s" (current-buffer)
                                 (error-message-string err)))
        nil))))
(put 'semantic-safe 'lisp-indent-function 1)

;;; Misc utilities
;;
(defsubst semantic-map-buffers (function)
  "Run FUNCTION for each Semantic enabled buffer found.
FUNCTION does not have arguments.  When FUNCTION is entered
`current-buffer' is a selected Semantic enabled buffer."
  (mode-local-map-file-buffers function #'semantic-active-p))

(defalias 'semantic-map-mode-buffers 'mode-local-map-mode-buffers)

(semantic-alias-obsolete 'define-mode-overload-implementation
                         'define-mode-local-override "23.2")

(defun semantic-install-function-overrides (overrides &optional transient mode)
  "Install the function OVERRIDES in the specified environment.
OVERRIDES must be an alist ((OVERLOAD .  FUNCTION) ...) where OVERLOAD
is a symbol identifying an overloadable entry, and FUNCTION is the
function to override it with.
If optional argument TRANSIENT is non-nil, installed overrides can in
turn be overridden by next installation.
If optional argument MODE is non-nil, it must be a major mode symbol.
OVERRIDES will be installed globally for this major mode.  If MODE is
nil, OVERRIDES will be installed locally in the current buffer.  This
later installation should be done in MODE hook."
  (mode-local-bind
   ;; Add the semantic- prefix to OVERLOAD short names.
   (mapcar
    #'(lambda (e)
        (let ((name (symbol-name (car e))))
          (if (string-match "^semantic-" name)
              e
            (cons (intern (format "semantic-%s" name)) (cdr e)))))
    overrides)
   (list 'constant-flag (not transient)
         'override-flag t)
   mode))

;;; User Interrupt handling
;;
(defvar semantic-current-input-throw-symbol nil
  "The current throw symbol for `semantic-exit-on-input'.")

(defmacro semantic-exit-on-input (symbol &rest forms)
  "Using SYMBOL as an argument to `throw', execute FORMS.
If FORMS includes a call to `semantic-throw-on-input', then
if a user presses any key during execution, this form macro
will exit with the value passed to `semantic-throw-on-input'.
If FORMS completes, then the return value is the same as `progn'."
  `(let ((semantic-current-input-throw-symbol ,symbol))
     (catch ,symbol
       ,@forms)))
(put 'semantic-exit-on-input 'lisp-indent-function 1)

(defmacro semantic-throw-on-input (from)
  "Exit with `throw' when in `semantic-exit-on-input' on user input.
FROM is an indication of where this function is called from as a value
to pass to `throw'.  It is recommended to use the name of the function
calling this one."
  `(when (and semantic-current-input-throw-symbol
              (or (input-pending-p) (accept-process-output)))
     (throw semantic-current-input-throw-symbol ,from)))


;;; Special versions of Find File
;;
(defun semantic-find-file-noselect (file &optional nowarn rawfile wildcards)
  "Call `find-file-noselect' with various features turned off.
Use this when referencing a file that will be soon deleted.
FILE, NOWARN, RAWFILE, and WILDCARDS are passed into `find-file-noselect'"
  (let* ((recentf-exclude '( (lambda (f) t) ))
	 ;; This is a brave statement.  Don't waste time loading in
	 ;; lots of modes.  Especially decoration mode can waste a lot
	 ;; of time for a buffer we intend to kill.
	 (semantic-init-hook nil)
	 ;; This disables the part of EDE that asks questions
	 (ede-auto-add-method 'never)
	 ;; Ask font-lock to not colorize these buffers, nor to
	 ;; whine about it either.
	 (font-lock-maximum-size 0)
	 (font-lock-verbose nil)
	 ;; Disable revision control
	 (vc-handled-backends nil)
	 ;; Don't prompt to insert a template if we visit an empty file
	 (auto-insert nil)
	 ;; We don't want emacs to query about unsafe local variables
	 (enable-local-variables
	  (if (featurep 'xemacs)
	      ;; XEmacs only has nil as an option?
	      nil
	    ;; Emacs 23 has the spiffy :safe option, nil otherwise.
	    (if (>= emacs-major-version 22)
		nil
	      :safe)))
	 ;; ... or eval variables
	 (enable-local-eval nil)
	 )
    (save-match-data
      (if (featurep 'xemacs)
	  (find-file-noselect file nowarn rawfile)
	(find-file-noselect file nowarn rawfile wildcards)))
    ))

;;; Database restriction settings
;;
(defmacro semanticdb-without-unloaded-file-searches (forms)
  "Execute FORMS with `unloaded' removed from the current throttle."
  `(let ((semanticdb-find-default-throttle
	  (if (featurep 'semantic/db-find)
	      (remq 'unloaded semanticdb-find-default-throttle)
	    nil)))
     ,forms))
(put 'semanticdb-without-unloaded-file-searches 'lisp-indent-function 1)


;; ;;; Editor goodies ;-)
;; ;;
;; (defconst semantic-fw-font-lock-keywords
;;   (eval-when-compile
;;     (let* (
;;            ;; Variable declarations
;; 	   (vl nil)
;;            (kv (if vl (regexp-opt vl t) ""))
;;            ;; Function declarations
;; 	   (vf '(
;; 		 "define-lex"
;; 		 "define-lex-analyzer"
;; 		 "define-lex-block-analyzer"
;; 		 "define-lex-regex-analyzer"
;; 		 "define-lex-spp-macro-declaration-analyzer"
;; 		 "define-lex-spp-macro-undeclaration-analyzer"
;; 		 "define-lex-spp-include-analyzer"
;; 		 "define-lex-simple-regex-analyzer"
;; 		 "define-lex-keyword-type-analyzer"
;; 		 "define-lex-sexp-type-analyzer"
;; 		 "define-lex-regex-type-analyzer"
;; 		 "define-lex-string-type-analyzer"
;; 		 "define-lex-block-type-analyzer"
;; 		 ;;"define-mode-overload-implementation"
;; 		 ;;"define-semantic-child-mode"
;; 		 "define-semantic-idle-service"
;; 		 "define-semantic-decoration-style"
;; 		 "define-wisent-lexer"
;; 		 "semantic-alias-obsolete"
;; 		 "semantic-varalias-obsolete"
;; 		 "semantic-make-obsolete-overload"
;; 		 "defcustom-mode-local-semantic-dependency-system-include-path"
;; 		 ))
;;            (kf (if vf (regexp-opt vf t) ""))
;;            ;; Regexp depths
;;            (kv-depth (if kv (regexp-opt-depth kv) nil))
;;            (kf-depth (if kf (regexp-opt-depth kf) nil))
;;            )
;;       `((,(concat
;;            ;; Declarative things
;;            "(\\(" kv "\\|" kf "\\)"
;;            ;; Whitespaces & names
;;            "\\>[ \t]*\\(\\sw+\\)?[ \t]*\\(\\sw+\\)?"
;;            )
;;          (1 font-lock-keyword-face)
;;          (,(+ 1 kv-depth kf-depth 1)
;;           (cond ((match-beginning 2)
;;                  font-lock-type-face)
;;                 ((match-beginning ,(+ 1 kv-depth 1))
;;                  font-lock-function-name-face)
;;                 )
;;           nil t)
;;          (,(+ 1 kv-depth kf-depth 1 1)
;;           (cond ((match-beginning 2)
;;                  font-lock-variable-name-face)
;;                 )
;;           nil t)))
;;       ))
;;   "Highlighted Semantic keywords.")

;; (when (fboundp 'font-lock-add-keywords)
;;   (font-lock-add-keywords 'emacs-lisp-mode
;;                           semantic-fw-font-lock-keywords))

;;; Interfacing with edebug
;;
(defun semantic-fw-add-edebug-spec ()
  (def-edebug-spec semantic-exit-on-input 'def-body))

(add-hook 'edebug-setup-hook 'semantic-fw-add-edebug-spec)

(provide 'semantic/fw)

;;; semantic/fw.el ends here
