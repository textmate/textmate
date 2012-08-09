;;; mh-alias.el --- MH-E mail alias completion and expansion

;; Copyright (C) 1994-1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Peter S. Galbraith <psg@debian.org>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;;; Change Log:

;;; Code:

(require 'mh-e)

(mh-require-cl)

(require 'goto-addr)

(defvar mh-alias-alist 'not-read
  "Alist of MH aliases.")
(defvar mh-alias-blind-alist nil
  "Alist of MH aliases that are blind lists.")
(defvar mh-alias-passwd-alist nil
  "Alist of aliases extracted from passwd file and their expansions.")
(defvar mh-alias-tstamp nil
  "Time aliases were last loaded.")
(defvar mh-alias-read-address-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map "," 'mh-alias-minibuffer-confirm-address)
    (define-key map " " 'self-insert-command)
    map))

(defvar mh-alias-system-aliases
  '("/etc/nmh/MailAliases" "/etc/mh/MailAliases"
    "/usr/lib/mh/MailAliases" "/usr/share/mailutils/mh/MailAliases"
    "/etc/passwd")
  "*A list of system files which are a source of aliases.
If these files are modified, they are automatically reread. This list
need include only system aliases and the passwd file, since personal
alias files listed in your \"Aliasfile:\" MH profile component are
automatically included. You can update the alias list manually using
\\[mh-alias-reload].")



;;; Alias Loading

(defun mh-alias-tstamp (arg)
  "Check whether alias files have been modified.
Return t if any file listed in the Aliasfile MH profile component has
been modified since the timestamp.
If ARG is non-nil, set timestamp with the current time."
  (if arg
      (let ((time (current-time)))
        (setq mh-alias-tstamp (list (nth 0 time) (nth 1 time))))
    (let ((stamp))
      (car (memq t (mapcar
                    (function
                     (lambda (file)
                       (when (and file (file-exists-p file))
                         (setq stamp (nth 5 (file-attributes file)))
                         (or (> (car stamp) (car mh-alias-tstamp))
                             (and (= (car stamp) (car mh-alias-tstamp))
                                  (> (cadr stamp) (cadr mh-alias-tstamp)))))))
                    (mh-alias-filenames t)))))))

(defun mh-alias-filenames (arg)
  "Return list of filenames that contain aliases.
The filenames come from the Aliasfile profile component and are
expanded.
If ARG is non-nil, filenames listed in `mh-alias-system-aliases' are
appended."
  (or mh-progs (mh-find-path))
  (save-excursion
    (let* ((filename (mh-profile-component "Aliasfile"))
           (filelist (and filename (split-string filename "[ \t]+")))
           (userlist
            (mapcar
             (function
              (lambda (file)
                (if (and mh-user-path file
                         (file-exists-p (expand-file-name file mh-user-path)))
                    (expand-file-name file mh-user-path))))
             filelist)))
      (if arg
          (if (stringp mh-alias-system-aliases)
              (append userlist (list mh-alias-system-aliases))
            (append userlist mh-alias-system-aliases))
        userlist))))

(defun mh-alias-gecos-name (gecos-name username comma-separator)
  "Return a usable address string from a GECOS-NAME and USERNAME.
Use only part of the GECOS-NAME up to the first comma if
COMMA-SEPARATOR is non-nil."
  (let ((res gecos-name))
    ;; Keep only string until first comma if COMMA-SEPARATOR is t.
    (if (and comma-separator
             (string-match "^\\([^,]+\\)," res))
        (setq res (match-string 1 res)))
    ;; Replace "&" with capitalized username
    (if (string-match "&" res)
        (setq res (mh-replace-regexp-in-string "&" (capitalize username) res)))
    ;; Remove " character
    (if (string-match "\"" res)
        (setq res (mh-replace-regexp-in-string "\"" "" res)))
    ;; If empty string, use username instead
    (if (string-equal "" res)
        (setq res username))
    ;; Surround by quotes if doesn't consist of simple characters
    (if (not (string-match "^[ a-zA-Z0-9-]+$" res))
        (setq res (concat "\"" res "\"")))
    res))

(defun mh-alias-local-users ()
  "Return an alist of local users from /etc/passwd.
Exclude all aliases already in `mh-alias-alist' from \"ali\""
  (let (passwd-alist)
    (with-current-buffer (get-buffer-create mh-temp-buffer)
      (erase-buffer)
      (cond
       ((eq mh-alias-local-users t)
        (if (file-readable-p "/etc/passwd")
            (insert-file-contents "/etc/passwd")))
       ((stringp mh-alias-local-users)
        (insert mh-alias-local-users "\n")
        (shell-command-on-region (point-min) (point-max) mh-alias-local-users t)
        (goto-char (point-min))))
      (while  (< (point) (point-max))
        (cond
         ((looking-at "\\([^:]*\\):[^:]*:\\([^:]*\\):[^:]*:\\([^:]*\\):")
          (when (> (string-to-number (match-string 2)) 200)
            (let* ((username (match-string 1))
                   (gecos-name (match-string 3))
                   (realname (mh-alias-gecos-name
                              gecos-name username
                              mh-alias-passwd-gecos-comma-separator-flag))
                   (alias-name (if mh-alias-local-users-prefix
                                   (concat mh-alias-local-users-prefix
                                           (mh-alias-suggest-alias realname t))
                                 username))
                   (alias-translation
                    (if (string-equal username realname)
                        (concat "<" username ">")
                      (concat realname " <" username ">"))))
              (when (not (mh-assoc-string alias-name mh-alias-alist t))
                (setq passwd-alist (cons (list alias-name alias-translation)
                                         passwd-alist)))))))
        (forward-line 1)))
    passwd-alist))

(defun mh-alias-reload ()
  "Reload MH aliases.

Since aliases are updated frequently, MH-E reloads aliases
automatically whenever an alias lookup occurs if an alias source has
changed. Sources include files listed in your \"Aliasfile:\" profile
component and your password file if option `mh-alias-local-users' is
turned on. However, you can reload your aliases manually by calling
this command directly.

This function runs `mh-alias-reloaded-hook' after the aliases have
been loaded."
  (interactive)
  (save-excursion
    (message "Loading MH aliases...")
    (mh-alias-tstamp t)
    (mh-exec-cmd-quiet t "ali" "-nolist" "-nouser")
    (setq mh-alias-alist nil)
    (setq mh-alias-blind-alist nil)
    (while  (< (point) (point-max))
      (cond
       ((looking-at "^[ \t]"))          ;Continuation line
       ((looking-at "\\(.+\\): .+: .*$") ; A new -blind- MH alias
        (when (not (mh-assoc-string (match-string 1) mh-alias-blind-alist t))
          (setq mh-alias-blind-alist
                (cons (list (match-string 1)) mh-alias-blind-alist))
          (setq mh-alias-alist (cons (list (match-string 1)) mh-alias-alist))))
       ((looking-at "\\(.+\\): .*$")    ; A new MH alias
        (when (not (mh-assoc-string (match-string 1) mh-alias-alist t))
          (setq mh-alias-alist
                (cons (list (match-string 1)) mh-alias-alist)))))
      (forward-line 1)))
  (when mh-alias-local-users
    (setq mh-alias-passwd-alist (mh-alias-local-users))
    ;; Update aliases with local users, but leave existing aliases alone.
    (let ((local-users mh-alias-passwd-alist)
          user)
      (while local-users
        (setq user (car local-users))
        (if (not (mh-assoc-string (car user) mh-alias-alist t))
            (setq mh-alias-alist (append mh-alias-alist (list user))))
        (setq local-users (cdr local-users)))))
  (run-hooks 'mh-alias-reloaded-hook)
  (message "Loading MH aliases...done"))

;;;###mh-autoload
(defun mh-alias-reload-maybe ()
  "Load new MH aliases."
  (if (or (eq mh-alias-alist 'not-read) ; Doesn't exist?
          (mh-alias-tstamp nil))        ; Out of date?
      (mh-alias-reload)))



;;; Alias Expansion

(defun mh-alias-ali (alias &optional user)
  "Return ali expansion for ALIAS.
ALIAS must be a string for a single alias.
If USER is t, then assume ALIAS is an address and call ali -user. ali
returns the string unchanged if not defined. The same is done here."
  (condition-case err
      (save-excursion
        (let ((user-arg (if user "-user" "-nouser")))
          (mh-exec-cmd-quiet t "ali" user-arg "-nolist" alias))
        (goto-char (point-max))
        (if (looking-at "^$") (delete-char -1))
        (buffer-substring (point-min)(point-max)))
    (error (progn
             (message "%s" (error-message-string err))
             alias))))

;;;###mh-autoload
(defun mh-alias-expand (alias)
  "Return expansion for ALIAS.
Blind aliases or users from /etc/passwd are not expanded."
  (cond
   ((mh-assoc-string alias mh-alias-blind-alist t)
    alias)                              ; Don't expand a blind alias
   ((mh-assoc-string alias mh-alias-passwd-alist t)
    (cadr (mh-assoc-string alias mh-alias-passwd-alist t)))
   (t
    (mh-alias-ali alias))))

(mh-require 'crm nil t)                 ; completing-read-multiple
(mh-require 'multi-prompt nil t)

;;;###mh-autoload
(defun mh-read-address (prompt)
  "Read an address from the minibuffer with PROMPT."
  (mh-alias-reload-maybe)
  (if (not mh-alias-alist)              ; If still no aliases, just prompt
      (read-string prompt)
    (let* ((minibuffer-local-completion-map mh-alias-read-address-map)
           (completion-ignore-case mh-alias-completion-ignore-case-flag)
           (the-answer
            (cond ((fboundp 'completing-read-multiple)
                   (mh-funcall-if-exists
                    completing-read-multiple prompt mh-alias-alist nil nil))
                  ((featurep 'multi-prompt)
                   (mh-funcall-if-exists
                    multi-prompt "," nil prompt mh-alias-alist nil nil))
                  (t (split-string
                      (completing-read prompt mh-alias-alist nil nil) ",")))))
      (if (not mh-alias-expand-aliases-flag)
          (mapconcat 'identity the-answer ", ")
        ;; Loop over all elements, checking if in passwd alias or blind first
        (mapconcat 'mh-alias-expand the-answer ",\n ")))))

;;;###mh-autoload
(defun mh-alias-minibuffer-confirm-address ()
  "Display the alias expansion if `mh-alias-flash-on-comma' is non-nil."
  (interactive)
  (when mh-alias-flash-on-comma
    (save-excursion
      (let* ((case-fold-search t)
             (beg (mh-beginning-of-word))
             (the-name (buffer-substring-no-properties beg (point))))
        (if (mh-assoc-string the-name mh-alias-alist t)
            (message "%s -> %s" the-name (mh-alias-expand the-name))
          ;; Check if it was a single word likely to be an alias
          (if (and (equal mh-alias-flash-on-comma 1)
                   (not (string-match " " the-name)))
              (message "No alias for %s" the-name))))))
  (self-insert-command 1))

;;;###mh-autoload
(defun mh-alias-letter-expand-alias ()
  "Expand mail alias before point."
  (mh-alias-reload-maybe)
  (let* ((begin (mh-beginning-of-word))
         (end (save-excursion
                (goto-char begin)
                (mh-beginning-of-word -1))))
    (when (>= end (point))
      (list
       begin (if (fboundp 'completion-at-point) end (point))
       (if (not mh-alias-expand-aliases-flag)
           mh-alias-alist
         (lambda (string pred action)
           (case action
             ((nil)
              (let ((res (try-completion string mh-alias-alist pred)))
                (if (or (eq res t)
                        (and (stringp res)
                             (eq t (try-completion res mh-alias-alist pred))))
                    (or (mh-alias-expand (if (stringp res) res string))
                        res)
                  res)))
             ((t) (all-completions string mh-alias-alist pred))
             ((lambda) (mh-test-completion string mh-alias-alist pred)))))))))


;;; Alias File Updating

(defun mh-alias-suggest-alias (string &optional no-comma-swap)
  "Suggest an alias for STRING.
Don't reverse the order of strings separated by a comma if
NO-COMMA-SWAP is non-nil."
  (cond
   ((string-match "^<\\(.*\\)>$" string)
    ;; <somename@foo.bar>  -> recurse, stripping brackets.
    (mh-alias-suggest-alias (match-string 1 string) no-comma-swap))
   ((string-match "^\\sw+$" string)
    ;; One word -> downcase it.
    (downcase string))
   ((string-match "^\\(\\sw+\\)\\s-+\\(\\sw+\\)$" string)
    ;; Two words -> first.last
    (downcase
     (format "%s.%s" (match-string 1 string) (match-string 2 string))))
   ((string-match "^\\([-a-zA-Z0-9._]+\\)@[-a-zA-z0-9_]+\\.+[a-zA-Z0-9]+$"
                  string)
    ;; email only -> downcase username
    (downcase (match-string 1 string)))
   ((string-match "^\"\\(.*\\)\".*" string)
    ;; "Some name" <somename@foo.bar>  -> recurse -> "Some name"
    (mh-alias-suggest-alias (match-string 1 string) no-comma-swap))
   ((string-match "^\\(.*\\) +<.*>$" string)
    ;; Some name <somename@foo.bar>  -> recurse -> Some name
    (mh-alias-suggest-alias (match-string 1 string) no-comma-swap))
   ((string-match (concat goto-address-mail-regexp " +(\\(.*\\))$") string)
    ;; somename@foo.bar (Some name)  -> recurse -> Some name
    (mh-alias-suggest-alias (match-string 1 string) no-comma-swap))
   ((string-match "^\\(Dr\\|Prof\\)\\.? +\\(.*\\)" string)
    ;; Strip out title
    (mh-alias-suggest-alias (match-string 2 string) no-comma-swap))
   ((string-match "^\\(.*\\), +\\(Jr\\.?\\|II+\\)$" string)
    ;; Strip out tails with comma
    (mh-alias-suggest-alias (match-string 1 string) no-comma-swap))
   ((string-match "^\\(.*\\) +\\(Jr\\.?\\|II+\\)$" string)
    ;; Strip out tails
    (mh-alias-suggest-alias (match-string 1 string) no-comma-swap))
   ((string-match "^\\(\\sw+\\) +[A-Z]\\.? +\\(.*\\)$" string)
    ;; Strip out initials
    (mh-alias-suggest-alias
     (format "%s %s" (match-string 1 string) (match-string 2 string))
     no-comma-swap))
   ((and (not no-comma-swap)
         (string-match "^\\([^,]+\\), +\\(.*\\)$" string))
    ;; Reverse order of comma-separated fields to handle:
    ;;  From: "Galbraith, Peter" <psg@debian.org>
    ;; but don't this for a name string extracted from the passwd file
    ;; with mh-alias-passwd-gecos-comma-separator-flag set to nil.
    (mh-alias-suggest-alias
     (format "%s %s" (match-string 2 string) (match-string 1 string))
     no-comma-swap))
   (t
    ;; Output string, with spaces replaced by dots.
    (mh-alias-canonicalize-suggestion string))))

(defun mh-alias-canonicalize-suggestion (string)
  "Process STRING to replace spaces by periods.
First all spaces and commas are replaced by periods. Then every run of
consecutive periods are replaced with a single period. Finally the
string is converted to lower case."
  (with-temp-buffer
    (insert string)
    ;; Replace spaces with periods
    (goto-char (point-min))
    (while (re-search-forward " +" nil t)
      (replace-match "." nil nil))
    ;; Replace commas with periods
    (goto-char (point-min))
    (while (re-search-forward ",+" nil t)
      (replace-match "." nil nil))
    ;; Replace consecutive periods with a single period
    (goto-char (point-min))
    (while (re-search-forward "\\.\\.+" nil t)
      (replace-match "." nil nil))
    ;; Convert to lower case
    (downcase-region (point-min) (point-max))
    ;; Whew! all done...
    (buffer-string)))

(defun mh-alias-which-file-has-alias (alias file-list)
  "Return the name of writable file which defines ALIAS from list FILE-LIST."
  (with-current-buffer (get-buffer-create mh-temp-buffer)
    (let ((the-list file-list)
          (found))
      (while the-list
        (erase-buffer)
        (when (file-writable-p (car file-list))
          (insert-file-contents (car file-list))
          (if (re-search-forward (concat "^" (regexp-quote alias) ":") nil t)
              (setq found (car file-list)
                    the-list nil)
            (setq the-list (cdr the-list)))))
      found)))

(defun mh-alias-insert-file (&optional alias)
  "Return filename which should be used to add ALIAS.
The value of the option `mh-alias-insert-file' is used if non-nil\;
otherwise the value of the \"Aliasfile:\" profile component is used.
If the alias already exists, try to return the name of the file that
contains it."
  (cond
   ((and mh-alias-insert-file (listp mh-alias-insert-file))
    (if (not (elt mh-alias-insert-file 1))        ; Only one entry, use it
        (car mh-alias-insert-file)
      (if (or (not alias)
              (string-equal alias (mh-alias-ali alias))) ;alias doesn't exist
          (completing-read "Alias file: "
                           (mapcar 'list mh-alias-insert-file) nil t)
        (or (mh-alias-which-file-has-alias alias mh-alias-insert-file)
            (completing-read "Alias file: "
                             (mapcar 'list mh-alias-insert-file) nil t)))))
   ((and mh-alias-insert-file (stringp mh-alias-insert-file))
    mh-alias-insert-file)
   (t
    ;; writable ones returned from (mh-alias-filenames):
    (let ((autolist (delq nil (mapcar (lambda (file)
                                        (if (and (file-writable-p file)
                                                 (not (string-equal
                                                       file "/etc/passwd")))
                                            file))
                                     (mh-alias-filenames t)))))
      (cond
       ((not autolist)
        (error "No writable alias file;
set `mh-alias-insert-file' or the \"Aliasfile:\" profile component"))
       ((not (elt autolist 1))        ; Only one entry, use it
        (car autolist))
       ((or (not alias)
            (string-equal alias (mh-alias-ali alias))) ;alias doesn't exist
        (completing-read "Alias file: " (mapcar 'list autolist) nil t))
       (t
        (or (mh-alias-which-file-has-alias alias autolist)
            (completing-read "Alias file: "
                             (mapcar 'list autolist) nil t))))))))

;;;###mh-autoload
(defun mh-alias-address-to-alias (address)
  "Return the ADDRESS alias if defined, or nil."
  (let* ((aliases (mh-alias-ali address t)))
    (if (string-equal aliases address)
        nil                             ; ali returned same string -> no.
      ;; Double-check that we have an individual alias. This means that the
      ;; alias doesn't expand into a list (of which this address is part).
      (car (delq nil (mapcar
                      (function
                       (lambda (alias)
                         (let ((recurse (mh-alias-ali alias nil)))
                           (if (string-match ".*,.*" recurse)
                               nil
                             alias))))
                      (split-string aliases ", +")))))))

;;;###mh-autoload
(defun mh-alias-for-from-p ()
  "Return t if sender's address has a corresponding alias."
  (mh-alias-reload-maybe)
  (save-excursion
    (if (not (mh-folder-line-matches-show-buffer-p))
        nil                             ;No corresponding show buffer
      (if (eq major-mode 'mh-folder-mode)
          (set-buffer mh-show-buffer))
      (let ((from-header (mh-extract-from-header-value)))
        (and from-header
             (mh-alias-address-to-alias from-header))))))

(defun mh-alias-add-alias-to-file (alias address &optional file)
  "Add ALIAS for ADDRESS in alias FILE without alias check or prompts.
Prompt for alias file if not provided and there is more than one
candidate.

If the alias exists already, you will have the choice of
inserting the new alias before or after the old alias. In the
former case, this alias will be used when sending mail to this
alias. In the latter case, the alias serves as an additional
folder name hint when filing messages."
  (if (not file)
      (setq file (mh-alias-insert-file alias)))
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (let ((alias-search (concat alias ":"))
          (letter)
          (case-fold-search t))
      (cond
       ;; Search for exact match (if we had the same alias before)
       ((re-search-forward
         (concat "^" (regexp-quote alias-search) " *\\(.*\\)") nil t)
        (let ((answer (read-string
                       (format (concat "Alias %s exists; insert new address "
                                       "[b]efore or [a]fter: ")
                               (match-string 1))))
              (case-fold-search t))
          (cond ((string-match "^b" answer))
                ((string-match "^a" answer)
                 (forward-line 1))
                (t
                 (error "Unrecognized response")))))
       ;; No, so sort-in at the right place
       ;; search for "^alias", then "^alia", etc.
       ((eq mh-alias-insertion-location 'sorted)
        (setq letter       (substring alias-search -1)
              alias-search (substring alias-search 0 -1))
        (while (and (not (equal alias-search ""))
                    (not (re-search-forward
                          (concat "^" (regexp-quote alias-search)) nil t)))
          (setq letter       (substring alias-search -1)
                alias-search (substring alias-search 0 -1)))
        ;; Next, move forward to sort alphabetically for following letters
        (beginning-of-line)
        (while (re-search-forward
                (concat "^" (regexp-quote alias-search) "[a-" letter "]")
                nil t)
          (forward-line 1)))
       ((eq mh-alias-insertion-location 'bottom)
        (goto-char (point-max)))
       ((eq mh-alias-insertion-location 'top)
        (goto-char (point-min)))))
    (beginning-of-line)
    (insert (format "%s: %s\n" alias address))
    (save-buffer)))

(defun mh-alias-add-alias (alias address)
  "Add ALIAS for ADDRESS in personal alias file.

This function prompts you for an alias and address. If the alias
exists already, you will have the choice of inserting the new
alias before or after the old alias. In the former case, this
alias will be used when sending mail to this alias. In the latter
case, the alias serves as an additional folder name hint when
filing messages."
  (interactive "P\nP")
  (mh-alias-reload-maybe)
  (setq alias (completing-read "Alias: " mh-alias-alist nil nil alias))
  (if (and address (string-match "^<\\(.*\\)>$" address))
      (setq address (match-string 1 address)))
  (setq address (read-string "Address: " address))
  (if (string-match "^<\\(.*\\)>$" address)
      (setq address (match-string 1 address)))
  (let ((address-alias (mh-alias-address-to-alias address))
        (alias-address (mh-alias-expand alias)))
    (if (string-equal alias-address alias)
        (setq alias-address nil))
    (cond
     ((and (equal alias address-alias)
           (equal address alias-address))
      (message "Already defined as %s" alias-address))
     (address-alias
      (if (y-or-n-p (format "Address has alias %s; set new one? "
                            address-alias))
          (mh-alias-add-alias-to-file alias address)))
     (t
      (mh-alias-add-alias-to-file alias address)))))

;;;###mh-autoload
(defun mh-alias-grab-from-field ()
  "Add alias for the sender of the current message."
  (interactive)
  (mh-alias-reload-maybe)
  (save-excursion
    (cond
     ((mh-folder-line-matches-show-buffer-p)
      (set-buffer mh-show-buffer))
     ((and (eq major-mode 'mh-folder-mode)
           (mh-get-msg-num nil))
      (set-buffer (get-buffer-create mh-temp-buffer))
      (insert-file-contents (mh-msg-filename (mh-get-msg-num t))))
     ((eq major-mode 'mh-folder-mode)
      (error "Cursor not pointing to a message")))
    (let* ((address (or (mh-extract-from-header-value)
                        (error "Message has no From: header")))
           (alias (mh-alias-suggest-alias address)))
      (mh-alias-add-alias alias address))))

(defun mh-alias-add-address-under-point ()
  "Insert an alias for address under point."
  (interactive)
  (let ((address (goto-address-find-address-at-point)))
    (if address
        (mh-alias-add-alias nil address)
      (message "No email address found under point"))))

(defun mh-alias-apropos (regexp)
  "Show all aliases or addresses that match a regular expression REGEXP."
  (interactive "sAlias regexp: ")
  (if mh-alias-local-users
      (mh-alias-reload-maybe))
  (let ((matches "")
        (group-matches "")
        (passwd-matches))
    (save-excursion
      (message "Reading MH aliases...")
      (mh-exec-cmd-quiet t "ali" "-nolist" "-nouser")
      (message "Parsing MH aliases...")
      (while (re-search-forward regexp nil t)
        (beginning-of-line)
        (cond
         ((looking-at "^[ \t]")         ;Continuation line
          (setq group-matches
                (concat group-matches
                        (buffer-substring
                         (save-excursion
                           (or (re-search-backward "^[^ \t]" nil t)
                               (point)))
                         (progn
                           (if (re-search-forward  "^[^ \t]" nil t)
                               (forward-char -1))
                           (point))))))
         (t
          (setq matches
                (concat matches
                        (buffer-substring (point)(progn (end-of-line)(point)))
                        "\n")))))
      (message "Parsing MH aliases...done")
      (when mh-alias-local-users
        (message "Making passwd aliases...")
        (setq passwd-matches
              (mapconcat
               (lambda (elem)
                 (if (or (string-match regexp (car elem))
                         (string-match regexp (cadr elem)))
                     (format "%s: %s\n" (car elem) (cadr elem))))
               mh-alias-passwd-alist ""))
        (message "Making passwd aliases...done")))
    (if (and (string-equal "" matches)
             (string-equal "" group-matches)
             (string-equal "" passwd-matches))
        (message "No matches")
      (with-output-to-temp-buffer mh-aliases-buffer
        (if (not (string-equal "" matches))
            (princ matches))
        (when (not (string-equal group-matches ""))
          (princ "\nGroup Aliases:\n\n")
          (princ group-matches))
        (when (not (string-equal passwd-matches ""))
          (princ "\nLocal User Aliases:\n\n")
          (princ passwd-matches))))))

(defun mh-folder-line-matches-show-buffer-p ()
  "Return t if the message under point in folder-mode is in the show buffer.
Return nil in any other circumstance (no message under point, no
show buffer, the message in the show buffer doesn't match."
  (and (eq major-mode 'mh-folder-mode)
       (mh-get-msg-num nil)
       mh-show-buffer
       (get-buffer mh-show-buffer)
       (buffer-file-name (get-buffer mh-show-buffer))
       (string-match ".*/\\([0-9]+\\)$"
                     (buffer-file-name (get-buffer mh-show-buffer)))
       (string-equal
        (match-string 1 (buffer-file-name (get-buffer mh-show-buffer)))
        (int-to-string (mh-get-msg-num nil)))))

(provide 'mh-alias)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-alias.el ends here
