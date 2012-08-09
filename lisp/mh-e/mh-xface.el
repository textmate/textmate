;;; mh-xface.el --- MH-E X-Face and Face header field display

;; Copyright (C) 2002-2003, 2005-2012  Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
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

(autoload 'message-fetch-field "message")

(defvar mh-show-xface-function
  (cond ((and (featurep 'xemacs) (locate-library "x-face") (not (featurep 'xface)))
         (load "x-face" t t)
         #'mh-face-display-function)
        ((>= emacs-major-version 21)
         #'mh-face-display-function)
        (t #'ignore))
  "Determine at run time what function should be called to display X-Face.")

(defvar mh-uncompface-executable
  (and (fboundp 'executable-find) (executable-find "uncompface")))



;;; X-Face Display

;;;###mh-autoload
(defun mh-show-xface ()
  "Display X-Face."
  (when (and window-system mh-show-use-xface-flag
             (or mh-decode-mime-flag mh-mhl-format-file
                 mh-clean-message-header-flag))
    (funcall mh-show-xface-function)))

(defun mh-face-display-function ()
  "Display a Face, X-Face, or X-Image-URL header field.
If more than one of these are present, then the first one found
in this order is used."
  (save-restriction
    (goto-char (point-min))
    (re-search-forward "\n\n" (point-max) t)
    (narrow-to-region (point-min) (point))
    (let* ((case-fold-search t)
           (face (message-fetch-field "face" t))
           (x-face (message-fetch-field "x-face" t))
           (url (message-fetch-field "x-image-url" t))
           raw type)
      (cond (face (setq raw (mh-face-to-png face)
                        type 'png))
            (x-face (setq raw (mh-uncompface x-face)
                          type 'pbm))
            (url (setq type 'url))
            (t (multiple-value-setq (type raw)
                 (values-list (mh-picon-get-image)))))
      (when type
        (goto-char (point-min))
        (when (re-search-forward "^from:" (point-max) t)
          ;; GNU Emacs
          (mh-do-in-gnu-emacs
            (if (eq type 'url)
                (mh-x-image-url-display url)
              (mh-funcall-if-exists
               insert-image (create-image
                             raw type t
                             :foreground
                             (mh-face-foreground 'mh-show-xface nil t)
                             :background
                             (mh-face-background 'mh-show-xface nil t))
               " ")))
          ;; XEmacs
          (mh-do-in-xemacs
            (cond
             ((eq type 'url)
              (mh-x-image-url-display url))
             ((eq type 'png)
              (when (featurep 'png)
                (set-extent-begin-glyph
                 (make-extent (point) (point))
                 (make-glyph (vector 'png ':data (mh-face-to-png face))))))
             ;; Try internal xface support if available...
             ((and (eq type 'pbm) (featurep 'xface))
              (set-glyph-face
               (set-extent-begin-glyph
                (make-extent (point) (point))
                (make-glyph (vector 'xface ':data (concat "X-Face: " x-face))))
               'mh-show-xface))
             ;; Otherwise try external support with x-face...
             ((and (eq type 'pbm)
                   (fboundp 'x-face-xmas-wl-display-x-face)
                   (fboundp 'executable-find) (executable-find "uncompface"))
              (mh-funcall-if-exists x-face-xmas-wl-display-x-face))
             ;; Picon display
             ((and raw (member type '(xpm xbm gif)))
              (when (featurep type)
                (set-extent-begin-glyph
                 (make-extent (point) (point))
                 (make-glyph (vector type ':data raw))))))
            (when raw (insert " "))))))))

(defun mh-face-to-png (data)
  "Convert base64 encoded DATA to png image."
  (with-temp-buffer
    (if (fboundp 'set-buffer-multibyte)
        (set-buffer-multibyte nil))
    (insert data)
    (ignore-errors (base64-decode-region (point-min) (point-max)))
    (buffer-string)))

(defun mh-uncompface (data)
  "Run DATA through `uncompface' to generate bitmap."
  (with-temp-buffer
    (if (fboundp 'set-buffer-multibyte)
        (set-buffer-multibyte nil))
    (insert data)
    (when (and mh-uncompface-executable
               (equal (call-process-region (point-min) (point-max)
                                           mh-uncompface-executable t '(t nil))
                      0))
      (mh-icontopbm)
      (buffer-string))))

(defun mh-icontopbm ()
  "Elisp substitute for `icontopbm'."
  (goto-char (point-min))
  (let ((end (point-max)))
    (while (re-search-forward "0x\\(..\\)\\(..\\)," nil t)
      (save-excursion
        (goto-char (point-max))
        (insert (string-to-number (match-string 1) 16))
        (insert (string-to-number (match-string 2) 16))))
    (delete-region (point-min) end)
    (goto-char (point-min))
    (insert "P4\n48 48\n")))



;;; Picon Display

;; XXX: This should be customizable. As a side-effect of setting this
;;   variable, arrange to reset mh-picon-existing-directory-list to 'unset.
(defvar mh-picon-directory-list
  '("~/.picons" "~/.picons/users" "~/.picons/usenix" "~/.picons/news"
    "~/.picons/domains" "~/.picons/misc"
    "/usr/share/picons/" "/usr/share/picons/users" "/usr/share/picons/usenix"
    "/usr/share/picons/news" "/usr/share/picons/domains"
    "/usr/share/picons/misc")
  "List of directories where picons reside.
The directories are searched for in the order they appear in the list.")

(defvar mh-picon-existing-directory-list 'unset
  "List of directories to search in.")

(defvar mh-picon-cache (make-hash-table :test #'equal))

(defvar mh-picon-image-types
  (loop for type in '(xpm xbm gif)
        when (or (mh-do-in-gnu-emacs
                   (ignore-errors
                     (mh-funcall-if-exists image-type-available-p type)))
                 (mh-do-in-xemacs (featurep type)))
        collect type))

(autoload 'message-tokenize-header "sendmail")

(defun* mh-picon-get-image ()
  "Find the best possible match and return contents."
  (mh-picon-set-directory-list)
  (save-restriction
    (let* ((from-field (ignore-errors (car (message-tokenize-header
                                            (mh-get-header-field "from:")))))
           (from (car (ignore-errors
                        (mh-funcall-if-exists ietf-drums-parse-address
                                              from-field))))
           (host (and from
                      (string-match "\\([^+]*\\)\\(+.*\\)?@\\(.*\\)" from)
                      (downcase (match-string 3 from))))
           (user (and host (downcase (match-string 1 from))))
           (canonical-address (format "%s@%s" user host))
           (cached-value (gethash canonical-address mh-picon-cache))
           (host-list (and host (delete "" (split-string host "\\."))))
           (match nil))
      (cond (cached-value (return-from mh-picon-get-image cached-value))
            ((not host-list) (return-from mh-picon-get-image nil)))
      (setq match
            (block loop
              ;; u@h search
              (loop for dir in mh-picon-existing-directory-list
                    do (loop for type in mh-picon-image-types
                             ;; [path]user@host
                             for file1 = (format "%s/%s.%s"
                                                 dir canonical-address type)
                             when (file-exists-p file1)
                             do (return-from loop file1)
                             ;; [path]user
                             for file2 = (format "%s/%s.%s" dir user type)
                             when (file-exists-p file2)
                             do (return-from loop file2)
                             ;; [path]host
                             for file3 = (format "%s/%s.%s" dir host type)
                             when (file-exists-p file3)
                             do (return-from loop file3)))
              ;; facedb search
              ;; Search order for user@foo.net:
              ;;   [path]net/foo/user
              ;;   [path]net/foo/user/face
              ;;   [path]net/user
              ;;   [path]net/user/face
              ;;   [path]net/foo/unknown
              ;;   [path]net/foo/unknown/face
              ;;   [path]net/unknown
              ;;   [path]net/unknown/face
              (loop for u in (list user "unknown")
                    do (loop for dir in mh-picon-existing-directory-list
                             do (loop for x on host-list by #'cdr
                                      for y = (mh-picon-generate-path x u dir)
                                      do (loop for type in mh-picon-image-types
                                               for z1 = (format "%s.%s" y type)
                                               when (file-exists-p z1)
                                               do (return-from loop z1)
                                               for z2 = (format "%s/face.%s"
                                                                y type)
                                               when (file-exists-p z2)
                                               do (return-from loop z2)))))))
      (setf (gethash canonical-address mh-picon-cache)
            (mh-picon-file-contents match)))))

(defun mh-picon-set-directory-list ()
  "Update `mh-picon-existing-directory-list' if needed."
  (when (eq mh-picon-existing-directory-list 'unset)
    (setq mh-picon-existing-directory-list
          (loop for x in mh-picon-directory-list
                when (file-directory-p x) collect x))))

(defun mh-picon-generate-path (host-list user directory)
  "Generate the image file path.
HOST-LIST is the parsed host address of the email address, USER
the username and DIRECTORY is the directory relative to which the
path is generated."
  (loop with acc = ""
        for elem in host-list
        do (setq acc (format "%s/%s" elem acc))
        finally return (format "%s/%s%s" directory acc user)))

(defun mh-picon-file-contents (file)
  "Return details about FILE.
A list of consisting of a symbol for the type of the file and the
file contents as a string is returned. If FILE is nil, then both
elements of the list are nil."
  (if (stringp file)
      (with-temp-buffer
        (if (fboundp 'set-buffer-multibyte)
            (set-buffer-multibyte nil))
        (let ((type (and (string-match ".*\\.\\(...\\)$" file)
                         (intern (match-string 1 file)))))
          (insert-file-contents-literally file)
          (list type (buffer-string))))
    (list nil nil)))



;;; X-Image-URL Display

(defvar mh-x-image-scaling-function
  (cond ((executable-find "convert")
         'mh-x-image-scale-with-convert)
        ((and (executable-find "anytopnm") (executable-find "pnmscale")
              (executable-find "pnmtopng"))
         'mh-x-image-scale-with-pnm)
        (t 'ignore))
  "Function to use to scale image to proper size.")

(defun mh-x-image-scale-with-pnm (input output)
  "Scale image in INPUT file and write to OUTPUT file using pnm tools."
  (let ((res (shell-command-to-string
              (format "anytopnm < %s | pnmscale -xysize 96 48 | pnmtopng > %s"
                      input output))))
    (unless (equal res "")
      (delete-file output))))

(defun mh-x-image-scale-with-convert (input output)
  "Scale image in INPUT file and write to OUTPUT file using ImageMagick."
  (call-process "convert" nil nil nil "-geometry" "96x48" input output))

(defvar mh-wget-executable nil)
(defvar mh-wget-choice
  (or (and (setq mh-wget-executable (executable-find "wget")) 'wget)
      (and (setq mh-wget-executable (executable-find "fetch")) 'fetch)
      (and (setq mh-wget-executable (executable-find "curl")) 'curl)))
(defvar mh-wget-option
  (cdr (assoc mh-wget-choice '((curl . "-o") (fetch . "-o") (wget . "-O")))))
(defvar mh-x-image-temp-file nil)
(defvar mh-x-image-url nil)
(defvar mh-x-image-marker nil)
(defvar mh-x-image-url-cache-file nil)

(defun mh-x-image-url-display (url)
  "Display image from location URL.
If the URL isn't present in the cache then it is fetched with wget."
  (let* ((cache-filename (mh-x-image-url-cache-canonicalize url))
         (state (mh-x-image-get-download-state cache-filename))
         (marker (set-marker (make-marker) (point))))
    (set (make-local-variable 'mh-x-image-marker) marker)
    (cond ((not (mh-x-image-url-sane-p url)))
          ((eq state 'ok)
           (mh-x-image-display cache-filename marker))
          ((or (not mh-wget-executable)
               (eq mh-x-image-scaling-function 'ignore)))
          ((eq state 'never))
          ((not mh-fetch-x-image-url)
           (set-marker marker nil))
          ((eq state 'try-again)
           (mh-x-image-set-download-state cache-filename nil)
           (mh-x-image-url-fetch-image url cache-filename marker
                                       'mh-x-image-scale-and-display))
          ((and (eq mh-fetch-x-image-url 'ask)
                (not (y-or-n-p (format "Fetch %s? " url))))
           (mh-x-image-set-download-state cache-filename 'never))
          ((eq state nil)
           (mh-x-image-url-fetch-image url cache-filename marker
                                       'mh-x-image-scale-and-display)))))

(defvar mh-x-image-cache-directory nil
  "Directory where X-Image-URL images are cached.")

;;;###mh-autoload
(defun mh-set-x-image-cache-directory (directory)
  "Set the DIRECTORY where X-Image-URL images are cached.
This is only done if `mh-x-image-cache-directory' is nil."
  ;; XXX This is the code that used to be in find-user-path. Is there
  ;; a good reason why the variable is set conditionally? Do we expect
  ;; the user to have set this variable directly?
  (unless mh-x-image-cache-directory
    (setq mh-x-image-cache-directory directory)))

(defun mh-x-image-url-cache-canonicalize (url)
  "Canonicalize URL.
Replace the ?/ character with a ?! character and append .png.
Also replaces special characters with `mh-url-hexify-string'
since not all characters, such as :, are valid within Windows
filenames.  In addition, replaces * with %2a. See URL
`http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/shell/reference/ifaces/iitemnamelimits/GetValidCharacters.asp'."
  (format "%s/%s.png" mh-x-image-cache-directory
          (mh-replace-regexp-in-string
           "\*" "%2a"
           (mh-url-hexify-string
            (with-temp-buffer
              (insert url)
              (mh-replace-string "/" "!")
              (buffer-string))))))

(defun mh-x-image-get-download-state (file)
  "Check the state of FILE by following any symbolic links."
  (unless (file-exists-p mh-x-image-cache-directory)
    (call-process "mkdir" nil nil nil mh-x-image-cache-directory))
  (cond ((file-symlink-p file)
         (intern (file-name-nondirectory (file-chase-links file))))
        ((not (file-exists-p file)) nil)
        (t 'ok)))

(defun mh-x-image-set-download-state (file data)
  "Setup a symbolic link from FILE to DATA."
  (if data
      (make-symbolic-link (symbol-name data) file t)
    (delete-file file)))

(defun mh-x-image-url-sane-p (url)
  "Check if URL is something sensible."
  (let ((len (length url)))
    (cond ((< len 5) nil)
          ((not (equal (substring url 0 5) "http:")) nil)
          ((> len 100) nil)
          (t t))))

(defun mh-x-image-display (image marker)
  "Display IMAGE at MARKER."
  (with-current-buffer (marker-buffer marker)
    (let ((inhibit-read-only t)
          (buffer-modified-flag (buffer-modified-p)))
      (unwind-protect
          (when (and (file-readable-p image) (not (file-symlink-p image))
                     (eq marker mh-x-image-marker))
            (goto-char marker)
            (mh-do-in-gnu-emacs
              (mh-funcall-if-exists insert-image (create-image image 'png)))
            (mh-do-in-xemacs
              (when (featurep 'png)
                (set-extent-begin-glyph
                 (make-extent (point) (point))
                 (make-glyph
                  (vector 'png ':data (with-temp-buffer
                                        (insert-file-contents-literally image)
                                        (buffer-string))))))))
        (set-buffer-modified-p buffer-modified-flag)))))

(defun mh-x-image-url-fetch-image (url cache-file marker sentinel)
  "Fetch and display the image specified by URL.
After the image is fetched, it is stored in CACHE-FILE. It will
be displayed in a buffer and position specified by MARKER. The
actual display is carried out by the SENTINEL function."
  (if mh-wget-executable
      (let ((buffer (get-buffer-create (generate-new-buffer-name
                                        mh-temp-fetch-buffer)))
            (filename (or (mh-funcall-if-exists make-temp-file "mhe-fetch")
                          (expand-file-name (make-temp-name "~/mhe-fetch")))))
        (with-current-buffer buffer
          (set (make-local-variable 'mh-x-image-url-cache-file) cache-file)
          (set (make-local-variable 'mh-x-image-marker) marker)
          (set (make-local-variable 'mh-x-image-temp-file) filename))
        (set-process-sentinel
         (start-process "*mh-x-image-url-fetch*" buffer
                        mh-wget-executable mh-wget-option filename url)
         sentinel))
    ;; Temporary failure
    (mh-x-image-set-download-state cache-file 'try-again)))

(defun mh-x-image-scale-and-display (process change)
  "When the wget PROCESS terminates scale and display image.
The argument CHANGE is ignored."
  (when (eq (process-status process) 'exit)
    (let (marker temp-file cache-filename wget-buffer)
      (with-current-buffer (setq wget-buffer (process-buffer process))
        (setq marker mh-x-image-marker
              cache-filename mh-x-image-url-cache-file
              temp-file mh-x-image-temp-file))
      (cond
       ;; Check if we have `convert'
       ((eq mh-x-image-scaling-function 'ignore)
        (message "The \"convert\" program is needed to display X-Image-URL")
        (mh-x-image-set-download-state cache-filename 'try-again))
       ;; Scale fetched image
       ((and (funcall mh-x-image-scaling-function temp-file cache-filename)
             nil))
       ;; Attempt to display image if we have it
       ((file-exists-p cache-filename)
        (mh-x-image-display cache-filename marker))
       ;; We didn't find the image. Should we try to display it the next time?
       (t (mh-x-image-set-download-state cache-filename 'try-again)))
      (ignore-errors
        (set-marker marker nil)
        (delete-process process)
        (kill-buffer wget-buffer)
        (delete-file temp-file)))))

(provide 'mh-xface)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-xface.el ends here
