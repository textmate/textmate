;;; mh-compat.el --- make MH-E compatible with various versions of Emacs

;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

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

;; This is a good place to gather code that is used for compatibility
;; between different versions of Emacs. Please document which versions
;; of Emacs that the defsubst, defalias, or defmacro applies. That
;; way, it's easy to occasionally go through this file and see which
;; macros we can retire.

;; Please use mh-gnus.el when providing compatibility with different
;; versions of Gnus.

;; Items are listed alphabetically (except for mh-require which is
;; needed sooner it would normally appear).

(require 'mh-acros)

(mh-do-in-gnu-emacs
  (defalias 'mh-require 'require))

(mh-do-in-xemacs
  (defun mh-require (feature &optional filename noerror)
    "If feature FEATURE is not loaded, load it from FILENAME.
If FEATURE is not a member of the list `features', then the feature
is not loaded; so load the file FILENAME.
If FILENAME is omitted, the printname of FEATURE is used as the file name.
If the optional third argument NOERROR is non-nil,
then return nil if the file is not found instead of signaling an error.

Simulate NOERROR argument in XEmacs which lacks it."
    (if (not (featurep feature))
        (if filename
            (load filename noerror t)
          (load (format "%s" feature) noerror t)))))

(defun-mh mh-assoc-string assoc-string (key list case-fold)
  "Like `assoc' but specifically for strings.
Case is ignored if CASE-FOLD is non-nil.
This function is used by Emacs versions that lack `assoc-string',
introduced in Emacs 22."
  (if case-fold
      (assoc-ignore-case key list)
    (assoc key list)))

;; For XEmacs.
(defalias 'mh-cancel-timer
  (if (fboundp 'cancel-timer)
      'cancel-timer
    'delete-itimer))

(defun mh-display-color-cells (&optional display)
  "Return the number of color cells supported by DISPLAY.
This function is used by XEmacs to return 2 when `device-color-cells'
or `display-color-cells' returns nil. This happens when compiling or
running on a tty and causes errors since `display-color-cells' is
expected to return an integer."
  (cond ((fboundp 'display-color-cells) ; GNU Emacs, XEmacs 21.5b28
         (or (display-color-cells display) 2))
        ((fboundp 'device-color-cells)  ; XEmacs 21.4
         (or (device-color-cells display) 2))
        (t 2)))

(defmacro mh-display-completion-list (completions &optional common-substring)
  "Display the list of COMPLETIONS.
See documentation for `display-completion-list' for a description of the
arguments COMPLETIONS and perhaps COMMON-SUBSTRING.
This macro is used by Emacs versions that lack a COMMON-SUBSTRING
argument, introduced in Emacs 22."
  (if (< emacs-major-version 22)
      `(display-completion-list ,completions)
    `(display-completion-list ,completions ,common-substring)))

(defmacro mh-face-foreground (face &optional frame inherit)
  "Return the foreground color name of FACE, or nil if unspecified.
See documentation for `face-foreground' for a description of the
arguments FACE, FRAME, and perhaps INHERIT.
This macro is used by Emacs versions that lack an INHERIT argument,
introduced in Emacs 22."
  (if (< emacs-major-version 22)
      `(face-foreground ,face ,frame)
    `(face-foreground ,face ,frame ,inherit)))

(defmacro mh-face-background (face &optional frame inherit)
  "Return the background color name of face, or nil if unspecified.
See documentation for `back-foreground' for a description of the
arguments FACE, FRAME, and INHERIT.
This macro is used by Emacs versions that lack an INHERIT argument,
introduced in Emacs 22."
  (if (< emacs-major-version 22)
      `(face-background ,face ,frame)
    `(face-background ,face ,frame ,inherit)))

(defun-mh mh-font-lock-add-keywords font-lock-add-keywords
  (mode keywords &optional how)
  "XEmacs does not have `font-lock-add-keywords'.
This function returns nil on that system.")

(defun-mh mh-image-load-path-for-library
  image-load-path-for-library (library image &optional path no-error)
  "Return a suitable search path for images used by LIBRARY.

It searches for IMAGE in `image-load-path' (excluding
\"`data-directory'/images\") and `load-path', followed by a path
suitable for LIBRARY, which includes \"../../etc/images\" and
\"../etc/images\" relative to the library file itself, and then
in \"`data-directory'/images\".

Then this function returns a list of directories which contains
first the directory in which IMAGE was found, followed by the
value of `load-path'. If PATH is given, it is used instead of
`load-path'.

If NO-ERROR is non-nil and a suitable path can't be found, don't
signal an error. Instead, return a list of directories as before,
except that nil appears in place of the image directory.

Here is an example that uses a common idiom to provide
compatibility with versions of Emacs that lack the variable
`image-load-path':

    ;; Shush compiler.
    (defvar image-load-path)

    (let* ((load-path (image-load-path-for-library \"mh-e\" \"mh-logo.xpm\"))
           (image-load-path (cons (car load-path)
                                  (when (boundp 'image-load-path)
                                    image-load-path))))
      (mh-tool-bar-folder-buttons-init))"
  (unless library (error "No library specified"))
  (unless image   (error "No image specified"))
  (let (image-directory image-directory-load-path)
    ;; Check for images in image-load-path or load-path.
    (let ((img image)
          (dir (or
                ;; Images in image-load-path.
                (mh-image-search-load-path image)
                ;; Images in load-path.
                (locate-library image)))
          parent)
      ;; Since the image might be in a nested directory (for
      ;; example, mail/attach.pbm), adjust `image-directory'
      ;; accordingly.
      (when dir
        (setq dir (file-name-directory dir))
        (while (setq parent (file-name-directory img))
          (setq img (directory-file-name parent)
                dir (expand-file-name "../" dir))))
      (setq image-directory-load-path dir))

    ;; If `image-directory-load-path' isn't Emacs's image directory,
    ;; it's probably a user preference, so use it. Then use a
    ;; relative setting if possible; otherwise, use
    ;; `image-directory-load-path'.
    (cond
     ;; User-modified image-load-path?
     ((and image-directory-load-path
           (not (equal image-directory-load-path
                       (file-name-as-directory
                        (expand-file-name "images" data-directory)))))
      (setq image-directory image-directory-load-path))
     ;; Try relative setting.
     ((let (library-name d1ei d2ei)
        ;; First, find library in the load-path.
        (setq library-name (locate-library library))
        (if (not library-name)
            (error "Cannot find library %s in load-path" library))
        ;; And then set image-directory relative to that.
        (setq
         ;; Go down 2 levels.
         d2ei (file-name-as-directory
               (expand-file-name
                (concat (file-name-directory library-name) "../../etc/images")))
         ;; Go down 1 level.
         d1ei (file-name-as-directory
               (expand-file-name
                (concat (file-name-directory library-name) "../etc/images"))))
        (setq image-directory
              ;; Set it to nil if image is not found.
              (cond ((file-exists-p (expand-file-name image d2ei)) d2ei)
                    ((file-exists-p (expand-file-name image d1ei)) d1ei)))))
     ;; Use Emacs's image directory.
     (image-directory-load-path
      (setq image-directory image-directory-load-path))
     (no-error
      (message "Could not find image %s for library %s" image library))
     (t
      (error "Could not find image %s for library %s" image library)))

    ;; Return an augmented `path' or `load-path'.
    (nconc (list image-directory)
           (delete image-directory (copy-sequence (or path load-path))))))

(defun-mh mh-image-search-load-path
  image-search-load-path (file &optional path)
  "Emacs 21 and XEmacs don't have `image-search-load-path'.
This function returns nil on those systems."
  nil)

;; For XEmacs.
(defalias 'mh-line-beginning-position
  (if (fboundp 'line-beginning-position)
      'line-beginning-position
    'point-at-bol))

;; For XEmacs.
(defalias 'mh-line-end-position
  (if (fboundp 'line-end-position)
      'line-end-position
    'point-at-eol))

(mh-require 'mailabbrev nil t)
(defun-mh mh-mail-abbrev-make-syntax-table
  mail-abbrev-make-syntax-table ()
  "Emacs 21 and XEmacs don't have `mail-abbrev-make-syntax-table'.
This function returns nil on those systems."
  nil)

(defun-mh mh-match-string-no-properties
  match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
This function is used by XEmacs that lacks `match-string-no-properties'.
The function `buffer-substring-no-properties' is used instead.
The argument STRING is ignored."
  (buffer-substring-no-properties
   (match-beginning num) (match-end num)))

(defun-mh mh-replace-regexp-in-string replace-regexp-in-string
  (regexp rep string &optional fixedcase literal subexp start)
  "Replace REGEXP with REP everywhere in STRING and return result.
This function is used by XEmacs that lacks `replace-regexp-in-string'.
The function `replace-in-string' is used instead.
The arguments FIXEDCASE, SUBEXP, and START, used by
`replace-in-string' are ignored."
  (replace-in-string string regexp rep literal))

(defun-mh mh-test-completion
  test-completion (string collection &optional predicate)
  "Return non-nil if STRING is a valid completion.
XEmacs does not have `test-completion'. This function returns nil
on that system." nil)

;; Copy of constant from url-util.el in Emacs 22; needed by Emacs 21.
(if (not (boundp 'url-unreserved-chars))
    (defconst mh-url-unreserved-chars
      '(
        ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
        ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
        ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
        ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
      "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396."))

(defun-mh mh-url-hexify-string url-hexify-string (str)
  "Escape characters in a string.
This is a copy of `url-hexify-string' from url-util.el in Emacs
22; needed by Emacs 21."
  (mapconcat
   (lambda (char)
     ;; Fixme: use a char table instead.
     (if (not (memq char mh-url-unreserved-chars))
         (if (> char 255)
               (error "Hexifying multibyte character %s" str)
           (format "%%%02X" char))
       (char-to-string char)))
   str ""))

(defun-mh mh-view-mode-enter
  view-mode-enter (&optional return-to exit-action)
  "Enter View mode.
This function is used by XEmacs that lacks `view-mode-enter'.
The function `view-mode' is used instead.
The arguments RETURN-TO and EXIT-ACTION are ignored."
  ;; Shush compiler.
  (if return-to nil)
  (if exit-action nil)
  (view-mode 1))

(defun-mh mh-window-full-height-p
  window-full-height-p (&optional WINDOW)
  "Return non-nil if WINDOW is not the result of a vertical split.
This function is defined in XEmacs as it lacks
`window-full-height-p'. The values of the functions
`window-height' and `frame-height' are compared instead. The
argument WINDOW is ignored."
  (= (1+ (window-height))
     (frame-height)))

(defmacro mh-write-file-functions ()
  "Return `write-file-functions' if it exists.
Otherwise return `local-write-file-hooks'.
This macro exists purely for compatibility. The former symbol is used
in Emacs 22 onward while the latter is used in previous versions and
XEmacs."
  (if (boundp 'write-file-functions)
      ''write-file-functions            ;Emacs 22 on
    ''local-write-file-hooks))          ;XEmacs

(provide 'mh-compat)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-compat.el ends here
