;;; adwaita-theme.el --- Tango-based custom theme for faces

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: William Stevenson <yhvh2000@gmail.com>

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

;;; Code:

(deftheme adwaita
  "Face colors similar to the default theme of Gnome 3 (Adwaita).
The colors are chosen to match Adwaita window decorations and the
default look of the Gnome 3 desktop.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'adwaita
   `(cursor ((,class (:background "#00BBFF"))))
   `(border-color ((,class (:background "#EDEDED"))))
   `(default ((,class (:background "#EDEDED" :foreground "#2E3436"))))
   `(fringe ((,class (:background "#E6E6E6"))))
   `(mode-line ((,class (:box (:line-width -1 :style released-button)
			 :background "white" :foreground "#2E3436"))))
   `(mode-line-inactive ((,class (:foreground "#C6C6C6" :background ,"white"))))
   `(header-line ((,class (:foreground "#CCCCCC" :background "black"))))

   `(minibuffer-prompt ((,class (:foreground "#0084C8" :bold t))))
   `(region ((,class (:foreground unspecified :background "#C2D5E9"))))
   `(dired-header ((,class (:bold t :foreground "#0084C8"))))
   `(widget-button ((,class (:bold t :foreground "#0084C8"))))

   `(success ((,class (:bold t :foreground "#4E9A06"))))
   `(warning ((,class (:foreground "#CE5C00"))))
   `(error ((,class (:foreground "#B50000"))))

   `(font-lock-builtin-face ((,class (:foreground "#A020F0"))))
   `(font-lock-constant-face ((,class (:foreground "#F5666D"))))
   `(font-lock-comment-face ((,class (:foreground "#204A87"))))
   `(font-lock-function-name-face ((,class (:foreground "#00578E" :bold t))))
   `(font-lock-keyword-face ((,class (:bold t :foreground "#A52A2A"))))
   `(font-lock-string-face ((,class (:foreground "#4E9A06"))))
   `(font-lock-type-face ((,class (:foreground "#2F8B58" :bold t))))
   `(font-lock-variable-name-face ((,class (:foreground "#0084C8" :bold t))))
   `(font-lock-warning-face ((,class (:foreground "#F5666D" :bold t))))

   `(link ((,class (:underline t :foreground "#0066CC"))))
   `(link-visited ((,class (:underline t :foreground "#6799CC"))))
   `(highlight ((,class (:foreground "white" :background "#4A90D9"))))
   `(isearch ((,class (:foreground "white" :background "#77A4DD"))))

   `(erc-action-face ((,class (:foreground "#F5666D"))))
   `(erc-button ((,class (:foreground "#A8799C"))))
   `(erc-current-nick-face ((,class (:bold t :foreground "#FF7092"))))
   `(erc-error-face ((,class (:foreground "#F5666D" :bold t))))
   `(erc-input-face ((,class (:foreground "black"))))
   `(erc-keyword-face ((,class (:foreground "#F5666D"))))
   `(erc-my-nick-face ((,class (:bold t :foreground "#FF8CA7"))))
   `(erc-nick-default-face ((,class (:bold t :foreground "#0084C8"))))
   `(erc-notice-face ((,class (:foreground "#0084C8"))))
   `(erc-prompt-face ((,class (:foreground "black"))))
   `(erc-timestamp-face ((,class (:foreground ,"#4CB64A"))))

   `(magit-log-sha1 ((,class (:foreground "#FF7092"))))
   `(magit-log-head-label-local ((,class (:foreground "#4F78B5"))))
   `(magit-log-head-label-remote ((,class (:foreground ,"#4CB64A"))))
   `(magit-branch ((,class (:bold t :foreground "#0084C8"))))
   `(magit-section-title ((,class (:bold t :foreground "#00578E"))))
   `(magit-item-highlight ((,class (:background "#FEFFBF"))))
   `(magit-diff-add ((,class (:bold t :foreground "#4CB64A"))))
   `(magit-diff-del ((,class (:bold nil :foreground "#F5666D"))))

   `(gnus-group-mail-1-empty ((,class (:foreground "#00578E"))))
   `(gnus-group-mail-1 ((,class (:bold t :foreground "#4F78B5"))))
   `(gnus-group-mail-3-empty ((,class (:foreground "#00578E"))))
   `(gnus-group-mail-3 ((,class (:bold t :foreground "#9CBB43"))))
   `(gnus-group-news-3-empty ((,class (:foreground "#00578E"))))
   `(gnus-group-news-3 ((,class (:bold t :foreground "#9CBB43"))))
   `(gnus-header-name ((,class (:bold t :foreground "#0084C8"))))
   `(gnus-header-subject ((,class (:bold t :foreground "#FF7092"))))
   `(gnus-header-content ((,class (:foreground "#FF7092"))))
   `(gnus-button ((,class (:bold t :foreground "#00578E"))))
   `(gnus-cite-1 ((,class (:foreground "#00578E"))))
   `(gnus-cite-2 ((,class (:foreground "#0084C8"))))

   `(diff-added ((,class (:bold t :foreground "#4E9A06"))))
   `(diff-removed ((,class (:bold t :foreground "#F5666D"))))))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; adwaita-theme.el  ends here
