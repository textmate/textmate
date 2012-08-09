;;; dichromacy-theme.el --- color theme suitable for color-blind users

;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: Chong Yidong <cyd@stupidchicken>

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

(deftheme dichromacy
  "Face colors suitable for red/green color-blind users.
The color palette is from B. Wong, Nature Methods 8, 441 (2011).
It is intended to provide good variability while being easily
differentiated by individuals with protanopia or deuteranopia.

Basic, Font Lock, Isearch, Gnus, Message, Flyspell, and
Ansi-Color faces are included.")

(let ((class '((class color) (min-colors 89)))
      (orange "#e69f00")
      (skyblue "#56b4e9")
      (bluegreen "#009e73")
      (yellow "#f8ec59")
      (blue "#0072b2")
      (vermillion "#d55e00")
      (redpurple "#cc79a7")
      (bluegray "#848ea9"))
  (custom-theme-set-faces
   'dichromacy
   `(default ((,class (:foreground "black" :background "white"))))
   `(cursor ((,class (:background "black"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#f7f7f7"))))
   `(highlight ((,class (:foreground ,blue :background "#e5e5e5"))))
   `(region ((,class (:foreground unspecified :background ,yellow))))
   `(secondary-selection ((,class (:background "#e5e5e5"))))
   `(isearch ((,class (:foreground "white" :background ,vermillion))))
   `(lazy-highlight ((,class (:foreground "white" :background ,redpurple))))
   `(trailing-whitespace ((,class (:background ,vermillion))))
   ;; Mode line faces
   `(mode-line ((,class (:box (:line-width -1 :style released-button)
			      :background "#e5e5e5" :foreground "black"))))
   `(mode-line-inactive ((,class (:box (:line-width -1 :style released-button)
				       :background "#b0b0b0"
				       :foreground "black"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:weight bold :foreground ,blue))))
   `(escape-glyph ((,class (:foreground ,vermillion))))
   `(error ((,class (:weight bold :slant italic
			     :foreground ,vermillion))))
   `(warning ((,class (:foreground ,orange))))
   `(success ((,class (:foreground ,bluegreen))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,blue))))
   `(font-lock-comment-face ((,class (:slant italic :foreground ,bluegreen))))
   `(font-lock-constant-face ((,class (:weight bold :foreground ,vermillion))))
   `(font-lock-function-name-face ((,class (:foreground ,vermillion))))
   `(font-lock-keyword-face ((,class (:weight bold :foreground ,skyblue))))
   `(font-lock-string-face ((,class (:foreground ,bluegray))))
   `(font-lock-type-face ((,class (:weight bold :foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:weight bold :foreground ,orange))))
   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue))))
   `(link-visited ((,class (:underline t :foreground ,redpurple))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:weight bold :foreground ,vermillion))))
   `(gnus-group-news-1-low ((,class (:foreground ,vermillion))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground ,orange))))
   `(gnus-group-news-2-low ((,class (:foreground ,orange))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground ,skyblue))))
   `(gnus-group-news-3-low ((,class (:foreground ,skyblue))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground ,redpurple))))
   `(gnus-group-news-4-low ((,class (:foreground ,redpurple))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground ,blue))))
   `(gnus-group-news-5-low ((,class (:foreground ,blue))))
   `(gnus-group-news-low ((,class (:foreground ,bluegreen))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground ,vermillion))))
   `(gnus-group-mail-1-low ((,class (:foreground ,vermillion))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground ,orange))))
   `(gnus-group-mail-2-low ((,class (:foreground ,orange))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground ,skyblue))))
   `(gnus-group-mail-3-low ((,class (:foreground ,skyblue))))
   `(gnus-group-mail-low ((,class (:foreground ,bluegreen))))
   `(gnus-header-content ((,class (:foreground ,redpurple))))
   `(gnus-header-from ((,class (:weight bold :foreground ,blue))))
   `(gnus-header-subject ((,class (:foreground ,orange))))
   `(gnus-header-name ((,class (:foreground ,skyblue))))
   `(gnus-header-newsgroups ((,class (:foreground ,vermillion))))
   ;; Message faces
   `(message-header-name ((,class (:foreground ,skyblue))))
   `(message-header-cc ((,class (:foreground ,vermillion))))
   `(message-header-other ((,class (:foreground ,bluegreen))))
   `(message-header-subject ((,class (:foreground ,orange))))
   `(message-header-to ((,class (:weight bold :foreground ,blue))))
   `(message-cited-text ((,class (:slant italic :foreground ,bluegreen))))
   `(message-separator ((,class (:weight bold :foreground ,redpurple))))
   ;; Flyspell
   `(flyspell-duplicate ((,class (:weight unspecified :foreground unspecified
				  :slant unspecified :underline ,orange))))
   `(flyspell-incorrect ((,class (:weight unspecified :foreground unspecified
				  :slant unspecified :underline ,redpurple)))))

  (custom-theme-set-variables
   'dichromacy
   `(ansi-color-names-vector ["black" ,vermillion ,bluegreen ,yellow
			      ,blue ,redpurple ,skyblue "white"])))

(provide-theme 'dichromacy)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dichromacy-theme.el ends here
