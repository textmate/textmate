;;; tsdh-light-theme.el --- Tassilo's light custom theme

;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

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

(deftheme tsdh-light
  "Minor tweaks to the Emacs white-background defaults.
Used and created by Tassilo Horn.")

(custom-theme-set-faces
 'tsdh-light
 '(default ((t (:background "white" :foreground "black"))))
 '(diff-added ((t (:inherit diff-changed :background "light green"))))
 '(diff-changed ((t (:background "light steel blue"))))
 '(diff-indicator-added ((t (:inherit diff-indicator-changed))))
 '(diff-indicator-changed ((t (:weight bold))))
 '(diff-indicator-removed ((t (:inherit diff-indicator-changed))))
 '(diff-removed ((t (:inherit diff-changed :background "sandy brown"))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :weight bold))))
 '(gnus-button ((t (:inherit button))))
 '(gnus-header-name ((t (:box (:line-width 1 :style released-button) :weight bold))))
 '(header-line ((t (:inherit mode-line :inverse-video t))))
 '(hl-line ((t (:background "grey95"))))
 '(minibuffer-prompt ((t (:background "yellow" :foreground "medium blue" :box (:line-width -1 :color "red" :style released-button) :weight bold))))
 '(mode-line ((t (:box (:line-width -1 :color "red" :style released-button) :family "DejaVu Sans"))))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "dim gray"))))
 '(org-agenda-date ((t (:inherit org-agenda-structure))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date :underline t))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "dark green"))))
 '(org-agenda-structure ((t (:foreground "Blue1" :weight bold :height 1.1 :family "DeJaVu Sans"))))
 '(org-hide ((t (:foreground "white"))))
 '(org-tag ((t (:weight bold))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :weight bold))))
 '(outline-2 ((t (:inherit font-lock-variable-name-face :weight bold))))
 '(outline-3 ((t (:inherit font-lock-keyword-face :weight bold))))
 '(outline-4 ((t (:inherit font-lock-comment-face :weight bold))))
 '(outline-5 ((t (:inherit font-lock-type-face :weight bold))))
 '(outline-6 ((t (:inherit font-lock-constant-face :weight bold))))
 '(outline-7 ((t (:inherit font-lock-builtin-face :weight bold))))
 '(outline-8 ((t (:inherit font-lock-string-face :weight bold))))
 '(rcirc-my-nick ((t (:foreground "LightSkyBlue" :weight bold))))
 '(region ((t (:background "lightgoldenrod1"))))
 '(show-paren-match ((t (:background "LightCyan2"))))
 '(show-paren-mismatch ((t (:background "deep pink"))))
 '(window-number-face ((t (:foreground "red" :weight bold)))))

(provide-theme 'tsdh-light)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tsdh-light-theme.el ends here
