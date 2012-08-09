;;; light-blue-theme.el --- Custom theme for faces

;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: Drew Adams <drew.adams@oracle.com>

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

;; Extracted from the settings in oneonone.el by Drew Adams.

;;; Code:

(deftheme light-blue
  "Face colors utilizing a light blue background.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'light-blue
   `(default ((,class (:background "LightBlue" :foreground "black"))))
   `(cursor ((,class (:background "red"))))
   `(fringe ((,class (:background "gray85"))))
   ;; Highlighting faces
   `(highlight ((,class (:background "cyan"))))
   `(region ((,class (:background "MediumAquamarine"))))
   `(secondary-selection ((,class (:background "white" :foreground "black"))))
   `(isearch ((,class (:background "green" :foreground "Black"))))
   `(lazy-highlight ((,class (:background "dark turquoise"))))
   `(query-replace ((,class (:inherit isearch :background "white" :foreground "black"))))
   `(match ((,class (:background "SkyBlue"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "PaleGoldenrod" :foreground "black" :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:overline "red" :underline "red"))))
   `(mode-line-inactive ((,class (:inherit mode-line :background "LightGray" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
   ;; Escape and prompt faces
   `(escape-glyph ((,class (:background "gold" :foreground "blue" :box (:line-width 1 :color "blue" :style released-button)))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#b35caf"))))
   `(font-lock-constant-face ((,class (:foreground "#00006DE06DE0"))))
   `(font-lock-function-name-face ((,class (:foreground "red"))))
   `(font-lock-keyword-face ((,class (:foreground "Blue3"))))
   `(font-lock-string-face ((,class (:foreground "Magenta4"))))
   `(font-lock-warning-face ((,class (:foreground "orange red" :weight bold))))
   ;; Compilation faces
   `(next-error ((,class (:inherit region :background "SkyBlue"))))))

(provide-theme 'light-blue)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; light-blue-theme.el ends here
