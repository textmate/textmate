;;; org-w3m.el --- Support from copy and paste from w3m to Org-mode

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Andy Stewart <lazycat dot manatee at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements copying HTML content from a w3m buffer and
;; transforming the text on the fly so that it can be pasted into
;; an org-mode buffer with hot links.  It will also work for regions
;; in gnus buffers that have been washed with w3m.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Acknowledgments:

;; Richard Riley <rileyrgdev at googlemail dot com>
;;
;;      The idea of transforming the HTML content with org-mode style is
;;      proposed by Richard, I'm just coding it.
;;

;;; Code:

(require 'org)

(defun org-w3m-copy-for-org-mode ()
  "Copy current buffer content or active region with `org-mode' style links.
This will encode `link-title' and `link-location' with
`org-make-link-string', and insert the transformed test into the kill ring,
so that it can be yanked into an Org-mode buffer with links working correctly."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (transform-start (point-min))
         (transform-end (point-max))
         return-content
         link-location link-title
         temp-position out-bound)
    (when regionp
      (setq transform-start (region-beginning))
      (setq transform-end (region-end))
      ;; Deactivate mark if current mark is activate.
      (if (fboundp 'deactivate-mark) (deactivate-mark)))
    (message "Transforming links...")
    (save-excursion
      (goto-char transform-start)
      (while (and (not out-bound)                 ; still inside region to copy
                  (not (org-w3m-no-next-link-p))) ; no next link current buffer
        ;; store current point before jump next anchor
        (setq temp-position (point))
        ;; move to next anchor when current point is not at anchor
        (or (get-text-property (point) 'w3m-href-anchor) (org-w3m-get-next-link-start))
        (if (<= (point) transform-end)  ; if point is inside transform bound
            (progn
              ;; get content between two links.
              (if (> (point) temp-position)
                  (setq return-content (concat return-content
                                               (buffer-substring
                                                temp-position (point)))))
              ;; get link location at current point.
              (setq link-location (get-text-property (point) 'w3m-href-anchor))
              ;; get link title at current point.
              (setq link-title (buffer-substring (point)
                                                 (org-w3m-get-anchor-end)))
              ;; concat `org-mode' style url to `return-content'.
              (setq return-content (concat return-content
                                           (org-make-link-string
                                            link-location link-title))))
          (goto-char temp-position)     ; reset point before jump next anchor
          (setq out-bound t)            ; for break out `while' loop
          ))
      ;; add the rest until end of the region to be copied
      (if (< (point) transform-end)
          (setq return-content
                (concat return-content
                        (buffer-substring (point) transform-end))))
      (org-kill-new return-content)
      (message "Transforming links...done, use C-y to insert text into Org-mode file")
      (message "Copy with link transformation complete."))))

(defun org-w3m-get-anchor-start ()
  "Move cursor to the start of current anchor.  Return point."
  ;; get start position of anchor or current point
  (goto-char (or (previous-single-property-change (point) 'w3m-anchor-sequence)
                 (point))))

(defun org-w3m-get-anchor-end ()
  "Move cursor to the end of current anchor.  Return point."
  ;; get end position of anchor or point
  (goto-char (or (next-single-property-change (point) 'w3m-anchor-sequence)
		 (point))))

(defun org-w3m-get-next-link-start ()
  "Move cursor to the start of next link.  Return point."
  (catch 'reach
    (while (next-single-property-change (point) 'w3m-anchor-sequence)
      ;; jump to next anchor
      (goto-char (next-single-property-change (point) 'w3m-anchor-sequence))
      (when (get-text-property (point) 'w3m-href-anchor)
        ;; return point when current is valid link
        (throw 'reach nil))))
  (point))

(defun org-w3m-get-prev-link-start ()
  "Move cursor to the start of previous link.  Return point."
  (catch 'reach
    (while (previous-single-property-change (point) 'w3m-anchor-sequence)
      ;; jump to previous anchor
      (goto-char (previous-single-property-change (point) 'w3m-anchor-sequence))
      (when (get-text-property (point) 'w3m-href-anchor)
        ;; return point when current is valid link
        (throw 'reach nil))))
  (point))

(defun org-w3m-no-next-link-p ()
  "Whether there is no next link after the cursor.
Return t if there is no next link; otherwise, return nil."
  (save-excursion
    (equal (point) (org-w3m-get-next-link-start))))

(defun org-w3m-no-prev-link-p ()
  "Whether there is no previous link after the cursor.
Return t if there is no previous link; otherwise, return nil."
  (save-excursion
    (equal (point) (org-w3m-get-prev-link-start))))

;; Install keys into the w3m keymap
(defvar w3m-mode-map)
(defvar w3m-minor-mode-map)
(when (and (boundp 'w3m-mode-map)
           (keymapp w3m-mode-map))
  (define-key w3m-mode-map "\C-c\C-x\M-w" 'org-w3m-copy-for-org-mode)
  (define-key w3m-mode-map "\C-c\C-x\C-w" 'org-w3m-copy-for-org-mode))
(when (and (boundp 'w3m-minor-mode-map)
           (keymapp w3m-minor-mode-map))
  (define-key w3m-minor-mode-map "\C-c\C-x\M-w" 'org-w3m-copy-for-org-mode)
  (define-key w3m-minor-mode-map "\C-c\C-x\C-w" 'org-w3m-copy-for-org-mode))
(add-hook
 'w3m-mode-hook
 (lambda ()
   (define-key w3m-mode-map "\C-c\C-x\M-w" 'org-w3m-copy-for-org-mode)
   (define-key w3m-mode-map "\C-c\C-x\C-w" 'org-w3m-copy-for-org-mode)))
(add-hook
 'w3m-minor-mode-hook
 (lambda ()
   (define-key w3m-minor-mode-map "\C-c\C-x\M-w" 'org-w3m-copy-for-org-mode)
   (define-key w3m-minor-mode-map "\C-c\C-x\C-w" 'org-w3m-copy-for-org-mode)))

(provide 'org-w3m)

;;; org-w3m.el ends here
