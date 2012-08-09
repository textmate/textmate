;;; reveal.el --- Automatically reveal hidden text at point -*- lexical-binding: t -*-

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: outlines

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

;; Reveal mode is a minor mode that makes sure that text around point
;; is always visible.  When point enters a region of hidden text,
;; `reveal-mode' temporarily makes it visible.
;;
;; This is normally used in conjunction with `outline-minor-mode',
;; `hs-minor-mode', `hide-ifdef-mode', ...
;;
;; It only works with packages that hide text using overlays.
;; Packages can provide special support for it by placing
;; a function in the `reveal-toggle-invisible' property on the symbol
;; used as the value of the `invisible' overlay property.
;; The function is called right after revealing (or re-hiding) the
;; text with two arguments: the overlay and a boolean that's non-nil
;; if we have just revealed the text.  When revealing, that function
;; may re-hide some of the text.

;;; Todo:

;; - find other hysteresis features.
;; - don't hide after a scroll command
;; - delay hiding by a couple seconds (i.e. hide in the background)

;;; Code:

(defgroup reveal nil
  "Reveal hidden text on the fly."
  :group 'convenience)

(defcustom reveal-around-mark t
  "Reveal text around the mark, if active."
  :type 'boolean
  :group 'reveal)

(defvar reveal-open-spots nil
  "List of spots in the buffer which are open.
Each element has the form (WINDOW . OVERLAY).")
(make-variable-buffer-local 'reveal-open-spots)

(defvar reveal-last-tick nil)
(make-variable-buffer-local 'reveal-last-tick)

;; Actual code

(defun reveal-post-command ()
  ;; Refresh the spots that might have changed.
  ;; `Refreshing' here means to try and re-hide the corresponding text.
  ;; We don't refresh everything correctly:
  ;; - we only refresh spots in the current window.
  ;; FIXME: do we actually know that (current-buffer) = (window-buffer) ?
  (with-local-quit
    (condition-case err
        (let ((old-ols
               (delq nil
                     (mapcar
                      (lambda (x)
                        ;; We refresh any spot in the current window as well
                        ;; as any spots associated with a dead window or
                        ;; a window which does not show this buffer any more.
                        (cond
                         ((eq (car x) (selected-window)) (cdr x))
                         ((not (and (window-live-p (car x))
                                    (eq (window-buffer (car x)) (current-buffer))))
                          ;; Adopt this since it's owned by a window that's
                          ;; either not live or at least not showing this
                          ;; buffer any more.
                          (setcar x (selected-window))
                          (cdr x))))
                      reveal-open-spots))))
          (setq old-ols (reveal-open-new-overlays old-ols))
          (reveal-close-old-overlays old-ols))
      (error (message "Reveal: %s" err)))))

(defun reveal-open-new-overlays (old-ols)
  (let ((repeat t))
    (while repeat
      (setq repeat nil)
      (dolist (ol (nconc (when (and reveal-around-mark mark-active)
                           (overlays-at (mark)))
                         (overlays-at (point))))
        (setq old-ols (delq ol old-ols))
        (when (overlay-start ol)        ;Check it's still live.
          (let ((inv (overlay-get ol 'invisible)) open)
            (when (and inv
                       ;; There's an `invisible' property.  Make sure it's
                       ;; actually invisible, and ellipsized.
                       (and (consp buffer-invisibility-spec)
                            (cdr (assq inv buffer-invisibility-spec)))
                       (or (setq open
                                 (or (overlay-get ol 'reveal-toggle-invisible)
                                     (and (symbolp inv)
                                          (get inv 'reveal-toggle-invisible))
                                     (overlay-get ol 'isearch-open-invisible-temporary)))
                           (overlay-get ol 'isearch-open-invisible)
                           (and (consp buffer-invisibility-spec)
                                (cdr (assq inv buffer-invisibility-spec))))
                       (overlay-put ol 'reveal-invisible inv))
              (push (cons (selected-window) ol) reveal-open-spots)
              (if (null open)
                  (overlay-put ol 'invisible nil)
                ;; Use the provided opening function and repeat (since the
                ;; opening function might have hidden a subpart around point
                ;; or moved/killed some of the overlays).
                (setq repeat t)
                (condition-case err
                    (funcall open ol nil)
                  (error (message "!!Reveal-show (funcall %s %s nil): %s !!"
                                  open ol err)
                         ;; Let's default to a meaningful behavior to avoid
                         ;; getting stuck in an infinite loop.
                         (setq repeat nil)
                         (overlay-put ol 'invisible nil))))))))))
  old-ols)

(defun reveal-close-old-overlays (old-ols)
  (if (not (eq reveal-last-tick
               (setq reveal-last-tick (buffer-modified-tick))))
      ;; The buffer was modified since last command: let's refrain from
      ;; closing any overlay because it tends to behave poorly when
      ;; inserting text at the end of an overlay (basically the overlay
      ;; should be rear-advance when it's open, but things like
      ;; outline-minor-mode make it non-rear-advance because it's
      ;; a better choice when it's closed).
      nil
    ;; The last command was only a point motion or some such
    ;; non-buffer-modifying command.  Let's close whatever can be closed.
    (dolist (ol old-ols)
      (if (and (overlay-start ol)       ;Check it's still live.
               (>= (point) (save-excursion
                             (goto-char (overlay-start ol))
                             (line-beginning-position 1)))
               (<= (point) (save-excursion
                             (goto-char (overlay-end ol))
                             (line-beginning-position 2)))
               ;; If the application has moved the overlay to some other
               ;; buffer, we'd better reset the buffer to its
               ;; original state.
               (eq (current-buffer) (overlay-buffer ol)))
          ;; Still near the overlay: keep it open.
          nil
        ;; Really close it.
        (let* ((inv (overlay-get ol 'reveal-invisible))
               (open (or (overlay-get ol 'reveal-toggle-invisible)
                         (get inv 'reveal-toggle-invisible)
                         (overlay-get ol 'isearch-open-invisible-temporary))))
          (if (and (overlay-start ol)   ;Check it's still live.
                   open)
              (condition-case err
                  (funcall open ol t)
                (error (message "!!Reveal-hide (funcall %s %s t): %s !!"
                                open ol err)))
            (overlay-put ol 'invisible inv))
          ;; Remove the overlay from the list of open spots.
          (overlay-put ol 'reveal-invisible nil)
          (setq reveal-open-spots
                (delq (rassoc ol reveal-open-spots)
                      reveal-open-spots)))))))

(defvar reveal-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Override the default move-beginning-of-line and move-end-of-line
    ;; which skips valuable invisible text.
    (define-key map [remap move-beginning-of-line] 'beginning-of-line)
    (define-key map [remap move-end-of-line] 'end-of-line)
    map))

;;;###autoload
(define-minor-mode reveal-mode
  "Toggle uncloaking of invisible text near point (Reveal mode).
With a prefix argument ARG, enable Reveal mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
Reveal mode if ARG is omitted or nil.

Reveal mode is a buffer-local minor mode.  When enabled, it
reveals invisible text around point."
  :group 'reveal
  :lighter (global-reveal-mode nil " Reveal")
  :keymap reveal-mode-map
  (if reveal-mode
      (progn
	(set (make-local-variable 'search-invisible) t)
	(add-hook 'post-command-hook 'reveal-post-command nil t))
    (kill-local-variable 'search-invisible)
    (remove-hook 'post-command-hook 'reveal-post-command t)))

;;;###autoload
(define-minor-mode global-reveal-mode
  "Toggle Reveal mode in all buffers (Global Reveal mode).
Reveal mode renders invisible text around point visible again.

With a prefix argument ARG, enable Global Reveal mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t :group 'reveal
  (setq-default reveal-mode global-reveal-mode)
  (if global-reveal-mode
      (progn
	(setq search-invisible t)
	(add-hook 'post-command-hook 'reveal-post-command))
    (setq search-invisible 'open)	;FIXME
    (remove-hook 'post-command-hook 'reveal-post-command)))

(provide 'reveal)

;;; reveal.el ends here
