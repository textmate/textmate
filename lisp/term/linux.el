;; -*- no-byte-compile: t -*-
;; The Linux console handles Latin-1 by default.

(defun terminal-init-linux ()
  "Terminal initialization function for linux."
  (unless (terminal-coding-system)
    (set-terminal-coding-system 'iso-latin-1))

  ;; It can't really display underlines.
  (tty-no-underline)

  (ignore-errors (when gpm-mouse-mode (require 't-mouse) (gpm-mouse-enable)))

  ;; Make Latin-1 input characters work, too.
  ;; Meta will continue to work, because the kernel
  ;; turns that into Escape.

  ;; The arg only matters in that it is not t or nil.
  (set-input-meta-mode 'iso-latin-1))

;;; linux.el ends here
