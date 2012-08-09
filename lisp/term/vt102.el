;; -*- no-byte-compile: t -*-

(defun terminal-init-vt102 ()
  "Terminal initialization function for vt102."
  (tty-run-terminal-initialization (selected-frame) "vt100"))

;;; vt102.el ends here
