;; -*- no-byte-compile: t -*-

(defun terminal-init-vt125 ()
  "Terminal initialization function for vt125."
  (tty-run-terminal-initialization (selected-frame) "vt100"))

;;; vt125.el ends here
