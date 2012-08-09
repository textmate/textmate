;; -*- no-byte-compile: t -*-
(defun terminal-init-apollo ()
  "Terminal initialization function for apollo."
  (tty-run-terminal-initialization (selected-frame) "vt100"))

;;; apollo.el ends here
