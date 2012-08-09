;; -*- no-byte-compile: t -*-
;; Treat a screen terminal similar to an xterm.
(load "term/xterm")

(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Use the xterm color initialization code.
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

;; screen.el ends here
