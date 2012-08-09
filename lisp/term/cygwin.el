;;; cygwin.el --- support for the Cygwin terminal -*- no-byte-compile: t -*-

;;; The Cygwin terminal can't really display underlines.

(defun terminal-init-cygwin ()
  "Terminal initialization function for cygwin."
  (tty-no-underline))

;;; cygwin.el ends here
