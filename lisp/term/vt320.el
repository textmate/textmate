;; -*- no-byte-compile: t -*-
(defun terminal-init-vt320 ()
  "Terminal initialization function for vt320."
  (tty-run-terminal-initialization (selected-frame) "vt100")
  ;; Make F11 an escape key.
  (define-key input-decode-map "\e[23~" [f11]) ;Probably redundant.
  (define-key local-function-key-map [f11] [?\e]))

;;; vt320.el ends here
