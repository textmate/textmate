;;; ede/shell.el --- A shell controlled by EDE.
;;
;; Copyright (C) 2009-2012 Free Software Foundation, Inc.
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;;
;; Run commands through a specialized EDE shell buffer.  Commands will
;; be run as shell commands so users can type in their own thing in
;; the shells for testing purposes.
;;
;; Each thing that EDE wants to use will create a shell to interact with it.

;;; Code:

(require 'ede)

(declare-function comint-send-input "comint")

(defmethod ede-shell-run-something ((target ede-target) command)
  "Create a shell to run stuff for TARGET.
COMMAND is a text string representing the thing to be run."
  (let* ((buff (ede-shell-buffer target))
	 (cp (ede-target-parent target))
	 (dd (oref cp :directory)))
    ;; Show the new buffer.
    (when (not (get-buffer-window buff))
      (switch-to-buffer-other-window buff t))
    ;; Force a shell into the buffer.
    (shell buff)
    (while (eq (point-min) (point))
      (accept-process-output))
    ;; Change the default directory
    (if (not (string= (file-name-as-directory (expand-file-name default-directory))
		      (file-name-as-directory (expand-file-name dd))))
	;; Go there.
	(setq command (concat (concat "cd " dd ";" command))))
    ;; Run the command itself.
    (ede-shell-run-command command)
    ))

(defun ede-shell-run-command (command)
  "Run the COMMAND in the current shell-buffer."
  (require 'comint)
  ;; go to end
  (goto-char (point-max))
  ;; Insert the stuff.
  (goto-char (point-max))
  (insert command)
  ;; Send the command.
  (comint-send-input)
  )

(defmethod ede-shell-buffer ((target ede-target))
  "Get the buffer for running shell commands for TARGET."
  (let ((name (ede-name target)))
    (get-buffer-create (format "*EDE Shell %s*" name))))

(provide 'ede/shell)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/shell"
;; End:

;;; ede/shell.el ends here
