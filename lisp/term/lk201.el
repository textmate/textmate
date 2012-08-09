;; -*- no-byte-compile: t -*-
;; Define function key sequences for DEC terminals.

(defvar lk201-function-map
  (let ((map (make-sparse-keymap)))

    ;; Termcap or terminfo should set these.
    ;; (define-key map "\e[A" [up])
    ;; (define-key map "\e[B" [down])
    ;; (define-key map "\e[C" [right])
    ;; (define-key map "\e[D" [left])

    (define-key map "\e[1~" [find])
    (define-key map "\e[2~" [insert])
    (define-key map "\e[3~" [delete])
    (define-key map "\e[4~" [select])
    (define-key map "\e[5~" [prior])
    (define-key map "\e[6~" [next])
    (define-key map "\e[11~" [f1])
    (define-key map "\e[12~" [f2])
    (define-key map "\e[13~" [f3])
    (define-key map "\e[14~" [f4])
    (define-key map "\e[15~" [f5])
    (define-key map "\e[17~" [f6])
    (define-key map "\e[18~" [f7])
    (define-key map "\e[19~" [f8])
    (define-key map "\e[20~" [f9])
    (define-key map "\e[21~" [f10])
    ;; Customarily F11 is used as the ESC key.
    ;; The file that includes this one, takes care of that.
    (define-key map "\e[23~" [f11])
    (define-key map "\e[24~" [f12])
    (define-key map "\e[25~" [f13])
    (define-key map "\e[26~" [f14])
    (define-key map "\e[28~" [help])
    (define-key map "\e[29~" [menu])
    (define-key map "\e[31~" [f17])
    (define-key map "\e[32~" [f18])
    (define-key map "\e[33~" [f19])
    (define-key map "\e[34~" [f20])

    ;; Termcap or terminfo should set these.
    ;; (define-key map "\eOA" [up])
    ;; (define-key map "\eOB" [down])
    ;; (define-key map "\eOC" [right])
    ;; (define-key map "\eOD" [left])

    ;; Termcap or terminfo should set these, but doesn't properly.
    ;; Termcap sets these to k1-k4, which get mapped to f1-f4 in term.c
    (define-key map "\eOP" [kp-f1])
    (define-key map "\eOQ" [kp-f2])
    (define-key map "\eOR" [kp-f3])
    (define-key map "\eOS" [kp-f4])

    (define-key map "\eOI" [kp-tab])
    (define-key map "\eOj" [kp-multiply])
    (define-key map "\eOk" [kp-add])
    (define-key map "\eOl" [kp-separator])
    (define-key map "\eOM" [kp-enter])
    (define-key map "\eOm" [kp-subtract])
    (define-key map "\eOn" [kp-decimal])
    (define-key map "\eOo" [kp-divide])
    (define-key map "\eOp" [kp-0])
    (define-key map "\eOq" [kp-1])
    (define-key map "\eOr" [kp-2])
    (define-key map "\eOs" [kp-3])
    (define-key map "\eOt" [kp-4])
    (define-key map "\eOu" [kp-5])
    (define-key map "\eOv" [kp-6])
    (define-key map "\eOw" [kp-7])
    (define-key map "\eOx" [kp-8])
    (define-key map "\eOy" [kp-9])
    map)
  "Function key definitions for DEC terminals.")

(defun terminal-init-lk201 ()
  ;; Use inheritance to let the main keymap override these defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the .emacs file.
  (let ((m (copy-keymap lk201-function-map)))
    (set-keymap-parent m (keymap-parent input-decode-map))
    (set-keymap-parent input-decode-map m)))

;;; lk201.el ends here
