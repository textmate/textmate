;;; calc-yank.el --- kill-ring functionality for Calc

;; Copyright (C) 1990-1993, 2001-2012  Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <jay.p.belanger@gmail.com>

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

;;; Code:

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

;;; Kill ring commands.

(defun calc-kill (nn &optional no-delete)
  (interactive "P")
  (if (eq major-mode 'calc-mode)
      (calc-wrapper
       (calc-force-refresh)
       (calc-set-command-flag 'no-align)
       (let ((num (max (calc-locate-cursor-element (point)) 1))
	     (n (prefix-numeric-value nn)))
	 (if (< n 0)
	     (progn
	       (if (eobp)
		   (setq num (1- num)))
	       (setq num (- num n)
		     n (- n))))
         (calc-check-stack num)
	 (let ((stuff (calc-top-list n (- num n -1))))
	   (calc-cursor-stack-index num)
	   (let ((first (point)))
	     (calc-cursor-stack-index (- num n))
	     (if (null nn)
		 (backward-char 1))   ; don't include newline for raw C-k
	     (copy-region-as-kill first (point))
	     (if (not no-delete)
		 (calc-pop-stack n (- num n -1))))
	   (setq calc-last-kill (cons (car kill-ring) stuff)))))
    (kill-line nn)))

(defun calc-force-refresh ()
  (if (or calc-executing-macro calc-display-dirty)
      (let ((calc-executing-macro nil))
	(calc-refresh))))

(defun calc-locate-cursor-element (pt)
  (save-excursion
    (goto-char (point-max))
    (calc-locate-cursor-scan (- calc-stack-top) calc-stack pt)))

(defun calc-locate-cursor-scan (n stack pt)
  (if (or (<= (point) pt)
	  (null stack))
      n
    (forward-line (- (nth 1 (car stack))))
    (calc-locate-cursor-scan (1+ n) (cdr stack) pt)))

(defun calc-kill-region (top bot &optional no-delete)
  (interactive "r")
  (if (eq major-mode 'calc-mode)
      (calc-wrapper
       (calc-force-refresh)
       (calc-set-command-flag 'no-align)
       (let* ((top-num (calc-locate-cursor-element top))
              (top-pos (save-excursion
                         (calc-cursor-stack-index top-num)
                         (point)))
	      (bot-num (calc-locate-cursor-element (1- bot)))
              (bot-pos (save-excursion
                         (calc-cursor-stack-index (max 0 (1- bot-num)))
                         (point)))
	      (num (- top-num bot-num -1)))
	 (copy-region-as-kill top-pos bot-pos)
	 (setq calc-last-kill (cons (car kill-ring)
				    (calc-top-list num bot-num)))
	 (if (not no-delete)
	     (calc-pop-stack num bot-num))))
    (if no-delete
	(copy-region-as-kill top bot)
      (kill-region top bot))))

(defun calc-copy-as-kill (n)
  (interactive "P")
  (calc-kill n t))

(defun calc-copy-region-as-kill (top bot)
  (interactive "r")
  (calc-kill-region top bot t))

;; This function uses calc-last-kill if possible to get an exact result,
;; otherwise it just parses the yanked string.
;; Modified to use Emacs 19 extended concept of kill-ring. -- daveg 12/15/96
;;;###autoload
(defun calc-yank ()
  (interactive)
  (calc-wrapper
   (calc-pop-push-record-list
    0 "yank"
    (let ((thing (if (fboundp 'current-kill)
		     (current-kill 0 t)
		   (car kill-ring-yank-pointer))))
      (if (eq (car-safe calc-last-kill) thing)
	  (cdr calc-last-kill)
	(if (stringp thing)
	    (let ((val (math-read-exprs (calc-clean-newlines thing))))
	      (if (eq (car-safe val) 'error)
		  (progn
		    (setq val (math-read-exprs thing))
		    (if (eq (car-safe val) 'error)
			(error "Bad format in yanked data")
		      val))
		val))))))))

;;; The Calc set- and get-register commands are modified versions of functions
;;; in register.el

(defvar calc-register-alist nil
  "Alist of elements (NAME . (TEXT . CALCVAL)).
NAME is a character (a number).
TEXT and CALCVAL are the TEXT and internal structure of stack entries.")

(defun calc-set-register (register text calcval)
  "Set the contents of the Calc register REGISTER to (TEXT . CALCVAL),
as well as set the contents of the Emacs register REGISTER to TEXT."
  (set-register register text)
  (let ((aelt (assq register calc-register-alist)))
    (if aelt
        (setcdr aelt (cons text calcval))
      (push (cons register (cons text calcval)) calc-register-alist))))

(defun calc-get-register (reg)
  "Return the CALCVAL portion of the contents of the Calc register REG,
unless the TEXT portion doesn't match the contents of the Emacs register REG,
in which case either return the contents of the Emacs register (if it is
text) or `nil'."
  (let ((cval (cdr (assq reg calc-register-alist)))
        (val (cdr (assq reg register-alist))))
    (if (stringp val)
        (if (and (stringp (car cval))
                 (string= (car cval) val))
            (cdr cval)
          val))))

(defun calc-copy-to-register (register start end &optional delete-flag)
  "Copy the lines in the region into register REGISTER.
With prefix arg, delete as well."
  (interactive "cCopy to register: \nr\nP")
  (if (eq major-mode 'calc-mode)
      (let* ((top-num (calc-locate-cursor-element start))
             (top-pos (save-excursion
                        (calc-cursor-stack-index top-num)
                        (point)))
             (bot-num (calc-locate-cursor-element (1- end)))
             (bot-pos (save-excursion
                        (calc-cursor-stack-index (max 0 (1- bot-num)))
                        (point)))
             (num (- top-num bot-num -1))
             (str (buffer-substring top-pos bot-pos)))
        (calc-set-register register str (calc-top-list num bot-num))
        (if delete-flag
            (calc-wrapper
             (calc-pop-stack num bot-num))))
    (copy-to-register register start end delete-flag)))

(defun calc-insert-register (register)
  "Insert the contents of register REGISTER."
  (interactive "cInsert register: ")
  (if (eq major-mode 'calc-mode)
      (let ((val (calc-get-register register)))
        (calc-wrapper
         (calc-pop-push-record-list
          0 "insr"
          (if (not val)
              (error "Bad format in register data")
            (if (consp val)
                val
              (let ((nval (math-read-exprs (calc-clean-newlines val))))
                (if (eq (car-safe nval) 'error)
                    (progn
                      (setq nval (math-read-exprs val))
                      (if (eq (car-safe nval) 'error)
                          (error "Bad format in register data")
                        nval))
                  nval)))))))
    (insert-register register)))

(defun calc-add-to-register (register start end prepend delete-flag)
  "Add the lines in the region to register REGISTER.
If PREPEND is non-nil, add them to the beginning of the register,
otherwise the end.  If DELETE-FLAG is non-nil, also delete the region."
  (let* ((top-num (calc-locate-cursor-element start))
         (top-pos (save-excursion
                    (calc-cursor-stack-index top-num)
                    (point)))
         (bot-num (calc-locate-cursor-element (1- end)))
         (bot-pos (save-excursion
                    (calc-cursor-stack-index (max 0 (1- bot-num)))
                    (point)))
         (num (- top-num bot-num -1))
         (str (buffer-substring top-pos bot-pos))
         (calcval (calc-top-list num bot-num))
         (cval (cdr (assq register calc-register-alist))))
    (if (not cval)
        (calc-set-register register str calcval)
      (if prepend
          (calc-set-register
           register
           (concat str (car cval))
           (append calcval (cdr cval)))
        (calc-set-register
         register
         (concat (car cval) str)
         (append (cdr cval) calcval))))
    (if delete-flag
        (calc-wrapper
         (calc-pop-stack num bot-num)))))

(defun calc-append-to-register (register start end &optional delete-flag)
  "Copy the lines in the region to the end of register REGISTER.
With prefix arg, also delete the region."
  (interactive "cAppend to register: \nr\nP")
  (if (eq major-mode 'calc-mode)
      (calc-add-to-register register start end nil delete-flag)
    (append-to-register register start end delete-flag)))

(defun calc-prepend-to-register (register start end &optional delete-flag)
  "Copy the lines in the region to the beginning of register REGISTER.
With prefix arg, also delete the region."
  (interactive "cPrepend to register: \nr\nP")
  (if (eq major-mode 'calc-mode)
      (calc-add-to-register register start end t delete-flag)
    (prepend-to-register register start end delete-flag)))



(defun calc-clean-newlines (s)
  (cond

   ;; Omit leading/trailing whitespace
   ((or (string-match "\\`[ \n\r]+\\([^\001]*\\)\\'" s)
	(string-match "\\`\\([^\001]*\\)[ \n\r]+\\'" s))
    (calc-clean-newlines (math-match-substring s 1)))

   ;; Convert newlines to commas
   ((string-match "\\`\\(.*\\)[\n\r]+\\([^\001]*\\)\\'" s)
    (calc-clean-newlines (concat (math-match-substring s 1) ","
				 (math-match-substring s 2))))

   (t s)))


(defun calc-do-grab-region (top bot arg)
  (when (memq major-mode '(calc-mode calc-trail-mode))
    (error "This command works only in a regular text buffer"))
  (let* ((from-buffer (current-buffer))
	 (calc-was-started (get-buffer-window "*Calculator*"))
	 (single nil)
	 data vals pos)
    (if arg
	(if (consp arg)
	    (setq single t)
	  (setq arg (prefix-numeric-value arg))
	  (if (= arg 0)
	      (setq top (point-at-bol)
		    bot (point-at-eol))
	    (save-excursion
	      (setq top (point))
	      (forward-line arg)
	      (if (> arg 0)
		  (setq bot (point))
		(setq bot top
		      top (point)))))))
    (setq data (buffer-substring top bot))
    (calc)
    (if single
	(setq vals (math-read-expr data))
      (setq vals (math-read-expr (concat "[" data "]")))
      (and (eq (car-safe vals) 'vec)
	   (= (length vals) 2)
	   (eq (car-safe (nth 1 vals)) 'vec)
	   (setq vals (nth 1 vals))))
    (if (eq (car-safe vals) 'error)
	(progn
	  (if calc-was-started
	      (pop-to-buffer from-buffer)
	    (calc-quit t)
	    (switch-to-buffer from-buffer))
	  (goto-char top)
	  (forward-char (+ (nth 1 vals) (if single 0 1)))
	  (error (nth 2 vals))))
    (calc-slow-wrapper
     (calc-enter-result 0 "grab" vals))))


(defun calc-do-grab-rectangle (top bot arg &optional reduce)
  (and (memq major-mode '(calc-mode calc-trail-mode))
       (error "This command works only in a regular text buffer"))
  (let* ((col1 (save-excursion (goto-char top) (current-column)))
	 (col2 (save-excursion (goto-char bot) (current-column)))
	 (from-buffer (current-buffer))
	 (calc-was-started (get-buffer-window "*Calculator*"))
	 data mat vals lnum pt pos)
    (if (= col1 col2)
	(save-excursion
	  (unless (= col1 0)
	    (error "Point and mark must be at beginning of line, or define a rectangle"))
	  (goto-char top)
	  (while (< (point) bot)
	    (setq pt (point))
	    (forward-line 1)
	    (setq data (cons (buffer-substring pt (1- (point))) data)))
	  (setq data (nreverse data)))
      (setq data (extract-rectangle top bot)))
    (calc)
    (setq mat (list 'vec)
	  lnum 0)
    (when arg
      (setq arg (if (consp arg) 0 (prefix-numeric-value arg))))
    (while data
      (if (natnump arg)
	  (progn
	    (if (= arg 0)
		(setq arg 1000000))
	    (setq pos 0
		  vals (list 'vec))
	    (let ((w (length (car data)))
		  j v)
	      (while (< pos w)
		(setq j (+ pos arg)
		      v (if (>= j w)
			    (math-read-expr (substring (car data) pos))
			  (math-read-expr (substring (car data) pos j))))
		(if (eq (car-safe v) 'error)
		    (setq vals v w 0)
		  (setq vals (nconc vals (list v))
			pos j)))))
	(if (string-match "\\` *-?[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]? *\\'"
			  (car data))
	    (setq vals (list 'vec (string-to-number (car data))))
	  (if (and (null arg)
		   (string-match "[[{][^][{}]*[]}]" (car data)))
	      (setq pos (match-beginning 0)
		    vals (math-read-expr (math-match-substring (car data) 0)))
	    (let ((s (if (string-match
			  "\\`\\([0-9]+:[ \t]\\)?\\(.*[^, \t]\\)[, \t]*\\'"
			  (car data))
			 (math-match-substring (car data) 2)
		       (car data))))
	      (setq pos -1
		    vals (math-read-expr (concat "[" s "]")))
	      (if (eq (car-safe vals) 'error)
		  (let ((v2 (math-read-expr s)))
		    (unless (eq (car-safe v2) 'error)
		      (setq vals (list 'vec v2)))))))))
      (if (eq (car-safe vals) 'error)
	  (progn
	    (if calc-was-started
		(pop-to-buffer from-buffer)
	      (calc-quit t)
	      (switch-to-buffer from-buffer))
	    (goto-char top)
	    (forward-line lnum)
	    (forward-char (+ (nth 1 vals) (min col1 col2) pos))
	    (error (nth 2 vals))))
      (unless (equal vals '(vec))
	(setq mat (cons vals mat)))
      (setq data (cdr data)
	    lnum (1+ lnum)))
    (calc-slow-wrapper
     (if reduce
	 (calc-enter-result 0 "grb+" (list reduce '(var add var-add)
					   (nreverse mat)))
       (calc-enter-result 0 "grab" (nreverse mat))))))


(defun calc-copy-to-buffer (nn)
  "Copy the top of stack into an editing buffer."
  (interactive "P")
  (let ((thebuf (and (not (memq major-mode '(calc-mode calc-trail-mode)))
		     (current-buffer)))
	(movept nil)
	oldbuf newbuf)
    (calc-wrapper
     (save-excursion
       (calc-force-refresh)
       (let ((n (prefix-numeric-value nn))
	     (eat-lnums calc-line-numbering)
	     (big-offset (if (eq calc-language 'big) 1 0))
	     top bot)
	 (setq oldbuf (current-buffer)
	       newbuf (or thebuf
			  (calc-find-writable-buffer (buffer-list) 0)
			  (calc-find-writable-buffer (buffer-list) 1)
			  (error "No other buffer")))
	 (cond ((and (or (null nn)
			 (consp nn))
		     (= (calc-substack-height 0)
			(- (1- (calc-substack-height 1)) big-offset)))
		(calc-cursor-stack-index 1)
		(if (looking-at
		     (if calc-line-numbering "[0-9]+: *[^ \n]" " *[^ \n]"))
		    (goto-char (1- (match-end 0))))
		(setq eat-lnums nil
		      top (point))
		(calc-cursor-stack-index 0)
		(setq bot (- (1- (point)) big-offset)))
	       ((> n 0)
		(calc-cursor-stack-index n)
		(setq top (point))
		(calc-cursor-stack-index 0)
		(setq bot (- (point) big-offset)))
	       ((< n 0)
		(calc-cursor-stack-index (- n))
		(setq top (point))
		(calc-cursor-stack-index (1- (- n)))
		(setq bot (point)))
	       (t
		(goto-char (point-min))
		(forward-line 1)
		(setq top (point))
		(calc-cursor-stack-index 0)
		(setq bot (point))))
	 (with-current-buffer newbuf
	   (if (consp nn)
	       (kill-region (region-beginning) (region-end)))
	   (push-mark (point) t)
	   (if (and overwrite-mode (not (consp nn)))
	       (calc-overwrite-string (with-current-buffer oldbuf
					(buffer-substring top bot))
				      eat-lnums)
	     (or (bolp) (setq eat-lnums nil))
	     (insert-buffer-substring oldbuf top bot)
	     (and eat-lnums
		  (let ((n 1))
		    (while (and (> (point) (mark))
				(progn
				  (forward-line -1)
				  (>= (point) (mark))))
		      (delete-char 4)
		      (setq n (1+ n)))
		    (forward-line n))))
	   (when thebuf
	     (setq movept (point)))
	   (when (get-buffer-window (current-buffer))
	     (set-window-point (get-buffer-window (current-buffer))
			       (point)))))))
    (when movept
      (goto-char movept))
    (when (and (consp nn)
	       (not thebuf))
      (calc-quit t)
      (switch-to-buffer newbuf))))

(defun calc-overwrite-string (str eat-lnums)
  (when (string-match "\n\\'" str)
    (setq str (substring str 0 -1)))
  (when eat-lnums
    (setq str (substring str 4)))
  (if (and (string-match "\\`[-+]?[0-9.]+\\(e-?[0-9]+\\)?\\'" str)
	   (looking-at "[-+]?[0-9.]+\\(e-?[0-9]+\\)?"))
      (progn
	(delete-region (point) (match-end 0))
	(insert str))
    (let ((i 0))
      (while (< i (length str))
	(if (= (setq last-command-event (aref str i)) ?\n)
	    (or (= i (1- (length str)))
		(let ((pt (point)))
		  (end-of-line)
		  (delete-region pt (point))
		  (if (eobp)
		      (insert "\n")
		    (forward-char 1))
		  (if eat-lnums (setq i (+ i 4)))))
	  (self-insert-command 1))
	(setq i (1+ i))))))

;; First, require that buffer is visible and does not begin with "*"
;; Second, require only that it not begin with "*Calc"
(defun calc-find-writable-buffer (buf mode)
  (and buf
       (if (or (string-match "\\`\\( .*\\|\\*Calc.*\\)"
			     (buffer-name (car buf)))
	       (and (= mode 0)
		    (or (string-match "\\`\\*.*" (buffer-name (car buf)))
			(not (get-buffer-window (car buf))))))
	   (calc-find-writable-buffer (cdr buf) mode)
	 (car buf))))


(defun calc-edit (n)
  (interactive "p")
  (calc-slow-wrapper
   (when (eq n 0)
     (setq n (calc-stack-size)))
   (let* ((flag nil)
	  (allow-ret (> n 1))
	  (list (math-showing-full-precision
		 (mapcar (if (> n 1)
			     (function (lambda (x)
					 (math-format-flat-expr x 0)))
			   (function
			    (lambda (x)
			      (if (math-vectorp x) (setq allow-ret t))
			      (math-format-nice-expr x (frame-width)))))
			 (if (> n 0)
			     (calc-top-list n)
			   (calc-top-list 1 (- n)))))))
     (calc-edit-mode (list 'calc-finish-stack-edit (or flag n)) allow-ret)
     (while list
       (insert (car list) "\n")
       (setq list (cdr list)))))
  (calc-show-edit-buffer))

(defun calc-alg-edit (str)
  (calc-edit-mode '(calc-finish-stack-edit 0))
  (calc-show-edit-buffer)
  (insert str "\n")
  (backward-char 1)
  (calc-set-command-flag 'do-edit))

(defvar calc-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'calc-edit-finish)
    (define-key map "\r" 'calc-edit-return)
    (define-key map "\C-c\C-c" 'calc-edit-finish)
    map)
  "Keymap for use by the calc-edit command.")

(defvar calc-original-buffer)
(defvar calc-return-buffer)
(defvar calc-one-window)
(defvar calc-edit-handler)
(defvar calc-restore-trail)
(defvar calc-allow-ret)
(defvar calc-edit-top)

(defun calc-edit-mode (&optional handler allow-ret title)
  "Calculator editing mode.  Press RET, LFD, or C-c C-c to finish.
To cancel the edit, simply kill the *Calc Edit* buffer."
  (interactive)
  (unless handler
    (error "This command can be used only indirectly through calc-edit"))
  (let ((oldbuf (current-buffer))
	(buf (get-buffer-create "*Calc Edit*")))
    (set-buffer buf)
    (kill-all-local-variables)
    (use-local-map calc-edit-mode-map)
    (setq buffer-read-only nil)
    (setq truncate-lines nil)
    (setq major-mode 'calc-edit-mode)
    (setq mode-name "Calc Edit")
    (run-mode-hooks 'calc-edit-mode-hook)
    (make-local-variable 'calc-original-buffer)
    (setq calc-original-buffer oldbuf)
    (make-local-variable 'calc-return-buffer)
    (setq calc-return-buffer oldbuf)
    (make-local-variable 'calc-one-window)
    (setq calc-one-window (and (one-window-p t) pop-up-windows))
    (make-local-variable 'calc-edit-handler)
    (setq calc-edit-handler handler)
    (make-local-variable 'calc-restore-trail)
    (setq calc-restore-trail (get-buffer-window (calc-trail-buffer)))
    (make-local-variable 'calc-allow-ret)
    (setq calc-allow-ret allow-ret)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (add-hook 'kill-buffer-hook (lambda ()
                                  (let ((calc-edit-handler nil))
                                    (calc-edit-finish t))
                                  (message "(Cancelled)")) t t)
    (insert (propertize
             (concat
              (or title title "Calc Edit Mode. ")
              "Press `C-c C-c'"
              (if allow-ret "" " or RET")
              " to finish, `C-x k RET' to cancel.\n\n")
             'font-lock-face 'italic 'read-only t 'rear-nonsticky t 'front-sticky t))
    (make-local-variable 'calc-edit-top)
    (setq calc-edit-top (point))))
(put 'calc-edit-mode 'mode-class 'special)

(defun calc-show-edit-buffer ()
  (let ((buf (current-buffer)))
    (if (and (one-window-p t) pop-up-windows)
	(pop-to-buffer (get-buffer-create "*Calc Edit*"))
      (and calc-embedded-info (get-buffer-window (aref calc-embedded-info 1))
	   (select-window (get-buffer-window (aref calc-embedded-info 1))))
      (switch-to-buffer (get-buffer-create "*Calc Edit*")))
    (setq calc-return-buffer buf)
    (if (and (< (window-width) (frame-width))
	     calc-display-trail)
	(let ((win (get-buffer-window (calc-trail-buffer))))
	  (if win
	      (delete-window win))))
    (set-buffer-modified-p nil)
    (goto-char calc-edit-top)))

(defun calc-edit-return ()
  (interactive)
  (if (and (boundp 'calc-allow-ret) calc-allow-ret)
      (newline)
    (calc-edit-finish)))

;; The variable calc-edit-disp-trail is local to calc-edit finish, but
;; is used by calc-finish-selection-edit and calc-finish-stack-edit.
(defvar calc-edit-disp-trail)

(defun calc-edit-finish (&optional keep)
  "Finish calc-edit mode.  Parse buffer contents and push them on the stack."
  (interactive "P")
  (message "Working...")
  (or (and (boundp 'calc-original-buffer)
	   (boundp 'calc-return-buffer)
	   (boundp 'calc-one-window)
	   (boundp 'calc-edit-handler)
	   (boundp 'calc-restore-trail)
	   (eq major-mode 'calc-edit-mode))
      (error "This command is valid only in buffers created by calc-edit"))
  (let ((buf (current-buffer))
	(original calc-original-buffer)
	(return calc-return-buffer)
	(one-window calc-one-window)
	(calc-edit-disp-trail calc-restore-trail))
    (save-excursion
      (when (or (null (buffer-name original))
		(progn
		  (set-buffer original)
		  (not (eq major-mode 'calc-mode))))
	(error "Original calculator buffer has been corrupted")))
    (goto-char calc-edit-top)
    (if (buffer-modified-p)
	(eval calc-edit-handler))
    (if (and one-window (not (one-window-p t)))
	(delete-window))
    (if (get-buffer-window return)
	(select-window (get-buffer-window return))
      (switch-to-buffer return))
    (if keep
	(bury-buffer buf)
      (kill-buffer buf))
    (if calc-edit-disp-trail
	(calc-wrapper
	 (calc-trail-display 1 t)))
    (message "")))

(defun calc-edit-cancel ()
  "Cancel calc-edit mode.  Ignore the Calc Edit buffer and don't change stack."
  (interactive)
  (let ((calc-edit-handler nil))
    (calc-edit-finish))
  (message "(Cancelled)"))

(defun calc-finish-stack-edit (num)
  (let ((buf (current-buffer))
	(str (buffer-substring calc-edit-top (point-max)))
	(start (point))
	pos)
    (if (and (integerp num) (> num 1))
	(while (setq pos (string-match "\n." str))
	  (aset str pos ?\,)))
    (switch-to-buffer calc-original-buffer)
    (let ((vals (let ((calc-language nil)
		      (math-expr-opers (math-standard-ops)))
		  (and (string-match "[^\n\t ]" str)
		       (math-read-exprs str)))))
      (when (eq (car-safe vals) 'error)
	(switch-to-buffer buf)
	(goto-char (+ start (nth 1 vals)))
	(error (nth 2 vals)))
      (calc-wrapper
       (if (symbolp num)
	   (progn
	     (set num (car vals))
	     (calc-refresh-evaltos num))
	 (if calc-edit-disp-trail
	     (calc-trail-display 1 t))
	 (and vals
	      (let ((calc-simplify-mode (if (eq last-command-event ?\C-j)
					    'none
					  calc-simplify-mode)))
		(if (>= num 0)
		    (calc-enter-result num "edit" vals)
		  (calc-enter-result 1 "edit" vals (- num))))))))))

(provide 'calc-yank)

;; Local variables:
;; generated-autoload-file: "calc-loaddefs.el"
;; End:

;;; calc-yank.el ends here
